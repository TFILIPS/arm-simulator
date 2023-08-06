use std::{ops::Range, iter::StepBy, fmt::Display};

use crate::utils::BitAccess;

use super::{SimulatedCPU, ARMv5CPU, names::{RegNames, FlagNames}};
use barrel_shifter::ShiftType;

pub mod barrel_shifter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShifterOperand {
    ImmediateShift { shift_amount: u8, shift: ShiftType, rm: RegNames },
    RegisterShift { rs: RegNames, shift: ShiftType, rm: RegNames },
    Immediate { rotate: u8, immediate: u8 }   
}
impl Display for ShifterOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShifterOperand::ImmediateShift { shift_amount, shift, rm } => {
                if let ShiftType::LSL = shift {
                    if *shift_amount == 0 {
                        return write!(f, "{:?}", rm);
                    }
                }
                write!(f, "{:?}, {:?} #{shift_amount}", rm, shift)
            },
            ShifterOperand::RegisterShift { rs, shift, rm } => {
                write!(f, "{rm}, {shift} {rs}")
            },
            ShifterOperand::Immediate { rotate, immediate } => {
                let value: i32 = ShiftType::ROR.compute(
                    *immediate as i32, 
                    rotate * 2, false
                ).0;
                write!(f, "#{value}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OffsetType {
    Immediate { offset: u16 },
    // this is basically the same as ScaledRegister with 0 for shift parameters
    Register { rm: RegNames },
    ScaledRegister { shift_imm: u8, shift: ShiftType, rm: RegNames }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AddressingMode {
    pub p: bool, pub u: bool, pub w: bool, pub rn: RegNames,
    pub offset_type: OffsetType
}
impl Display for AddressingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let offset_string: String = match self.offset_type {
            OffsetType::Immediate { offset } => {
                if offset != 0 {
                    format!(", #{}{offset}", if self.u { "" } else { "-" })
                }
                else {String::new()}
            },
            OffsetType::Register { rm } => format!(", {rm}"),
            OffsetType::ScaledRegister { shift_imm, shift, rm } => {
                format!(", {rm}, {shift} #{shift_imm}")
            }
        };
        let rn: RegNames = self.rn;
        if self.p {
            if self.w { write!(f,"[{rn}{offset_string}]!") }
            else { write!(f,"[{rn}{offset_string}]")}
        }
        else { write!(f,"[{rn}]{offset_string}") }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AddressingModeMultiple { 
    pub p: bool, pub u: bool, pub w: bool, 
    pub rn: RegNames, pub register_list: u16
}
impl Display for AddressingModeMultiple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut registers: Vec<String> = Vec::new();
        for i in 0..16 {
            if self.register_list.get_bit(i) {
                registers.push(format!("{}", RegNames::from(i as u32)));
            }
        }
        write!(f, "{0}{1} {2}{3}, {{{4}}}",
            if self.u { "I" } else { "D" },
            if self.p { "B" } else { "A" },
            self.rn, 
            if self.w { "!" } else { "" },
            registers.join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BranchOperator {
    Register(RegNames),
    Offset(i32, bool),
}

impl ARMv5CPU {
    // returns shifted value + possibly new carry flag
    pub fn perform_shift(&self, so: ShifterOperand) -> (i32, bool) {
        let carry = self.flags[FlagNames::C];
        match so {
            ShifterOperand::ImmediateShift { shift_amount, shift, rm } => {
                let value: i32 = self.get_register_intern(rm);
                if let (ShiftType::ROR, 0) = (&shift, shift_amount) {
                    ShiftType::RRX.compute(value, 0, carry)
                }
                else {
                    shift.compute(value, shift_amount, carry)
                }
            },
            ShifterOperand::RegisterShift { rs, shift, rm } => {
                let value: i32 = self.get_register_intern(rm);
                let amount: u8 = self.get_register_intern(rs) as u8;
                shift.compute(value, amount, carry)
            },
            ShifterOperand::Immediate { rotate, immediate } => {
                ShiftType::ROR.compute(immediate as i32, rotate * 2, carry)
            }
        }
    }

    pub fn compute_modify_address(&mut self, am: AddressingMode) -> u32 {
        let offset: u32 = match am.offset_type {
            OffsetType::Immediate { offset } => {
                offset as u32
            },
            OffsetType::Register { rm } => {
                self.get_register_intern(rm) as u32
            },
            OffsetType::ScaledRegister { shift_imm, shift, rm } => {
                let value: i32 = self.get_register_intern(rm);
                let carry: bool = self.flags[FlagNames::C];
                
                if let (ShiftType::ROR, 0) = (&shift, shift_imm) {
                    ShiftType::RRX.compute(value, 0, carry).0 as u32
                }
                else {
                    shift.compute(value, shift_imm, carry).0 as u32
                }
            }
        };

        let op = if am.u {u32::wrapping_add} else {u32::wrapping_sub};
        
        let address: u32;
        if am.p {
            address = op(self.get_register_intern(am.rn) as u32, offset);
            if am.w {
                self.set_register(am.rn, address as i32);
            }
        }
        else {
            address = self.get_register_intern(am.rn) as u32;
            self.set_register(am.rn, op(address, offset) as i32);
        }
        address
    }

    pub fn compute_modify_address_multiple(
        &mut self, amm: &AddressingModeMultiple
    ) -> StepBy<Range<u32>> {
        let base_address: u32 = self.get_register_intern(amm.rn) as u32;
        
        let op = if amm.u {u32::wrapping_add} else {u32::wrapping_sub};
        let num_regs: u32 = amm.register_list.count_ones();
        let new_address: u32 = op(base_address, num_regs * 4);
        if amm.w {
            self.set_register(amm.rn, new_address as i32);
        }

        let mut range: Range<u32> = if amm.u {
            base_address..new_address
        } 
        else {
            new_address+4..base_address+4
        };

        if amm.p {
            range.start = op(range.start, 4);
            range.end = op(range.end, 4);
        }

        range.step_by(4)
    }
}

#[cfg(test)]
mod tests {
    use crate::simulated_cpu::{
        SimulatedCPU, ARMv5CPU, names::{FlagNames, RegNames}, 
        operands::{OffsetType, AddressingModeMultiple}
    };
    use crate::utils::{ConsoleOutput, ConsoleExit, T, F};
    use super::{ShifterOperand, barrel_shifter::ShiftType, AddressingMode};

    macro_rules! shifter_operand_tests {
        ($($test_name:ident: $test_values:expr), *) 
        => {$(
            #[test]
            fn $test_name() {
                #[allow(overflowing_literals)]
                let (so, val_in, shift, c_in, val_out, c_out) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);

                match so {
                    ShifterOperand::ImmediateShift { rm, .. } => {
                        cpu.set_register(rm, val_in);
                    },
                    ShifterOperand::RegisterShift { rm, rs, .. } => {
                        cpu.set_register(rm, val_in);
                        cpu.set_register(rs, shift);
                    }
                    _ => {}
                }
                cpu.set_flag(FlagNames::C, c_in);

                let (res_val, res_c) = cpu.perform_shift(so);

                assert_eq!(val_out, res_val as u32);
                assert_eq!(c_out, res_c);
            }
        )*}
    }

    shifter_operand_tests! {
        shifter_operand_test_1: (
            ShifterOperand::Immediate { 
                rotate: 11, immediate: 197 
            }, 
            0, 0, F, 0x0003_1400, F
        ),
        shifter_operand_test_2: (
            ShifterOperand::ImmediateShift { 
                shift_amount: 26, shift: ShiftType::LSR, rm: RegNames::R7 
            }, 
            0xCF2A_C100, 0, F, 51, T
        ),
        shifter_operand_test_3: (
            ShifterOperand::RegisterShift { 
                rs: RegNames::LR, shift: ShiftType::ROR, rm: RegNames::LR 
            }, 
            0x8A9B_DAEC, 0x8A9B_DAEC, F, 0xAEC8A9BD, T
        ),
        shifter_operand_test_4: (
            ShifterOperand::RegisterShift { 
                rs: RegNames::R2, shift: ShiftType::ASR, rm: RegNames::R11 
            }, 
            0x80D3_1E6E, 0, T, 0x80D3_1E6E, T
        ),
        shifter_operand_test_5: (
            ShifterOperand::ImmediateShift { 
                shift_amount: 0, shift: ShiftType::ROR, rm: RegNames::R1
            }, 
            0x7298_6CBC, 0, T, 0xB94C365E, F
        )
    }

    
    macro_rules! addressing_mode_tests {
        ($($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                #[allow(overflowing_literals)]
                let (am, addr_in, offset_in, addr_out, addr_mem) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_register(am.rn, addr_in);

                match am.offset_type {
                    OffsetType::Register { rm } => {
                        cpu.set_register(rm, offset_in);
                    },
                    OffsetType::ScaledRegister { rm, .. } => {
                        cpu.set_register(rm, offset_in);
                    },
                    _ => {}
                }

                let res_addr = cpu.compute_modify_address(am);

                assert_eq!(addr_out, res_addr as u32);
                assert_eq!(addr_mem, cpu.get_register(am.rn) as u32);
            }
        )*}
    }

    addressing_mode_tests! {
        addressing_mode_test_1: (
            AddressingMode {
                p: T, u: T, w: F, rn: RegNames::R5,
                offset_type: OffsetType::Immediate {
                    offset: 0x40A6
                }
            }, 0x0267_B018, 0x0000_0000, 0x0267_F0BE, 0x0267_B018
        ),
        addressing_mode_test_2: (
            AddressingMode {
                p: T, u: F, w: F, rn: RegNames::R8,
                offset_type: OffsetType::Register { 
                    rm: RegNames::LR
                }
            }, 0x0196_608F, 0x0009_4AB9, 0x018D_15D6, 0x0196_608F
        ),
        addressing_mode_test_3: (
            AddressingMode {
                p: F, u: T, w: T, rn: RegNames::PC,
                offset_type: OffsetType::ScaledRegister { 
                    shift_imm: 11, shift: ShiftType::LSR, 
                    rm: RegNames::R4 
                }
            }, 0x01F1_7621, 0xFF9F_14FF, 0x01F1_7629, 0x0211_6A0B
        ),
        addressing_mode_test_4: (
            AddressingMode {
                p: T, u: F, w: T, rn: RegNames::R9,
                offset_type: OffsetType::Immediate {
                    offset: 0x0005
                }
            }, 0x0000_0000, 0x0000_0000, 0xFFFF_FFFB, 0xFFFF_FFFB
        ),
        addressing_mode_test_5: (
            AddressingMode {
                p: F, u: T, w: T, rn: RegNames::R6,
                offset_type: OffsetType::ScaledRegister { 
                    shift_imm: 7, shift: ShiftType::LSL, 
                    rm: RegNames::PC 
                }
            }, 0xFFFF_FFFF, 0x0000_0002, 0xFFFF_FFFF, 0x0000_04FF
        )
    }


    macro_rules! addressing_mode_multiple_tests {
        ($($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                #[allow(overflowing_literals)]
                let (amm, addr_in, addr_mem, addrs_out) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_register(amm.rn, addr_in);

                let res_addrs = cpu.compute_modify_address_multiple(&amm);

                for (i, res_addr) in res_addrs.enumerate() {
                    assert_eq!(addrs_out[i], res_addr, "res_addr_id: {i}");
                }
                assert_eq!(addr_mem, cpu.get_register(amm.rn) as u32);

            }
        )*}
    }

    addressing_mode_multiple_tests! {
        addressing_mode_multiple_test_1: (
            AddressingModeMultiple {
                p: T, u: T, w: F, rn: RegNames::R5,
                register_list: 0x3A46
            }, 0x02A2_E28D, 0x02A2_E28D, [
                0x02A2_E291, 0x02A2_E295, 0x02A2_E299, 0x02A2_E29D,
                0x02A2_E2A1, 0x02A2_E2A5, 0x02A2_E2A9
            ]
        ),
        addressing_mode_multiple_test_2: (
            AddressingModeMultiple {
                p: T, u: F, w: T, rn: RegNames::R0,
                register_list: 0x9E9B
            }, 0x0252_BC7A, 0x0252_BC52, [
                0x0252_BC52, 0x0252_BC56, 0x0252_BC5A, 0x0252_BC5E,
                0x0252_BC62, 0x0252_BC66, 0x0252_BC6A, 0x0252_BC6E,
                0x0252_BC72, 0x0252_BC76
            ]
        ),
        addressing_mode_multiple_test_3: (
            AddressingModeMultiple {
                p: F, u: T, w: F, rn: RegNames::R9,
                register_list: 0x208A
            }, 0x000B_B297, 0x000B_B297, [
                0x000B_B297, 0x000B_B29B, 0x000B_B29F, 0x000B_B2A3
            ]
        ),
        addressing_mode_multiple_test_4: (
            AddressingModeMultiple {
                p: F, u: F, w: T, rn: RegNames::SP,
                register_list: 0x6C03
            }, 0x0043_002B, 0x0043_0013, [
                0x0043_0017, 0x0043_001B, 0x0043_001F, 0x0043_0023, 
                0x0043_0027, 0x0043_002B
            ]
        )
    }
}

