use std::{ops::Range, iter::StepBy, fmt::Display};

use crate::utils::BitAccess;

use super::{SimulatedCPU, ARMv5CPU, names::{RegNames, FlagNames}};
use barrel_shifter::ShiftType;

pub mod barrel_shifter;

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
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



#[derive(Clone, Copy)]
pub enum OffsetType {
    Immediate { offset: u16 },
    // this is basically the same as ScaledRegister with 0 for shift parameters
    Register { rm: RegNames },
    ScaledRegister { shift_imm: u8, shift: ShiftType, rm: RegNames }
}

#[derive(Clone, Copy)]
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

    pub fn compute_modify_address(&mut self, am: AddressingMode) -> usize {
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
        address as usize
    }

    pub fn compute_modify_address_multiple(
        &mut self, amm: &AddressingModeMultiple
    ) -> StepBy<Range<usize>> {
        let base_address: usize = self.get_register_intern(amm.rn) as usize;
        
        let op = if amm.u {usize::wrapping_add} else {usize::wrapping_sub};
        let num_regs: u32 = amm.register_list.count_ones();
        let new_address = op(base_address, (num_regs * 4) as usize);
        if amm.w {
            self.set_register(amm.rn, new_address as i32);
        }

        let mut range: Range<usize> = if amm.u {
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