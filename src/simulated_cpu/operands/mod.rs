use std::{ops::Range, iter::StepBy, fmt::Display};

use super::{names::{RegNames, FlagNames}, SimulatedCPU};
use barrel_shifter::ShiftType;

pub mod barrel_shifter;

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    Immediate { p: bool, u: bool, w: bool, offset: u16 },
    // this is basically the same as ScaledRegister with 0 for shift parameters
    Register { p: bool, u: bool, w: bool, rm: RegNames },
    ScaledRegister { p: bool, u: bool, w: bool, 
        shift_imm: u8, shift: ShiftType, rm: RegNames }
}

#[derive(Debug, Clone, Copy)]
pub struct AddressingModeMultiple { 
    pub(super) p: bool, pub(super) u: bool, pub(super) w: bool, 
    pub(super) rn: RegNames, pub(super) register_list: u16
}

impl SimulatedCPU {
    // returns shifted value + possibly new carry flag
    pub(super) fn perform_shift(&self, so: ShifterOperand) -> (i32, bool) {
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

    pub(super) fn compute_modify_address(
        &mut self, rn: RegNames, am: AddressingMode
    ) -> usize {
        let (p, u, w, offset): (bool, bool, bool, u32) = match am {
            AddressingMode::Immediate { p, u, w, offset} => {
                (p, u, w, offset as u32)
            },
            AddressingMode::Register { p, u, w, rm } => {
                (p, u, w, self.get_register_intern(rm) as u32)
            },
            AddressingMode::ScaledRegister { 
                p, u, w, shift_imm, shift, rm 
            } => {
                let value: i32 = self.get_register_intern(rm);
                let carry: bool = self.flags[FlagNames::C];
                
                if let (ShiftType::ROR, 0) = (&shift, shift_imm) {
                    (p, u, w, ShiftType::RRX.compute(value, 0, carry).0 as u32)
                }
                else {
                    (p, u, w, shift.compute(value, shift_imm, carry).0 as u32)
                }
            }
        };

        let op = if u {u32::wrapping_add} else {u32::wrapping_sub};
        
        let address: u32;
        if p {
            address = op(self.get_register_intern(rn) as u32, offset);
            if w {
                self.set_register(rn, address as i32);
            }
        }
        else {
            address = self.get_register_intern(rn) as u32;
            self.set_register(rn, op(address, offset) as i32);
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