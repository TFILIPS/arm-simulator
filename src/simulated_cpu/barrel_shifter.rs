use core::panic;
use std::mem::transmute;
use super::{names::{RegNames, FlagNames}, SimulatedCPU};

#[repr(u32)]
#[derive(Debug)]
#[allow(dead_code)]
pub enum ShiftType { LSL, LSR, ASR, ROR, RRX } //RRX is special case of ROR
// allow convertion from u32 to ShiftType
impl From<u32> for ShiftType {
    fn from(value: u32) -> Self {
        const NUM_SHIFTS: u32 = 5;
        if value >= NUM_SHIFTS {
            panic!("Convertion to ShiftType failed! value >= {NUM_SHIFTS}");
        }
        unsafe { transmute(value) }
    }
}

//improvements to be made: inplement bitacces for i32
// be carefull when executing rxx from register: wrong results
impl ShiftType {
    pub fn compute(&self, value: i32, amount: u8, carry: bool) -> (i32, bool) {
        let result: i32 = match self {
            ShiftType::LSL => {
                value.checked_shl(amount as u32).unwrap_or(0)
            },
            ShiftType::LSR => {
                (value as u32).checked_shr(amount as u32).unwrap_or(0) as i32
            },
            ShiftType::ASR => {
                value.checked_shr(amount as u32).unwrap_or(0)
            },
            ShiftType::ROR => {
                value.rotate_right((amount as u32) % 32)
            }
            ShiftType::RRX => {
                let result = ((value as u32) >> 1) as i32;
                if carry { result | 1 << 31 } else { result }
            }
        };

        let new_carry: bool = match self {
            ShiftType::LSL | ShiftType::LSR | ShiftType::ASR 
            if amount > 32 => { 
                false 
            },
            ShiftType::LSL | ShiftType::LSR | ShiftType::ASR | ShiftType::ROR
            if amount == 0 => { 
                carry 
            },
            ShiftType::LSL => {
                ((value >> (32 - amount)) & 1) == 1
            }
            ShiftType::LSR | ShiftType::ASR | ShiftType::ROR  => {
                ((value >> ((amount - 1) % 32)) & 1) == 1
            },
            ShiftType::RRX => {
                value & 1 == 1
            }
        };

        (result, new_carry)
    }
}

#[derive(Debug)]
pub enum ShifterOperand {
    ImmediateShift { shift_amount: u8, shift: ShiftType, rm: RegNames },
    RegisterShift { rs: RegNames, shift: ShiftType, rm: RegNames },
    Immediate { rotate: u8, immediate: u8 }   
}

impl SimulatedCPU {
    // returns shifted value + carry flag
    pub(super) fn perform_shift(&self, so: ShifterOperand) -> (i32, bool) {
        let carry = self.flags[FlagNames::C];

        match so {
            ShifterOperand::ImmediateShift { shift_amount, shift, rm } => {
                let value: i32 = self.get_register(rm);

                if let (ShiftType::ROR, 0) = (&shift, shift_amount) {
                    ShiftType::RRX.compute(value, 0, carry)
                }
                else {
                    shift.compute(value, shift_amount, carry)
                }
            },
            ShifterOperand::RegisterShift { rs, shift, rm } => {
                let value: i32 = self.get_register(rm);
                let amount: u8 = self.get_register(rs) as u8;
                shift.compute(value, amount, carry)
            },
            ShifterOperand::Immediate { rotate, immediate } => {
                ShiftType::ROR.compute(immediate as i32, rotate * 2, carry)
            },
        }
    } 
}