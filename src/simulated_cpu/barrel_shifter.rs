use super::names::RegNames;

pub enum ShiftType { LSL, LSR, ASR, ROR, RRX }
pub enum ShifterOperand {
    ImmediateShift { shift_amount: u8, shift: ShiftType, rm: RegNames },
    RegisterShift { rs: RegNames, shift: ShiftType, rm: RegNames },
    Immediate { rotate: u8, immediate: u8 }   
}