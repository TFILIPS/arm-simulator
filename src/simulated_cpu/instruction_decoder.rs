use std::{mem::transmute};
use crate::utils::BitAccess;
use super::{SimulatedCPU, names::FlagNames};

#[repr(u32)]
#[allow(dead_code)]
enum Condition { 
    EQ, NE, HS, LO, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL, Unconditional
}
impl Condition {
    fn from_instruction(instruction: u32) -> Condition {
        let cond_bits: u32 = instruction.cut_bits(28..32);
        unsafe { transmute(cond_bits) }
    }

    fn is_satisfied(&self, flags: &[bool; 4]) -> bool {
        //let &[n, z, c, v] = flags;
        let n: bool = flags[FlagNames::N];
        let z: bool = flags[FlagNames::Z];
        let c: bool = flags[FlagNames::C];
        let v: bool = flags[FlagNames::V];

        match self {
            Condition::EQ => z,
            Condition::NE => !z,
            Condition::HS => c,
            Condition::LO => !c,
            Condition::MI => n,
            Condition::PL => !n,
            Condition::VS => v,
            Condition::VC => !v,
            Condition::HI => c && !z,
            Condition::LS => !c || z,
            Condition::GE => n == v,
            Condition::LT => n != v,
            Condition::GT => !z && (n == v),
            Condition::LE => z || (n != v),
            _ => true
        }
    }
}

impl SimulatedCPU {
    pub(super) fn execute_instruction(&self, instruction: u32) {
        let cond: Condition = Condition::from_instruction(instruction);
        if !cond.is_satisfied(&self.flags) {
            return;
        }

        if instruction.cut_bits(26..=27) == 0b00 {
            self.execute_data_processig_instruction(instruction);

            //if instruction.cut_bits(24..=25) == 0b00 &&
            //   instruction.cut_bits(4..=7) == 0b1001 && 
            //   instruction.cut_bits() != 0b1111 {
            //    //multiplication functions
            //}
        }
    }

    const DATA_PROCESSIG_INSTRUCTIONS: [fn(&SimulatedCPU); 16] = [
        SimulatedCPU::and, SimulatedCPU::eor, SimulatedCPU::sub, SimulatedCPU::rsb, 
        SimulatedCPU::add, SimulatedCPU::adc, SimulatedCPU::sbc, SimulatedCPU::rsc, 
        SimulatedCPU::tst, SimulatedCPU::teq, SimulatedCPU::cmp, SimulatedCPU::cmn,
        SimulatedCPU::orr, SimulatedCPU::mov, SimulatedCPU::bic, SimulatedCPU::mvn
    ];

    fn execute_data_processig_instruction(&self, instruction: u32) {
        let opcode: usize = instruction.cut_bits(21..=24) as usize;
        SimulatedCPU::DATA_PROCESSIG_INSTRUCTIONS[opcode](self);
    }

}