use std::{mem::transmute};
use crate::utils::BitAccess;
use super::{SimulatedCPU, names::{FlagNames, RegNames}, barrel_shifter::ShifterOperand};

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



// https://developer.arm.com/documentation/ddi0406/c/Application-Level-Architecture/ARM-Instruction-Set-Encoding/ARM-instruction-set-encoding?lang=en
impl SimulatedCPU {
    pub(super) fn execute_instruction(&mut self, instruction: u32) {
        let cond: Condition = Condition::from_instruction(instruction);
        if !cond.is_satisfied(&self.flags) {
            return;
        }

        if instruction.cut_bits(28..=31) != 0b1111 {
            match instruction.cut_bits(26..=27) {
                0b00 => self.handle_data_and_miscellaneos(instruction),
                0b01 | 0b10 => todo!("Load/store + media"),
                0b11 => todo!("Coprocessor and supervisor calls"),
                _ => ()
            }
        }
        else {
            todo!("Unconditional instructions")
        }
    }

    fn handle_data_and_miscellaneos(&mut self, instruction: u32) {
        let op: bool = instruction.get_bit(25);
        let op1: u32 = instruction.cut_bits(20..=24);
        let op2: u32 = instruction.cut_bits(4..=7);

        let opcode: usize = instruction.cut_bits(21..=24) as usize;
        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();
        let s: bool = instruction.get_bit(20);

        if !op {
            if (op1 & 0b11001) ^ 0b10001 != 0 {
                if (op2 & 0b0001) ^ 0b0000 == 0 {
                    // Data-processing immediate shift
                    SimulatedCPU::DATA_PROCESSIG_INSTRUCTIONS[opcode](
                        self, s, rn, rd, ShifterOperand::ImmediateShift { 
                            shift_amount: instruction.cut_bits(7..=11)
                                            .try_into().unwrap(), 
                            shift: instruction.cut_bits(5..=6).into(), 
                            rm: instruction.cut_bits(0..=3).into()
                        }
                    );
                }
                else if (op2 & 0b1001) ^ 0b0001 == 0 {
                    // Data-processing register shift
                    SimulatedCPU::DATA_PROCESSIG_INSTRUCTIONS[opcode](
                        self, s, rn, rd, ShifterOperand::RegisterShift { 
                            rs: instruction.cut_bits(8..=11).into(), 
                            shift: instruction.cut_bits(5..=6).into(), 
                            rm: instruction.cut_bits(0..=3).into()
                        }
                    );
                }
            }
        }
        else {
            if (op1 & 0b11001) ^ 0b10001 != 0 {
                // Data-processing immediate
                SimulatedCPU::DATA_PROCESSIG_INSTRUCTIONS[opcode](
                    self, s, rn, rd, ShifterOperand::Immediate { 
                        rotate: instruction.cut_bits(8..=11)
                                    .try_into().unwrap(), 
                        immediate: instruction.cut_bits(0..=7)
                                    .try_into().unwrap() 
                    }
                );
            }
        }
    }

    const DATA_PROCESSIG_INSTRUCTIONS: [
        fn(&mut SimulatedCPU, bool, RegNames, RegNames, ShifterOperand); 16
    ] = [
        SimulatedCPU::and, SimulatedCPU::eor, SimulatedCPU::sub,
        SimulatedCPU::rsb, SimulatedCPU::add, SimulatedCPU::adc,
        SimulatedCPU::sbc, SimulatedCPU::rsc, SimulatedCPU::tst,
        SimulatedCPU::teq, SimulatedCPU::cmp, SimulatedCPU::cmn,
        SimulatedCPU::orr, SimulatedCPU::mov, SimulatedCPU::bic,
        SimulatedCPU::mvn
    ];

}