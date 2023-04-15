use core::panic;
use std::mem::transmute;
use crate::utils::BitAccess;
use super::{
    SimulatedCPU, 
    names::{FlagNames, RegNames}, 
    barrel_shifter::ShifterOperand
};

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
            if !bm(op1, 0b11001, 0b10000) {
                if bm(op2, 0b0001, 0b0000) {
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
                else if bm(op2, 0b1001, 0b0001) {
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
            else {
                if bm(op2, 0b1000, 0b0000) {
                    self.handle_miscellaneos(instruction);
                }
                else if bm(op2, 0b1001, 0b1000) {
                    // Halfword multiply and multiply accumulate
                    panic!("Undefined in ARMv5!")
                }
            }

            if bm(op2, 0b1111, 0b1001) {
                if bm(op1, 0b10000, 0b00000) {
                    self.handle_multiply(instruction);
                }
                else if bm(op1, 0b10000, 0b10000) {
                    self.handle_synchronization(instruction);
                }
            }

            if bm(op2, 0b10000, 0b00000) || bm(op2, 0b1111, 0b1001) {
                if !bm(op1, 0b10010, 0b00010) {
                    // Extra load/store instructions
                }
                else {
                    // Extra load/store instructions, unprivileged
                }
            }
        }
        else {
            if !bm(op1, 0b11001, 0b10000) {
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
            else if bm(op1, 0b11111, 0b10000) {
                //16-bit immediate load, MOV (immediate)
            }
            else if bm(op1, 0b11111, 0b10100) {
                //High halfword 16-bit immediate load, MOVT
            }
            else if bm(op1, 0b11011, 0b10010) {
                // MSR (immediate), and hints
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

    fn handle_miscellaneos(&mut self, instruction: u32) {
        let op: u32 = instruction.cut_bits(21..=22);
        let op2: u32 = instruction.cut_bits(4..=6);

        match (op2, op) {
            (0b000, 0b00 | 0b10) => self.mrs(),
            (0b000, 0b01 | 0b11) => self.msr(),
            (0b001, 0b01) => self.bx(),
            (0b001, 0b11) => self.clz(),
            (0b011, 0b01) => self.blx(),
            (0b111, 0b01) => self.bkpt(),
            (0b111, 0b11) | (0b010, 0b01) | (0b101, _) | 
            (0b110, 0b11) | (0b111, 0b10) => panic!("Not supported by ARMv5!"),
            _ => panic!("Undefined in ARMv5!")
        }
    }

    fn handle_multiply(&mut self, instruction: u32) {
        let op: u32 = instruction.cut_bits(21..=23);
        let x: bool = instruction.get_bit(20);

        match op {
            0b000 => self.mul(),
            0b001 => self.mla(),
            0b100 => self.umull(),
            0b101 => self.umlal(),
            0b110 => self.smull(),
            0b111 => self.smlal(),
            0b010 | 0b011 if !x => panic!("Not supported by ARMv5"),
            _ => panic!("Undefined")
        }
    }

    fn handle_synchronization(&mut self, instruction: u32) {
        if instruction.get_bit(23) {
            panic!("Not supported by ARMv5");
        }

        if bm(instruction.cut_bits(20..=21), 0b11, 0b00) {
            //maybe combine swp and swpb to one function lets first look at str and ldr
            if instruction.get_bit(22) {
                self.swpb();
            }
            else {
                self.swp();
            }
        }
        else {
            panic!("Undefined")
        }
    }
}

//maybe trait for u32 or macro (so compiler inlines it)
fn bm(value: u32, mask: u32, expectation: u32) -> bool {
    (value & mask) ^ expectation == 0
}