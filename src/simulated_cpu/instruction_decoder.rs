use std::mem::transmute;

use crate::utils::{BitAccess, T, F};
use super::{
    SimulatedCPU, 
    names::{FlagNames, RegNames}, 
    operands::{ShifterOperand, AddressingMode, AddressingModeMultiple}
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
                0b01 => self.handle_load_store(instruction),
                0b10 => self.handle_branch_and_block_transfer(instruction),
                0b11 => self.handle_copro_and_swi(instruction),
                _ => ()
            }
        }
        else {
            todo!("Unconditional instructions")
        }
    }

    fn handle_branch_and_block_transfer(&mut self, instruction: u32) {
        if instruction.get_bit(25) {
            let mut imm: i32 = instruction.cut_bits(0..24) as i32;
            imm = (imm << 8) >> 8; //sign extend
            self.b(instruction.get_bit(24), imm);
            return;
        }
            
        let amm: AddressingModeMultiple = AddressingModeMultiple { 
            p: instruction.get_bit(24),
            u: instruction.get_bit(23),
            w: instruction.get_bit(21), 
            rn: instruction.cut_bits(16..=19).into(), 
            register_list: instruction.cut_bits(0..=15) as u16
        };
        if instruction.get_bit(20) { self.ldm(amm); } else { self.stm(amm); }
    }

    fn handle_load_store(&mut self, instruction: u32) {
        let p: bool = instruction.get_bit(24);
        let u: bool = instruction.get_bit(23);
        let w: bool = instruction.get_bit(21);

        let am: AddressingMode = if instruction.get_bit(25) {
            if instruction.get_bit(4) {
                //media instructions
                panic!("Not supported in ARMv5!");
            }
            AddressingMode::ScaledRegister { 
                p, u, w, 
                shift_imm: instruction.cut_bits(7..=11) as u8, 
                shift: instruction.cut_bits(5..=6).into(), 
                rm: instruction.cut_bits(0..=3).into() 
            }
        }
        else {
            AddressingMode::Immediate { 
                p, u, w, 
                offset: instruction.cut_bits(0..=11) as u16
            }
        };

        let l: bool = instruction.get_bit(20);
        let b: bool = instruction.get_bit(22);

        let function = match (p, b, w, l) {
            (F, F, T, F) => SimulatedCPU::strt,
            (_, F, _, F) => SimulatedCPU::str,
            (F, F, T, T) => SimulatedCPU::ldrt,
            (_, F, _, T) => SimulatedCPU::ldr,
            (F, T, T, F) => SimulatedCPU::strbt,
            (_, T, _, F) => SimulatedCPU::strb,
            (F, T, T, T) => SimulatedCPU::ldrbt,
            (_, T, _, T) => SimulatedCPU::ldrb
        };

        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();
        function(self, rn, rd, am);
    }



    fn _handle_unconditional_instructions(&mut self, _instruction: u32) {

    }

    fn handle_copro_and_swi(&mut self, instruction: u32) {
        //works at the moment, need to be expanded for cop calls
        if instruction.cut_bits(24..=25) == 0b11 { self.swi(); }
        else { panic!("Unsupported at the moment!"); }
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
                    return;
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
                    return;
                }
            }
            else {
                if bm(op2, 0b1000, 0b0000) {
                    self.handle_miscellaneos(instruction);
                    return;
                }
            }

            if bm(op2, 0b1111, 0b1001) {
                if bm(op1, 0b10000, 0b00000) {
                    self.handle_multiply(instruction);
                    return;
                }
                else if bm(op1, 0b10000, 0b10000) {
                    self.handle_synchronization(instruction);
                    return;
                }
            }
            else if bm(op2, 0b1111, 0b1011) || bm(op2, 0b1101, 0b1101) {
                if !bm(op1, 0b10010, 0b00010) {
                    self.handle_extra_load_store(instruction);
                    return;
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
                return;
            }
            else if bm(op1, 0b11011, 0b10010) {
                self.msr();
                return;
            }
        }
        panic!("Undefined in ARMv5!")
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
            (0b001, 0b01) => self.bx(instruction.cut_bits(0..=3).into()),
            (0b001, 0b11) => self.clz(
                instruction.cut_bits(12..=15).into(),
                instruction.cut_bits(0..=3).into()
            ),
            (0b011, 0b01) => self.blx(), //register variant
            (0b111, 0b01) => self.bkpt(),
            (0b111, 0b11) | (0b010, 0b01) | (0b101, _) | 
            (0b110, 0b11) | (0b111, 0b10) => panic!("Not supported by ARMv5!"),
            _ => panic!("Undefined in ARMv5!")
        }
    }

    fn handle_multiply(&mut self, instruction: u32) {
        let op: u32 = instruction.cut_bits(21..=23);
        let x: bool = instruction.get_bit(20);

        let s: bool = instruction.get_bit(20);
        let rd_rdhi: RegNames = instruction.cut_bits(16..=19).into();
        let rn_rdlo: RegNames = instruction.cut_bits(12..=15).into();
        let rs: RegNames = instruction.cut_bits(8..=11).into();
        let rm: RegNames = instruction.cut_bits(0..=3).into();

        // possibility to create array 
        match op {
            0b000 => self.mul(s, rd_rdhi, rn_rdlo, rs, rm),
            0b001 => self.mla(s, rd_rdhi, rn_rdlo, rs, rm),
            0b100 => self.umull(s, rd_rdhi, rn_rdlo, rs, rm),
            0b101 => self.umlal(s, rd_rdhi, rn_rdlo, rs, rm),
            0b110 => self.smull(s, rd_rdhi, rn_rdlo, rs, rm),
            0b111 => self.smlal(s, rd_rdhi, rn_rdlo, rs, rm),
            0b010 | 0b011 if !x => panic!("Not supported by ARMv5!"),
            _ => panic!("Undefined in ARMv5!")
        }
    }

    fn handle_synchronization(&mut self, instruction: u32) {
        if instruction.get_bit(23) {
            panic!("Not supported by ARMv5!");
        }

        if instruction.cut_bits(20..=21) == 0 {
            //maybe combine swp and swpb to one function lets first look at str and ldr
            let rn: RegNames = instruction.cut_bits(16..=19).into();
            let rd: RegNames = instruction.cut_bits(12..=15).into();
            let rm: RegNames = instruction.cut_bits(0..=3).into();

            if instruction.get_bit(22) {
                self.swpb(rn, rd, rm);
            }
            else {
                self.swp(rn, rd, rm);
            }
        }
        else {
            panic!("Undefined in ARMv5!")
        }
    }

    fn handle_extra_load_store(&mut self, instruction: u32) {
        let p: bool = instruction.get_bit(24);
        let u: bool = instruction.get_bit(23);
        let w: bool = instruction.get_bit(21);

        let am = if instruction.get_bit(24) { //register not immeidate
            AddressingMode::Register { p, u, w, 
                rm: instruction.cut_bits(0..=3).into()
            }
        }
        else {
            AddressingMode::Immediate { p, u, w, 
                offset: (instruction.cut_bits(8..=11) << 4 
                    + instruction.cut_bits(0..=3)) as u16
            }
        };

        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();

        let op2: u32 = instruction.cut_bits(5..=6);

        if instruction.get_bit(20) { //load not store
            match op2 {
                0b01 => self.ldrh(rn, rd, am),
                0b10 => self.ldrsb(rn, rd, am),
                0b11 => self.ldrsh(rn, rd, am),
                _ => ()
            }
        }
        else if op2 == 0b01 {
            self.strh(rn, rd, am)
        }
        else {
            panic!("Not supported in ARMv5!");
        }
    }
}

//maybe trait for u32 or macro (so compiler inlines it)
fn bm(value: u32, mask: u32, expectation: u32) -> bool {
    (value & mask) ^ expectation == 0
}