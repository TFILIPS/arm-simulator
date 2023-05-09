use crate::utils::{BitAccess, T, F};
use super::{
    SimulatedCPU, ARMv5CPU, names::RegNames, instructions::*,
    operands::{
        ShifterOperand, AddressingMode, OffsetType, AddressingModeMultiple
    }
};

pub trait InstructionDecoder<I: Instruction<C, S>, C: SimulatedCPU<S>, S> {
    fn decode(instruction_bits: u32) -> I;
}

pub struct ARMv5Decoder;
impl InstructionDecoder<ARMv5Instruction, ARMv5CPU, i32> for ARMv5Decoder {
    fn decode(instruction_bits: u32) -> ARMv5Instruction {
        //shorten variable name
        let inst_bits: u32 = instruction_bits;
        if inst_bits.cut_bits(28..=31) != 0b1111 {
            match inst_bits.cut_bits(26..=27) {
                0b00 => ARMv5Decoder::data_and_miscellaneos(inst_bits),
                0b01 => ARMv5Decoder::load_store(inst_bits),
                0b10 => ARMv5Decoder::branch_and_block_transfer(inst_bits),
                0b11 => ARMv5Decoder::copro_and_swi(inst_bits),
                _ => panic!("Unreachable code!")
            }
        }
        else {
            todo!("Unconditional instructions")
        }
    }
}
impl ARMv5Decoder {
    const UNDEFINED_INSTRUCTION: ARMv5Instruction = ARMv5Instruction{
        condition: Condition::AL,
        instruction_type: ARMv5InstructionType::Undefined
    };

    fn data_and_miscellaneos(inst_bits: u32) -> ARMv5Instruction {
        let op: bool = inst_bits.get_bit(25);
        let op1: u32 = inst_bits.cut_bits(20..=24);
        let op2: u32 = inst_bits.cut_bits(4..=7);

        if !op {
            if !bm(op1, 0b11001, 0b10000) {
                if bm(op2, 0b0001, 0b0000) {
                    let so: ShifterOperand = ShifterOperand::ImmediateShift { 
                        shift_amount: inst_bits.cut_bits(7..=11)
                            .try_into().unwrap(), 
                        shift: inst_bits.cut_bits(5..=6).into(), 
                        rm: inst_bits.cut_bits(0..=3).into()
                    };
                    return ARMv5Decoder::data(inst_bits, so);
                }
                else if bm(op2, 0b1001, 0b0001) {
                    let so: ShifterOperand = ShifterOperand::RegisterShift { 
                        rs: inst_bits.cut_bits(8..=11).into(), 
                        shift: inst_bits.cut_bits(5..=6).into(), 
                        rm: inst_bits.cut_bits(0..=3).into()
                    };
                    return ARMv5Decoder::data(inst_bits, so);
                }
            }
            else if bm(op2, 0b1000, 0b0000) {
                return ARMv5Decoder::miscellaneos(inst_bits);
            }

            if bm(op2, 0b1111, 0b1001) {
                if bm(op1, 0b10000, 0b00000) {
                    return ARMv5Decoder::multiply(inst_bits);
                }
                else if bm(op1, 0b10000, 0b10000) {
                    return ARMv5Decoder::synchronization(inst_bits);
                }
            }
            else if bm(op2, 0b1111, 0b1011) || bm(op2, 0b1101, 0b1101) {
                if !bm(op1, 0b10010, 0b00010) {
                    return ARMv5Decoder::extra_load_store(inst_bits);
                }
            }
        }
        else {
            if !bm(op1, 0b11001, 0b10000) {
                let so: ShifterOperand = ShifterOperand::Immediate { 
                    rotate: inst_bits.cut_bits(8..=11).try_into().unwrap(), 
                    immediate: inst_bits.cut_bits(0..=7).try_into().unwrap() 
                };
                return ARMv5Decoder::data(inst_bits, so);
            }
            else if bm(op1, 0b11011, 0b10010) {
                return ARMv5Instruction {
                    condition: Condition::from_instruction(inst_bits),
                    instruction_type: ARMv5InstructionType::Generic { 
                        op: ARMv5GenericOperation::MSR
                    }
                };
            }
        }
        ARMv5Decoder::UNDEFINED_INSTRUCTION
    }

    fn data(inst_bits: u32, so: ShifterOperand) -> ARMv5Instruction {
        let opcode: u32 = inst_bits.cut_bits(21..=24);
        let rn: RegNames = inst_bits.cut_bits(16..=19).into();
        let rd: RegNames = inst_bits.cut_bits(12..=15).into();
        let s: bool = inst_bits.get_bit(20);

        ARMv5Instruction { 
            condition: Condition::from_instruction(inst_bits),
            instruction_type: ARMv5InstructionType::DataProcessing { 
                op: ARMv5DataProcessingOperation::from_bits(opcode), 
                s, rn, rd, so 
            }
        }
    }

    fn miscellaneos(inst_bits: u32) -> ARMv5Instruction {
        let op: u32 = inst_bits.cut_bits(21..=22);
        let op2: u32 = inst_bits.cut_bits(4..=6);
        
        let rm: RegNames = inst_bits.cut_bits(0..=3).into();
        let rd: RegNames = inst_bits.cut_bits(12..=15).into();
        
        let instruction_type: ARMv5InstructionType = match (op2, op) {
            (0b000, 0b00 | 0b10) => ARMv5InstructionType::Generic { 
                op: ARMv5GenericOperation::MRS
            },
            (0b000, 0b01 | 0b11) => ARMv5InstructionType::Generic { 
                op: ARMv5GenericOperation::MSR
            },
            (0b001, 0b01) => ARMv5InstructionType::Branch {
                op: ARMv5BranchOperation::BX, si: 0, rm
            },
            (0b001, 0b11) => ARMv5InstructionType::Miscellaneous { 
                op: ARMv5MiscellaneousOperation::CLZ, rd, rm 
            },
            (0b011, 0b01) => ARMv5InstructionType::Branch {
                op: ARMv5BranchOperation::BLX, si: 0, rm
            },
            (0b111, 0b01) => ARMv5InstructionType::Generic {
                op: ARMv5GenericOperation::BKPT
            },        
            _ => return ARMv5Decoder::UNDEFINED_INSTRUCTION
        };

        let condition: Condition = Condition::from_instruction(inst_bits);

        ARMv5Instruction { condition,  instruction_type }
    }

    fn multiply(inst_bits: u32) -> ARMv5Instruction {
        let op = match inst_bits.cut_bits(21..=23) {
            0b000 => ARMv5MultiplyOperation::MUL,
            0b001 => ARMv5MultiplyOperation::MLA,
            0b100 => ARMv5MultiplyOperation::UMULL,
            0b101 => ARMv5MultiplyOperation::UMLAL,
            0b110 => ARMv5MultiplyOperation::SMULL,
            0b111 => ARMv5MultiplyOperation::SMLAL,
            _ => return ARMv5Decoder::UNDEFINED_INSTRUCTION
        };

        let condition: Condition = Condition::from_instruction(inst_bits);

        let s: bool = inst_bits.get_bit(20);
        let rd_hi: RegNames = inst_bits.cut_bits(16..=19).into();
        let rn_lo: RegNames = inst_bits.cut_bits(12..=15).into();
        let rs: RegNames = inst_bits.cut_bits(8..=11).into();
        let rm: RegNames = inst_bits.cut_bits(0..=3).into();

        ARMv5Instruction { 
            condition, instruction_type: ARMv5InstructionType::Multiply { 
                op, s, rn_lo, rd_hi, rs, rm
            } 
        }
    }

    fn synchronization(inst_bits: u32) -> ARMv5Instruction {
        if inst_bits.get_bit(23) || inst_bits.cut_bits(20..=21) != 0 {
            return ARMv5Decoder::UNDEFINED_INSTRUCTION;
        }

        //maybe combine swp and swpb to one function lets first look at str and ldr        
        let op: ARMv5SynchronizationOperation = if inst_bits.get_bit(22) {
            ARMv5SynchronizationOperation::SWPB
        }
        else {
            ARMv5SynchronizationOperation::SWP
        };

        let condition: Condition = Condition::from_instruction(inst_bits);
        let rn: RegNames = inst_bits.cut_bits(16..=19).into();
        let rd: RegNames = inst_bits.cut_bits(12..=15).into();
        let rm: RegNames = inst_bits.cut_bits(0..=3).into();

        ARMv5Instruction { 
            condition, instruction_type: ARMv5InstructionType::Synchronization {
                 op, rn, rd, rm 
            }
        }
    }

    fn extra_load_store(inst_bits: u32) -> ARMv5Instruction {
        let op1: bool = inst_bits.get_bit(20);
        let op2: u32 = inst_bits.cut_bits(5..=6);

        let op: ARMv5LoadStoreOperation = if op1 {
            match op2 {
                0b01 => ARMv5LoadStoreOperation::LDRH,
                0b10 => ARMv5LoadStoreOperation::LDRSB,
                0b11 => ARMv5LoadStoreOperation::LDRSH,
                _ => return ARMv5Decoder::UNDEFINED_INSTRUCTION
            }
        }
        else if op2 == 0b01 { ARMv5LoadStoreOperation::STRH }
        else { return ARMv5Decoder::UNDEFINED_INSTRUCTION };

        let offset_type: OffsetType = if inst_bits.get_bit(24) {
            OffsetType::Register { rm: inst_bits.cut_bits(0..=3).into() }
        }
        else {
            OffsetType::Immediate { 
                offset: (inst_bits.cut_bits(8..=11) << 4 
                    + inst_bits.cut_bits(0..=3)) as u16
            }
        };

        let condition: Condition = Condition::from_instruction(inst_bits);

        let p: bool = inst_bits.get_bit(24);
        let u: bool = inst_bits.get_bit(23);
        let w: bool = inst_bits.get_bit(21);

        let rn: RegNames = inst_bits.cut_bits(16..=19).into();
        let rd: RegNames = inst_bits.cut_bits(12..=15).into();

        ARMv5Instruction {
            condition, instruction_type: ARMv5InstructionType::LoadStore { 
                op, rd, am: AddressingMode { p, u, w, rn, offset_type }
            }
        }
    }

    fn load_store(inst_bits: u32) -> ARMv5Instruction {
        let offset_type: OffsetType = if inst_bits.get_bit(25) {
            if inst_bits.get_bit(4) {
                //media instructions
                return ARMv5Decoder::UNDEFINED_INSTRUCTION;
            }
            OffsetType::ScaledRegister { 
                shift_imm: inst_bits.cut_bits(7..=11) as u8, 
                shift: inst_bits.cut_bits(5..=6).into(), 
                rm: inst_bits.cut_bits(0..=3).into() 
            }
        }
        else {
            OffsetType::Immediate { 
                offset: inst_bits.cut_bits(0..=11) as u16
            }
        };

        let p: bool = inst_bits.get_bit(24);
        let u: bool = inst_bits.get_bit(23);
        let w: bool = inst_bits.get_bit(21);
        let l: bool = inst_bits.get_bit(20);
        let b: bool = inst_bits.get_bit(22);

        let op: ARMv5LoadStoreOperation = match (p, b, w, l) {
            (F, F, T, F) => ARMv5LoadStoreOperation::STRT,
            (_, F, _, F) => ARMv5LoadStoreOperation::STR,
            (F, F, T, T) => ARMv5LoadStoreOperation::LDRT,
            (_, F, _, T) => ARMv5LoadStoreOperation::LDR,
            (F, T, T, F) => ARMv5LoadStoreOperation::STRBT,
            (_, T, _, F) => ARMv5LoadStoreOperation::STRB,
            (F, T, T, T) => ARMv5LoadStoreOperation::LDRBT,
            (_, T, _, T) => ARMv5LoadStoreOperation::LDRB
        };

        let condition: Condition = Condition::from_instruction(inst_bits);
        let rn: RegNames = inst_bits.cut_bits(16..=19).into();
        let rd: RegNames = inst_bits.cut_bits(12..=15).into();

        ARMv5Instruction {
            condition, instruction_type: ARMv5InstructionType::LoadStore { 
                op, rd, am: AddressingMode { p, u, w, rn, offset_type }
            }
        }
    }

    fn branch_and_block_transfer(inst_bits: u32) -> ARMv5Instruction {
        let condition: Condition = Condition::from_instruction(inst_bits);
        let instruction_type: ARMv5InstructionType = if inst_bits.get_bit(25) {
            let mut imm: i32 = inst_bits.cut_bits(0..24) as i32;
            imm = (imm << 8) >> 8; //sign extend
            let op: ARMv5BranchOperation = match inst_bits.get_bit(24) {
                true => ARMv5BranchOperation::BL,
                false => ARMv5BranchOperation::B,
            };
            ARMv5InstructionType::Branch { op, si: imm, rm: 0.into() }
        }
        else{
            let amm: AddressingModeMultiple = AddressingModeMultiple { 
                p: inst_bits.get_bit(24),
                u: inst_bits.get_bit(23),
                w: inst_bits.get_bit(21), 
                rn: inst_bits.cut_bits(16..=19).into(), 
                register_list: inst_bits.cut_bits(0..=15) as u16
            };
            let op: ARMv5LoadStoreMultipleOperation = if inst_bits.get_bit(20) {
                ARMv5LoadStoreMultipleOperation::LDM
            }
            else {
                ARMv5LoadStoreMultipleOperation::STM
            };
            ARMv5InstructionType::LoadStoreMultiple { op, amm }
        };

        ARMv5Instruction { condition, instruction_type }
    }

    fn copro_and_swi(inst_bits: u32) -> ARMv5Instruction {
        //works at the moment, need to be expanded for cop calls
        if inst_bits.cut_bits(24..=25) == 0b11 {
            let condition: Condition = Condition::from_instruction(inst_bits);
            ARMv5Instruction { 
                condition, 
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::SWI 
                }
            }
        }
        else { return ARMv5Decoder::UNDEFINED_INSTRUCTION;  }
    }
}

fn bm(value: u32, mask: u32, expectation: u32) -> bool {
    (value & mask) ^ expectation == 0
}
