use crate::utils::{BitAccess, T, F};
use super::{
    SimulatedCPU, ARMv5CPU, names::RegNames, instructions::*,
    operands::{
        ShifterOperand, AddressingMode, OffsetType,
        AddressingModeMultiple, BranchOperator
    }
};

pub trait InstructionDecoder<I: Instruction<C, S>, C: SimulatedCPU<S>, S> {
    fn decode(instruction_bits: u32) -> I;
}

// ToDo: Refactor condition
// ToDo: Try to use memorization
//https://developer.arm.com/documentation/ddi0406/c/Application-Level-Architecture/ARM-Instruction-Set-Encoding/ARM-instruction-set-encoding?lang=en
pub struct ARMv5Decoder;
impl InstructionDecoder<ARMv5Instruction, ARMv5CPU, i32> for ARMv5Decoder {
    fn decode(instruction_bits: u32) -> ARMv5Instruction {
        let category = if instruction_bits.cut_bits(28..=31) != 0b1111 {
            match instruction_bits.cut_bits(26..=27) {
                0b00 => ARMv5Decoder::data_and_miscellaneos,
                0b01 => ARMv5Decoder::load_store,
                0b10 => ARMv5Decoder::branch_and_block_transfer,
                0b11 => ARMv5Decoder::coprocessor_and_swi,
                _ => panic!("Unreachable code!")
            }
        }
        else { ARMv5Decoder::unconditional };
        category(instruction_bits)
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
                op: ARMv5BranchOperation::BX, bo: BranchOperator::Register(rm)
            },
            (0b001, 0b11) => ARMv5InstructionType::Miscellaneous { 
                op: ARMv5MiscellaneousOperation::CLZ, rd, rm 
            },
            (0b011, 0b01) => ARMv5InstructionType::Branch {
                op: ARMv5BranchOperation::BLX, bo: BranchOperator::Register(rm)
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

        let offset_type: OffsetType = if inst_bits.get_bit(22) {
            OffsetType::Immediate { 
                offset: ((inst_bits.cut_bits(8..=11) << 4) |
                    inst_bits.cut_bits(0..=3)) as u16
            }
        }
        else {
            OffsetType::Register { 
                rm: inst_bits.cut_bits(0..=3).into()
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
                false => ARMv5BranchOperation::B
            };
            ARMv5InstructionType::Branch { op, 
                bo: BranchOperator::Offset(imm, false)
            }
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

    fn coprocessor_and_swi(inst_bits: u32) -> ARMv5Instruction {
        let op1: u32 = inst_bits.cut_bits(20..=25);

        let op: ARMv5GenericOperation = if bm(op1, 0b110000, 0b110000) {
            ARMv5GenericOperation::SWI 
        }
        else if !bm(inst_bits.cut_bits(8..=11), 0b1110, 0x1010) {
            if bm(op1, 0b100001, 0b0) && !bm(op1, 0b111011, 0b0) {
                ARMv5GenericOperation::STC
            }
            else if bm(op1, 0b100001, 0b1) && !bm(op1, 0b111011, 0b1) {
                ARMv5GenericOperation::LDC
            }
            else if bm(op1, 0b110000, 0b100000) {
                if inst_bits.get_bit(4) {
                    if inst_bits.get_bit(20) { ARMv5GenericOperation::MRC }
                    else { ARMv5GenericOperation::MCR }
                }
                else { ARMv5GenericOperation::CDP }
            }
            else { return ARMv5Decoder::UNDEFINED_INSTRUCTION; }
        }
        else { return ARMv5Decoder::UNDEFINED_INSTRUCTION; };

        ARMv5Instruction { 
            condition: Condition::from_instruction(inst_bits), 
            instruction_type: ARMv5InstructionType::Generic { op }
        }
    }

    fn unconditional(inst_bits: u32) -> ARMv5Instruction {
        let op1: u32 = inst_bits.cut_bits(20..=27);
        if bm(op1, 0b11100000, 0b10100000) {
            let mut imm: i32 = inst_bits.cut_bits(0..24) as i32;
            imm = (imm << 8) >> 8; //sign extend
            return ARMv5Instruction {
                condition: Condition::Unconditional,
                instruction_type: ARMv5InstructionType::Branch {
                    op: ARMv5BranchOperation::BLX, 
                    bo: BranchOperator::Offset(imm, inst_bits.get_bit(24)) 
                }
            };
        }
        let op: ARMv5GenericOperation = 
            if bm(op1, 0b11100001, 0b11000000) {
                if !bm(op1, 0b11111011, 0b11000000) {
                    ARMv5GenericOperation::STC2
                }
                else { return ARMv5Decoder::UNDEFINED_INSTRUCTION; }
            }
            else if bm(op1, 0b11100001, 0b11000001) {
                if !bm(op1, 0b11111011, 0b11000001) {
                    ARMv5GenericOperation::LDC2
                }
                else { return ARMv5Decoder::UNDEFINED_INSTRUCTION; }
            }
            else if bm(op1, 0b11110000, 0b11100000) {
                if inst_bits.get_bit(4) {
                    if inst_bits.get_bit(20) { ARMv5GenericOperation::MRC2 }
                    else { ARMv5GenericOperation::MCR2 }
                }
                else { ARMv5GenericOperation::CDP2 }
            }
            else { return ARMv5Decoder::UNDEFINED_INSTRUCTION; };

            ARMv5Instruction { 
                condition: Condition::Unconditional, 
                instruction_type: ARMv5InstructionType::Generic { op } 
            }
    }
}

fn bm(value: u32, mask: u32, expectation: u32) -> bool {
    (value & mask) ^ expectation == 0
}


#[cfg(test)]
mod tests {
    use crate::simulated_cpu::{instructions::*, names::RegNames, operands::{
            ShifterOperand, barrel_shifter::ShiftType, 
            AddressingMode, OffsetType, AddressingModeMultiple, BranchOperator
        }
    };

    use super::{InstructionDecoder, ARMv5Decoder};

    macro_rules! decoder_tests {
        ($($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (bits, instruction) = $test_values;
                let result = ARMv5Decoder::decode(bits);

                assert_eq!(instruction, result);
            }
        )*}
    }

    decoder_tests! {
        and: (
            0xB00342A9,
            ARMv5Instruction {
                condition: Condition::LT,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::AND,
                    s: false,
                    rd: RegNames::R4,
                    rn: RegNames::R3,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R9,
                        shift: ShiftType::LSR,
                        shift_amount: 5
                    }
                }
            }
        ),
        eor: (
            0xd02a0557,
            ARMv5Instruction {
                condition: Condition::LE,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::EOR,
                    s: false,
                    rd: RegNames::R0,
                    rn: RegNames::R10,
                    so: ShifterOperand::RegisterShift {
                        rm: RegNames::R7,
                        shift: ShiftType::ASR,
                        rs: RegNames::R5
                    }
                }
            }
        ),
        sub: (
            0xe058f003,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::SUB,
                    s: true,
                    rd: RegNames::PC,
                    rn: RegNames::R8,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R3,
                        shift: ShiftType::LSL,
                        shift_amount: 0
                    }
                }
            }
        ),
        rsb: (
            0xc0697004,
            ARMv5Instruction {
                condition: Condition::GT,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::RSB,
                    s: false,
                    rd: RegNames::R7,
                    rn: RegNames::R9,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R4,
                        shift: ShiftType::LSL,
                        shift_amount: 0
                    }
                }
            }
        ),
        add: (
            0xe08da465,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::ADD,
                    s: false,
                    rd: RegNames::R10,
                    rn: RegNames::SP,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R5,
                        shift: ShiftType::ROR,
                        shift_amount: 8
                    }
                }
            }
        ),
        adc: (
            0x50a32f8f,
            ARMv5Instruction {
                condition: Condition::PL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::ADC,
                    s: false,
                    rd: RegNames::R2,
                    rn: RegNames::R3,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::PC,
                        shift: ShiftType::LSL,
                        shift_amount: 31
                    }
                }
            }
        ),
        sbc: (
            0xb2d260ba,
            ARMv5Instruction {
                condition: Condition::LT,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::SBC,
                    s: true,
                    rd: RegNames::R6,
                    rn: RegNames::R2,
                    so: ShifterOperand::Immediate { 
                        immediate: 186, rotate: 0
                    }
                }
            }
        ),
        rsc: (
            0x40e4900a,
            ARMv5Instruction {
                condition: Condition::MI,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::RSC,
                    s: false,
                    rd: RegNames::R9,
                    rn: RegNames::R4,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R10,
                        shift: ShiftType::LSL,
                        shift_amount: 0
                    }
                }
            }
        ),
        tst: (
            0xe1120116,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::TST,
                    s: true,
                    rd: RegNames::R0,
                    rn: RegNames::R2,
                    so: ShifterOperand::RegisterShift {
                        rm: RegNames::R6,
                        shift: ShiftType::LSL,
                        rs: RegNames::R1
                    }
                }
            }
        ),
        teq: (
            0xe3340415,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::TEQ,
                    s: true,
                    rd: RegNames::R0,
                    rn: RegNames::R4,
                    so: ShifterOperand::Immediate { 
                        immediate: 21, rotate: 4
                    }
                }
            }
        ),
        cmp: (
            0xe15c000c,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::CMP,
                    s: true,
                    rd: RegNames::R0,
                    rn: RegNames::R12,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R12,
                        shift: ShiftType::LSL,
                        shift_amount: 0
                    }
                }
            }
        ),
        cmn: (
            0xa1750b77,
            ARMv5Instruction {
                condition: Condition::GE,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::CMN,
                    s: true,
                    rd: RegNames::R0,
                    rn: RegNames::R5,
                    so: ShifterOperand::RegisterShift {
                        rm: RegNames::R7,
                        shift: ShiftType::ROR,
                        rs: RegNames::R11
                    }
                }
            }
        ),
        orr: (
            0x119ec002,
            ARMv5Instruction {
                condition: Condition::NE,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::ORR,
                    s: true,
                    rd: RegNames::R12,
                    rn: RegNames::LR,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R2,
                        shift: ShiftType::LSL,
                        shift_amount: 0
                    }
                }
            }
        ),
        mov: (
            0xe1a0b008,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::MOV,
                    s: false,
                    rd: RegNames::R11,
                    rn: RegNames::R0,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R8,
                        shift: ShiftType::LSL,
                        shift_amount: 0
                    }
                }
            }
        ),
        bic: (
            0x01dce065,
            ARMv5Instruction {
                condition: Condition::EQ,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::BIC,
                    s: true,
                    rd: RegNames::LR,
                    rn: RegNames::R12,
                    so: ShifterOperand::ImmediateShift {
                        rm: RegNames::R5,
                        shift: ShiftType::ROR,
                        shift_amount: 0
                    }
                }
            }
        ),
        mvn: (
            0x73e0be57,
            ARMv5Instruction {
                condition: Condition::VC,
                instruction_type: ARMv5InstructionType::DataProcessing {
                    op: ARMv5DataProcessingOperation::MVN,
                    s: false,
                    rd: RegNames::R11,
                    rn: RegNames::R0,
                    so: ShifterOperand::Immediate { 
                        immediate: 87, rotate: 14
                    }
                }
            }
        ),

        mul: (
            0x20000d97,
            ARMv5Instruction {
                condition: Condition::HS,
                instruction_type: ARMv5InstructionType::Multiply { 
                    op: ARMv5MultiplyOperation::MUL,
                    s: false,
                    rn_lo: RegNames::R0,
                    rd_hi: RegNames::R0,
                    rm: RegNames::R7,
                    rs: RegNames::SP
                }
            }
        ),
        mla: (
            0xe0329691,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Multiply { 
                    op: ARMv5MultiplyOperation::MLA,
                    s: true,
                    rn_lo: RegNames::R9,
                    rd_hi: RegNames::R2,
                    rm: RegNames::R1,
                    rs: RegNames::R6
                }
            }
        ),
        smull: (
            0x60ccd092,
            ARMv5Instruction {
                condition: Condition::VS,
                instruction_type: ARMv5InstructionType::Multiply { 
                    op: ARMv5MultiplyOperation::SMULL,
                    s: false,
                    rn_lo: RegNames::SP,
                    rd_hi: RegNames::R12,
                    rm: RegNames::R2,
                    rs: RegNames::R0
                }
            }
        ),
        umull: (
            0x80997a90,
            ARMv5Instruction {
                condition: Condition::HI,
                instruction_type: ARMv5InstructionType::Multiply { 
                    op: ARMv5MultiplyOperation::UMULL,
                    s: true,
                    rn_lo: RegNames::R7,
                    rd_hi: RegNames::R9,
                    rm: RegNames::R0,
                    rs: RegNames::R10
                }
            }
        ),
        smlal: (
            0xe0ea4c91,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Multiply { 
                    op: ARMv5MultiplyOperation::SMLAL,
                    s: false,
                    rn_lo: RegNames::R4,
                    rd_hi: RegNames::R10,
                    rm: RegNames::R1,
                    rs: RegNames::R12
                }
            }
        ),
        umlal: (
            0x40a8e29b,
            ARMv5Instruction {
                condition: Condition::MI,
                instruction_type: ARMv5InstructionType::Multiply { 
                    op: ARMv5MultiplyOperation::UMLAL,
                    s: false,
                    rn_lo: RegNames::LR,
                    rd_hi: RegNames::R8,
                    rm: RegNames::R11,
                    rs: RegNames::R2
                }
            }
        ),

        clz: (
            0x616fcf13,
            ARMv5Instruction {
                condition: Condition::VS,
                instruction_type: ARMv5InstructionType::Miscellaneous {
                    op: ARMv5MiscellaneousOperation::CLZ,
                    rd: RegNames::R12,
                    rm: RegNames::R3
                }
            }
        ),


        b: (
            0x9a0241c0,
            ARMv5Instruction {
                condition: Condition::LS,
                instruction_type: ARMv5InstructionType::Branch { 
                    op: ARMv5BranchOperation::B,
                    bo: BranchOperator::Offset(147904, false)
                }
            }
        ),
        bl: (
            0xeb2ad361,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Branch { 
                    op: ARMv5BranchOperation::BL,
                    bo: BranchOperator::Offset(2806625, false)
                }
            }
        ),
        bx: (
            0x812fff15,
            ARMv5Instruction {
                condition: Condition::HI,
                instruction_type: ARMv5InstructionType::Branch { 
                    op: ARMv5BranchOperation::BX,
                    bo: BranchOperator::Register(RegNames::R5)
                }
            }
        ),
        //not yet implemented!!!
        //blx_label: ( 
        //    0xfa1bc32e,
        //    ARMv5Instruction {
        //        condition: Condition::VS,
        //        instruction_type: ARMv5InstructionType::Branch { 
        //            op: ARMv5BranchOperation::BLX,
        //            si: 72081330, 
        //            rm: RegNames::R0
        //        }
        //    }
        //),
        blx_register: (
            0x112fff39,
            ARMv5Instruction {
                condition: Condition::NE,
                instruction_type: ARMv5InstructionType::Branch { 
                    op: ARMv5BranchOperation::BLX,
                    bo: BranchOperator::Register(RegNames::R9)
                }
            }
        ),

        ldr: (
            0xa5b3adff,
            ARMv5Instruction {
                condition: Condition::GE,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDR, 
                    rd: RegNames::R10, 
                    am: AddressingMode { 
                        p: true, u: true, w: true,
                        rn: RegNames::R3, 
                        offset_type: OffsetType::Immediate { 
                            offset: 03583
                        }
                    }
                }
            }
        ),
        ldrb: (
            0xe6d75fe1,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDRB, 
                    rd: RegNames::R5, 
                    am: AddressingMode { 
                        p: false, u: true, w: false, 
                        rn: RegNames::R7, 
                        offset_type: OffsetType::ScaledRegister {
                            rm: RegNames::R1,
                            shift: ShiftType::ROR,
                            shift_imm: 31 
                        }
                    }
                }
            }
        ),
        ldrbt: (
            0xa6f4e00e,
            ARMv5Instruction {
                condition: Condition::GE,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDRBT, 
                    rd: RegNames::LR, 
                    am: AddressingMode { 
                        p: false, u: true, w: true, 
                        rn: RegNames::R4, 
                        offset_type: OffsetType::ScaledRegister {
                            rm: RegNames::LR,
                            shift: ShiftType::LSL,
                            shift_imm: 0
                        }
                    }
                }
            }
        ),
        //found misstake decoding register/offset
        ldrh: (
            0xe1d5c0b0,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDRH, 
                    rd: RegNames::R12, 
                    am: AddressingMode { 
                        p: true, u: true, w: false, 
                        rn: RegNames::R5, 
                        offset_type: OffsetType::Immediate { 
                            offset: 0 
                        }
                    }
                }
            }
        ),
        //found misstake, wrong calc order
        ldrsb: (
            0x51db78d1,
            ARMv5Instruction {
                condition: Condition::PL,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDRSB, 
                    rd: RegNames::R7, 
                    am: AddressingMode { 
                        p: true, u: true, w: false, 
                        rn: RegNames::R11, 
                        offset_type: OffsetType::Immediate { 
                            offset: 129
                        }
                    }
                }
            }
        ),
        ldrsh: (
            0x31d630f0,
            ARMv5Instruction {
                condition: Condition::LO,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDRSH, 
                    rd: RegNames::R3, 
                    am: AddressingMode { 
                        p: true, u: true, w: false, 
                        rn: RegNames::R6, 
                        offset_type: OffsetType::Immediate { 
                            offset: 0 
                        }
                    }
                }
            }
        ),
        ldrt: (
            0x143b6008,
            ARMv5Instruction {
                condition: Condition::NE,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::LDRT, 
                    rd: RegNames::R6, 
                    am: AddressingMode { 
                        p: false, u: false, w: true, 
                        rn: RegNames::R11, 
                        offset_type: OffsetType::Immediate { 
                            offset: 8
                        }
                    }
                }
            }
        ),
        str: (
            0xc78d2065,
            ARMv5Instruction {
                condition: Condition::GT,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::STR, 
                    rd: RegNames::R2, 
                    am: AddressingMode { 
                        p: true, u: true, w: false, 
                        rn: RegNames::SP, 
                        offset_type: OffsetType::ScaledRegister { 
                            rm: RegNames::R5,
                            shift: ShiftType::ROR,
                            shift_imm: 0
                        }
                    }
                }
            }
        ),
        strb: (
            0x57e1022a,
            ARMv5Instruction {
                condition: Condition::PL,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::STRB, 
                    rd: RegNames::R0, 
                    am: AddressingMode { 
                        p: true, u: true, w: true, 
                        rn: RegNames::R1, 
                        offset_type: OffsetType::ScaledRegister { 
                            rm: RegNames::R10,
                            shift: ShiftType::LSR,
                            shift_imm: 4
                        }
                    }
                }
            }
        ),
        strbt: (
            0x36e7b00b,
            ARMv5Instruction {
                condition: Condition::LO,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::STRBT, 
                    rd: RegNames::R11, 
                    am: AddressingMode { 
                        p: false, u: true, w: true, 
                        rn: RegNames::R7, 
                        offset_type: OffsetType::ScaledRegister { 
                            rm: RegNames::R11,
                            shift: ShiftType::LSL,
                            shift_imm: 0
                        }
                    }
                }
            }
        ),
        strh: (
            0x718850b3,
            ARMv5Instruction {
                condition: Condition::VC,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::STRH, 
                    rd: RegNames::R5, 
                    am: AddressingMode { 
                        p: true, u: true, w: false, 
                        rn: RegNames::R8, 
                        offset_type: OffsetType::Register { 
                            rm: RegNames::R3
                        }
                    }
                }
            }
        ),
        strt: (
            0xa4ac0000,
            ARMv5Instruction {
                condition: Condition::GE,
                instruction_type: ARMv5InstructionType::LoadStore { 
                    op: ARMv5LoadStoreOperation::STRT, 
                    rd: RegNames::R0, 
                    am: AddressingMode { 
                        p: false, u: true, w: true, 
                        rn: RegNames::R12, 
                        offset_type: OffsetType::Immediate { 
                            offset: 0 
                        }
                    }
                }
            }
        ),

        ldm_1: (
            0x089eb130,
            ARMv5Instruction {
                condition: Condition::EQ,
                instruction_type: ARMv5InstructionType::LoadStoreMultiple { 
                    op: ARMv5LoadStoreMultipleOperation::LDM,
                    amm: AddressingModeMultiple { 
                        p: false, u: true, w: false, 
                        rn: RegNames::LR, 
                        register_list: 0b1011000100110000
                    }
                }
            }
        ),
        ldm_2: (
            0xe93a2024,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::LoadStoreMultiple { 
                    op: ARMv5LoadStoreMultipleOperation::LDM,
                    amm: AddressingModeMultiple { 
                        p: true, u: false, w: true, 
                        rn: RegNames::R10, 
                        register_list: 0b0010000000100100
                    }
                }
            }
        ),
        stm_1: (
            0x99a4d730,
            ARMv5Instruction {
                condition: Condition::LS,
                instruction_type: ARMv5InstructionType::LoadStoreMultiple { 
                    op: ARMv5LoadStoreMultipleOperation::STM,
                    amm: AddressingModeMultiple { 
                        p: true, u: true, w: true, 
                        rn: RegNames::R4, 
                        register_list: 0b1101011100110000
                    }
                }
            }
        ),
        stm_2: (
            0xe8070180,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::LoadStoreMultiple { 
                    op: ARMv5LoadStoreMultipleOperation::STM,
                    amm: AddressingModeMultiple { 
                        p: false, u: false, w: false, 
                        rn: RegNames::R7, 
                        register_list: 0b0000000110000000
                    }
                }
            }
        ),

        swp: (
            0xd109e09e,
            ARMv5Instruction {
                condition: Condition::LE,
                instruction_type: ARMv5InstructionType::Synchronization { 
                    op: ARMv5SynchronizationOperation::SWP, 
                    rd: RegNames::LR, rm: RegNames::LR, rn: RegNames::R9
                }
            }
        ),
        swpb: (
            0x61472091,
            ARMv5Instruction {
                condition: Condition::VS,
                instruction_type: ARMv5InstructionType::Synchronization { 
                    op: ARMv5SynchronizationOperation::SWPB, 
                    rd: RegNames::R2, rm: RegNames::R1, rn: RegNames::R7
                }
            }
        ),

        swi: (
            0x1fdf055e,
            ARMv5Instruction {
                condition: Condition::NE,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::SWI
                }
            }
        ),
        bkpt: (
            0xe12a5373,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::BKPT
                }
            }
        ),
        mrs: (
            0xe10f3000,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::MRS
                }
            }
        ),
        msr: (
            0x216ff004,
            ARMv5Instruction {
                condition: Condition::HS,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::MSR
                }
            }
        ),
        cdp: (
            0x7efc574d,
            ARMv5Instruction {
                condition: Condition::VC,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::CDP
                }
            }
        ),
        cdp2: (
            0xfef1f64e,
            ARMv5Instruction {
                condition: Condition::Unconditional,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::CDP2
                }
            }
        ),
        ldc: (
            0x8d904daa,
            ARMv5Instruction {
                condition: Condition::HI,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::LDC
                }
            }
        ),
        ldc2: (
            0xfd96bf7b,
            ARMv5Instruction {
                condition: Condition::Unconditional,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::LDC2
                }
            }
        ),
        mcr: (
            0x9ee2c610,
            ARMv5Instruction {
                condition: Condition::LS,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::MCR
                }
            }
        ),
        mcr2: (
            0xfe4631b0,
            ARMv5Instruction {
                condition: Condition::Unconditional,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::MCR2
                }
            }
        ),
        mrc: (
            0xeede8a3b,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::MRC
                }
            }
        ),
        mrc2: (
            0xfeb5d712,
            ARMv5Instruction {
                condition: Condition::Unconditional,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::MRC2
                }
            }
        ),
        stc: (
            0xdd80d4d5,
            ARMv5Instruction {
                condition: Condition::LE,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::STC
                }
            }
        ),
        stc2: (
            0xfd8f052e,
            ARMv5Instruction {
                condition: Condition::Unconditional,
                instruction_type: ARMv5InstructionType::Generic { 
                    op: ARMv5GenericOperation::STC2
                }
            }
        ),

        undefined: (
            0xec518764,
            ARMv5Instruction {
                condition: Condition::AL,
                instruction_type: ARMv5InstructionType::Undefined
            }
        )
    }
}
