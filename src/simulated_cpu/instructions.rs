use std::{ops::Range, iter::StepBy, fmt::Display, mem::transmute};

use super::{
    SimulatedCPU, ARMv5CPU, names::{RegNames, FlagNames},
    operands::{ShifterOperand, AddressingMode, AddressingModeMultiple}
};
use crate::utils::{
    slice_to_u32, BitAccess, slice_to_u16, u32_to_array, u16_to_array
};

pub trait Instruction<C: SimulatedCPU<S> , S>: Display {
    fn execute(&self, cpu: &mut C);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Condition { 
    EQ, NE, HS, LO, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL, Unconditional
}
impl Condition {
    pub(super) fn from_instruction(instruction: u32) -> Condition {
        let cond_bits: u32 = instruction.cut_bits(28..32);
        unsafe { transmute(cond_bits) }
    }

    fn is_satisfied(&self, flags: &[bool; 4]) -> bool {
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
impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Condition::AL | Condition::Unconditional = self {
            write!(f, "")
        }
        else {
            write!(f, "{:?}", self)   
        }
    }
}

#[repr(u32)]
#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum ARMv5DataProcessingOperation {
    AND, EOR, SUB, RSB, ADD, ADC, SBC, RSC,
    TST, TEQ, CMP, CMN, ORR, MOV, BIC, MVN
}
impl ARMv5DataProcessingOperation {
    const NUM_OPERATIONS: u32 = 16;

    pub fn from_bits(bits: u32) -> ARMv5DataProcessingOperation {
        if bits >= ARMv5DataProcessingOperation::NUM_OPERATIONS {
            panic!("Convertion to ARMv5DataProcessingOperation failed!");
        }
        unsafe { transmute(bits) }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ARMv5MultiplyOperation {
    MUL, MLA, SMULL, UMULL, SMLAL, UMLAL
}

#[derive(Debug, Clone, Copy)]
pub enum ARMv5MiscellaneousOperation {
    CLZ
}

#[derive(Debug, Clone, Copy)]
pub enum ARMv5BranchOperation {
    B, BL, BX, BLX
}

#[derive(Debug, Clone, Copy)]
pub enum ARMv5LoadStoreOperation {
    LDR, LDRB, LDRBT, LDRH, LDRSB, LDRSH, 
    LDRT, STR, STRB, STRBT, STRH, STRT
}

#[derive(Debug, Clone, Copy)]
pub enum ARMv5LoadStoreMultipleOperation {
    LDM, STM
}

#[derive(Debug, Clone, Copy)]
pub enum ARMv5SynchronizationOperation {
    SWP, SWPB
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum ARMv5GenericOperation {
    SWI, BKPT, MRS, MSR, CDP, CDP2, LDC, 
    LDC2, MCR, MCR2, MRC, MRC2, STC, STC2
}

//maybe add undefined
pub enum ARMv5InstructionType {
    DataProcessing { 
        op: ARMv5DataProcessingOperation, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand 
    },
    Multiply { 
        op: ARMv5MultiplyOperation, s:bool, rn_lo: RegNames, 
        rd_hi: RegNames, rs: RegNames, rm: RegNames 
    },
    Miscellaneous { 
        op: ARMv5MiscellaneousOperation, rd: RegNames, rm: RegNames 
    },
    Branch { 
        op: ARMv5BranchOperation, si: i32, rm: RegNames
    },
    LoadStore {
        op: ARMv5LoadStoreOperation, rd: RegNames, am: AddressingMode
    },
    LoadStoreMultiple {
         op: ARMv5LoadStoreMultipleOperation, amm: AddressingModeMultiple
    },
    Synchronization {
        op: ARMv5SynchronizationOperation, rn: RegNames, 
        rd:RegNames, rm: RegNames
    },
    Generic { 
        op: ARMv5GenericOperation },
    Undefined
}

//maybe provide constructor
pub struct ARMv5Instruction {
    pub condition: Condition,
    pub instruction_type: ARMv5InstructionType
}
impl Instruction<ARMv5CPU, i32> for ARMv5Instruction {
    fn execute(&self, cpu: &mut ARMv5CPU) {
        if !self.condition.is_satisfied(&cpu.flags) { return; }

        match self.instruction_type {
            ARMv5InstructionType::DataProcessing { op, s, rn, rd, so } => {
                let function = match op {
                    ARMv5DataProcessingOperation::AND => ARMv5CPU::and,
                    ARMv5DataProcessingOperation::EOR => ARMv5CPU::eor,
                    ARMv5DataProcessingOperation::SUB => ARMv5CPU::sub,
                    ARMv5DataProcessingOperation::RSB => ARMv5CPU::rsb,
                    ARMv5DataProcessingOperation::ADD => ARMv5CPU::add,
                    ARMv5DataProcessingOperation::ADC => ARMv5CPU::adc,
                    ARMv5DataProcessingOperation::SBC => ARMv5CPU::sbc,
                    ARMv5DataProcessingOperation::RSC => ARMv5CPU::rsc,
                    ARMv5DataProcessingOperation::TST => ARMv5CPU::tst,
                    ARMv5DataProcessingOperation::TEQ => ARMv5CPU::teq,
                    ARMv5DataProcessingOperation::CMP => ARMv5CPU::cmp,
                    ARMv5DataProcessingOperation::CMN => ARMv5CPU::cmn,
                    ARMv5DataProcessingOperation::ORR => ARMv5CPU::orr,
                    ARMv5DataProcessingOperation::MOV => ARMv5CPU::mov,
                    ARMv5DataProcessingOperation::BIC => ARMv5CPU::bic,
                    ARMv5DataProcessingOperation::MVN => ARMv5CPU::mvn
                };
                function(cpu, s, rn, rd, so);
            }
            ARMv5InstructionType::Multiply { op, s, rd_hi, rn_lo, rs, rm } => {
                let function = match op {
                    ARMv5MultiplyOperation::MUL => ARMv5CPU::mul,
                    ARMv5MultiplyOperation::MLA => ARMv5CPU::mla,
                    ARMv5MultiplyOperation::SMULL => ARMv5CPU::smull,
                    ARMv5MultiplyOperation::UMULL => ARMv5CPU::umull,
                    ARMv5MultiplyOperation::SMLAL => ARMv5CPU::smlal,
                    ARMv5MultiplyOperation::UMLAL => ARMv5CPU::umlal
                };
                function(cpu, s, rd_hi, rn_lo, rs, rm);
            },
            ARMv5InstructionType::Miscellaneous { op, rd, rm } => {
                let function = match op {
                    ARMv5MiscellaneousOperation::CLZ => ARMv5CPU::clz
                };
                function(cpu, rd, rm);
            },
            ARMv5InstructionType::Branch { op, si, rm } => {
                match op {
                    ARMv5BranchOperation::B => ARMv5CPU::b(cpu, false, si),
                    ARMv5BranchOperation::BL => ARMv5CPU::b(cpu, true, si),
                    ARMv5BranchOperation::BX => ARMv5CPU::bx(cpu, false, rm),
                    ARMv5BranchOperation::BLX => ARMv5CPU::bx(cpu, true, rm)
                }
            },
            ARMv5InstructionType::LoadStore { op, rd, am } => {
                let function = match op {
                    ARMv5LoadStoreOperation::LDR => ARMv5CPU::ldr,
                    ARMv5LoadStoreOperation::LDRB => ARMv5CPU::ldrb,
                    ARMv5LoadStoreOperation::LDRBT => ARMv5CPU::ldrbt,
                    ARMv5LoadStoreOperation::LDRH => ARMv5CPU::ldrh,
                    ARMv5LoadStoreOperation::LDRSB => ARMv5CPU::ldrsb,
                    ARMv5LoadStoreOperation::LDRSH => ARMv5CPU::ldrsh,
                    ARMv5LoadStoreOperation::LDRT => ARMv5CPU::ldrt,
                    ARMv5LoadStoreOperation::STR => ARMv5CPU::str,
                    ARMv5LoadStoreOperation::STRB => ARMv5CPU::strb,
                    ARMv5LoadStoreOperation::STRBT => ARMv5CPU::strbt,
                    ARMv5LoadStoreOperation::STRH => ARMv5CPU::strh,
                    ARMv5LoadStoreOperation::STRT => ARMv5CPU::strt
                };
                function(cpu, rd, am);
            },
            ARMv5InstructionType::LoadStoreMultiple { op, amm } => {
                let function = match op {
                    ARMv5LoadStoreMultipleOperation::LDM => ARMv5CPU::ldm,
                    ARMv5LoadStoreMultipleOperation::STM => ARMv5CPU::stm
                };
                function(cpu, amm);
            },
            ARMv5InstructionType::Synchronization { op, rn, rd, rm } => {
                let function = match op {
                    ARMv5SynchronizationOperation::SWP => ARMv5CPU::swp,
                    ARMv5SynchronizationOperation::SWPB => ARMv5CPU::swpb,
                };
                function(cpu, rn, rd, rm);
            },
            ARMv5InstructionType::Generic { op } => {
                let function = match op {
                    ARMv5GenericOperation::SWI => ARMv5CPU::swi,
                    ARMv5GenericOperation::BKPT => ARMv5CPU::bkpt,
                    ARMv5GenericOperation::MRS => ARMv5CPU::mrs,
                    ARMv5GenericOperation::MSR => ARMv5CPU::msr,
                    ARMv5GenericOperation::CDP => ARMv5CPU::cdp,
                    ARMv5GenericOperation::CDP2 => ARMv5CPU::cdp2,
                    ARMv5GenericOperation::LDC => ARMv5CPU::ldc,
                    ARMv5GenericOperation::LDC2 => ARMv5CPU::ldc2,
                    ARMv5GenericOperation::MCR => ARMv5CPU::mcr,
                    ARMv5GenericOperation::MCR2 => ARMv5CPU::mcr2,
                    ARMv5GenericOperation::MRC => ARMv5CPU::mrc,
                    ARMv5GenericOperation::MRC2 => ARMv5CPU::mrc2,
                    ARMv5GenericOperation::STC => ARMv5CPU::stc,
                    ARMv5GenericOperation::STC2 => ARMv5CPU::stc2
                };
                function(cpu);
            }
            ARMv5InstructionType::Undefined =>
                cpu.output_device.output_err("Undefined Instruction!\n")
        }
    }
}
impl Display for ARMv5Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cond: Condition = self.condition;
        match self.instruction_type {
            ARMv5InstructionType::DataProcessing { op, s, rn, rd, so } => {
                let s: &str = if s {"S"} else {""};
                match op {
                    ARMv5DataProcessingOperation::TST |
                    ARMv5DataProcessingOperation::TEQ |
                    ARMv5DataProcessingOperation::CMP |
                    ARMv5DataProcessingOperation::CMN |
                    ARMv5DataProcessingOperation::MOV |
                    ARMv5DataProcessingOperation::MVN => write!(
                        f, "{:?}{cond}{s} {rd}, {so}", op
                    ),
                    _ => write!(
                        f, "{:?}{cond}{s} {rd}, {rn}, {so}", op
                    )
                }
            },
            ARMv5InstructionType::Multiply { op, s, rn_lo, rd_hi, rs, rm } => {
                let s: &str = if s {"S"} else {""};
                match op {
                    ARMv5MultiplyOperation::MUL => write!(
                        f, "{:?}{cond}{s} {rd_hi}, {rm}, {rs}", op
                    ),
                    ARMv5MultiplyOperation::MLA => write!(
                        f, "{:?}{cond}{s} {rd_hi}, {rm}, {rs}, {rn_lo}", op
                    ),
                    _ => write!(
                        f, "{:?}{cond}{s} {rd_hi}, {rn_lo}, {rm}, {rs}", op
                    )
                }
            },
            ARMv5InstructionType::Miscellaneous { op, rd, rm } => {
                write!(f, "{:?}{cond} {rd}, {rm}", op)
            },
            ARMv5InstructionType::Branch { op, si, rm } => {
                match op {
                    ARMv5BranchOperation::B | ARMv5BranchOperation::BL => {
                        //improve with lable or correct address
                        write!(f, "{:?}{cond} 0x{:x}", op, si)
                    },
                    ARMv5BranchOperation::BX | ARMv5BranchOperation::BLX => {
                        write!(f, "{op:?}{cond} {rm}")
                    }
                }
            },
            ARMv5InstructionType::LoadStore { op, rd, am } => {
                write!(f, "{:?}{cond} {rd}, {am}", op)
            },
            ARMv5InstructionType::LoadStoreMultiple { op, amm } => {
                write!(f, "{:?}{cond}{amm}", op)
            },
            ARMv5InstructionType::Synchronization { op, rn, rd, rm } => {
                write!(f, "{:?}{cond} {rd} {rm} {rn}", op)
            },
            ARMv5InstructionType::Generic { op } => {
                write!(f, "{:?} #0", op)
            }
            ARMv5InstructionType::Undefined => write!(f, "?")
        }
    }
}

impl ARMv5CPU {
    fn and(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a & b;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    fn eor(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a ^ b;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    fn sub(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_sub(b);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = (result as u32) < (a as u32);
            self.flags[FlagNames::V] = overflow;
        }
    }

    fn rsb(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = b.overflowing_sub(a);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = (result as u32) < (b as u32);
            self.flags[FlagNames::V] = overflow;
        }
    }

    fn add(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_add(b);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = (result as u32) < (a as u32);
            self.flags[FlagNames::V] = overflow;
        }
    }


    fn adc(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);
        let c: i32 = if self.flags[FlagNames::C] {1} else {0};

        // carrying_add not available yet
        let (ir, o1): (i32, bool) = (a).overflowing_add(b);
        let (result, o2): (i32, bool) = ir.overflowing_add(c);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] 
                = (result as u32) <= (a as u32) && (b != 0 || c != 0);
            self.flags[FlagNames::V] = o1 || o2;
        }
    }

    fn sbc(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);
        let c: i32 = if self.flags[FlagNames::C] {0} else {1};

        let (ir, o1): (i32, bool) = a.overflowing_sub(b);
        let (result, o2): (i32, bool) = ir.overflowing_sub(c);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = 
                (result as u32) <= (a as u32) && (b != 0 || c != 0);
            self.flags[FlagNames::V] = o1 || o2;
        }
    }

    fn rsc(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);
        let c: i32 = if self.flags[FlagNames::C] {0} else {1};

        let (ir, o1): (i32, bool) = b.overflowing_sub(a);
        let (result, o2): (i32, bool) = ir.overflowing_sub(c);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = 
                (result as u32) <= (b as u32) && (a != 0 || c != 0);
            self.flags[FlagNames::V] = o1 || o2;
        }
    }

    fn tst(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a & b;

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = carry;
    }

    fn teq(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a ^ b;

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = carry;
    }

    fn cmp(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_sub(b);

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = (result as u32) < (a as u32);
        self.flags[FlagNames::V] = overflow;
    }

    fn cmn(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_add(b);

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = (result as u32) < (a as u32);
        self.flags[FlagNames::V] = overflow;
    }

    fn orr(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a | b;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    fn mov(
        &mut self, s: bool, _: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let (value, carry): (i32, bool) = self.perform_shift(so);
        self.set_register(rd, value);

        if s {
            self.flags[FlagNames::N] = value < 0;
            self.flags[FlagNames::Z] = value == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    fn bic(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a & !b;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    fn mvn(
        &mut self, s: bool, _: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        let (value, carry): (i32, bool) = self.perform_shift(so);
        let result: i32 = !value;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }


    // Multiply instructions
    fn mul(
        &mut self, s: bool, rd: RegNames, _: RegNames,
        rs: RegNames, rm: RegNames 
    ) {
        let a: i32 = self.get_register_intern(rm);
        let b: i32 = self.get_register_intern(rs);

        let result: i32 = a.wrapping_mul(b);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    fn mla(
        &mut self, s: bool, rd: RegNames, rn: RegNames, 
        rs: RegNames, rm: RegNames 
    ) {
        let a: i32 = self.get_register_intern(rm);
        let b: i32 = self.get_register_intern(rs);
        let c: i32 = self.get_register_intern(rn);

        let result: i32 = a.wrapping_mul(b).wrapping_add(c);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    fn smull(
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        let a: i64 = self.get_register_intern(rm) as i64;
        let b: i64 = self.get_register_intern(rs) as i64;

        let result: i64 = a.wrapping_mul(b);
        self.set_register(rdhi, (result >> 32) as i32);
        self.set_register(rdlo, result as i32);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    fn umull(        
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        let a: u64 = self.get_register_intern(rm) as u32 as u64;
        let b: u64 = self.get_register_intern(rs) as u32 as u64;

        let result: i64 = a.wrapping_mul(b) as i64;

        self.set_register(rdhi, (result >> 32) as i32);
        self.set_register(rdlo, result as i32);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    fn smlal(
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        let a: i64 = self.get_register_intern(rm) as i64;
        let b: i64 = self.get_register_intern(rs) as i64;
        let c: i64 = (self.get_register_intern(rdlo) as i64) 
            + ((self.get_register_intern(rdhi) as i64) << 32);

        let result: i64 = a.wrapping_mul(b).wrapping_add(c);
        self.set_register(rdhi, (result >> 32) as i32);
        self.set_register(rdlo, result as i32);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    fn umlal(
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        let a: u64 = self.get_register_intern(rm) as u32 as u64;
        let b: u64 = self.get_register_intern(rs) as u32 as u64;
        let c: i64 = (self.get_register_intern(rdlo) as i64) 
            + ((self.get_register_intern(rdhi) as i64) << 32);

        let result: i64 = (a.wrapping_mul(b) as i64).wrapping_add(c);
        self.set_register(rdhi, (result >> 32) as i32);
        self.set_register(rdlo, result as i32);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }


    // Miscellaneous arithmetic instructions
    fn clz(&mut self, rd: RegNames, rm: RegNames) {
        let a: i32 = self.get_register_intern(rm);

        let result: i32 = a.leading_zeros() as i32;
        self.set_register(rd, result);
    }
    

    // Branch instructions
    fn b(&mut self, l: bool, si: i32) {
        let prog_addr: i32 = self.get_register_intern(RegNames::PC);

        if l {
            let link_addr: i32 = prog_addr.wrapping_sub(4);
            self.set_register(RegNames::LR, link_addr);
        }

        let new_prog_addr: i32 = prog_addr.wrapping_add(si << 2);
        self.set_register(RegNames::PC, new_prog_addr);
    }

    fn bx(&mut self, l: bool, rm: RegNames) {
        // This behaviour is incorrect! After switching to 
        // Thumb state on a non T CPU the next executed instruction
        // causes an UndefinedInstructionExeption. Then the cpu
        // switches back to ARM.
        if l {
            let prog_addr: i32 = self.get_register_intern(RegNames::PC);
            let link_addr: i32 = prog_addr.wrapping_sub(4);
            self.set_register(RegNames::LR, link_addr);
        }

        let target: i32 = self.get_register_intern(rm);
        self.set_register(RegNames::PC, target);
    }


    // Load and store instructions
    fn ldr(&mut self, rd: RegNames, am: AddressingMode) {
        let mut address: usize = self.compute_modify_address(am);

        // do this only if unaligned access flag is 0 (maybe only in armv6 but need to check)
        let rot_bits: u32 = (address as u32).cut_bits(0..=1) * 8;
        address &= 0xFFFFFFFC;

        // improvement: memory boundary check
        let bytes: &[u8] = &self.memory[address..address+4];
        let mut value: u32 = slice_to_u32(bytes, &self.encoding);
        value = value.rotate_right(rot_bits);

        if let RegNames::PC = rd {
            value &= 0xFFFFFFFE;
            //set T bit when LSB is 1
        }
        self.set_register(rd, value as i32);
    }

    fn ldrb(&mut self, rd: RegNames, am: AddressingMode) {
        let address: usize = self.compute_modify_address(am);
        let value: u32 = self.memory[address] as u32;
        self.set_register(rd, value as i32);
    }

    fn ldrbt(&mut self, rd: RegNames, am: AddressingMode) { 
        self.ldrb(rd, am); 
    }

    fn ldrh(&mut self, rd: RegNames, am: AddressingMode) {
        let address: usize = self.compute_modify_address(am);
        let bytes: &[u8] = &self.memory[address..address+2];
        let value: u32 = slice_to_u16(bytes, &self.encoding) as u32;
        self.set_register(rd, value as i32);
    }

    fn ldrsb(&mut self, rd: RegNames, am: AddressingMode) {
        let address: usize = self.compute_modify_address(am);
        let value: i32 = self.memory[address] as i8 as i32;
        self.set_register(rd, value);
    }

    fn ldrsh(&mut self, rd: RegNames, am: AddressingMode) {
        let address: usize = self.compute_modify_address(am);
        let bytes: &[u8] = &self.memory[address..address+2];
        let value: i32 = slice_to_u16(bytes, &self.encoding) as i16 as i32;
        self.set_register(rd, value);
    }

    fn ldrt(&mut self, rd: RegNames, am: AddressingMode) { 
        self.ldr(rd, am); 
    }

    fn str(&mut self, rd: RegNames, am: AddressingMode) {
        let mut address: usize = self.compute_modify_address(am);
        address &= 0xFFFFFFFC;

        let value: u32 = self.get_register_intern(rd) as u32;
        let bytes: [u8; 4] = u32_to_array(value, &self.encoding);
        
        self.memory.splice(address..address+4, bytes);
    }

    fn strb(&mut self, rd: RegNames, am: AddressingMode) {
        let address: usize = self.compute_modify_address(am);
        let value: u8 = self.get_register_intern(rd) as u8;
        self.memory[address] = value;
    }

    fn strbt(&mut self, rd: RegNames, am: AddressingMode) { 
        self.strb(rd, am); 
    }

    fn strh(&mut self, rd: RegNames, am: AddressingMode) {
        let address: usize = self.compute_modify_address(am);

        let value: u16 = self.get_register_intern(rd) as u16;
        let bytes: [u8; 2] = u16_to_array(value, &self.encoding);
        
        self.memory.splice(address..address+2, bytes);
    }

    fn strt(&mut self, rd: RegNames, am: AddressingMode) { 
        self.str(rd, am); 
    }

    fn ldm(&mut self, amm: AddressingModeMultiple) {
        let mut addresses: StepBy<Range<usize>> = 
            self.compute_modify_address_multiple(&amm);

        for i in 0..16 {
            if amm.register_list.get_bit(i) {
                let address: usize = addresses.next().unwrap() & 0xFFFFFFFC;
                let bytes: &[u8] = &self.memory[address..address+4];
                let mut value: u32 = slice_to_u32(bytes, &self.encoding);
                if i == 15 {
                    value &= 0xFFFFFFFE;
                    //set T bit when LSB is 1
                }
                let reg_name: RegNames = (i as u32).into();
                self.set_register(reg_name, value as i32);
            }
        }
    }

    fn stm(&mut self, amm: AddressingModeMultiple) {
        let mut addresses: StepBy<Range<usize>> = 
            self.compute_modify_address_multiple(&amm);

        for i in 0..16 {
            if amm.register_list.get_bit(i) {
                let address: usize = addresses.next().unwrap() & 0xFFFFFFFC;
                
                let reg_name: RegNames = (i as u32).into();
                let value: i32 = self.get_register_intern(reg_name);

                let bytes: [u8; 4] = match self.encoding {
                    crate::utils::Endian::Little => value.to_le_bytes(),
                    crate::utils::Endian::Big => value.to_be_bytes(),
                };
                self.memory.splice(address..address+4, bytes);
            }
        }
    }

    fn swp(&mut self, rn: RegNames, rd: RegNames, rm: RegNames) {
        let mut address: usize = self.get_register_intern(rn) as u32 as usize;
        let rot_bits: u32 = (address as u32).cut_bits(0..=1);
        address &= 0xFFFFFFFC;

        let bytes: &[u8] = &self.memory[address..address+4];
        let mut lv: u32 = slice_to_u32(bytes, &self.encoding);
        lv = lv.rotate_right(rot_bits);

        let sv: u32 = self.get_register_intern(rm) as u32;
        let bytes: [u8; 4] = u32_to_array(sv, &self.encoding);
        
        self.memory.splice(address..address+4, bytes);
        self.set_register(rd, lv as i32);
    }

    fn swpb(&mut self, rn: RegNames, rd: RegNames, rm: RegNames) {
        let address: usize = self.get_register_intern(rn) as u32 as usize;

        let lv: u8 = self.memory[address];
        let sv: u8 = self.get_register_intern(rm) as u8;
        
        self.memory[address] = sv;
        self.set_register(rd, lv as i32);
    }

    // Exception-generating instructions
    fn swi(&mut self) {
        let r0: i32 = self.get_register_intern(RegNames::R0);
        let r7: i32 = self.get_register_intern(RegNames::R7);
        match (r0, r7) {
            (1, 4) => {
                let l: usize = 
                    self.get_register_intern(RegNames::R2) as u32 as usize;
                let a: usize = 
                    self.get_register_intern(RegNames::R1) as u32 as usize;
                self.output_device.output(
                    &String::from_utf8_lossy(&self.memory[a..a+l])
                );
            },
            (x, 1) => self.exit_behaviour.exit(x),
            (_, _) => ()
        }
    }

    fn bkpt(&mut self) { }

    // Status register access instructions
    fn mrs(&mut self) {
        self.output_device
            .output_err("Full status register not supported yes!\n");
    }

    fn msr(&mut self) {
        self.output_device
            .output_err("Full status register not supported yes!\n");
    }


    // Coprocessor instructions
    fn cdp(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn cdp2(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn ldc(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn ldc2(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn mcr(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn mcr2(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn mrc(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn mrc2(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn stc(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }

    fn stc2(&mut self) {
        self.output_device
            .output_err("Coprocessor instructions not supported!\n");
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::{T, F, ConsoleOutput, ConsoleExit, Endian};
    use crate::simulated_cpu::{
        SimulatedCPU, ARMv5CPU, RegNames, FlagNames, 
        operands::{
            ShifterOperand, barrel_shifter::ShiftType, 
            AddressingMode, OffsetType 
        }
    };

    macro_rules! data_processing_tests {
        (function: $function:ident, $($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (a, b, s, shift, c_in, result, exp_flags) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_register(RegNames::R1, a);
                cpu.set_register(RegNames::R2, b);
                cpu.flags[FlagNames::C] = c_in;

                let so = ShifterOperand::ImmediateShift{
                    rm: RegNames::R2,
                    shift: ShiftType::LSR,
                    shift_amount: shift
                };

                cpu.$function(s, RegNames::R1, RegNames::R0, so);
                assert_eq!(result, cpu.get_register_intern(RegNames::R0));
                assert_eq!(exp_flags, cpu.flags);
            }
        )*}
    }

    data_processing_tests! {
        function: and,
        and_test_1: 
            (0b1011101, 0b1101011, T, 0, T, 0b1001001, [F, F, T, F]),
        and_test_2: 
            (0b101010, 0b101010, T, 1, F, 0, [F, T, F, F]),
        and_test_3: 
            (i32::MIN, -1, T, 0, F, i32::MIN, [T, F, F, F]),
        and_test_4: 
            (-1, 4, T, 3, F, 0, [F, T, T, F]),
        and_test_5: 
            (1, 1, F, 1, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: eor,
        eor_test_1: 
            (0b1011101, 0b1101011, T, 0, T, 0b0110110, [F, F, T, F]),
        eor_test_2: 
            (0b101010, 0b1010100, T, 1, F, 0, [F, T, F, F]),
        eor_test_3: 
            (i32::MIN, 0, T, 0, F, i32::MIN, [T, F, F, F]),
        eor_test_4: 
            (0, 4, T, 3, F, 0, [F, T, T, F]),
        eor_test_5: 
            (0, 1, F, 1, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: sub,
        sub_test_1: 
            (5, 7, T, 0, T, -2, [T, F, F, F]),
        sub_test_2: 
            (3, 3, T, 0, F, 0, [F, T, T, F]),
        sub_test_3: 
            (i32::MIN, 1, T, 0, F, i32::MAX, [F, F, T, T]),
        sub_test_4: 
            (10, 2, T, 1, F, 9, [F, F, T, F]),
        sub_test_5: 
            (3, 3, F, 0, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: rsb,
        rsb_test_1: 
            (7, 5, T, 0, T, -2, [T, F, F, F]),
        rsb_test_2: 
            (3, 3, T, 0, F, 0, [F, T, T, F]),
        rsb_test_3: 
            (1, i32::MIN, T, 0, F, i32::MAX, [F, F, T, T]),
        rsb_test_4: 
            (6, 16, T, 1, F, 2, [F, F, T, F]),
        rsb_test_5: 
            (3, 3, F, 0, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: add,
        add_test_1: 
            (12, 7, T, 0, T, 19, [F; 4]),
        add_test_2: 
            (1, -1, T, 0, F, 0, [F, T, T, F]),
        add_test_3: 
            (i32::MAX, 2, T, 1, F, i32::MIN, [T, F, F, T]),
        add_test_4: 
            (i32::MIN, -1, T, 0, F, i32::MAX, [F, F, T, T]),
        add_test_5: 
            (1, -1, F, 0, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: adc,
        adc_test_1: 
            (12, 7, T, 0, F, 19, [F; 4]),
        adc_test_2: 
            (1, -2, T, 0, T, 0, [F, T, T, F]),
        adc_test_3: 
            (i32::MAX, 2, T, 1, T, i32::MIN + 1, [T, F, F, T]),
        adc_test_4: 
            (i32::MIN, -1, T, 0, F, i32::MAX, [F, F, T, T]),
        adc_test_5: 
            (1, -2, F, 0, T, 0, [F, F, T, F])
    }

    data_processing_tests! {
        function: sbc,
        sbc_test_1: 
            (5, 7, T, 0, T, -2, [T, F, F, F]),
        sbc_test_2:
            (3, 2, T, 0, F, 0, [F, T, T, F]),
        sbc_test_3: 
            (i32::MIN + 1, 1, T, 0, F, i32::MAX, [F, F, T, T]),
        sbc_test_4: 
            (10, 1, T, 1, T, 10, [F; 4]),
        sbc_test_5: 
            (3, 2, F, 0, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: rsc,
        rsc_test_1: 
            (7, 5, T, 0, T, -2, [T, F, F, F]),
        rsc_test_2:
            (2, 3, T, 0, F, 0, [F, T, T, F]),
        rsc_test_3: 
            (1, i32::MIN + 1, T, 0, F, i32::MAX, [F, F, T, T]),
        rsc_test_4: 
            (1, 10, T, 1, T, 4, [F, F, T, F]),
        rsc_test_5: 
            (2, 3, F, 0, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: tst,
        tst_test_1: 
            (0b1011101, 0b1101011, T, 0, T, 0, [F, F, T, F]),
        tst_test_2: 
            (0b101010, 0b101010, T, 1, F, 0, [F, T, F, F]),
        tst_test_3: 
            (i32::MIN, -1, F, 0, T, 0, [T, F, T, F]),
        tst_test_4: 
            (-1, 4, F, 3, F, 0, [F, T, T, F]),
        tst_test_5: 
            (1, 1, T, 2, T, 0, [F, T, F, F])
    }

    data_processing_tests! {
        function: teq,
        teq_test_1: 
            (0b1011101, 0b1101011, T, 0, T, 0, [F, F, T, F]),
        teq_test_2: 
            (0b101010, 0b1010100, T, 1, F, 0, [F, T, F, F]),
        teq_test_3: 
            (i32::MIN, 0, F, 0, T, 0, [T, F, T, F]),
        teq_test_4: 
            (0, 4, F, 3, F, 0, [F, T, T, F]),
        teq_test_5: 
            (0, 1, T, 2, T, 0, [F, T, F, F])
    }

    data_processing_tests! {
        function: cmp,
        cmp_test_1: 
            (5, 7, T, 0, T, 0, [T, F, F, F]),
        cmp_test_2: 
            (3, 3, T, 0, F, 0, [F, T, T, F]),
        cmp_test_3: 
            (i32::MIN, 1, F, 0, T, 0, [F, F, T, T]),
        cmp_test_4: 
            (10, 2, F, 1, F, 0, [F, F, T, F]),
        cmp_test_5: 
            (3, -3, T, 0, T, 0, [F; 4])
    }

    data_processing_tests! {
        function: cmn,
        cmn_test_1: 
            (12, 7, T, 0, T, 0, [F; 4]),
        cmn_test_2: 
            (1, -1, T, 0, F, 0, [F, T, T, F]),
        cmn_test_3: 
            (i32::MAX, 2, F, 1, F, 0, [T, F, F, T]),
        cmn_test_4: 
            (i32::MIN, -1, F, 0, T, 0, [F, F, T, T]),
        cmn_test_5: 
            (1, 1, T, 1, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: orr,
        orr_test_1: 
            (0b1011101, 0b1001001, T, 0, T, 0b1011101, [F, F, T, F]),
        orr_test_2: 
            (0, 0, T, 1, F, 0, [F, T, F, F]),
        orr_test_3: 
            (1, -1, T, 0, F, -1, [T, F, F, F]),
        orr_test_4: 
            (i32::MIN, 4, T, 3, F, i32::MIN, [T, F, T, F]),
        orr_test_5: 
            (0, 1, F, 1, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: mov,
        mov_test_1: 
            (-1, 23, T, 0, T, 23, [F, F, T, F]),
        mov_test_2: 
            (-1, 7, T, 1, F, 3, [F, F, T, F]),
        mov_test_3: 
            (-1, -30, T, 0, F, -30, [T, F, F, F]),
        mov_test_4: 
            (-1, 3, T, 3, T, 0, [F, T, F, F]),
        mov_test_5: 
            (-1, -30, F, 0, T, -30, [F, F, T, F])
    }

    data_processing_tests! {
        function: bic,
        bic_test_1: 
            (0b1011101, 0b0010100, T, 0, T, 0b1001001, [F, F, T, F]),
        bic_test_2: 
            (0b0101010, 0b1010100, T, 1, T, 0, [F, T, F, F]),
        bic_test_3: 
            (i32::MIN, 0, T, 0, F, i32::MIN, [T, F, F, F]),
        bic_test_4: 
            (1, 12, T, 3, F, 0, [F, T, T, F]),
        bic_test_5: 
            (1, -2, F, 1, F, 0, [F; 4])
    }

    data_processing_tests! {
        function: mvn,
        mvn_test_1: 
            (-1, -24, T, 0, T, 23, [F, F, T, F]),
        mvn_test_2: 
            (-1, -1, T, 1, F, i32::MIN, [T, F, T, F]),
        mvn_test_3: 
            (-1, 30, T, 1, T, -16, [T, F, F, F]),
        mvn_test_4: 
            (-1, -1, T, 0, F, 0, [F, T, F, F]),
        mvn_test_5: 
            (-1, 29, F, 0, T, -30, [F, F, T, F])
    }

    macro_rules! multiply_tests {
        (function: $function:ident, $($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (a, b, c, d, s, res_lo, res_hi, exp_flags) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_register(RegNames::R0, a);
                cpu.set_register(RegNames::R1, b);
                cpu.set_register(RegNames::R2, c);
                cpu.set_register(RegNames::R3, d);

                cpu.$function(s, RegNames::R3, 
                    RegNames::R2, RegNames::R0, RegNames::R1);

                assert_eq!(res_lo, cpu.get_register_intern(RegNames::R2));
                assert_eq!(res_hi, cpu.get_register_intern(RegNames::R3));
                assert_eq!(exp_flags, cpu.flags);
            }
        )*}
    }

    multiply_tests! {
        function: mul,
        mul_test_1: 
            (17, 32, 1, 3, T, 1, 544, [F; 4]),
        mul_test_2: 
            (34, -9, -8, 2, T, -8, -306, [T, F, F, F]),
        mul_test_3: 
            (0, 85, 3, -5, T, 3, 0, [F, T, F, F]),
        mul_test_4: 
            (32768, 131072, 0, 8, T, 0, 0, [F, T, F, F]),
        mul_test_5: 
            (-38, 987, -1, 0, F, -1, -37506, [F; 4])
    }

    multiply_tests! {
        function: mla,
        mla_test_1: 
            (12, 17, 6, 3, T, 6, 210, [F; 4]),
        mla_test_2: 
            (8, -26, 50, 2, T, 50, -158, [T, F, F, F]),
        mla_test_3: 
            (2, 45, -90, -5, T, -90, 0, [F, T, F, F]),
        mla_test_4: 
            (32768, 131072, 15, 8, T, 15, 15, [F; 4]),
        mla_test_5: 
            (-38, 187, -14, 0, F, -14, -7120, [F; 4])
    }

    multiply_tests! {
        function: smull,
        smull_test_1: 
            (11, 89, 1, 3, T, 979, 0, [F; 4]),
        smull_test_2: 
            (17, -28, -8, 2, T, -476, -1, [T, F, F, F]),
        smull_test_3: 
            (0, 85, 3, -5, T, 0, 0, [F, T, F, F]),
        smull_test_4: 
            (334375, 230893, 0, 8, T, -104564453, 17, [F; 4]),
        smull_test_5: 
            (17, -28, -8, 0, F, -476, -1, [F; 4])
    }

    multiply_tests! {
        function: umull,
        umull_test_1: 
            (26, 92, 1, 3, T, 2392, 0, [F; 4]),
        umull_test_2: 
            (-1, -1, -8, 2, T, 1, -2, [T, F, F, F]),
        umull_test_3: 
            (0, 129, 3, -5, T, 0, 0, [F, T, F, F]),
        umull_test_4: 
            (838299, 756273, 0, 8, T, -1672260181, 147, [F; 4]),
        umull_test_5: 
            (-1, -1, -1, 0, F, 1, -2, [F; 4])
    }
    
    multiply_tests! {
        function: smlal,
        smlal_test_1: 
            (83, 32, 102, 0, T, 2758, 0, [F; 4]),
        smlal_test_2: 
            (729276, -937263, 76, 120, T, -623611448, -40, [T, F, F, F]),
        smlal_test_3: 
            (2, -85, 170, 0, T, 0, 0, [F, T, F, F]),
        smlal_test_4: 
            (-042003, 300160, 8838, -3729, T, 277290246, -3732, [T, F, F, F]),
        smlal_test_5: 
            (729276, -937263, 76, 120, F, -623611448, -40, [F; 4])
    }
    
    multiply_tests! {
        function: umlal,
        umlal_test_1: 
            (15, 64, 111, 0, T, 1071, 0, [F; 4]),
        umlal_test_2: 
            (-1, -1, 10, -92, T, 11, -94, [T, F, F, F]),
        umlal_test_3: 
            (3, 43, -129, 0, T, 0, 0, [F, T, F, F]),
        umlal_test_4: 
            (57356, -97526, 736, -847, T, -1298733224, 56507, [F; 4]),
        umlal_test_5: 
            (-1, -1, 10, -92, F, 11, -94, [F; 4])
    }


    macro_rules! miscellaneous_arithmetic_tests {
        (function: $function:ident, $($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (a, result) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_register(RegNames::R0, a);

                cpu.$function(RegNames::R1, RegNames::R0);

                assert_eq!(result, cpu.get_register_intern(RegNames::R1));
            }
        )*}
    }

    miscellaneous_arithmetic_tests! {
        function: clz,
        clz_test_1: (0, 32),
        clz_test_2: (-1, 0),
        clz_test_3: (1 << 31, 0),
        clz_test_4: (1 << 16, 15),
        clz_test_5: (15, 28)
    }


    //todo branch instructions but first implement unconditional instructions (bx)

    macro_rules! load_tests {
        (function: $function:ident, $($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (bytes, address, offset, encoding, result) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_encoding(encoding);
                cpu.set_register(RegNames::R1, address);
                let am = AddressingMode {
                    p: T, u: T, w: F, rn: RegNames::R1, 
                    offset_type: OffsetType::Immediate{ offset }
                };

                let address = (address + (offset as i32)) as u32 as usize;
                cpu.memory.splice(address..address+4, bytes);

                cpu.$function(RegNames::R0, am);
                assert_eq!(result, cpu.get_register(RegNames::R0));            
            }
        )*}
    }

    load_tests! {
        function: ldr,
        ldr_test_1: ([58, 222, 104, 177], 0x2124, 0x0, Endian::Big, 987654321),
        ldr_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, 525252525),
        ldr_test_3: ([0, 92, 166, 215], 0x2124, 0x64, Endian::Big, 6072023),
        ldr_test_4: ([0, 92, 166, 215], 0x2126, 0x0, Endian::Big, 6029312),
        ldr_test_5: ([0, 92, 166, 215], 0x2124, 0x1, Endian::Little, 10902528)
    }

    load_tests! {
        function: ldrb,
        ldrb_test_1: ([58, 222, 104, 177], 0x2127, 0, Endian::Big, 58),
        ldrb_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, 173),
        ldrb_test_3: ([0, 92, 166, 215], 0x2124, 0x64, Endian::Big, 0)
    }

    load_tests! {
        function: ldrbt,
        ldrbt_test_1: ([58, 222, 104, 177], 0x2127, 0, Endian::Big, 58),
        ldrbt_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, 173),
        ldrbt_test_3: ([0, 92, 166, 215], 0x2124, 0x64, Endian::Big, 0)
    }

    load_tests! {
        function: ldrh,
        ldrh_test_1: ([58, 222, 104, 177], 0x2126, 0, Endian::Big, 15070),
        ldrh_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, 47021),
        ldrh_test_3: ([0, 92, 166, 215], 0x2124, 0x64, Endian::Big, 92)
    }

    // found fault: ldrsb did not extend the sign
    load_tests! {
        function: ldrsb,
        ldrsb_test_1: ([58, 222, 104, 177], 0x2127, 0, Endian::Big, 58),
        ldrsb_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, -83),
        ldrsb_test_3: ([255, 92, 166, 215], 0x2124, 0x64, Endian::Big, -1)
    }

    // found fault: ldrsh did not extend the sign
    load_tests! {
        function: ldrsh,
        ldrsh_test_1: ([58, 222, 104, 177], 0x2126, 0, Endian::Big, 15070),
        ldrsh_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, -18515),
        ldrsh_test_3: ([255, 255, 166, 215], 0x2124, 0x64, Endian::Big, -1)
    }

    load_tests! {
        function: ldrt,
        ldrt_test_1: ([58, 222, 104, 177], 0x2124, 0, Endian::Big, 987654321),
        ldrt_test_2: ([173, 183, 78, 31], 0, 0, Endian::Little, 525252525),
        ldrt_test_3: ([0, 92, 166, 215], 0x2124, 0x64, Endian::Big, 6072023)
    }

    macro_rules! store_tests {
        (function: $function:ident, $($test_name:ident: $test_values:expr),*)
        => {$(
            #[test]
            fn $test_name() {
                let (value, address, offset, encoding, result) = $test_values;

                let mut cpu = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
                cpu.set_encoding(encoding);
                cpu.set_register(RegNames::R0, value);
                cpu.set_register(RegNames::R1, address);
                let am = AddressingMode {
                    p: T, u: T, w: F, rn: RegNames::R1, 
                    offset_type: OffsetType::Immediate{ offset }
                };

                cpu.$function(RegNames::R0, am);

                let address = (address + (offset as i32)) as u32 as usize;
                let bytes = &cpu.memory[address..address+4];
                assert_eq!(result, bytes);            
            }
        )*}
    }

    store_tests! {
        function: str,
        str_test_1: (987654321, 0x2124, 0, Endian::Big, [58, 222, 104, 177]),
        str_test_2: (525252525, 0, 0, Endian::Little, [173, 183, 78, 31]),
        str_test_3: (6072023, 0x2124, 0x64, Endian::Big, [0, 92, 166, 215]),
        str_test_4: (6072023, 0x2125, 0x64, Endian::Little, [166, 92, 0, 0]),
        str_test_5: (6072023, 0x2124, 0x66, Endian::Big, [166, 215, 0, 0])
    }

    store_tests! {
        function: strb,
        strb_test_1: (987654321, 0x2127, 0, Endian::Big, [177, 0, 0, 0]),
        strb_test_2: (525252525, 0, 0, Endian::Little, [173, 0, 0, 0]),
        strb_test_3: (6072023, 0x2124, 0x64, Endian::Big, [215, 0, 0, 0])
    }

    store_tests! {
        function: strbt,
        strbt_test_1: (987654321, 0x2127, 0, Endian::Big, [177, 0, 0, 0]),
        strbt_test_2: (525252525, 0, 0, Endian::Little, [173, 0, 0, 0]),
        strbt_test_3: (6072023, 0x2124, 0x64, Endian::Big, [215, 0, 0, 0])
    }

    store_tests! {
        function: strh,
        strh_test_1: (987654321, 0x2126, 0, Endian::Big, [104, 177, 0, 0]),
        strh_test_2: (525252525, 0, 0, Endian::Little, [173, 183, 0, 0]),
        strh_test_3: (6072023, 0x2124, 0x64, Endian::Big, [166, 215, 0, 0])
    }

    store_tests! {
        function: strt,
        strt_test_1: (987654321, 0x2124, 0, Endian::Big, [58, 222, 104, 177]),
        strt_test_2: (525252525, 0, 0, Endian::Little, [173, 183, 78, 31]),
        strt_test_3: (6072023, 0x2124, 0x64, Endian::Big, [0, 92, 166, 215])
    }

}
