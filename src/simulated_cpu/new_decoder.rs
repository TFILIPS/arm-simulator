use std::{io::Write, mem::transmute, fmt::Display};

use crate::utils::{BitAccess, T, F};
use super::{
    SimulatedCPU, 
    names::{FlagNames, RegNames}, 
    operands::{ShifterOperand, AddressingMode, AddressingModeMultiple}
};

#[repr(u32)]
#[derive(Debug)]
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

type MultiplyFunctionSignature = 
    fn(&mut SimulatedCPU, bool, RegNames, RegNames, RegNames, RegNames);
type LoadStoreSignature =
    fn(&mut SimulatedCPU, RegNames, RegNames, AddressingMode);

pub enum InstructionDecoder {
    Action, Disassembly { buffer: Vec<u8> }
}
impl InstructionDecoder {
    pub fn decode(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        if instruction.cut_bits(28..=31) != 0b1111 {
            match instruction.cut_bits(26..=27) {
                0b00 => self.handle_data_and_miscellaneos(instruction),
                0b01 => self.handle_load_store(instruction),
                0b10 => self.handle_branch_and_block_transfer(instruction),
                0b11 => self.handle_copro_and_swi(instruction),
                _ => None
            }
        }
        else {
            todo!("Unconditional instructions")
        }
    }

    pub fn get_string(&self) -> Option<String> {
        match self {
            InstructionDecoder::Action => None,
            InstructionDecoder::Disassembly { buffer } => {
                Some(String::from_utf8_lossy(&buffer).to_string()) 
            }
        }
    }

    const DPIS: [
        (fn(&mut SimulatedCPU, bool, RegNames, 
            RegNames, ShifterOperand), &str); 16
    ] = [
        (SimulatedCPU::and, "AND"), (SimulatedCPU::eor, "EOR"), 
        (SimulatedCPU::sub, "SUB"), (SimulatedCPU::rsb, "RSB"), 
        (SimulatedCPU::add, "ADD"), (SimulatedCPU::adc, "ADC"),
        (SimulatedCPU::sbc, "SBC"), (SimulatedCPU::rsc, "RSC"), 
        (SimulatedCPU::tst, "TST"), (SimulatedCPU::teq, "TEQ"), 
        (SimulatedCPU::cmp, "CMP"), (SimulatedCPU::cmn, "CMN"),
        (SimulatedCPU::orr, "ORR"), (SimulatedCPU::mov, "MOV"), 
        (SimulatedCPU::bic, "BIC"), (SimulatedCPU::mvn, "MVN")
    ];

    fn handle_data_and_miscellaneos(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        let op: bool = instruction.get_bit(25);
        let op1: u32 = instruction.cut_bits(20..=24);
        let op2: u32 = instruction.cut_bits(4..=7);

        if !op {
            if !bm(op1, 0b11001, 0b10000) {
                if bm(op2, 0b0001, 0b0000) {
                    let so: ShifterOperand = ShifterOperand::ImmediateShift { 
                        shift_amount: instruction.cut_bits(7..=11)
                            .try_into().unwrap(), 
                        shift: instruction.cut_bits(5..=6).into(), 
                        rm: instruction.cut_bits(0..=3).into()
                    };
                    return self.handle_data(instruction, so);
                }
                else if bm(op2, 0b1001, 0b0001) {
                    let so: ShifterOperand = ShifterOperand::RegisterShift { 
                        rs: instruction.cut_bits(8..=11).into(), 
                        shift: instruction.cut_bits(5..=6).into(), 
                        rm: instruction.cut_bits(0..=3).into()
                    };
                    return self.handle_data(instruction, so);
                }
            }
            else if bm(op2, 0b1000, 0b0000) {
                return self.handle_miscellaneos(instruction);
            }

            if bm(op2, 0b1111, 0b1001) {
                if bm(op1, 0b10000, 0b00000) {
                    return self.handle_multiply(instruction);
                }
                else if bm(op1, 0b10000, 0b10000) {
                    return self.handle_synchronization(instruction);
                }
            }
            else if bm(op2, 0b1111, 0b1011) || bm(op2, 0b1101, 0b1101) {
                if !bm(op1, 0b10010, 0b00010) {
                    return self.handle_extra_load_store(instruction);
                }
            }
        }
        else {
            if !bm(op1, 0b11001, 0b10000) {
                let so: ShifterOperand = ShifterOperand::Immediate { 
                    rotate: instruction.cut_bits(8..=11).try_into().unwrap(), 
                    immediate: instruction.cut_bits(0..=7).try_into().unwrap() 
                };
                return self.handle_data(instruction, so);
            }
            else if bm(op1, 0b11011, 0b10010) {
                let cond: Condition = Condition::from_instruction(instruction);
                return match self {
                    InstructionDecoder::Action => Some(Box::new(
                        move |cpu: &mut SimulatedCPU| 
                        if cond.is_satisfied(&cpu.flags) { cpu.msr() }
                    )),
                    InstructionDecoder::Disassembly { buffer } => {
                        writeln!(buffer, "MSR").unwrap();
                        None
                    }
                }
            }
        }
        None
    }

    fn handle_data(&mut self, instruction: u32, so: ShifterOperand) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        let cond: Condition = Condition::from_instruction(instruction);

        let opcode: usize = instruction.cut_bits(21..=24) as usize;
        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();
        let s: bool = instruction.get_bit(20);

        match self {
            InstructionDecoder::Action => {
                Some(Box::new(move |cpu: &mut SimulatedCPU| {
                    if cond.is_satisfied(&cpu.flags) {
                        InstructionDecoder::DPIS[opcode].0(cpu, s, rn, rd, so);
                    }
                }))
            },
            InstructionDecoder::Disassembly { buffer } => {
                writeln!(buffer, "{:}{cond}{:} {rd}, {rn}, {so}", 
                    InstructionDecoder::DPIS[opcode].1, if s {"S"} else {""}
                ).unwrap();
                None
            }
        }
    }

    fn handle_miscellaneos(&mut self, instruction: u32)
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        let cond: Condition = Condition::from_instruction(instruction);

        let op: u32 = instruction.cut_bits(21..=22);
        let op2: u32 = instruction.cut_bits(4..=6);

        let rm: RegNames = instruction.cut_bits(0..=3).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();

        let (f, n): (Box<dyn Fn(&mut SimulatedCPU)>, &str) = match (op2, op) {
            (0b000, 0b00 | 0b10) => 
                (Box::new(|cpu: &mut SimulatedCPU| cpu.mrs()), "MRS"),
            (0b000, 0b01 | 0b11) => 
                (Box::new(|cpu: &mut SimulatedCPU| cpu.msr()), "MSR"),
            (0b001, 0b01) => 
                (Box::new(move |cpu: &mut SimulatedCPU| cpu.bx(rm)), "BX"),
            (0b001, 0b11) => 
                (Box::new(move |cpu: &mut SimulatedCPU| cpu.clz(rd,rm)), "CLZ"),
            (0b011, 0b01) => 
                (Box::new(|cpu: &mut SimulatedCPU| cpu.blx()), "BLX"),
            (0b111, 0b01) => 
                (Box::new(|cpu: &mut SimulatedCPU| cpu.bkpt()), "BKPT"),        
            _ => return None
        };

        match self {
            InstructionDecoder::Action => Some(Box::new(
                move |cpu: &mut SimulatedCPU| 
                if cond.is_satisfied(&cpu.flags) { f(cpu) }
            )),
            InstructionDecoder::Disassembly { buffer } => {
                match n {
                    "BX" => write!(buffer, "BX{cond} {rm}").unwrap(),
                    "CLZ" => write!(buffer, "CLZ{cond} {rd}, {rm}").unwrap(),
                    "MRS" | "MSR" | "BLX" | "BKPT" => 
                        writeln!(buffer, "{n}{cond}").unwrap(),
                    _ => ()
                }
                None
            }
        }
    }

    fn handle_multiply(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        let cond: Condition = Condition::from_instruction(instruction);
        let op: u32 = instruction.cut_bits(21..=23);

        let (function, name): (MultiplyFunctionSignature, &str) = match op {
            0b000 => (SimulatedCPU::mul, "MUL"),
            0b001 => (SimulatedCPU::mla, "MLA"),
            0b100 => (SimulatedCPU::umull, "UMULL"),
            0b101 => (SimulatedCPU::umlal, "UMLAL"),
            0b110 => (SimulatedCPU::smull, "SMULL"),
            0b111 => (SimulatedCPU::smlal, "SMLAL"),
            _ => return None
        };

        let s: bool = instruction.get_bit(20);
        let rd_hi: RegNames = instruction.cut_bits(16..=19).into();
        let rn_lo: RegNames = instruction.cut_bits(12..=15).into();
        let rs: RegNames = instruction.cut_bits(8..=11).into();
        let rm: RegNames = instruction.cut_bits(0..=3).into();

        match self {
            InstructionDecoder::Action => Some(Box::new(
                move |cpu: &mut SimulatedCPU| 
                if cond.is_satisfied(&cpu.flags) {
                    function(cpu, s, rd_hi, rn_lo, rs, rm);
                }
            )),
            InstructionDecoder::Disassembly { buffer } => {
                write!(buffer, "{name}{cond}{:}", if s {"S"} else {""})
                    .unwrap();
                match name {
                    "MUL" => 
                        writeln!(buffer, "{rd_hi}, {rm}, {rs}"),
                    "MLA" => writeln!(buffer, "{rd_hi}, {rm}, {rs}, {rn_lo}"),
                    _ => writeln!(buffer, "{rn_lo}, {rd_hi}, {rm}, {rs}")
                }.unwrap();
                None
            }
        }
    }

    fn handle_synchronization(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        if instruction.get_bit(23) || instruction.cut_bits(20..=21) != 0 {
            return None; 
        }

        //maybe combine swp and swpb to one function lets first look at 
        //str and ldr        
        let function: fn(&mut SimulatedCPU, RegNames, RegNames, RegNames); 
        let name: &str;
        if instruction.get_bit(22) {
            function = SimulatedCPU::swpb;
            name = "SWPB";
        }
        else {
            function = SimulatedCPU::swp;
            name = "SWP";
        };

        let cond: Condition = Condition::from_instruction(instruction);
        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();
        let rm: RegNames = instruction.cut_bits(0..=3).into();
        
        match self {
            InstructionDecoder::Action => Some(Box::new(
                move |cpu: &mut SimulatedCPU| 
                if cond.is_satisfied(&cpu.flags) {
                    function(cpu, rn, rd, rm);
                }
            )),
            InstructionDecoder::Disassembly { buffer } => {
                writeln!(buffer, "{name}{cond} {rd} {rm} {rn}").unwrap();
                None
            }
        }
    }

    fn handle_extra_load_store(&mut self, instruction: u32)
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {
        
        let op1: bool = instruction.get_bit(20);
        let op2: u32 = instruction.cut_bits(5..=6);

        let (function, name): (LoadStoreSignature, &str) = if op1 {
            match op2 {
                0b01 => (SimulatedCPU::ldrh, "LDRH"),
                0b10 => (SimulatedCPU::ldrsb, "LDRSB"),
                0b11 => (SimulatedCPU::ldrsh, "LDRSH"),
                _ =>  return None
            }
        }
        else if op2 == 0b01 { (SimulatedCPU::strh, "STRH") }
        else { return None };

        let p: bool = instruction.get_bit(24);
        let u: bool = instruction.get_bit(23);
        let w: bool = instruction.get_bit(21);

        let am: AddressingMode = if instruction.get_bit(24) {
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

        let cond: Condition = Condition::from_instruction(instruction);
        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();

        match self {
            InstructionDecoder::Action => Some(Box::new(
                move |cpu: &mut SimulatedCPU| 
                if cond.is_satisfied(&cpu.flags) {
                    function(cpu, rn, rd, am);
                }
            )),
            InstructionDecoder::Disassembly { buffer } => {
                writeln!(buffer, "{name}{cond} {rd} comming soon!").unwrap();
                None
            }
        }
    }

    fn handle_load_store(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        let p: bool = instruction.get_bit(24);
        let u: bool = instruction.get_bit(23);
        let w: bool = instruction.get_bit(21);

        let am: AddressingMode = if instruction.get_bit(25) {
            if instruction.get_bit(4) {
                //media instructions
                return None;
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

        let (function, name): (LoadStoreSignature, &str) = match (p, b, w, l) {
            (F, F, T, F) => (SimulatedCPU::strt, "STRT"),
            (_, F, _, F) => (SimulatedCPU::str, "STR"),
            (F, F, T, T) => (SimulatedCPU::ldrt, "LDRT"),
            (_, F, _, T) => (SimulatedCPU::ldr, "LDR"),
            (F, T, T, F) => (SimulatedCPU::strbt, "STRBT"),
            (_, T, _, F) => (SimulatedCPU::strb, "STRB"),
            (F, T, T, T) => (SimulatedCPU::ldrbt, "LDRBT"),
            (_, T, _, T) => (SimulatedCPU::ldrb, "LDRB")
        };

        let cond: Condition = Condition::from_instruction(instruction);
        let rn: RegNames = instruction.cut_bits(16..=19).into();
        let rd: RegNames = instruction.cut_bits(12..=15).into();
        
        match self {
            InstructionDecoder::Action => Some(Box::new(
                move |cpu: &mut SimulatedCPU| 
                if cond.is_satisfied(&cpu.flags) {
                    function(cpu, rn, rd, am);
                }
            )),
            InstructionDecoder::Disassembly { buffer } => {
                writeln!(buffer, "{name}{cond} {rd} comming soon!").unwrap();
                None
            }
        }
    }

    fn handle_branch_and_block_transfer(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        let cond: Condition = Condition::from_instruction(instruction);
        if instruction.get_bit(25) {
            let mut imm: i32 = instruction.cut_bits(0..24) as i32;
            imm = (imm << 8) >> 8; //sign extend
            let l: bool = instruction.get_bit(24);
            return match self {
                InstructionDecoder::Action => Some(Box::new(
                    move |cpu: &mut SimulatedCPU| 
                    if cond.is_satisfied(&cpu.flags) {
                        SimulatedCPU::b(cpu, l, imm);
                    }
                )),
                InstructionDecoder::Disassembly { buffer } => {
                    writeln!(buffer, "B{:} #{imm}", 
                        if l {"L"} else {""}).unwrap();
                    None
                },
            };
        }
            
        let amm: AddressingModeMultiple = AddressingModeMultiple { 
            p: instruction.get_bit(24),
            u: instruction.get_bit(23),
            w: instruction.get_bit(21), 
            rn: instruction.cut_bits(16..=19).into(), 
            register_list: instruction.cut_bits(0..=15) as u16
        };

        let function: fn(&mut SimulatedCPU, AddressingModeMultiple); 
        let name: &str;
        if instruction.get_bit(20) {
            function = SimulatedCPU::ldm;
            name = "LDM";
        }
        else {
            function = SimulatedCPU::stm;
            name = "STM";
        };
        match self {
            InstructionDecoder::Action => Some(Box::new(
                move |cpu: &mut SimulatedCPU| 
                if cond.is_satisfied(&cpu.flags) {
                    function(cpu, amm);
                }
            )),
            InstructionDecoder::Disassembly { buffer } => {
                writeln!(buffer, "{name}{cond} comming soon!").unwrap();
                None
            }
        }
    }

    fn handle_copro_and_swi(&mut self, instruction: u32) 
        -> Option<Box<dyn Fn(&mut SimulatedCPU)>> {

        //works at the moment, need to be expanded for cop calls
        if instruction.cut_bits(24..=25) == 0b11 {
            let cond: Condition = Condition::from_instruction(instruction);
            match self {
                InstructionDecoder::Action => Some(Box::new(
                    move |cpu: &mut SimulatedCPU| 
                    if cond.is_satisfied(&cpu.flags) {
                        SimulatedCPU::swi(cpu)
                    }
                )),
                InstructionDecoder::Disassembly { buffer } => {
                    writeln!(buffer, "SWI{cond} #0").unwrap(); //todo: read number
                    None
                }
            }
        }
        else { None }
    }
}

fn bm(value: u32, mask: u32, expectation: u32) -> bool {
    (value & mask) ^ expectation == 0
}