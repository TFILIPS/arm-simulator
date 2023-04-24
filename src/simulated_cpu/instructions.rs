use std::{process::exit, ops::Range, iter::StepBy};

use crate::utils::{slice_to_u32, BitAccess, slice_to_u16, u32_to_array, u16_to_array};

use super::{
    SimulatedCPU, 
    names::{RegNames, FlagNames},
    operands::{ShifterOperand, AddressingMode, AddressingModeMultiple}
};

const DEBUG_PRINT: bool = false;

//improvements: access registers directly as done with the flags
impl SimulatedCPU {
    pub(super) fn and(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("and"); }

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

    pub(super) fn eor(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("eor"); }

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

    pub(super) fn sub(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("sub"); }
        
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

    pub(super) fn rsb(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("rsb"); }

        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = b.overflowing_sub(a);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = (result as u32) < (a as u32);
            self.flags[FlagNames::V] = overflow;
        }
    }

    pub(super) fn add(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("add"); }

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


    pub(super) fn adc(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("adc"); }

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

    pub(super) fn sbc(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("sbc"); }

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

    pub(super) fn rsc(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("rsc"); }

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
                (result as u32) <= (a as u32) && (b != 0 || c != 0);
            self.flags[FlagNames::V] = o1 || o2;
        }
    }

    pub(super) fn tst(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("tst"); }

        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a & b;

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = carry;
    }

    pub(super) fn teq(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("teq"); }

        let a: i32 = self.get_register_intern(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a ^ b;

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = carry;
    }

    pub(super) fn cmp(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("cmp"); }

        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_sub(b);

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = (result as u32) < (a as u32);
        self.flags[FlagNames::V] = overflow;
    }

    pub(super) fn cmn(
        &mut self, _: bool, rn: RegNames, _: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("cmn"); }

        let a: i32 = self.get_register_intern(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_add(b);

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = (result as u32) < (a as u32);
        self.flags[FlagNames::V] = overflow;
    }

    pub(super) fn orr(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("orr"); }

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

    pub(super) fn mov(
        &mut self, s: bool, _: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("mov"); }

        let (value, carry): (i32, bool) = self.perform_shift(so);
        self.set_register(rd, value);

        if s {
            self.flags[FlagNames::N] = value < 0;
            self.flags[FlagNames::Z] = value == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    pub(super) fn bic(
        &mut self, s: bool, rn: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("bic"); }

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

    pub(super) fn mvn(
        &mut self, s: bool, _: RegNames, rd: RegNames, so: ShifterOperand
    ) {
        if DEBUG_PRINT { println!("mvn"); }

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
    pub(super) fn mul(
        &mut self, s: bool, rd: RegNames, rs: RegNames, rm: RegNames 
    ) {
        if DEBUG_PRINT { println!("mul"); }

        let a: i32 = self.get_register_intern(rm);
        let b: i32 = self.get_register_intern(rs);

        let result = a.wrapping_mul(b);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    pub(super) fn mla(
        &mut self, s: bool, rd: RegNames, rn: RegNames, 
        rs: RegNames, rm: RegNames 
    ) {
        if DEBUG_PRINT { println!("mla"); }

        let a: i32 = self.get_register_intern(rm);
        let b: i32 = self.get_register_intern(rs);
        let c: i32 = self.get_register_intern(rn);

        let result = a.wrapping_mul(b).wrapping_add(c);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
        }
    }

    pub(super) fn smull(
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        if DEBUG_PRINT { println!("smull"); }

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

    pub(super) fn umull(        
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        if DEBUG_PRINT { println!("umull"); }

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

    pub(super) fn smlal(
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        if DEBUG_PRINT { println!("smlal"); }

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

    pub(super) fn umlal(
        &mut self, s: bool, rdhi: RegNames, rdlo: RegNames,
        rs: RegNames, rm: RegNames
    ) {
        if DEBUG_PRINT { println!("umlal"); }

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
    pub(super) fn clz(&mut self, rd: RegNames, rm: RegNames) {
        if DEBUG_PRINT { println!("clz"); }

        let a: i32 = self.get_register_intern(rm);

        let result: i32 = a.leading_zeros() as i32;
        self.set_register(rd, result);
    }
    

    // Branch instructions
    pub(super) fn b(&mut self, l: bool, si: i32) {
        if DEBUG_PRINT { println!("{:}", if l {"bl"} else {"b"}); }

        let prog_addr: i32 = self.get_register_intern(RegNames::PC);

        if l {
            let link_addr: i32 = prog_addr.wrapping_sub(4);
            self.set_register(RegNames::LR, link_addr);
        }

        let new_prog_addr = prog_addr.wrapping_add(si << 2);
        self.set_register(RegNames::PC, new_prog_addr);
    }

    pub(super) fn blx(&mut self) {
        // This behaviour is incorrect! After switching to 
        // Thumb state on a non T CPU the next executed instruction
        // causes an UndefinedInstructionExeption. Then the cpu
        // switches back to ARM.
        panic!("Thumb instruction set not supported!");
    }

    pub(super) fn bx(&mut self, rm: RegNames) {
        // This behaviour is incorrect! After switching to 
        // Thumb state on a non T CPU the next executed instruction
        // causes an UndefinedInstructionExeption. Then the cpu
        // switches back to ARM.
        if DEBUG_PRINT { println!("bx"); }

        let target = self.get_register_intern(rm);
        self.set_register(RegNames::PC, target);
    }


    // Load and store instructions
    pub(super) fn ldr(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("ldr"); }

        let mut address: usize = self.compute_modify_address(rn, am);
        let rot_bits: u32 = (address as u32).cut_bits(0..=1);
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

    pub(super) fn ldrb(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("ldrb"); }

        let address: usize = self.compute_modify_address(rn, am);
        let value: u32 = self.memory[address] as u32;
        self.set_register(rd, value as i32);
    }

    pub(super) fn ldrbt(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) { self.ldrb(rn, rd, am); }

    pub(super) fn ldrh(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("ldrh"); }

        let address: usize = self.compute_modify_address(rn, am);
        let bytes: &[u8] = &self.memory[address..address+2];
        let value: u32 = slice_to_u16(bytes, &self.encoding) as u32;
        self.set_register(rd, value as i32);
    }

    pub(super) fn ldrsb(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("ldrsb"); }

        let address: usize = self.compute_modify_address(rn, am);
        let value: i32 = self.memory[address] as i32;
        self.set_register(rd, value);
    }

    pub(super) fn ldrsh(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("ldrsh"); }

        let address: usize = self.compute_modify_address(rn, am);
        let bytes: &[u8] = &self.memory[address..address+2];
        let value: i32 = slice_to_u16(bytes, &self.encoding) as i32;
        self.set_register(rd, value);
    }

    pub(super) fn ldrt(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) { self.ldr(rn, rd, am); }

    pub(super) fn str(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("str"); }

        let mut address: usize = self.compute_modify_address(rn, am);
        address &= 0xFFFFFFFC;

        let value = self.get_register_intern(rd) as u32;
        let bytes: [u8; 4] = u32_to_array(value, &self.encoding);
        
        self.memory.splice(address..address+4, bytes);
    }

    pub(super) fn strb(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("strb"); }

        let address: usize = self.compute_modify_address(rn, am);
        let value: u8 = self.get_register_intern(rd) as u8;
        self.memory[address] = value;
    }

    pub(super) fn strbt(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) { self.strb(rn, rd, am); }

    pub(super) fn strh(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) {
        if DEBUG_PRINT { println!("str"); }

        let address: usize = self.compute_modify_address(rn, am);

        let value: u16 = self.get_register_intern(rd) as u16;
        let bytes: [u8; 2] = u16_to_array(value, &self.encoding);
        
        self.memory.splice(address..address+2, bytes);
    }

    pub(super) fn strt(
        &mut self, rn: RegNames, rd: RegNames, am: AddressingMode
    ) { self.str(rn, rd, am); }

    pub(super) fn ldm(&mut self, amm: AddressingModeMultiple) {
        if DEBUG_PRINT { println!("ldm"); }

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

    pub(super) fn stm(&mut self, amm: AddressingModeMultiple) {
        if DEBUG_PRINT { println!("stm"); }

        let mut addresses: StepBy<Range<usize>> = 
            self.compute_modify_address_multiple(&amm);

        for i in 0..16 {
            if amm.register_list.get_bit(i) {
                let address: usize = addresses.next().unwrap() & 0xFFFFFFFC;
                
                let reg_name: RegNames = (i as u32).into();
                let value = self.get_register_intern(reg_name);

                let bytes: [u8; 4] = match self.encoding {
                    crate::utils::Endian::Little => value.to_le_bytes(),
                    crate::utils::Endian::Big => value.to_be_bytes(),
                };
                self.memory.splice(address..address+4, bytes);
            }
        }
    }

    pub(super) fn swp(&mut self, rn: RegNames, rd: RegNames, rm: RegNames) {
        if DEBUG_PRINT { println!("swp"); }

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

    pub(super) fn swpb(&mut self, rn: RegNames, rd: RegNames, rm: RegNames) {
        if DEBUG_PRINT { println!("swpb"); }

        let address: usize = self.get_register_intern(rn) as u32 as usize;

        let lv: u8 = self.memory[address];
        let sv: u8 = self.get_register_intern(rm) as u8;
        
        self.memory[address] = sv;
        self.set_register(rd, lv as i32);
    }

    // Exception-generating instructions
    pub(super) fn swi(&mut self) {
        if DEBUG_PRINT { println!("swi"); }
        match (self.get_register_intern(RegNames::R0), self.get_register_intern(RegNames::R7)) {
            (1, 4) => {
                let len = self.get_register_intern(RegNames::R2) as u32 as usize;
                let addr = self.get_register_intern(RegNames::R1) as u32 as usize;
                print!("{:#}", String::from_utf8_lossy(&self.memory[addr..addr+len]));
            },
            (x, 1) => exit(x),
            (_, _) => ()
        }
    }

    pub(super) fn bkpt(&mut self) {
        if DEBUG_PRINT { println!("bkpt") }
        // stop execution in webinterface
        // meanwhile do nothing
    }


    // Status register access instructions
    pub(super) fn mrs(&mut self) {
        panic!("Register not supported yet!")
    }

    pub(super) fn msr(&mut self) {
        panic!("Register not supported yet!")
    }


    // Coprocessor instructions
    pub(super) fn _cdp(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _cdp2(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _ldc(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _ldc2(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mcr(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mcr2(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mrc(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mrc2(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _stc(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _stc2(&mut self) {
        panic!("Coprocessor instructions not supported!")
    }
}


#[cfg(test)]
mod tests {
    use crate::simulated_cpu::{
        SimulatedCPU, 
        RegNames, 
        operands::{ShifterOperand, barrel_shifter::ShiftType}
    };

    macro_rules! data_processing_tests {
        (function: $function:ident, $($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (a, b, s, shift, exp_res, exp_flags) = $test_values;

                let mut cpu: SimulatedCPU = SimulatedCPU::new();
                cpu.set_register(RegNames::R1, a);
                cpu.set_register(RegNames::R2, b);
                let so = ShifterOperand::ImmediateShift{
                    rm: RegNames::R2,
                    shift: ShiftType::LSR,
                    shift_amount: shift
                };
                cpu.$function(s, RegNames::R1, RegNames::R0, so);
                assert_eq!(exp_res, cpu.get_register_intern(RegNames::R0));
                assert_eq!(exp_flags, cpu.flags);
            }
        )*}
    }

    data_processing_tests! {
        function: and,
        and_test_1: 
            (0b1011101, 0b1101011, true, 0, 0b1001001, [false; 4]),
        and_test_2: 
            (0b101010, 0b101010, true, 1, 0, [false, true, false, false]),
        and_test_3: 
            (i32::MIN, -1, true, 0, i32::MIN, [true, false, false, false]),
        and_test_4: 
            (-1, 4, true, 3, 0, [false, true, true, false]),
        and_test_5: 
            (1, 1, false, 1, 0, [false; 4])
    }

    data_processing_tests! {
        function: add,
        add_test_1: 
            (12, 7, true, 0, 19, [false; 4]),
        add_test_2: 
            (1, -1, true, 0, 0, [false, true, true, false]),
        add_test_3: 
            (i32::MAX, 1, true, 0, i32::MIN, [true, false, false, true]),
        add_test_4: 
            (-3, -17, false, 0, -20, [false; 4]),
        add_test_5: 
            (i32::MIN, -1, true, 0, i32::MAX, [false, false, true, true])
    }
}
