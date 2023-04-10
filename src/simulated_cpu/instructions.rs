use std::result;

use super::{SimulatedCPU, names::{RegNames, FlagNames}, barrel_shifter::ShifterOperand};

const PRINT_FUNCTION: bool = false;

impl SimulatedCPU {
    //data processing

    pub(super) fn and(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        if PRINT_FUNCTION {
            println!("and(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
        }
        
        let a: i32 = self.get_register(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result = a & b;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    pub(super) fn eor(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        if PRINT_FUNCTION {
            println!("eor(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
        }

        let a: i32 = self.get_register(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result = a ^ b;
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    pub(super) fn sub(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        if PRINT_FUNCTION {
            println!("sub(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
        }

        let a: i32 = self.get_register(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_sub(b);
        let (_, carry): (u32, bool) = (a as u32).overflowing_sub(b as u32);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
            self.flags[FlagNames::V] = overflow;
        }
    }

    pub(super) fn rsb(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        if PRINT_FUNCTION {
            println!("rsb(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
        }

        let a: i32 = self.get_register(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = b.overflowing_sub(a);
        let (_, carry): (u32, bool) = (b as u32).overflowing_sub(a as u32);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
            self.flags[FlagNames::V] = overflow;
        }
    }

    pub(super) fn add(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        if PRINT_FUNCTION {
            println!("add(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
        }

        let a: i32 = self.get_register(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_add(b);
        let (_, carry): (u32, bool) = (a as u32).overflowing_add(b as u32);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C] = carry;
            self.flags[FlagNames::V] = overflow;
        }
    }

    pub(super) fn adc(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("adc(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn sbc(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("sbc(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn rsc(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("rsc(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn tst(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("tst(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn teq(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("teq(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn cmp(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("cmp(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn cmn(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("cmn(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn orr(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("orr(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn mov(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("mov(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);

        let (value, carry) = self.perform_shift(so);
        self.set_register(rd, value);
    }

    pub(super) fn bic(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("bic(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn mvn(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("mvn(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    // Miscellaneos 
    













}

