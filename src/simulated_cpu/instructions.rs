use super::{SimulatedCPU, names::RegNames, barrel_shifter::ShifterOperand};

impl SimulatedCPU {
    //data processing

    pub(super) fn and(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("and(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn eor(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("eor(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn sub(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("sub(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn rsb(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("rsb(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn add(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("add(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn adc(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("adc(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn sbc(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("sbc(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn rsc(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("rsc(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn tst(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("tst(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn teq(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("teq(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn cmp(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("cmp(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn cmn(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("cmn(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn orr(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("orr(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn mov(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("mov(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn bic(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("bic(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    pub(super) fn mvn(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("mvn(s: {s}, rd: {:?}, rn: {:?}, so: {:?})", rd, rn, so);
    }

    // Miscellaneos 
    













}

