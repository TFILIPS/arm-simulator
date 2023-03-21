use super::{SimulatedCPU, names::RegNames, barrel_shifter::ShifterOperand};

impl SimulatedCPU {
    //data processing

    pub(super) fn and(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("and");
    }

    pub(super) fn eor(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("eor");
    }

    pub(super) fn sub(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("sub");
    }

    pub(super) fn rsb(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("rsb");
    }

    pub(super) fn add(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("add");
    }

    pub(super) fn adc(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("adc");
    }

    pub(super) fn sbc(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("sbc");
    }

    pub(super) fn rsc(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("rsc");
    }

    pub(super) fn tst(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("tst");
    }

    pub(super) fn teq(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("teq");
    }

    pub(super) fn cmp(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("cmp");
    }

    pub(super) fn cmn(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("cmn");
    }

    pub(super) fn orr(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("orr");
    }

    pub(super) fn mov(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("mov");
    }

    pub(super) fn bic(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("bic");
    }

    pub(super) fn mvn(
        &self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        println!("mvn");
    }

    // Miscellaneos 
    













}

