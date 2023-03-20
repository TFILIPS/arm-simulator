use super::{SimulatedCPU};

impl SimulatedCPU {
    pub(super) fn and(&self) {
        println!("and");
    }

    pub(super) fn eor(&self) {
        println!("eor");
    }

    pub(super) fn sub(&self) {
        println!("sub");
    }

    pub(super) fn rsb(&self) {
        println!("rsb");
    }

    pub(super) fn add(&self) {
        println!("add");
    }

    pub(super) fn adc(&self) {
        println!("adc");
    }

    pub(super) fn sbc(&self) {
        println!("sbc");
    }

    pub(super) fn rsc(&self) {
        println!("rsc");
    }

    pub(super) fn tst(&self) {
        println!("tst");
    }

    pub(super) fn teq(&self) {
        println!("teq");
    }

    pub(super) fn cmp(&self) {
        println!("cmp");
    }

    pub(super) fn cmn(&self) {
        println!("cmn");
    }

    pub(super) fn orr(&self) {
        println!("orr");
    }

    pub(super) fn mov(&self) {
        println!("mov");
    }

    pub(super) fn bic(&self) {
        println!("bic");
    }

    pub(super) fn mvn(&self) {
        println!("mvn");
    }
}

