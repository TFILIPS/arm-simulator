use super::{
    SimulatedCPU, 
    names::{RegNames, FlagNames},
    barrel_shifter::ShifterOperand
};

impl SimulatedCPU {
    //data processing: doto r15 special behaviour

    pub(super) fn and(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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


    pub(super) fn adc(//difficulties getting carry and overflow (not sure if this is 100% correct)
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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

    pub(super) fn sbc( //kapitel über casrry flag bei c instructions
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);
        let c: i32 = if self.flags[FlagNames::C] {0} else {1};

        let (ir, o1): (i32, bool) = a.overflowing_sub(b);
        let (result, o2): (i32, bool) = ir.overflowing_sub(c);
        self.set_register(rd, result);

        if s {
            self.flags[FlagNames::N] = result < 0;
            self.flags[FlagNames::Z] = result == 0;
            self.flags[FlagNames::C]
                = (result as u32) <= (a as u32) && (b != 0 || c != 0);
            self.flags[FlagNames::V] = o1 || o2;
        }
    }

    pub(super) fn rsc(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, _: bool, rn: RegNames, 
        _: RegNames, so: ShifterOperand
    ) { 
        let a: i32 = self.get_register(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a & b;

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = carry;
    }

    pub(super) fn teq(
        &mut self, _: bool, rn: RegNames, 
        _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
        let (b, carry): (i32, bool) = self.perform_shift(so);

        let result: i32 = a ^ b;

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = carry;
    }

    pub(super) fn cmp(
        &mut self, _: bool, rn: RegNames, 
        _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_sub(b);

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = (result as u32) < (a as u32);
        self.flags[FlagNames::V] = overflow;
    }

    pub(super) fn cmn(
        &mut self, _: bool, rn: RegNames, 
        _: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
        let (b, _): (i32, bool) = self.perform_shift(so);

        let (result,  overflow): (i32, bool) = a.overflowing_add(b);

        self.flags[FlagNames::N] = result < 0;
        self.flags[FlagNames::Z] = result == 0;
        self.flags[FlagNames::C] = (result as u32) < (a as u32);
        self.flags[FlagNames::V] = overflow;
    }

    pub(super) fn orr(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, s: bool, _: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {

        let (value, carry): (i32, bool) = self.perform_shift(so);
        self.set_register(rd, value);

        if s {
            self.flags[FlagNames::N] = value < 0;
            self.flags[FlagNames::Z] = value == 0;
            self.flags[FlagNames::C] = carry;
        }
    }

    pub(super) fn bic(
        &mut self, s: bool, rn: RegNames, 
        rd: RegNames, so: ShifterOperand
    ) {
        let a: i32 = self.get_register(rn);
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
        &mut self, s: bool, _: RegNames, 
        rd: RegNames, so: ShifterOperand
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
    pub(super) fn _mul() {
        todo!()
    }

    pub(super) fn _mla() {
        todo!()
    }

    pub(super) fn _smull() {
        todo!()
    }

    pub(super) fn _umull() {
        todo!()
    }

    pub(super) fn _smlal() {
        todo!()
    }

    pub(super) fn _umlal() {
        todo!()
    }

    // Miscellaneous arithmetic instructions
    pub(super) fn clz(&mut self) {
        //only mi in armv5
        todo!()
    }

    // Branch instructions
    pub(super) fn _b() {
        todo!()
    }

    pub(super) fn _bl() {
        todo!()
    }

    pub(super) fn blx(&mut self) {
        todo!()
    }

    pub(super) fn bx(&mut self) {
        todo!()
    }

    // Load and store instructions
    pub(super) fn _ldr() {
        todo!()
    }

    pub(super) fn _ldrb() {
        todo!()
    }

    pub(super) fn _ldrbt() {
        todo!()
    }

    pub(super) fn _ldrh() {
        todo!()
    }

    pub(super) fn _ldrsb() {
        todo!()
    }

    pub(super) fn _ldrsh() {
        todo!()
    }

    pub(super) fn _ldrt() {
        todo!()
    }

    pub(super) fn _str() {
        todo!()
    }

    pub(super) fn _strb() {
        todo!()
    }

    pub(super) fn _strbt() {
        todo!()
    }

    pub(super) fn _strh() {
        todo!()
    }

    pub(super) fn _strt() {
        todo!()
    }

    pub(super) fn _ldm() {
        todo!()
    }

    pub(super) fn _stm() {
        todo!()
    }

    pub(super) fn _swp() {
        todo!()
    }

    pub(super) fn _swpb() {
        todo!()
    }

    // Exception-generating instructions
    pub(super) fn _swi() {
        todo!()
    }

    pub(super) fn bkpt(&mut self) {
        todo!()
    }

    // Status register access instructions
    pub(super) fn mrs(&mut self) {
        todo!()
    }

    pub(super) fn msr(&mut self) {
        todo!()
    }


    // Coprocessor instructions
    pub(super) fn _cdp() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _cdp2() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _ldc() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _ldc2() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mcr() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mcr2() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mrc() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _mrc2() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _stc() {
        panic!("Coprocessor instructions not supported!")
    }

    pub(super) fn _stc2() {
        panic!("Coprocessor instructions not supported!")
    }
}

#[cfg(test)]
mod tests {
    use crate::simulated_cpu::{
        SimulatedCPU, 
        RegNames, 
        barrel_shifter::{ShifterOperand, ShiftType}
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
                assert_eq!(exp_res, cpu.get_register(RegNames::R0));
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