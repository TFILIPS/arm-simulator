use std::{mem::transmute, fmt::Display};

#[repr(u32)]
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftType { 
    LSL, LSR, ASR, ROR, RRX //RRX is encoded as a special case of ROR
} 
// Allow convertion from u32 to ShiftType
impl From<u32> for ShiftType {
    fn from(value: u32) -> Self {
        const NUM_SHIFTS: u32 = 5;
        if value >= NUM_SHIFTS {
            panic!("Convertion to ShiftType failed! value >= {NUM_SHIFTS}");
        }
        unsafe { transmute(value) }
    }
}
impl Display for ShiftType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ShiftType {
    pub(super) fn compute(
        &self, value: i32, amount: u8, carry: bool
    ) -> (i32, bool) {
        
        let result: i32 = match self {
            ShiftType::LSL => {
                value.checked_shl(amount as u32).unwrap_or(0)
            },
            ShiftType::LSR => {
                (value as u32).checked_shr(amount as u32).unwrap_or(0) as i32
            },
            ShiftType::ASR => {
                value.checked_shr(amount as u32).unwrap_or(0)
            },
            ShiftType::ROR => {
                value.rotate_right((amount as u32) % 32)
            }
            ShiftType::RRX => {
                let result = ((value as u32) >> 1) as i32;
                if carry { result | 1 << 31 } else { result }
            }
        };

        //possible improvement: inplement bitacces for i32
        let new_carry: bool = match self {
            ShiftType::LSL | ShiftType::LSR | ShiftType::ASR 
            if amount > 32 => { 
                false 
            },
            ShiftType::LSL | ShiftType::LSR | ShiftType::ASR | ShiftType::ROR 
            if amount == 0 => { 
                carry 
            },
            ShiftType::LSL => {
                ((value >> (32 - amount)) & 1) == 1
            }
            ShiftType::LSR | ShiftType::ASR | ShiftType::ROR  => {
                ((value >> ((amount - 1) % 32)) & 1) == 1
            },
            ShiftType::RRX => {
                value & 1 == 1
            }
        };

        (result, new_carry)
    }
}

#[cfg(test)]
mod tests {
    use super::ShiftType;
    use crate::utils::{T, F};

    macro_rules! barrel_shifter_tests {
        (shift: $shift:ident, $($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                #[allow(overflowing_literals)]
                let (val_in, c_in , shift_amount, val_out, c_out) = $test_values;

                let (res_val, res_c) = 
                    ShiftType::$shift.compute(val_in, shift_amount, c_in);

                assert_eq!(val_out, res_val as u32);
                assert_eq!(c_out, res_c);
            }
        )*}
    }
    
    barrel_shifter_tests! {
        shift: LSL,
        lsl_test_1: (0xA31A_C315, T, 05, 0x6358_62A0, F),
        lsl_test_2: (0x04ED_9896, T, 00, 0x04ED_9896, T),
        lsl_test_3: (0xDA0A_101C, F, 32, 0x0000_0000, F),
        lsl_test_4: (0xFFFF_FFFF, F, 14, 0xFFFF_C000, T)
    }

    barrel_shifter_tests! {
        shift: LSR,
        lsr_test_1: (0xA46F_BE2B, F, 08, 0x00A4_6FBE, F),
        lsr_test_2: (0x12C3_3828, T, 46, 0x0000_0000, F),
        lsr_test_3: (0xFF36_6BDA, F, 27, 0x0000_001F, T)
    }

    barrel_shifter_tests! {
        shift: ASR,
        asr_test_1: (0xD698_4185, T, 13, 0xFFFE_B4C2, F),
        asr_test_2: (0x238E_3984, F, 09, 0x0011_C71C, T),
        asr_test_3: (0xFBC9_75BB, T, 01, 0xFDE4_BADD, T)
    }

    barrel_shifter_tests! {
        shift: ROR,
        ror_test_1: (0x5454_7814, T, 02, 0x1515_1E05, F),
        ror_test_2: (0x9537_23D6, F, 11, 0x7AD2_A6E4, F),
        ror_test_3: (0x2593_14F9, F, 05, 0xC92C_98A7, T),
        ror_test_4: (0xFFE2_2B87, T, 36, 0x7FFE_22B8, F)
    }

    barrel_shifter_tests! {
        shift: RRX,
        rrx_test_1: (0x7AB3_8766, T, 00, 0xBD59_C3B3, F),
        rrx_test_2: (0xA794_B0ED, F, 07, 0x53ca_5876, T)
    }
}
