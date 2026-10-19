use std::{ops::{Index, IndexMut}, fmt::Display};
use wasm_bindgen::prelude::wasm_bindgen;

use crate::utils::variant_from_index;

#[repr(u32)]
#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ARMv5FlagNames { 
    N, Z, C, V 
}
// Enable enum FlagNames to index arrays
impl<T> Index<ARMv5FlagNames> for [T] {
    type Output = T;
    fn index(&self, index: ARMv5FlagNames) -> &Self::Output {
        return &self[index as usize];
    }
}
impl<T> IndexMut<ARMv5FlagNames> for [T] {
    fn index_mut(&mut self, index: ARMv5FlagNames) -> &mut Self::Output {
        return &mut self[index as usize];
    }
}

#[repr(u32)]
#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ARMv5RegNames { 
    R0, R1, R2, R3, R4, R5, R6, 
    R7, R8, R9, R10, R11, R12,
    SP, LR, PC
}
// Enable enum RegNames to index arrays
impl<T> Index<ARMv5RegNames> for [T] {
    type Output = T;
    fn index(&self, index: ARMv5RegNames) -> &Self::Output {
        return &self[index as usize];
    }
}
impl<T> IndexMut<ARMv5RegNames> for [T] {
    fn index_mut(&mut self, index: ARMv5RegNames) -> &mut Self::Output {
        return &mut self[index as usize];
    }
}
impl ARMv5RegNames {
    const ALL: [ARMv5RegNames; 16] = [
        Self::R0, Self::R1, Self::R2, Self::R3, Self::R4, Self::R5, Self::R6,
        Self::R7, Self::R8, Self::R9, Self::R10, Self::R11, Self::R12,
        Self::SP, Self::LR, Self::PC
    ];
}
// Allow conversion from u32 to RegNames
impl From<u32> for ARMv5RegNames {
    fn from(value: u32) -> Self {
        variant_from_index(&ARMv5RegNames::ALL, value)
    }
}
impl Display for ARMv5RegNames {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
#[cfg(test)]
mod tests {
    use super::ARMv5RegNames;

    #[test]
    fn reg_names_from_u32() {
        for value in 0..16 {
            assert_eq!(value, ARMv5RegNames::from(value) as u32);
        }
    }

    #[test]
    #[should_panic(expected = "ARMv5RegNames failed! 16 is not in 0..16")]
    fn reg_names_from_u32_out_of_range() {
        let _ = ARMv5RegNames::from(16);
    }
}
