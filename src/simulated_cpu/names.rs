use std::{ops::{Index, IndexMut}, mem::transmute, fmt::Display};
use wasm_bindgen::prelude::wasm_bindgen;

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
impl From<u32> for ARMv5FlagNames{
    fn from(value: u32) -> Self {
        const NUM_FLAGS: u32 = 16;
        if value >= NUM_FLAGS {
            panic!("Convertion to FlagNames failed! value >= {NUM_FLAGS}");
        }
        unsafe { transmute(value) }
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
// Allow convertion from u32 to RegNames
impl From<u32> for ARMv5RegNames {
    fn from(value: u32) -> Self {
        const NUM_REGISTERS: u32 = 16;
        if value >= NUM_REGISTERS {
            panic!("Convertion to RegNames failed! value >= {NUM_REGISTERS}");
        }
        unsafe { transmute(value) }
    }
}
impl Display for ARMv5RegNames {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}