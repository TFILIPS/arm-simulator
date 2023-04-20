use std::{ops::{Index, IndexMut}, mem::transmute};

#[derive(Debug, Clone, Copy)]
pub enum FlagNames { 
    N, Z, C, V 
}
// Enable enum FlagNames to index arrays
impl<T> Index<FlagNames> for [T] {
    type Output = T;
    fn index(&self, index: FlagNames) -> &Self::Output {
        return &self[index as usize];
    }
}
impl<T> IndexMut<FlagNames> for [T] {
    fn index_mut(&mut self, index: FlagNames) -> &mut Self::Output {
        return &mut self[index as usize];
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum RegNames { 
    R0, R1, R2, R3, R4, R5, R6, 
    R7, R8, R9, R10, R11, R12,
    SP, LR, PC
}
// Enable enum RegNames to index arrays
impl<T> Index<RegNames> for [T] {
    type Output = T;
    fn index(&self, index: RegNames) -> &Self::Output {
        return &self[index as usize];
    }
}
impl<T> IndexMut<RegNames> for [T] {
    fn index_mut(&mut self, index: RegNames) -> &mut Self::Output {
        return &mut self[index as usize];
    }
}
// Allow convertion from u32 to RegNames
impl From<u32> for RegNames {
    fn from(value: u32) -> Self {
        const NUM_REGISTERS: u32 = 16;
        if value >= NUM_REGISTERS {
            panic!("Convertion to RegNames failed! value >= {NUM_REGISTERS}");
        }
        unsafe { transmute(value) }
    }
}