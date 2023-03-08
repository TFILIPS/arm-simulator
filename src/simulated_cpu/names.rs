use std::ops::{Index, IndexMut};

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