use std::{error::Error, ops::{Bound, RangeBounds}};

pub const F: bool = false;
pub const T: bool = true;

#[derive(Debug, Clone, Copy)]
pub enum Endian { Little, Big }

pub fn slice_to_u16(slice: &[u8], encoding: &Endian) -> u16 {
    let buffer: [u8; 2] = slice
        .try_into()
        .expect("No conversion to u16 possible! Given slice has length != 2.");

    match encoding {
        Endian::Little => u16::from_le_bytes(buffer),
        Endian::Big => u16::from_be_bytes(buffer),
    }
}

pub fn slice_to_u32(slice: &[u8], encoding: &Endian) -> u32 {
    let buffer: [u8; 4] = slice
        .try_into()
        .expect("No conversion to u32 possible! Given slice has length != 4.");

    match encoding {
        Endian::Little => u32::from_le_bytes(buffer),
        Endian::Big => u32::from_be_bytes(buffer),
    }
}

pub fn u16_to_array(value: u16, encoding: &Endian) -> [u8; 2] {
    match encoding {
        crate::utils::Endian::Little => value.to_le_bytes(),
        crate::utils::Endian::Big => value.to_be_bytes(),
    }
}

pub fn u32_to_array(value: u32, encoding: &Endian) -> [u8; 4] {
    match encoding {
        crate::utils::Endian::Little => value.to_le_bytes(),
        crate::utils::Endian::Big => value.to_be_bytes(),
    }
}

// Returns the enum variant at position index, e.g. for decoding bit fields
pub(crate) fn variant_from_index<T: Copy>(variants: &[T], index: u32) -> T {
    match variants.get(index as usize) {
        Some(variant) => *variant,
        None => panic!(
            "Conversion to {} failed! {index} is not in 0..{}",
            std::any::type_name::<T>(), variants.len()
        )
    }
}


pub trait BitAccess {
   fn get_bit(&self, index: usize) -> bool;
   fn cut_bits<T: RangeBounds<usize>>(&self, range: T) -> Self;
}

impl BitAccess for u32 {
    fn get_bit(&self, index: usize) -> bool {
        if index >= 32 {
            panic!("Provided index is to large! index >= 32");
        }
        *self >> index & 1 == 1
    }

    fn cut_bits<T: RangeBounds<usize>>(&self, range: T) -> u32 {
        let (start, end): (usize, usize) = get_bounds(range, 31);
        let lshift: usize = 31 - end;
        (*self << lshift) >> (start + lshift)
    }
}

impl BitAccess for u16 {
    fn get_bit(&self, index: usize) -> bool {
        if index >= 16 {
            panic!("Provided index is to large! index >= 16");
        }
        *self >> index & 1 == 1
    }

    fn cut_bits<T: RangeBounds<usize>>(&self, range: T) -> u16 {
        let (start, end): (usize, usize) = get_bounds(range, 15);
        let lshift: usize = 15 - end;
        (*self << lshift) >> (start + lshift)
    }
}

fn get_bounds<T: RangeBounds<usize>>(range: T, max: usize) -> (usize, usize) {
    let start: usize = match range.start_bound() {
        Bound::Included(&x) => x,
        Bound::Excluded(&x) => x + 1,
        Bound::Unbounded => 0
    };
    if start > max {
        panic!("Provided start index is to large! index > {max}");
    }

    let end: usize = match range.end_bound() {
        Bound::Included(&x) => x,
        Bound::Excluded(&x) if x > 0 => x - 1,
        Bound::Excluded(_) => panic!("Provided end index is negative!"),
        Bound::Unbounded => max
    };
    if end > max {
        panic!("Provided end index is to large! index > {max}");
    }
    
    if start > end {
        panic!("The start index is larger than the end index! {start} > {end}");
    }
    (start, end)
}

// Access to memory outside of the simulated memory
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemoryError {
    pub address: u32, pub size: u32
}

impl Error for MemoryError {}

impl std::fmt::Display for MemoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Memory access to 0x{:08X} ({}) is outside of memory",
            self.address, byte_count(self.size))
    }
}

pub fn byte_count(size: u32) -> String {
    if size == 1 { String::from("1 byte") } else { format!("{size} bytes") }
}

pub trait Memory {
    fn get_memory(
        &self, address: u32, size: u32
    ) -> Result<&[u8], MemoryError>;

    fn set_memory(
        &mut self, address: u32, bytes: &[u8]
    ) -> Result<(), MemoryError>;
}
