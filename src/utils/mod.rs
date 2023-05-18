use std::{ops::{Bound, RangeBounds}, process::exit};

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
        let (start, end) = get_bounds(range, 31);
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
        let (start, end) = get_bounds(range, 15);
        let lshift: usize = 15 - end;
        (*self << lshift) >> (start + lshift)
    }
}

fn get_bounds<T: RangeBounds<usize>>(range: T, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        Bound::Included(&x) => x,
        Bound::Excluded(&x) => x + 1,
        Bound::Unbounded => 0
    };
    if start > max {
        panic!("Provided start index is to large! index > {max}");
    }

    let end = match range.end_bound() {
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

pub trait OutputDevice {
    fn output(&self, msg: &str);
    fn output_err(&self, err: &str);
}

pub struct ConsoleOutput;
impl OutputDevice for ConsoleOutput {
    fn output(&self, msg: &str) {
        print!("{msg}");
    }
    fn output_err(&self, err: &str) {
        println!("{err}");
    }
}

pub trait ExitBehaviour {
    fn exit(&self, code: i32);
}

pub struct ConsoleExit;
impl ExitBehaviour for ConsoleExit {
    fn exit(&self, code: i32) {
        exit(code);
    }
}
