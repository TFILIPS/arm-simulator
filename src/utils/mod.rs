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

pub trait OutputDevice {
    fn output(&mut self, bytes: &[u8]);
    fn output_err(&mut self, bytes: &[u8]);
    fn flush(&mut self);
}

// ToDo: Add write buffer here like so:
// self.write_buffer.extend(&self.memory[a..a+l]);
// if let Ok(s) = &String::from_utf8(WRITE_BUFFER.clone()) {
//     self.output_device.output(s);
//     WRITE_BUFFER.clear();
// }
// Also implement a flush so everything is printed before exiting
pub struct ConsoleOutput {
    output_buffer: Vec<u8>,
    error_buffer: Vec<u8>
}
impl OutputDevice for ConsoleOutput {
    fn output(&mut self, bytes: &[u8]) {
        self.output_buffer.extend(bytes);
        if let Ok(string) = &String::from_utf8(self.output_buffer.clone()) {
            print!("{string}");
            self.output_buffer.clear();
        }
    }
    fn output_err(&mut self, bytes: &[u8]) {
        self.error_buffer.extend(bytes);
        if let Ok(string) = &String::from_utf8(self.error_buffer.clone()) {
            eprint!("{string}");
            self.error_buffer.clear();
        }
    }
    fn flush(&mut self) {
        print!("{}", String::from_utf8_lossy(&self.output_buffer));
        self.output_buffer.clear();
        eprint!("{}", String::from_utf8_lossy(&self.error_buffer));
        self.error_buffer.clear();
    }
}
impl ConsoleOutput {
    pub fn new() -> ConsoleOutput {
        ConsoleOutput {
            output_buffer: Vec::new(),
            error_buffer: Vec::new()
        }
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

// ToDo: implement Error [https://stackoverflow.com/questions/42584368/how-do-you-define-custom-error-types-in-rust]
#[derive(Debug)]
pub struct MemoryException {
    pub address: usize, pub size: usize, pub msg: String
}

pub trait Memory {
    fn get_memory(
        &self, address: u32, size: u32
    ) -> Result<&[u8], MemoryException>;

    fn set_memory(
        &mut self, address: u32, bytes: &[u8]
    ) -> Result<(), MemoryException>;
}

pub trait ExitOnError<T> {
    fn unwarp_or_exit(self) -> T;
}

impl<T> ExitOnError<T> for Result<T, &'static str> {
    fn unwarp_or_exit(self) -> T {
        self.unwrap_or_else(|msg| {
            eprintln!("{msg}");
            exit(1);
        })
    }
}
