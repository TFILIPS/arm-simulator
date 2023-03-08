use names::RegNames;

use crate::elf_loader::utils::Endian;

use self::names::FlagNames;

pub mod names;

pub const MEMORY_SIZE: usize = 2usize.pow(26);

pub struct SimulatedCPU {
    pub registers: [i32; 16],
    pub flags: [bool; 4],
    pub memory: Vec<u8>,
    pub encoding: Endian
}

impl SimulatedCPU {
    pub fn new() -> Self {  
        Self {
            registers: [0i32; 16],
            flags: [false; 4],
            memory: vec![0u8; MEMORY_SIZE],
            encoding: Endian::Little
        }
    }

    pub fn step(&mut self) {
        self.registers[RegNames::PC] += 4;
    }
}