use names::RegNames;
use crate::utils::*;

pub mod names;
mod instruction_decoder;

pub const MEMORY_SIZE: usize = 2usize.pow(26);

pub struct SimulatedCPU {
    registers: [i32; 16],
    flags: [bool; 4],
    memory: Vec<u8>,
    encoding: Endian
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
        let address: usize = self.registers[RegNames::PC] as u32 as usize;
        // error when end of memory
        let instruction: &[u8] = &self.memory[address..address+4];
        let bits = slice_to_u32(&instruction, &self.encoding);

        self.execute_instruction(bits);

        self.registers[RegNames::PC] = 
            self.registers[RegNames::PC].wrapping_add(4);
    }

    pub fn get_memory(&mut self) -> &mut Vec<u8> {
        &mut self.memory
    }

    pub fn set_register(&mut self, register: RegNames, value: i32) {
        self.registers[register] = value;
    }

    pub fn get_register(&self, register: RegNames) -> i32 {
        self.registers[register]
    }

    pub fn set_encoding(&mut self, encoding: Endian) {
        self.encoding = encoding;
    }
}