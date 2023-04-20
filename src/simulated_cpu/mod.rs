use names::{RegNames, FlagNames};
use crate::utils::{Endian, slice_to_u32};

pub mod names;
mod instruction_decoder;
mod instructions;
mod operands;

pub const MEMORY_SIZE: usize = 2usize.pow(26);

// improvement: simulate whole status register including T flag
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
        let address: usize = self.next_instruction_address();
        let bytes: &[u8] = &self.memory[address..address+4];
        let instruction: u32 = slice_to_u32(&bytes, &self.encoding);

        self.execute_instruction(instruction);
        self.increase_pc();
    }

    pub fn get_register(&self, register: RegNames) -> i32 {
        self.registers[register]
    }

    pub fn set_register(&mut self, register: RegNames, value: i32) {
        self.registers[register] = value;
    }

    pub fn _get_flag(&self, flag: FlagNames) -> bool {
        self.flags[flag]
    }

    pub fn _set_flag(&mut self, flag: FlagNames, value: bool) {
        self.flags[flag] = value
    }

    pub fn get_memory(&mut self) -> &mut Vec<u8> {
        &mut self.memory
    }

    pub fn set_encoding(&mut self, encoding: Endian) {
        self.encoding = encoding;
    }
    

    fn next_instruction_address(&self) -> usize {
        ((self.registers[RegNames::PC] as u32 as usize) - 8) & 0xFFFFFFFC
    }

    fn increase_pc(&mut self) {
        self.registers[RegNames::PC] = self.registers[RegNames::PC]
            .wrapping_add(4);
    }
}