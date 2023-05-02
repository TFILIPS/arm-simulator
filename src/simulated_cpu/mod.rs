use names::{RegNames, FlagNames};
use instruction_decoder::InstructionDecoder;
use crate::utils::{Endian, slice_to_u32};

use self::{instruction_decoder::ARMv5Decoder, instructions::Instruction};

pub mod names;

mod instructions;
mod operands;
mod instruction_decoder;

pub const MEMORY_SIZE: usize = 2usize.pow(26);

pub trait SimulatedCPU<S> {
    fn step(&mut self);
    fn disassemble_memory(&self, start: u32, length: u32) -> String;

    fn get_register(&self, register: RegNames) -> S;
    fn set_register(&mut self, register: RegNames, value: S);
    fn get_flag(&self, flag: FlagNames) -> bool;
    fn set_flag(&mut self, flag: FlagNames, value: bool);
    fn get_memory(&mut self) -> &mut Vec<u8>;
    fn set_encoding(&mut self, encoding: Endian);
}

// improvement: simulate whole status register including T flag
pub struct ARMv5CPU {
    registers: [i32; 16],
    flags: [bool; 4],
    memory: Vec<u8>,
    encoding: Endian
}
impl SimulatedCPU<i32> for ARMv5CPU {
    fn step(&mut self) {
        let pc_value: i32 = self.registers[RegNames::PC];
        let address: usize = pc_value as u32 as usize;

        let bytes: &[u8] = &self.memory[address..address+4];
        let bits: u32 = slice_to_u32(&bytes, &self.encoding);

        let instruction: Box<dyn Instruction<ARMv5CPU, i32>> = 
            Box::new(ARMv5Decoder::decode(bits));
        instruction.execute(self);

        //only increase programcounter if it was not changed by the instruction
        if pc_value == self.registers[RegNames::PC] {
            self.registers[RegNames::PC] = 
                self.registers[RegNames::PC].wrapping_add(4);
        }
    }

    fn disassemble_memory(&self, _start: u32, _length: u32) -> String {
        todo!()
    }

    fn get_register(&self, register: RegNames) -> i32 {
        self.registers[register]
    }

    fn set_register(&mut self, register: RegNames, value: i32) {
        self.registers[register] = value;
    }

    fn get_flag(&self, flag: FlagNames) -> bool {
        self.flags[flag]
    }

    fn set_flag(&mut self, flag: FlagNames, value: bool) {
        self.flags[flag] = value
    }

    fn get_memory(&mut self) -> &mut Vec<u8> {
        &mut self.memory
    }

    fn set_encoding(&mut self, encoding: Endian) {
        self.encoding = encoding;
    }
}
impl ARMv5CPU {
    pub fn new() -> Self {  
        Self {
            registers: [0i32; 16],
            flags: [false; 4],
            memory: vec![0u8; MEMORY_SIZE],
            encoding: Endian::Little
        }
    }

    fn get_register_intern(&self, register: RegNames) -> i32 {
        if let RegNames::PC = register { 
            self.registers[RegNames::PC].wrapping_add(8) 
        }
        else { 
            self.registers[register] 
        }
    }
}