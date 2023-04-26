use names::{RegNames, FlagNames};
use new_decoder::InstructionDecoder;
use crate::utils::{Endian, slice_to_u32};

pub mod names;
//mod instruction_decoder;
mod instructions;
mod operands;
mod new_decoder;

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
        let pc_value: i32 = self.registers[RegNames::PC];
        let address: usize = pc_value as u32 as usize;

        let bytes: &[u8] = &self.memory[address..address+4];
        let instruction: u32 = slice_to_u32(&bytes, &self.encoding);

        //self.execute_instruction(instruction);
        if let Some(action) = InstructionDecoder::Action.decode(instruction) {
            action(self);
        }
        else {
            panic!("Unsupported Instruction!");
        }

        if pc_value != self.registers[RegNames::PC] {
            self.registers[RegNames::PC] &= 0xFFFFFFFC_u32 as i32;
        }
        else {
            self.registers[RegNames::PC] = 
                self.registers[RegNames::PC].wrapping_add(4);
        }
    }

    pub fn disassemble_memory(&self, start: u32, length: u32) -> String {
        let mut decoder: InstructionDecoder = InstructionDecoder::Disassembly { 
            buffer: Vec::new()
        };

        for i in (start..start+length).step_by(4) {
            let address: usize = i as usize;
            let bytes: &[u8] = &self.memory[address..address+4];
            let instruction: u32 = slice_to_u32(&bytes, &self.encoding);
            decoder.decode(instruction);
        }

        decoder.get_string().unwrap()
    }

    pub fn _get_register(&self, register: RegNames) -> i32 {
        self.registers[register]
    }

    fn get_register_intern(&self, register: RegNames) -> i32 {
        if let RegNames::PC = register { 
            self.registers[RegNames::PC].wrapping_add(8) 
        }
        else { 
            self.registers[register] 
        }
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
}