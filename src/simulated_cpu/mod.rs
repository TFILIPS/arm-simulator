use std::{io::Write, collections::HashMap};

use names::{RegNames, FlagNames};
use instruction_decoder::{InstructionDecoder, ARMv5Decoder};
use instructions::Instruction;
use crate::utils::{Endian, slice_to_u32, OutputDevice, ExitBehaviour};

pub mod names;

mod instructions;
mod operands;
mod instruction_decoder;

pub const MEMORY_SIZE: usize = 2usize.pow(26);

pub trait SimulatedCPU<S> {
    fn step(&mut self);
    fn disassemble_memory(
        &self, start: u32, end: u32, 
        labels: Vec<(u32, String)>
    ) -> String;

    fn get_register(&self, register: RegNames) -> S;
    fn set_register(&mut self, register: RegNames, value: S);
    fn get_registers(&self) -> &[S];
    fn get_flag(&self, flag: FlagNames) -> bool;
    fn set_flag(&mut self, flag: FlagNames, value: bool);
    fn get_flags(&self) -> &[bool];
    fn get_memory(&mut self) -> &mut Vec<u8>;
    fn set_encoding(&mut self, encoding: Endian);
}

// improvement: simulate whole status register including T flag
pub struct ARMv5CPU {
    registers: [i32; 16],
    flags: [bool; 4],
    memory: Vec<u8>,
    encoding: Endian,
    output_device: Box<dyn OutputDevice>,
    exit_behaviour: Box<dyn ExitBehaviour>
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

    fn disassemble_memory(
        &self, start: u32, end: u32, labels: Vec<(u32, String)>
    ) -> String {
        let mut buffer: Vec<u8> = Vec::new();
        let label_map: HashMap<u32, String> = labels.into_iter().collect();

        for address in (start..end).step_by(4) {
            if let Some(label) = label_map.get(&address) {
                writeln!(buffer, "-------- | {label}:").unwrap();
            }

            let address: usize = address as usize;
            let bytes: &[u8] = &self.memory[address..address+4];
            let bits: u32 = slice_to_u32(&bytes, &self.encoding);

            let instruction: Box<dyn Instruction<ARMv5CPU, i32>> = 
                Box::new(ARMv5Decoder::decode(bits));

            writeln!(buffer, "{address:08X} |     {instruction}").unwrap();
        }
        String::from_utf8_lossy(&buffer).to_string()
    }

    fn get_register(&self, register: RegNames) -> i32 {
        self.registers[register]
    }

    fn set_register(&mut self, register: RegNames, value: i32) {
        self.registers[register] = value;
    }

    fn get_registers(&self) -> &[i32] {
        &self.registers
    }

    fn get_flag(&self, flag: FlagNames) -> bool {
        self.flags[flag]
    }

    fn set_flag(&mut self, flag: FlagNames, value: bool) {
        self.flags[flag] = value
    }

    fn get_flags(&self) -> &[bool] {
        &self.flags
    }

    fn get_memory(&mut self) -> &mut Vec<u8> {
        &mut self.memory
    }

    fn set_encoding(&mut self, encoding: Endian) {
        self.encoding = encoding;
    }
}
impl ARMv5CPU {
    pub fn new<O, E>(output_device: O, exit_behaviour: E) -> Self 
    where O: OutputDevice + 'static, E: ExitBehaviour + 'static {
        Self {
            registers: [0i32; 16],
            flags: [false; 4],
            memory: vec![0u8; MEMORY_SIZE],
            encoding: Endian::Little,
            output_device: Box::new(output_device),
            exit_behaviour: Box::new(exit_behaviour)
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
