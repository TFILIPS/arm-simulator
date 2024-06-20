use std::{io::Write, collections::HashMap};

use crate::utils::{Endian, slice_to_u32, Memory, MemoryException};

use instructions::{Instruction, ARMv5Instruction};
use names::{ARMv5RegNames, ARMv5FlagNames};
use instruction_decoder::{InstructionDecoder, ARMv5Decoder};

pub mod names;

mod instructions;
mod operands;
mod instruction_decoder;

pub trait SimulatedCPU {
    type RegType;
    type RegNames;
    type FlagNames;

    fn step(&mut self) -> Result<SimulationEvent, SimulationException>;
    fn disassemble_memory(&self, start: u32, end: u32, labels: Vec<(u32, String)>) -> String;
    fn get_register(&self, register: Self::RegNames) -> Self::RegType;
    fn set_register(&mut self, register: Self::RegNames, value: Self::RegType);
    fn get_registers(&self) -> &[Self::RegType];
    fn get_flag(&self, flag: Self::FlagNames) -> bool;
    fn set_flag(&mut self, flag: Self::FlagNames, value: bool);
    fn get_flags(&self) -> &[bool];
    fn set_encoding(&mut self, encoding: Endian);
}

pub enum SimulationEvent {
    None,
    Breakpoint,
    Exit{exit_code: i32},
    ConsoleOutput{stream: i32, message: Vec<u8>}
}

#[derive(Debug)]
pub enum SimulationExceptionKind {
    DataAbort{memory_address: usize, size: usize}, 
    UnsupportedInstruction, 
    UndefinedInstruction
}
#[derive(Debug)]
pub struct SimulationException { 
    pub kind: SimulationExceptionKind, 
    pub msg: String 
}
impl From<MemoryException> for SimulationException {
    fn from(value: MemoryException) -> Self {
        SimulationException { 
            kind: SimulationExceptionKind::DataAbort { 
                memory_address: value.address, 
                size: value.size 
            },
            msg: value.msg
        }
    }
}

pub struct ARMv5CPU {
    registers: [i32; 16],
    flags: [bool; 4],
    memory: Vec<u8>,
    encoding: Endian
}
impl SimulatedCPU for ARMv5CPU {
    type RegType = i32;
    type RegNames = ARMv5RegNames;
    type FlagNames = ARMv5FlagNames;

    fn step(&mut self) -> Result<SimulationEvent, SimulationException> {
        let address: u32 = self.registers[ARMv5RegNames::PC] as u32;
        let result: SimulationEvent = self.load_and_execute_instruction(address)?;
        
        //only increase program counter if it was not changed by the instruction
        if address == (self.registers[ARMv5RegNames::PC] as u32) {
            self.registers[ARMv5RegNames::PC] = 
                self.registers[ARMv5RegNames::PC].wrapping_add(4);
        }

        Ok(result)
    }

    fn disassemble_memory(
        &self, start: u32, end: u32, labels: Vec<(u32, String)>
    ) -> String {
        let mut buffer: Vec<u8> = Vec::new();
        let label_map: HashMap<u32, String> = labels.into_iter().collect();

        // ToDo: Return a list of tuples instead of a single string
        for address in (start..end).step_by(4) {
            if let Some(label) = label_map.get(&address) {
                writeln!(buffer, "-------- | {label}:").unwrap();
            }

            // ToDo: Better error handling
            let bytes: &[u8] = &self.get_memory(address, 4)
                .expect("Memory access out of bounds!");
            let bits: u32 = slice_to_u32(&bytes, &self.encoding);

            let instruction: ARMv5Instruction = ARMv5Decoder::decode(bits);

            writeln!(buffer, "{address:08X} |     {instruction}").unwrap();
        }
        String::from_utf8_lossy(&buffer).to_string()
    }

    fn get_register(&self, register: ARMv5RegNames) -> i32 {
        self.registers[register]
    }

    fn set_register(&mut self, register: ARMv5RegNames, value: i32) {
        self.registers[register] = value;
    }

    fn get_registers(&self) -> &[i32] {
        &self.registers
    }

    fn get_flag(&self, flag: ARMv5FlagNames) -> bool {
        self.flags[flag]
    }

    fn set_flag(&mut self, flag: ARMv5FlagNames, value: bool) {
        self.flags[flag] = value
    }

    fn get_flags(&self) -> &[bool] {
        &self.flags
    }

    fn set_encoding(&mut self, encoding: Endian) {
        self.encoding = encoding;
    }
}

impl Memory for ARMv5CPU {
    fn get_memory(
        &self, address: u32, size: u32
    ) -> Result<&[u8], MemoryException> {
        let (start, end): (usize, usize) = 
            ARMv5CPU::check_memory_access_get_boundaries(address, size)?;

        Ok(&self.memory[start..end])
    }

    fn set_memory(
        &mut self, address: u32, bytes: &[u8]
    ) -> Result<(), MemoryException> {
        let size: u32 = bytes.len() as u32;
        let (start, end): (usize, usize) = 
            ARMv5CPU::check_memory_access_get_boundaries(address, size)?;
        self.memory.splice(start..end, bytes.iter().cloned());
        Ok(())
    }
}


impl ARMv5CPU {
    const MEMORY_SIZE: usize = 2usize.pow(26);

    pub fn new() -> Self {
        Self {
            registers: [0i32; 16],
            flags: [false; 4],
            memory: vec![0u8; ARMv5CPU::MEMORY_SIZE],
            encoding: Endian::Little
        }
    }

    //This is not actually dead code, but the compiler thinks so
    #[allow(dead_code)] 
    pub fn reset(&mut self) {
        self.registers = [0i32; 16];
        self.flags = [false; 4];
        self.memory = vec![0u8; ARMv5CPU::MEMORY_SIZE];
        self.encoding = Endian::Little;
    }

    fn get_register_intern(&self, register: ARMv5RegNames) -> i32 {
        if let ARMv5RegNames::PC = register { 
            self.registers[ARMv5RegNames::PC].wrapping_add(8) 
        }
        else { 
            self.registers[register] 
        }
    }

    fn check_memory_access_get_boundaries(
        address: u32, size: u32
    ) -> Result<(usize, usize), MemoryException> {
        let start: usize = address as usize;
        if let Some(end) = address.checked_add(size) {
            let end: usize = end as usize;
            if end < ARMv5CPU::MEMORY_SIZE {
                return Ok((start, end));
            }
        }
        Err(MemoryException { 
            address: address as usize, 
            size: size as usize,
            msg: "Memory access out of bounds!".to_string()
        })
    }

    fn load_and_execute_instruction(
        &mut self, address: u32
    ) -> Result<SimulationEvent, SimulationException>{
        match self.get_memory(address, 4) {
            Ok(bytes) => {
                let bits: u32 = slice_to_u32(&bytes, &self.encoding);
                let instruction: ARMv5Instruction = ARMv5Decoder::decode(bits);

                instruction.execute(self).or_else(|err|
                    //Clarify returned error message
                    Err(SimulationException {
                        kind: err.kind,
                        msg: format!(
                            "Exception caused by instruction at 0x{:X}: {}",
                            address, err.msg
                        )
                    })
                )
            }
            Err(mem_err) => {
                //Convert and clarify returned error message
                let sim_err: SimulationException = mem_err.into();
                Err(SimulationException {
                    kind: sim_err.kind,
                    msg: format!(
                        "Unable to fetch instruction at address 0x{:X}!\n",
                        address
                    )
                })
            }
        }
    }
}
