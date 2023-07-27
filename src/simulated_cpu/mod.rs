use std::{io::Write, collections::HashMap};

use names::{RegNames, FlagNames};
use instruction_decoder::{InstructionDecoder, ARMv5Decoder};
use instructions::Instruction;
use crate::utils::{Endian, slice_to_u32, OutputDevice, ExitBehaviour};

pub mod names;

mod instructions;
mod operands;
mod instruction_decoder;

pub trait SimulatedCPU<S> {
    fn step(&mut self) -> Result<(), SimulationException>;
    fn disassemble_memory(
        &self, start: u32, end: u32, labels: Vec<(u32, String)>
    ) -> String;

    fn get_register(&self, register: RegNames) -> S;
    fn set_register(&mut self, register: RegNames, value: S);
    fn get_registers(&self) -> &[S];
    fn get_flag(&self, flag: FlagNames) -> bool;
    fn set_flag(&mut self, flag: FlagNames, value: bool);
    fn get_flags(&self) -> &[bool];

    fn get_memory(
        &self, address: u32, size: u32
    ) -> Result<&[u8], SimulationException>;

    fn set_memory(
        &mut self, address: u32, bytes: &[u8]
    ) -> Result<(), SimulationException>;

    fn mem_size(&self) -> usize;
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
    fn step(&mut self) -> Result<(), SimulationException> {
        let address: u32 = self.registers[RegNames::PC] as u32;
        let mut result: Result<(), SimulationException> = Ok(());

        match self.get_memory(address, 4) {
            Ok(bytes) => {
                let bits: u32 = slice_to_u32(&bytes, &self.encoding);
                let instruction: Box<dyn Instruction<ARMv5CPU, i32>> = 
                    Box::new(ARMv5Decoder::decode(bits));
                if let Err(err) = instruction.execute(self) {
                    result = Err(SimulationException { 
                        kind: err.kind, 
                        msg: format!(
                            "Exception caused by instruction at 0x{address:X}: {}\n", err.msg)
                    });
                }
            },
            Err(err) => {
                result = Err(SimulationException { 
                    kind: err.kind, 
                    msg: format!("Unable to fetch instruction at address 0x{address:X}!\n")
                });
            }
        }

        if let Err(err) = &result {
            self.output_device.output_err(&err.msg);
            self.exit_behaviour.exit(-1);
        }
        
        //only increase programcounter if it was not changed by the instruction
        if address == (self.registers[RegNames::PC] as u32) {
            self.registers[RegNames::PC] = 
                self.registers[RegNames::PC].wrapping_add(4);
        }

        result
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

    fn get_memory(
        &self, address: u32, size: u32
    ) -> Result<&[u8], SimulationException> {
        let (start, end): (usize, usize) = 
            ARMv5CPU::check_memory_access_get_boundaries(address, size)?;

        Ok(&self.memory[start..end])
    }

    fn set_memory(
        &mut self, address: u32, bytes: &[u8]
    ) -> Result<(), SimulationException> {
        let size: u32 = bytes.len() as u32;
        let (start, end): (usize, usize) = 
            ARMv5CPU::check_memory_access_get_boundaries(address, size)?;
        self.memory.splice(start..end, bytes.iter().cloned());
        Ok(())
    }

    fn set_encoding(&mut self, encoding: Endian) {
        self.encoding = encoding;
    }

    fn mem_size(&self) -> usize {
        ARMv5CPU::MEMORY_SIZE
    }
}
impl ARMv5CPU {
    const MEMORY_SIZE: usize = 2usize.pow(26);

    pub fn new<O, E>(output_device: O, exit_behaviour: E) -> Self 
    where O: OutputDevice + 'static, E: ExitBehaviour + 'static {
        Self {
            registers: [0i32; 16],
            flags: [false; 4],
            memory: vec![0u8; ARMv5CPU::MEMORY_SIZE],
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

    fn check_memory_access_get_boundaries(
        address: u32, size: u32
    ) -> Result<(usize, usize), SimulationException> {
        let start: usize = address as usize;
        if let Some(end) = address.checked_add(size) {
            let end: usize = end as usize;
            if end < ARMv5CPU::MEMORY_SIZE {
                return Ok((start, end));
            }
        }
        Err(SimulationException{
            kind: SimulationExceptionKind::DataAbort {
                memory_address: address as usize, size: size as usize
            }, 
            msg: "Memory access out of bounds!".to_string()
        })
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct SimulationException { kind: SimulationExceptionKind, msg: String }

#[derive(Debug)]
pub enum SimulationExceptionKind {
    DataAbort{memory_address: usize, size: usize}, 
    UnsupportedInstruction, UndefinedInstruction
}