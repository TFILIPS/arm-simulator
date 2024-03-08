use std::collections::HashMap;
use wasm_bindgen::prelude::*;

use elf_loader::ELFFile;
use simulated_cpu::{names::ARMv5FlagNames, ARMv5CPU, SimulatedCPU};
use utils::Memory;
pub use simulated_cpu::{SimulationEvent, SimulationException, names::ARMv5RegNames};

mod elf_loader;
mod simulated_cpu;
pub mod utils;

const DEFAULT_STACK_POINTER: u32 = 0x40000;

#[wasm_bindgen]
pub struct ARMSimulator {
    elf_file: Option<ELFFile>,
    simulated_cpu: ARMv5CPU,
    dynamic_breakpoints: HashMap<u32, bool>,
}
impl ARMSimulator {
    pub fn new() -> ARMSimulator {
        ARMSimulator { 
            elf_file: None,
            simulated_cpu: ARMv5CPU::new(), 
            dynamic_breakpoints: HashMap::new() 
        }
    }
 
    pub fn load_elf_file(&mut self, path: &str) -> Result<(), &'static str> {
        let elf: elf_loader::ELFFile = elf_loader::ELFFile::load(path)?;
        elf.check_header_values()?;

        self.simulated_cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
        self.simulated_cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
        self.simulated_cpu.set_encoding(elf.get_encoding());
        elf.load_into_memory(&mut self.simulated_cpu)?;

        self.elf_file = Some(elf);
        Ok(())
    }


    pub fn step(&mut self) -> Result<SimulationEvent, SimulationException> {
        self.simulated_cpu.step()
    }

    pub fn multiple_steps(&mut self, n: usize) -> Result<SimulationEvent, SimulationException> { 
        for _ in 0..n {
            if self.hit_active_breakepoint() {
                return Ok(SimulationEvent::Breakpoint);
            }

            let result = self.simulated_cpu.step();
            match result {
                Ok(SimulationEvent::None) => continue,
                _ => return result
            };
        }
        Ok(SimulationEvent::None)
    }

    pub fn get_disassembly(&self) -> Result<String, &'static str> {
        if let Some(elf_file) = &self.elf_file {
            let (text_start, text_end) = elf_file.get_text_section_range()?;
            let labels: Vec<(u32, String)> = elf_file.get_labels()?;
            Ok(self.simulated_cpu.disassemble_memory(text_start, text_end, labels))
        }
        else {
            Err("No ELF file loaded!")
        }
    }

    pub fn get_flags(&self) -> Vec<bool> {
        Vec::from(self.simulated_cpu.get_flags())
    }
}

#[wasm_bindgen]
impl ARMSimulator {
    pub fn get_register(&self, register: ARMv5RegNames) -> i32 {
        self.simulated_cpu.get_register(register)
    }

    pub fn set_register(&mut self, register: ARMv5RegNames, value: i32) {
        self.simulated_cpu.set_register(register, value);
    }

    pub fn get_registers(&self) -> Vec<i32> {
        Vec::from(self.simulated_cpu.get_registers())
    }

    pub fn get_program_counter(&self) -> u32 { 
        self.simulated_cpu.get_register(ARMv5RegNames::PC) as u32
    }

    pub fn get_flag(&self, flag: ARMv5FlagNames) -> bool {
        self.simulated_cpu.get_flag(flag.into())
    }

    pub fn set_flag(&mut self, flag: ARMv5FlagNames, value: bool) {
        self.simulated_cpu.set_flag(flag.into(), value);
    }

    pub fn get_memory(&self, address: u32, size: u32) -> Result<Vec<u8>, String> {
        match self.simulated_cpu.get_memory(address, size) {
            Ok(bytes) => Ok(Vec::from(bytes)),
            Err(err) => Err(err.msg)
        }
    }

    pub fn set_memory(&mut self, address: u32, bytes: &[u8]) -> Result<(), String> {
        match self.simulated_cpu.set_memory(address, bytes) {
            Ok(()) => Ok(()),
            Err(err) => Err(err.msg)
        }
    }

    pub fn add_breakpoint(&mut self, address: u32) {
        if !self.dynamic_breakpoints.contains_key(&address) {
            self.dynamic_breakpoints.insert(address, true);
        }  
    }

    pub fn remove_breakpoint(&mut self, address: u32) {
        if self.dynamic_breakpoints.contains_key(&address) {
            self.dynamic_breakpoints.remove(&address);
        }
    }

    pub fn reload_simulator(&mut self) -> Result<(), String> {  
        self.simulated_cpu.reset();
        self.simulated_cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
        if let Some(elf) = &self.elf_file {
            self.simulated_cpu.set_encoding(elf.get_encoding());
            self.simulated_cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
            elf.load_into_memory(&mut self.simulated_cpu)?;
        }
        Ok(())
    }
}

impl ARMSimulator {
    fn hit_active_breakepoint(&mut self) -> bool {
        let address: u32 = self.simulated_cpu.get_register(ARMv5RegNames::PC) as u32;
        if let Some(active) = self.dynamic_breakpoints.get(&address) {
            if *active {
                self.dynamic_breakpoints.insert(address, false);
                return true;
            }
            else {
                self.dynamic_breakpoints.insert(address, true);
            }
        }
        false
    }
}
