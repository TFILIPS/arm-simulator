use std::collections::HashSet;
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
    dynamic_breakpoints: HashSet<u32>,
    // Breakpoint the last run stopped at. Resuming from it must not stop
    // again right away. Executing any instruction clears it.
    stopped_at_breakpoint: Option<u32>
}
impl ARMSimulator {
    pub fn new() -> ARMSimulator {
        ARMSimulator { 
            elf_file: None,
            simulated_cpu: ARMv5CPU::new(), 
            dynamic_breakpoints: HashSet::new(),
            stopped_at_breakpoint: None
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
        self.stopped_at_breakpoint = None;
        Ok(())
    }


    pub fn step(&mut self) -> Result<SimulationEvent, SimulationException> {
        self.stopped_at_breakpoint = None;
        self.simulated_cpu.step()
    }

    pub fn multiple_steps(&mut self, n: usize) -> Result<SimulationEvent, SimulationException> { 
        for _ in 0..n {
            let address: u32 = self.get_program_counter();
            if self.dynamic_breakpoints.contains(&address) 
                && self.stopped_at_breakpoint != Some(address) {
                self.stopped_at_breakpoint = Some(address);
                return Ok(SimulationEvent::Breakpoint);
            }

            match self.step()? {
                SimulationEvent::None => continue,
                event => return Ok(event)
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
        self.dynamic_breakpoints.insert(address);
    }

    pub fn remove_breakpoint(&mut self, address: u32) {
        self.dynamic_breakpoints.remove(&address);
    }

    pub fn reload_simulator(&mut self) -> Result<(), String> {  
        self.simulated_cpu.reset();
        self.stopped_at_breakpoint = None;
        self.simulated_cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
        if let Some(elf) = &self.elf_file {
            self.simulated_cpu.set_encoding(elf.get_encoding());
            self.simulated_cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
            elf.load_into_memory(&mut self.simulated_cpu)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{ARMSimulator, ARMv5RegNames, SimulationEvent};

    // 0x1000: mov r0, r0
    // 0x1004: mov r0, r0   <- breakpoint
    // 0x1008: b 0x1000
    fn loop_with_breakpoint() -> ARMSimulator {
        let program: [u32; 3] = [0xE1A0_0000, 0xE1A0_0000, 0xEAFF_FFFC];
        let bytes: Vec<u8> = program.iter().flat_map(|i| i.to_le_bytes()).collect();

        let mut sim = ARMSimulator::new();
        sim.set_memory(0x1000, &bytes).unwrap();
        sim.set_register(ARMv5RegNames::PC, 0x1000);
        sim.add_breakpoint(0x1004);
        sim
    }

    // Runs in chunks of one instruction, like a UI would with larger chunks
    fn instructions_until_breakpoint(sim: &mut ARMSimulator) -> usize {
        for executed in 0..100 {
            if let SimulationEvent::Breakpoint = sim.multiple_steps(1).unwrap() {
                return executed;
            }
        }
        panic!("Breakpoint was never hit!");
    }

    #[test]
    fn continue_from_breakpoint_stops_on_next_hit() {
        let mut sim = loop_with_breakpoint();
        assert_eq!(1, instructions_until_breakpoint(&mut sim));
        assert_eq!(3, instructions_until_breakpoint(&mut sim));
        assert_eq!(0x1004, sim.get_program_counter());
    }

    #[test]
    fn single_step_from_breakpoint_keeps_it_active() {
        let mut sim = loop_with_breakpoint();
        instructions_until_breakpoint(&mut sim);
        sim.step().unwrap();
        assert_eq!(2, instructions_until_breakpoint(&mut sim));
    }

    #[test]
    fn moving_pc_after_breakpoint_keeps_it_active() {
        let mut sim = loop_with_breakpoint();
        instructions_until_breakpoint(&mut sim);
        sim.set_register(ARMv5RegNames::PC, 0x1000);
        assert_eq!(1, instructions_until_breakpoint(&mut sim));
    }

    #[test]
    fn chunk_ending_on_breakpoint_stops_in_next_chunk() {
        let mut sim = loop_with_breakpoint();
        assert!(matches!(sim.multiple_steps(1).unwrap(), SimulationEvent::None));
        assert!(matches!(sim.multiple_steps(100).unwrap(), SimulationEvent::Breakpoint));
        assert_eq!(0x1004, sim.get_program_counter());
    }

    #[test]
    fn output_landing_on_breakpoint_stops_afterwards() {
        // 0x1000: svc #0 (write 1 byte to stdout)
        // 0x1004: mov r0, r0   <- breakpoint
        let mut sim = loop_with_breakpoint();
        sim.set_memory(0x1000, &0xEF00_0000u32.to_le_bytes()).unwrap();
        sim.set_register(ARMv5RegNames::R7, 4);
        sim.set_register(ARMv5RegNames::R0, 1);
        sim.set_register(ARMv5RegNames::R1, 0x2000);
        sim.set_register(ARMv5RegNames::R2, 1);

        assert!(matches!(sim.multiple_steps(100).unwrap(), SimulationEvent::ConsoleOutput { .. }));
        assert!(matches!(sim.multiple_steps(100).unwrap(), SimulationEvent::Breakpoint));
        assert_eq!(0x1004, sim.get_program_counter());
    }

    #[test]
    fn reload_while_stopped_at_breakpoint_keeps_it_active() {
        let mut sim = ARMSimulator::new();
        sim.load_elf_file("./sample_programs/hallo").unwrap();
        let entry_point: u32 = sim.get_program_counter();
        sim.add_breakpoint(entry_point);

        assert_eq!(0, instructions_until_breakpoint(&mut sim));
        sim.reload_simulator().unwrap();
        assert_eq!(0, instructions_until_breakpoint(&mut sim));
        assert_eq!(entry_point, sim.get_program_counter());
    }
}
