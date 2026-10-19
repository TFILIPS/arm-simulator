#![deny(unsafe_code)]
// ARM and ELF are written in capitals like in the specifications
#![allow(clippy::upper_case_acronyms)]

use std::collections::HashSet;

use elf_loader::ELFFile;
use simulated_cpu::{ARMv5CPU, CpuEvent, SimulatedCPU};
use utils::Memory;
pub use elf_loader::ElfError;
pub use simulated_cpu::{
    SimulationException, SimulationExceptionKind, UnsupportedReason, 
    names::{ARMv5RegNames, ARMv5FlagNames}
};
pub use utils::MemoryError;

mod elf_loader;
mod os;
mod simulated_cpu;
mod utils;
mod wasm;

const DEFAULT_STACK_POINTER: u32 = 0x40000;

pub enum SimulationEvent {
    None,
    Breakpoint,
    Exit{exit_code: i32},
    ConsoleOutput{stream: i32, message: Vec<u8>},
    // A read found no input. The PC stays at the SWI, so the system call
    // runs again when the program continues after push_input or close_input.
    WaitingForInput
}

pub struct ARMSimulator {
    elf_file: Option<ELFFile>,
    simulated_cpu: ARMv5CPU,
    dynamic_breakpoints: HashSet<u32>,
    // Breakpoint the last run stopped at. Resuming from it must not stop
    // again right away. Executing any instruction clears it.
    stopped_at_breakpoint: Option<u32>,
    input: os::Input
}
impl ARMSimulator {
    pub fn new() -> ARMSimulator {
        ARMSimulator { 
            elf_file: None,
            simulated_cpu: ARMv5CPU::new(), 
            dynamic_breakpoints: HashSet::new(),
            stopped_at_breakpoint: None,
            input: os::Input::default()
        }
    }
 
    pub fn load_elf_file(&mut self, path: &str) -> Result<(), ElfError> {
        let elf: ELFFile = ELFFile::load(path)?;
        self.load_elf(elf)
    }

    pub fn load_elf_bytes(&mut self, bytes: Vec<u8>) -> Result<(), ElfError> {
        let elf: ELFFile = ELFFile::load_raw(bytes)?;
        self.load_elf(elf)
    }

    fn load_elf(&mut self, elf: ELFFile) -> Result<(), ElfError> {
        elf.check_header_values()?;
        self.elf_file = Some(elf);
        self.reload_simulator()
    }


    pub fn step(&mut self) -> Result<SimulationEvent, SimulationException> {
        self.stopped_at_breakpoint = None;
        let address: u32 = self.get_program_counter();

        match self.simulated_cpu.step()? {
            CpuEvent::None => Ok(SimulationEvent::None),
            CpuEvent::Breakpoint => Ok(SimulationEvent::Breakpoint),
            CpuEvent::SoftwareInterrupt { .. } => {
                match os::handle_syscall(&mut self.simulated_cpu, &mut self.input) {
                    Ok(SimulationEvent::WaitingForInput) => {
                        // Like a blocked process: the SWI runs again later.
                        // A breakpoint on it was already passed.
                        self.simulated_cpu.set_register(ARMv5RegNames::PC, address as i32);
                        self.stopped_at_breakpoint = Some(address);
                        Ok(SimulationEvent::WaitingForInput)
                    },
                    Ok(event) => Ok(event),
                    Err(kind) => {
                        // Like for other exceptions the PC stays at the instruction
                        self.simulated_cpu.set_register(ARMv5RegNames::PC, address as i32);
                        Err(SimulationException { address, kind })
                    }
                }
            }
        }
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

    pub fn get_disassembly(&self) -> Result<String, ElfError> {
        if let Some(elf_file) = &self.elf_file {
            let (text_start, text_end) = elf_file.get_text_section_range()?;

            // Disassembly reads whole words, which all have to be in memory
            let size: u32 = (text_end - text_start).checked_next_multiple_of(4)
                .ok_or("The .text section is too large! Check ELF file.")?;
            if self.simulated_cpu.get_memory(text_start, size).is_err() {
                return Err("The .text section is outside of memory! Check ELF file.".into());
            }

            let labels: Vec<(u32, String)> = elf_file.get_labels()?;
            Ok(self.simulated_cpu.disassemble_memory(text_start, text_end, labels))
        }
        else {
            Err("No ELF file loaded!".into())
        }
    }

    pub fn get_flags(&self) -> Vec<bool> {
        Vec::from(self.simulated_cpu.get_flags())
    }

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
        self.simulated_cpu.get_flag(flag)
    }

    pub fn set_flag(&mut self, flag: ARMv5FlagNames, value: bool) {
        self.simulated_cpu.set_flag(flag, value);
    }

    pub fn get_memory(&self, address: u32, size: u32) -> Result<Vec<u8>, MemoryError> {
        self.simulated_cpu.get_memory(address, size).map(Vec::from)
    }

    pub fn set_memory(&mut self, address: u32, bytes: &[u8]) -> Result<(), MemoryError> {
        self.simulated_cpu.set_memory(address, bytes)
    }

    pub fn add_breakpoint(&mut self, address: u32) {
        self.dynamic_breakpoints.insert(address);
    }

    pub fn remove_breakpoint(&mut self, address: u32) {
        self.dynamic_breakpoints.remove(&address);
    }

    // Input for the program (stdin), read by the read system call
    pub fn push_input(&mut self, bytes: &[u8]) {
        self.input.push(bytes);
    }

    // End of input (EOF): reads return 0 once the remaining input is read
    pub fn close_input(&mut self) {
        self.input.close();
    }

    pub fn is_input_closed(&self) -> bool {
        self.input.is_closed()
    }

    pub fn reload_simulator(&mut self) -> Result<(), ElfError> {  
        self.simulated_cpu.reset();
        self.stopped_at_breakpoint = None;
        self.input.clear();
        self.simulated_cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
        if let Some(elf) = &self.elf_file {
            self.simulated_cpu.set_encoding(elf.get_encoding());
            self.simulated_cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
            elf.load_into_memory(&mut self.simulated_cpu)?;
        }
        Ok(())
    }
}

impl Default for ARMSimulator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ARMSimulator, ARMv5RegNames, ARMv5FlagNames, ElfError, SimulationEvent, 
        SimulationException, SimulationExceptionKind, DEFAULT_STACK_POINTER
    };

    // 32-bit little endian ARM executable
    const HALLO: &[u8] = include_bytes!("../sample_programs/hallo");
    // Echoes its input: read(0, 0x2000, 64) and write the bytes back to
    // stdout, until read returns 0 at the end of input, then exit(0)
    const ECHO: [u32; 15] = [
        0xE3A0_0000, // 0x1000: mov r0, #0
        0xE3A0_1A02, // 0x1004: mov r1, #0x2000
        0xE3A0_2040, // 0x1008: mov r2, #64
        0xE3A0_7003, // 0x100C: mov r7, #3
        0xEF00_0000, // 0x1010: swi #0           read
        0xE350_0000, // 0x1014: cmp r0, #0
        0xDA00_0004, // 0x1018: ble 0x1030
        0xE1A0_2000, // 0x101C: mov r2, r0
        0xE3A0_0001, // 0x1020: mov r0, #1
        0xE3A0_7004, // 0x1024: mov r7, #4
        0xEF00_0000, // 0x1028: swi #0           write
        0xEAFF_FFF3, // 0x102C: b 0x1000
        0xE3A0_0000, // 0x1030: mov r0, #0
        0xE3A0_7001, // 0x1034: mov r7, #1
        0xEF00_0000, // 0x1038: swi #0           exit
    ];
    // Address of its read system call
    const READ_SWI: u32 = 0x1010;

    fn load_echo(sim: &mut ARMSimulator) {
        let bytes: Vec<u8> = ECHO.iter().flat_map(|i| i.to_le_bytes()).collect();
        sim.set_memory(0x1000, &bytes).unwrap();
        sim.set_register(ARMv5RegNames::PC, 0x1000);
    }

    pub(crate) fn echo_simulator() -> ARMSimulator {
        let mut sim = ARMSimulator::new();
        load_echo(&mut sim);
        sim
    }

    // Runs until something else than output happens, collects the output
    fn run_collecting(sim: &mut ARMSimulator, output: &mut Vec<u8>) -> SimulationEvent {
        loop {
            match sim.multiple_steps(10_000).unwrap() {
                SimulationEvent::ConsoleOutput { message, .. } => output.extend(message),
                event => return event
            }
        }
    }

    #[test]
    fn read_waits_at_swi_until_input() {
        let mut sim = echo_simulator();
        let mut output: Vec<u8> = Vec::new();

        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::WaitingForInput));
        assert_eq!(READ_SWI, sim.get_program_counter());
        // Running again without input keeps waiting at the same instruction
        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::WaitingForInput));
        assert_eq!(READ_SWI, sim.get_program_counter());

        sim.push_input(b"ab1c\nxyz\n");
        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::WaitingForInput));
        assert_eq!(b"ab1c\nxyz\n".to_vec(), output);

        sim.close_input();
        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::Exit { exit_code: 0 }));
    }

    #[test]
    fn breakpoint_on_waiting_read_does_not_stop_again() {
        let mut sim = echo_simulator();
        let mut output: Vec<u8> = Vec::new();
        sim.add_breakpoint(READ_SWI);

        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::Breakpoint));
        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::WaitingForInput));
        sim.push_input(b"a\n");
        // The read completes; the next loop iteration reaches the breakpoint again
        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::Breakpoint));
        assert_eq!(b"a\n".to_vec(), output);
    }

    #[test]
    fn reload_clears_input() {
        let mut sim = echo_simulator();
        sim.push_input(b"old\n");
        sim.close_input();
        sim.reload_simulator().unwrap();
        assert!(!sim.is_input_closed());

        // Without an ELF file the reload also cleared the program
        load_echo(&mut sim);
        let mut output: Vec<u8> = Vec::new();
        assert!(matches!(run_collecting(&mut sim, &mut output), SimulationEvent::WaitingForInput));
        assert!(output.is_empty());
    }

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
    fn text_section_outside_memory_is_error() {
        // Section headers start at 640 in hallo, .text is section 1 and its
        // address field is at +12. The loaded segment stays unchanged.
        let mut bytes: Vec<u8> = include_bytes!("../sample_programs/hallo").to_vec();
        let text_address: usize = 640 + 40 + 12;
        bytes[text_address..text_address + 4].copy_from_slice(&0x7FFF_0000u32.to_le_bytes());

        let mut sim = ARMSimulator::new();
        sim.load_elf_bytes(bytes).unwrap();
        assert!(sim.get_disassembly().is_err());
    }

    #[test]
    fn loading_starts_with_clean_cpu() {
        let mut sim = ARMSimulator::new();
        sim.set_register(ARMv5RegNames::R5, 42);
        sim.set_flag(ARMv5FlagNames::Z, true);
        sim.set_memory(0x1000, &[1, 2, 3, 4]).unwrap();

        sim.load_elf_bytes(HALLO.to_vec()).unwrap();

        assert_eq!(0, sim.get_register(ARMv5RegNames::R5));
        assert!(!sim.get_flag(ARMv5FlagNames::Z));
        assert_eq!(vec![0; 4], sim.get_memory(0x1000, 4).unwrap());
    }

    #[test]
    fn reload_restores_start_state() {
        let mut sim = ARMSimulator::new();
        sim.load_elf_bytes(HALLO.to_vec()).unwrap();
        let entry_point: u32 = sim.get_program_counter();
        let program: Vec<u8> = sim.get_memory(entry_point, 36).unwrap();

        sim.multiple_steps(3).unwrap();
        sim.set_register(ARMv5RegNames::R5, 42);
        sim.set_memory(entry_point, &[0; 4]).unwrap();

        sim.reload_simulator().unwrap();

        let mut expected_registers: Vec<i32> = vec![0; 16];
        expected_registers[13] = DEFAULT_STACK_POINTER as i32;
        expected_registers[15] = entry_point as i32;
        assert_eq!(expected_registers, sim.get_registers());
        assert_eq!(program, sim.get_memory(entry_point, 36).unwrap());
    }

    #[test]
    fn failing_system_call_keeps_pc_at_swi() {
        // swi #0 with write(1, 0xFFFF_FF00, 0x200)
        let mut sim = ARMSimulator::new();
        sim.set_memory(0x1000, &0xEF00_0000u32.to_le_bytes()).unwrap();
        sim.set_register(ARMv5RegNames::PC, 0x1000);
        sim.set_register(ARMv5RegNames::R7, 4);
        sim.set_register(ARMv5RegNames::R0, 1);
        sim.set_register(ARMv5RegNames::R1, 0xFFFF_FF00u32 as i32);
        sim.set_register(ARMv5RegNames::R2, 0x200);

        assert_eq!(
            Some(SimulationException { 
                address: 0x1000, 
                kind: SimulationExceptionKind::DataAbort { memory_address: 0xFFFF_FF00, size: 0x200 } 
            }),
            sim.step().err()
        );
        assert_eq!(0x1000, sim.get_program_counter());
    }

    #[test]
    fn invalid_elf_file_is_elf_error() {
        let mut sim = ARMSimulator::new();
        let error: ElfError = sim.load_elf_bytes(vec![0; 10]).unwrap_err();
        assert_eq!("ELF file is too short to be valid!", error.to_string());
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
