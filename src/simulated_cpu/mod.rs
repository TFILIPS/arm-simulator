use std::{io::Write, collections::HashMap, error::Error, fmt::Display};

use crate::utils::{Endian, slice_to_u32, byte_count, Memory, MemoryError};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimulationExceptionKind {
    PrefetchAbort,
    DataAbort{memory_address: u32, size: u32}, 
    UndefinedInstruction,
    UnsupportedInstruction{reason: &'static str}, 
    UnsupportedSyscall{number: i32}
}
impl From<MemoryError> for SimulationExceptionKind {
    fn from(value: MemoryError) -> Self {
        SimulationExceptionKind::DataAbort { 
            memory_address: value.address, 
            size: value.size 
        }
    }
}

// Exception caused by the instruction at address
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimulationException { 
    pub address: u32,
    pub kind: SimulationExceptionKind
}
impl Error for SimulationException {}
impl Display for SimulationException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let address: u32 = self.address;
        match &self.kind {
            SimulationExceptionKind::PrefetchAbort => write!(f, 
                "Prefetch abort at 0x{address:08X}: no instruction outside of memory"
            ),
            SimulationExceptionKind::DataAbort { memory_address, size } => write!(f,
                "Data abort at 0x{address:08X}: access to 0x{memory_address:08X} ({}) is outside of memory",
                byte_count(*size)
            ),
            SimulationExceptionKind::UndefinedInstruction => write!(f,
                "Undefined instruction at 0x{address:08X}"
            ),
            SimulationExceptionKind::UnsupportedInstruction { reason } => write!(f,
                "Unsupported instruction at 0x{address:08X}: {reason}"
            ),
            SimulationExceptionKind::UnsupportedSyscall { number } => write!(f,
                "Unsupported system call {number} (r7) at 0x{address:08X}: supported are 1 = exit and 4 = write"
            )
        }
    }
}

pub struct ARMv5CPU {
    registers: [i32; 16],
    flags: [bool; 4],
    memory: Vec<u8>,
    encoding: Endian,
    // Address of the next instruction. Writing the PC during execution
    // changes this value, while the PC itself stays at the current instruction.
    next_pc: u32
}
impl SimulatedCPU for ARMv5CPU {
    type RegType = i32;
    type RegNames = ARMv5RegNames;
    type FlagNames = ARMv5FlagNames;

    fn step(&mut self) -> Result<SimulationEvent, SimulationException> {
        let address: u32 = self.registers[ARMv5RegNames::PC] as u32;
        self.next_pc = address.wrapping_add(4);

        // On an exception the PC stays at the faulting instruction
        let result: SimulationEvent = self.load_and_execute_instruction(address)?;

        self.registers[ARMv5RegNames::PC] = self.next_pc as i32;
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
    ) -> Result<&[u8], MemoryError> {
        let (start, end): (usize, usize) = 
            ARMv5CPU::check_memory_access_get_boundaries(address, size)?;

        Ok(&self.memory[start..end])
    }

    fn set_memory(
        &mut self, address: u32, bytes: &[u8]
    ) -> Result<(), MemoryError> {
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
            encoding: Endian::Little,
            next_pc: 0
        }
    }

    //This is not actually dead code, but the compiler thinks so
    #[allow(dead_code)] 
    pub fn reset(&mut self) {
        self.registers = [0i32; 16];
        self.flags = [false; 4];
        self.memory = vec![0u8; ARMv5CPU::MEMORY_SIZE];
        self.encoding = Endian::Little;
        self.next_pc = 0;
    }

    fn get_register_intern(&self, register: ARMv5RegNames) -> i32 {
        if let ARMv5RegNames::PC = register {
            self.registers[ARMv5RegNames::PC].wrapping_add(8)
        }
        else {
            self.registers[register]
        }
    }

    // Writing the PC from an instruction is a branch
    fn set_register_intern(&mut self, register: ARMv5RegNames, value: i32) {
        if let ARMv5RegNames::PC = register {
            self.next_pc = value as u32;
        }
        else {
            self.registers[register] = value;
        }
    }

    fn check_memory_access_get_boundaries(
        address: u32, size: u32
    ) -> Result<(usize, usize), MemoryError> {
        let start: usize = address as usize;
        if let Some(end) = address.checked_add(size) {
            let end: usize = end as usize;
            if end <= ARMv5CPU::MEMORY_SIZE {
                return Ok((start, end));
            }
        }
        Err(MemoryError { address, size })
    }

    fn load_and_execute_instruction(
        &mut self, address: u32
    ) -> Result<SimulationEvent, SimulationException>{
        let bits: u32 = match self.get_memory(address, 4) {
            Ok(bytes) => slice_to_u32(bytes, &self.encoding),
            Err(_) => return Err(SimulationException { 
                address, kind: SimulationExceptionKind::PrefetchAbort 
            })
        };

        let instruction: ARMv5Instruction = ARMv5Decoder::decode(bits);
        instruction.execute(self).map_err(|kind| SimulationException { address, kind })
    }
}

#[cfg(test)]
impl ARMv5CPU {
    // Unit tests call instruction functions directly instead of step(),
    // so a written PC is still pending in next_pc
    fn get_register_after_execute(&self, register: ARMv5RegNames) -> i32 {
        if let ARMv5RegNames::PC = register {
            self.next_pc as i32
        }
        else {
            self.registers[register]
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::{Memory, MemoryError};
    use super::{
        SimulatedCPU, ARMv5CPU, SimulationException, SimulationExceptionKind,
        names::ARMv5RegNames
    };

    macro_rules! step_tests {
        ($($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                #[allow(overflowing_literals)]
                let (instruction, r0, r1, exp_pc, exp_err): (u32, i32, i32, i32, bool) 
                    = $test_values;

                let mut cpu = ARMv5CPU::new();
                cpu.set_memory(0x1000, &instruction.to_le_bytes()).unwrap();
                cpu.set_register(ARMv5RegNames::PC, 0x1000);
                cpu.set_register(ARMv5RegNames::R0, r0);
                cpu.set_register(ARMv5RegNames::R1, r1);

                let result = cpu.step();

                assert_eq!(exp_err, result.is_err());
                assert_eq!(exp_pc, cpu.get_register(ARMv5RegNames::PC));
            }
        )*}
    }

    step_tests! {
        // mov r0, r0
        step_falls_through: (0xE1A0_0000, 0, 0, 0x1004, false),
        // b .
        step_branch_to_self: (0xEAFF_FFFE, 0, 0, 0x1000, false),
        // mov pc, r0
        step_mov_pc_to_self: (0xE1A0_F000, 0x1000, 0, 0x1000, false),
        // ldr r0, [r1] with r1 outside of memory
        step_data_abort_keeps_pc: (0xE591_0000, 0, 0xFFFF_FFF0, 0x1000, true)
    }

    macro_rules! memory_bounds_tests {
        ($($test_name:ident: $test_values:expr),*)
        => {$(
            #[test]
            fn $test_name() {
                let (address, size, exp_ok): (u32, u32, bool) = $test_values;

                let mut cpu = ARMv5CPU::new();
                let bytes: Vec<u8> = vec![0xAB; size as usize];

                assert_eq!(exp_ok, cpu.get_memory(address, size).is_ok());
                assert_eq!(exp_ok, cpu.set_memory(address, &bytes).is_ok());
            }
        )*}
    }

    memory_bounds_tests! {
        memory_last_byte: (0x3FF_FFFF, 1, true),
        memory_last_word: (0x3FF_FFFC, 4, true),
        memory_word_crossing_end: (0x3FF_FFFE, 4, false),
        memory_one_past_end: (0x400_0000, 1, false),
        memory_empty_at_end: (0x400_0000, 0, true)
    }

    #[test]
    fn memory_error_reports_access() {
        let cpu = ARMv5CPU::new();
        assert_eq!(
            Err(MemoryError { address: 0x3FF_FFFE, size: 4 }), 
            cpu.get_memory(0x3FF_FFFE, 4)
        );
    }

    #[test]
    fn fetch_outside_memory_is_prefetch_abort() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_register(ARMv5RegNames::PC, 0x7FFF_0000);

        assert_eq!(
            Some(SimulationException { 
                address: 0x7FFF_0000, kind: SimulationExceptionKind::PrefetchAbort 
            }),
            cpu.step().err()
        );
    }

    #[test]
    fn data_abort_reports_accessed_address() {
        // ldr r0, [r1] with r1 outside of memory
        let mut cpu = ARMv5CPU::new();
        cpu.set_memory(0x1000, &0xE591_0000u32.to_le_bytes()).unwrap();
        cpu.set_register(ARMv5RegNames::PC, 0x1000);
        cpu.set_register(ARMv5RegNames::R1, 0xFFFF_FFF0u32 as i32);

        assert_eq!(
            Some(SimulationException { 
                address: 0x1000, 
                kind: SimulationExceptionKind::DataAbort { memory_address: 0xFFFF_FFF0, size: 4 }
            }),
            cpu.step().err()
        );
    }

    macro_rules! exception_message_tests {
        ($($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (kind, message): (SimulationExceptionKind, &str) = $test_values;
                let exception = SimulationException { address: 0x10084, kind };
                assert_eq!(message, exception.to_string());
            }
        )*}
    }

    exception_message_tests! {
        message_prefetch_abort: (
            SimulationExceptionKind::PrefetchAbort,
            "Prefetch abort at 0x00010084: no instruction outside of memory"
        ),
        message_data_abort: (
            SimulationExceptionKind::DataAbort { memory_address: 0xFFFF_FF00, size: 512 },
            "Data abort at 0x00010084: access to 0xFFFFFF00 (512 bytes) is outside of memory"
        ),
        message_data_abort_single_byte: (
            SimulationExceptionKind::DataAbort { memory_address: 0x400_0000, size: 1 },
            "Data abort at 0x00010084: access to 0x04000000 (1 byte) is outside of memory"
        ),
        message_undefined_instruction: (
            SimulationExceptionKind::UndefinedInstruction,
            "Undefined instruction at 0x00010084"
        ),
        message_unsupported_instruction: (
            SimulationExceptionKind::UnsupportedInstruction { 
                reason: "coprocessor instructions are not supported" 
            },
            "Unsupported instruction at 0x00010084: coprocessor instructions are not supported"
        ),
        message_unsupported_syscall: (
            SimulationExceptionKind::UnsupportedSyscall { number: 3 },
            "Unsupported system call 3 (r7) at 0x00010084: supported are 1 = exit and 4 = write"
        )
    }
}
