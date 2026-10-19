// JavaScript interface of the simulator for the web app. The simulator itself
// stays plain Rust, everything specific to wasm-bindgen is in this module.

use std::collections::HashMap;
use wasm_bindgen::prelude::*;

use crate::{
    ARMSimulator, ARMv5FlagNames, ARMv5RegNames, SimulationEvent,
    SimulationException, SimulationExceptionKind, UnsupportedReason
};

// Why step or run returned
#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StopReason {
    // Nothing happened that stops the program, it can continue
    None,
    Breakpoint,
    Exit,
    Error
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExceptionKind {
    PrefetchAbort,
    DataAbort,
    UndefinedInstruction,
    UnsupportedStatusRegister,
    UnsupportedCoprocessor,
    UnsupportedSyscall
}

// Output of the program to one stream (1 = stdout, 2 = stderr)
#[wasm_bindgen]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutputChunk {
    stream: i32,
    text: String
}
#[wasm_bindgen]
impl OutputChunk {
    #[wasm_bindgen(getter)]
    pub fn stream(&self) -> i32 {
        self.stream
    }

    #[wasm_bindgen(getter)]
    pub fn text(&self) -> String {
        self.text.clone()
    }
}

// The exception as data, so the web app can show it in its own language
#[wasm_bindgen(js_name = SimulationException)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmException {
    exception: SimulationException
}
#[wasm_bindgen(js_class = SimulationException)]
impl WasmException {
    #[wasm_bindgen(getter)]
    pub fn kind(&self) -> ExceptionKind {
        match self.exception.kind {
            SimulationExceptionKind::PrefetchAbort => ExceptionKind::PrefetchAbort,
            SimulationExceptionKind::DataAbort { .. } => ExceptionKind::DataAbort,
            SimulationExceptionKind::UndefinedInstruction => ExceptionKind::UndefinedInstruction,
            SimulationExceptionKind::UnsupportedInstruction {
                reason: UnsupportedReason::StatusRegister
            } => ExceptionKind::UnsupportedStatusRegister,
            SimulationExceptionKind::UnsupportedInstruction {
                reason: UnsupportedReason::Coprocessor
            } => ExceptionKind::UnsupportedCoprocessor,
            SimulationExceptionKind::UnsupportedSyscall { .. } => ExceptionKind::UnsupportedSyscall
        }
    }

    // Address of the instruction that caused the exception
    #[wasm_bindgen(getter)]
    pub fn address(&self) -> u32 {
        self.exception.address
    }

    #[wasm_bindgen(getter)]
    pub fn memory_address(&self) -> Option<u32> {
        match self.exception.kind {
            SimulationExceptionKind::DataAbort { memory_address, .. } => Some(memory_address),
            _ => None
        }
    }

    #[wasm_bindgen(getter)]
    pub fn size(&self) -> Option<u32> {
        match self.exception.kind {
            SimulationExceptionKind::DataAbort { size, .. } => Some(size),
            _ => None
        }
    }

    #[wasm_bindgen(getter)]
    pub fn syscall_number(&self) -> Option<i32> {
        match self.exception.kind {
            SimulationExceptionKind::UnsupportedSyscall { number } => Some(number),
            _ => None
        }
    }

    // English message as a fallback
    #[wasm_bindgen(getter)]
    pub fn message(&self) -> String {
        self.exception.to_string()
    }
}

#[wasm_bindgen(js_name = ARMSimulator)]
pub struct WasmSimulator {
    simulator: ARMSimulator,
    output: OutputBuffer,
    exit_code: Option<i32>,
    exception: Option<SimulationException>
}

#[wasm_bindgen(js_class = ARMSimulator)]
impl WasmSimulator {
    #[wasm_bindgen(constructor)]
    pub fn new(elf_file: Vec<u8>) -> Result<WasmSimulator, JsError> {
        #[cfg(target_family = "wasm")]
        console_error_panic_hook::set_once();

        let mut simulator: ARMSimulator = ARMSimulator::new();
        simulator.load_elf_bytes(elf_file)?;
        Ok(WasmSimulator::from_simulator(simulator))
    }

    // Executes one instruction, breakpoints are ignored
    pub fn step(&mut self) -> StopReason {
        if self.exit_code.is_some() {
            return StopReason::Exit;
        }
        self.exception = None;

        let result = self.simulator.step();
        self.handle(result)
    }

    // Executes up to max_steps instructions. Stops early at breakpoints, at
    // the end of the program and at exceptions.
    pub fn run(&mut self, max_steps: u32) -> StopReason {
        if self.exit_code.is_some() {
            return StopReason::Exit;
        }
        self.exception = None;

        for _ in 0..max_steps {
            let result = self.simulator.multiple_steps(1);
            match self.handle(result) {
                StopReason::None => continue,
                reason => return reason
            }
        }
        StopReason::None
    }

    // Output of the program since the last call, in the order it was written
    pub fn take_output(&mut self) -> Vec<OutputChunk> {
        self.output.take()
    }

    pub fn exit_code(&self) -> Option<i32> {
        self.exit_code
    }

    pub fn exception(&self) -> Option<WasmException> {
        self.exception.clone().map(|exception| WasmException { exception })
    }

    pub fn reload_simulator(&mut self) -> Result<(), JsError> {
        self.output.clear();
        self.exit_code = None;
        self.exception = None;
        self.simulator.reload_simulator()?;
        Ok(())
    }

    pub fn get_disassembly(&self) -> Result<String, JsError> {
        Ok(self.simulator.get_disassembly()?)
    }

    pub fn get_registers(&self) -> Vec<i32> {
        self.simulator.get_registers()
    }

    pub fn get_register(&self, register: ARMv5RegNames) -> i32 {
        self.simulator.get_register(register)
    }

    // Moving the PC lets an ended program continue from there
    pub fn set_register(&mut self, register: ARMv5RegNames, value: i32) {
        if register == ARMv5RegNames::PC {
            self.exit_code = None;
            self.exception = None;
        }
        self.simulator.set_register(register, value);
    }

    pub fn get_program_counter(&self) -> u32 {
        self.simulator.get_program_counter()
    }

    pub fn get_flag(&self, flag: ARMv5FlagNames) -> bool {
        self.simulator.get_flag(flag)
    }

    pub fn set_flag(&mut self, flag: ARMv5FlagNames, value: bool) {
        self.simulator.set_flag(flag, value);
    }

    pub fn get_memory(&self, address: u32, size: u32) -> Result<Vec<u8>, JsError> {
        Ok(self.simulator.get_memory(address, size)?)
    }

    pub fn set_memory(&mut self, address: u32, bytes: &[u8]) -> Result<(), JsError> {
        Ok(self.simulator.set_memory(address, bytes)?)
    }

    pub fn add_breakpoint(&mut self, address: u32) {
        self.simulator.add_breakpoint(address);
    }

    pub fn remove_breakpoint(&mut self, address: u32) {
        self.simulator.remove_breakpoint(address);
    }
}

impl WasmSimulator {
    fn from_simulator(simulator: ARMSimulator) -> WasmSimulator {
        WasmSimulator {
            simulator,
            output: OutputBuffer::default(),
            exit_code: None,
            exception: None
        }
    }

    fn handle(&mut self, result: Result<SimulationEvent, SimulationException>) -> StopReason {
        match result {
            Ok(SimulationEvent::None) => StopReason::None,
            Ok(SimulationEvent::ConsoleOutput { stream, message }) => {
                self.output.push(stream, &message);
                StopReason::None
            },
            Ok(SimulationEvent::Breakpoint) => StopReason::Breakpoint,
            Ok(SimulationEvent::Exit { exit_code }) => {
                self.output.flush();
                self.exit_code = Some(exit_code);
                StopReason::Exit
            },
            Err(exception) => {
                self.exception = Some(exception);
                StopReason::Error
            }
        }
    }
}

// Collects the program output in order. The bytes of a character that is
// split over several writes wait until the character is complete.
#[derive(Default)]
struct OutputBuffer {
    chunks: Vec<OutputChunk>,
    incomplete: HashMap<i32, Vec<u8>>
}

impl OutputBuffer {
    fn push(&mut self, stream: i32, bytes: &[u8]) {
        let incomplete: &mut Vec<u8> = self.incomplete.entry(stream).or_default();
        incomplete.extend_from_slice(bytes);
        let text: String = take_complete_text(incomplete);
        self.append(stream, &text);
    }

    // At the end of the program incomplete characters are replaced
    fn flush(&mut self) {
        let mut incomplete: Vec<(i32, Vec<u8>)> = self.incomplete.drain().collect();
        incomplete.sort();
        for (stream, bytes) in incomplete {
            self.append(stream, &String::from_utf8_lossy(&bytes));
        }
    }

    fn take(&mut self) -> Vec<OutputChunk> {
        std::mem::take(&mut self.chunks)
    }

    fn clear(&mut self) {
        self.chunks.clear();
        self.incomplete.clear();
    }

    fn append(&mut self, stream: i32, text: &str) {
        if text.is_empty() {
            return;
        }
        match self.chunks.last_mut() {
            Some(chunk) if chunk.stream == stream => chunk.text.push_str(text),
            _ => self.chunks.push(OutputChunk { stream, text: text.to_string() })
        }
    }
}

// Removes and returns the complete characters. Invalid bytes become U+FFFD,
// an incomplete character at the end stays in bytes.
fn take_complete_text(bytes: &mut Vec<u8>) -> String {
    let mut text: String = String::new();
    let mut rest: &[u8] = bytes;
    loop {
        match std::str::from_utf8(rest) {
            Ok(valid) => {
                text.push_str(valid);
                rest = &[];
                break;
            },
            Err(error) => {
                let (valid, after) = rest.split_at(error.valid_up_to());
                text.push_str(&String::from_utf8_lossy(valid));
                match error.error_len() {
                    Some(length) => {
                        text.push(char::REPLACEMENT_CHARACTER);
                        rest = &after[length..];
                    },
                    None => {
                        rest = after;
                        break;
                    }
                }
            }
        }
    }
    *bytes = rest.to_vec();
    text
}

#[cfg(test)]
mod tests {
    use super::{OutputBuffer, OutputChunk, StopReason, WasmSimulator, ExceptionKind};
    use crate::{ARMSimulator, ARMv5RegNames};

    // 32-bit little endian ARM executable
    const HALLO: &[u8] = include_bytes!("../sample_programs/hallo");

    fn chunk(stream: i32, text: &str) -> OutputChunk {
        OutputChunk { stream, text: text.to_string() }
    }

    fn hallo() -> WasmSimulator {
        let mut simulator = ARMSimulator::new();
        simulator.load_elf_bytes(HALLO.to_vec()).unwrap();
        WasmSimulator::from_simulator(simulator)
    }

    #[test]
    fn output_keeps_order_and_merges_streams() {
        let mut output = OutputBuffer::default();
        output.push(1, b"ab");
        output.push(1, b"c");
        output.push(2, b"x");
        output.push(1, b"y");

        assert_eq!(vec![chunk(1, "abc"), chunk(2, "x"), chunk(1, "y")], output.take());
        assert_eq!(Vec::<OutputChunk>::new(), output.take());
    }

    #[test]
    fn output_waits_for_split_characters() {
        // "ä" is 0xC3 0xA4 in UTF-8
        let mut output = OutputBuffer::default();
        output.push(1, &[b'a', 0xC3]);
        assert_eq!(vec![chunk(1, "a")], output.take());

        output.push(1, &[0xA4]);
        assert_eq!(vec![chunk(1, "ä")], output.take());
    }

    #[test]
    fn output_replaces_invalid_bytes() {
        let mut output = OutputBuffer::default();
        output.push(1, &[b'a', 0xFF, b'b']);
        assert_eq!(vec![chunk(1, "a\u{FFFD}b")], output.take());
    }

    #[test]
    fn output_flush_replaces_incomplete_characters() {
        let mut output = OutputBuffer::default();
        output.push(2, &[0xC3]);
        output.flush();
        assert_eq!(vec![chunk(2, "\u{FFFD}")], output.take());
    }

    #[test]
    fn run_until_exit_collects_output() {
        let mut sim = hallo();

        assert_eq!(StopReason::Exit, sim.run(1000));
        assert_eq!(Some(0), sim.exit_code());
        assert_eq!(vec![chunk(1, "Hello Innsbruck!\n")], sim.take_output());
    }

    #[test]
    fn ended_program_does_not_continue() {
        let mut sim = hallo();
        sim.run(1000);
        let pc: u32 = sim.get_program_counter();

        assert_eq!(StopReason::Exit, sim.run(1000));
        assert_eq!(StopReason::Exit, sim.step());
        assert_eq!(pc, sim.get_program_counter());
    }

    #[test]
    fn moving_pc_continues_ended_program() {
        let mut sim = hallo();
        sim.run(1000);
        sim.take_output();

        sim.set_register(ARMv5RegNames::PC, 0x10074);
        assert_eq!(None, sim.exit_code());
        assert_eq!(StopReason::Exit, sim.run(1000));
        assert_eq!(vec![chunk(1, "Hello Innsbruck!\n")], sim.take_output());
    }

    #[test]
    fn reload_clears_state() {
        let mut sim = hallo();
        sim.run(1000);

        sim.reload_simulator().unwrap_or_else(|_| panic!("reload failed"));
        assert_eq!(None, sim.exit_code());
        assert!(sim.take_output().is_empty());
        assert_eq!(StopReason::None, sim.run(2));
    }

    #[test]
    fn run_stops_at_breakpoint() {
        let mut sim = hallo();
        sim.add_breakpoint(0x10080);

        assert_eq!(StopReason::Breakpoint, sim.run(1000));
        assert_eq!(0x10080, sim.get_program_counter());
        assert_eq!(StopReason::Exit, sim.run(1000));
    }

    #[test]
    fn exception_is_reported_as_data() {
        // swi #0 with write(1, 0xFFFF_FF00, 0x200)
        let mut sim = WasmSimulator::from_simulator(ARMSimulator::new());
        sim.simulator.set_memory(0x1000, &0xEF00_0000u32.to_le_bytes()).unwrap();
        sim.set_register(ARMv5RegNames::PC, 0x1000);
        sim.set_register(ARMv5RegNames::R7, 4);
        sim.set_register(ARMv5RegNames::R0, 1);
        sim.set_register(ARMv5RegNames::R1, 0xFFFF_FF00u32 as i32);
        sim.set_register(ARMv5RegNames::R2, 0x200);

        assert_eq!(StopReason::Error, sim.step());
        let exception = sim.exception().unwrap();
        assert_eq!(ExceptionKind::DataAbort, exception.kind());
        assert_eq!(0x1000, exception.address());
        assert_eq!(Some(0xFFFF_FF00), exception.memory_address());
        assert_eq!(Some(0x200), exception.size());
        assert_eq!(None, exception.syscall_number());
        assert_eq!(
            "Data abort at 0x00001000: access to 0xFFFFFF00 (512 bytes) is outside of memory",
            exception.message()
        );
    }
}
