use wasm_bindgen::prelude::*;

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames, SimulationException};

#[cfg(not(target_family = "wasm"))]
use utils::{ConsoleOutput, ConsoleExit, OutputDevice, ExitBehaviour};
#[cfg(target_family = "wasm")]
use utils::{OutputDevice, ExitBehaviour};

mod elf_loader;
mod simulated_cpu;
mod utils;

const DEFAULT_SP: u32 = 0x40000;

#[wasm_bindgen]
pub struct ARMSimulator {
    elf_file: ELFFile,
    simulated_cpu: Box<dyn SimulatedCPU<i32>>,
    #[cfg(target_family = "wasm")]
    js_functions: NeededJSFunctions
}

#[cfg(not(target_family = "wasm"))]
impl ARMSimulator { 
    pub fn from_bytes(elf_bytes: &[u8]) -> Result<ARMSimulator, String> {

        let elf_file: ELFFile = ARMSimulator::get_loaded_elf_file(elf_bytes)?;

        let simulated_cpu: Box<dyn SimulatedCPU<i32>> = 
            ARMSimulator::get_new_cpu(&elf_file, DEFAULT_SP)?;

        Ok(ARMSimulator { elf_file, simulated_cpu })
    }

    pub fn from_file(path: &str) -> Result<ARMSimulator, String> {
        let elf_file: ELFFile = ELFFile::load(path)?;
        elf_file.check_header_values()?;

        let simulated_cpu: Box<dyn SimulatedCPU<i32>> = 
            ARMSimulator::get_new_cpu(&elf_file, DEFAULT_SP)?;

        Ok(ARMSimulator { elf_file, simulated_cpu })
    }

    pub fn reload_simulator(&mut self) -> Result<(), String> {
        self.simulated_cpu = 
            ARMSimulator::get_new_cpu(&self.elf_file, DEFAULT_SP)?;
        Ok(())
    }

    pub fn step(&mut self) -> Result<(), SimulationException> {
        self.simulated_cpu.step()
    }

    pub fn get_flags(&self) -> Vec<bool> {
        Vec::from(self.simulated_cpu.get_flags())
    }


    fn get_new_cpu(
        elf_file: &ELFFile, stack_pointer: u32
    ) -> Result<Box<dyn SimulatedCPU<i32>>, String> {

        let mut cpu: ARMv5CPU = 
            ARMv5CPU::new(ConsoleOutput::new(), ConsoleExit);

        cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
        cpu.set_register(RegNames::SP, stack_pointer as i32);
        cpu.set_encoding(elf_file.get_encoding());
        elf_file.load_into_memory(&mut cpu)?;
        Ok(Box::new(cpu))
    }
}

#[wasm_bindgen]
#[cfg(target_family = "wasm")]
impl ARMSimulator {
    #[wasm_bindgen(constructor)]
    pub fn new(
        elf_bytes: &[u8], js_functions: NeededJSFunctions
    ) -> Result<ARMSimulator, String> {

        let elf_file: ELFFile = ARMSimulator::get_loaded_elf_file(elf_bytes)?;

        let simulated_cpu: Box<dyn SimulatedCPU<i32>> = 
            ARMSimulator::get_new_cpu(&elf_file, DEFAULT_SP, &js_functions)?;

        Ok(ARMSimulator { elf_file, simulated_cpu, js_functions })
    }

    pub fn reload_simulator(&mut self) -> Result<(), String> {
        self.simulated_cpu = ARMSimulator::get_new_cpu(
            &self.elf_file, DEFAULT_SP, &self.js_functions)?;
        Ok(())
    }

    pub fn step(&mut self) {
        self.simulated_cpu.step();
        self.js_functions.update.call0(&JsValue::NULL)
            .expect("Error while trying to execute js function update!");

    }

    fn get_new_cpu(
        elf_file: &ELFFile, stack_pointer: u32, js_functions: &NeededJSFunctions
    ) -> Result<Box<dyn SimulatedCPU<i32>>, String> {

        let mut cpu: Box<dyn SimulatedCPU<i32>> = Box::new(ARMv5CPU::new(
            WebOutput { 
                print: js_functions.print.clone(), 
                print_err: js_functions.print_err.clone()
            },
            WebExit { 
                print: js_functions.print.clone(), 
                stop: js_functions.stop.clone()
            }
        ));

        cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
        cpu.set_register(RegNames::SP, stack_pointer as i32);
        cpu.set_encoding(elf_file.get_encoding());
        elf_file.load_memory(cpu.get_memory())?;
        Ok(cpu)
    }

    pub fn multiple_steps(&mut self, n: usize) { 
        for _ in 0..n {
            if let Err(_) = self.simulated_cpu.step() {
                return;
            }
        }
    }
}

#[wasm_bindgen]
impl ARMSimulator {
    pub fn get_disassembly(&self) -> Result<String, String> {
        let (text_start, text_end) = self.elf_file.get_text_section_range()?;
        let labels: Vec<(u32, String)> = self.elf_file.get_labels()?;
        Ok(self.simulated_cpu.disassemble_memory(text_start, text_end, labels))
    }

    pub fn get_registers(&self) -> Vec<i32> {
        Vec::from(self.simulated_cpu.get_registers())
    }

    pub fn get_register(&self, reg_num: u32) -> i32 {
        self.simulated_cpu.get_register(reg_num.into())
    }

    pub fn set_register(&mut self, reg: u32, value: i32) {
        self.simulated_cpu.set_register(reg.into(), value);
    }

    pub fn get_program_counter(&self) -> u32 { 
        self.simulated_cpu.get_register(RegNames::PC) as u32
    }
}

impl ARMSimulator {
    pub fn get_loaded_elf_file(elf_bytes: &[u8]) -> Result<ELFFile, String> {
        let bytes: Vec<u8> = Vec::from(elf_bytes);
        let elf_file: ELFFile = ELFFile::load_raw(bytes)?;
        elf_file.check_header_values()?;
        Ok(elf_file)
    }
}

#[cfg(target_family = "wasm")]
#[wasm_bindgen]
pub struct NeededJSFunctions {
    print: js_sys::Function, 
    print_err: js_sys::Function,
    stop: js_sys::Function,
    update: js_sys::Function
}
#[cfg(target_family = "wasm")]
#[wasm_bindgen]
impl NeededJSFunctions {
    #[wasm_bindgen(constructor)]
    pub fn new(
        print: js_sys::Function, print_err: js_sys::Function,
        stop: js_sys::Function, update: js_sys::Function
    ) -> NeededJSFunctions {

        NeededJSFunctions { print, print_err, stop, update }
    }
}

struct WebOutput {
    print: js_sys::Function,
    print_err: js_sys::Function
}
impl OutputDevice for WebOutput {
    fn output(&mut self, bytes: &[u8]) {
        let msg: &str = &String::from_utf8_lossy(bytes);
        self.print.call1(&JsValue::NULL, &JsValue::from_str(msg))
            .expect("Error while trying to execute js function print!");
    }
    fn output_err(&mut self, bytes: &[u8]) {
        let err: &str = &String::from_utf8_lossy(bytes);
        self.print_err.call1(&JsValue::NULL, &JsValue::from_str(err))
            .expect("Error while trying to execute js function print_err!");
    }

    fn flush(&mut self) {
        // Do nothing
    }
}

struct WebExit {
        print: js_sys::Function, 
        stop: js_sys::Function 
}
impl ExitBehaviour for WebExit {
    fn exit(&self, code: i32) {
        let return_str: &str = &format!("\n!> Exit Code: {code}\n\n");
        self.print.call1(&JsValue::NULL, &JsValue::from_str(return_str))
            .expect("Error while trying to execute js function print!");
        self.stop.call0(&JsValue::NULL)
            .expect("Error while trying to execute js function stop!");
    }
}