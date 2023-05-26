use wasm_bindgen::prelude::*;

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames};
use utils::{OutputDevice, ExitBehaviour};

mod elf_loader;
mod simulated_cpu;
mod utils;

static mut ELF_FILE: Option<ELFFile> = None;
static mut CPU: Option<Box<dyn SimulatedCPU<i32>>> = None;

static mut RUNNING: bool = false;

#[wasm_bindgen(raw_module = "../../src/components/Console.tsx")]
extern "C" {
    fn print_to_web_console(msg: &str);
    fn print_error_to_web_console(err: &str);
}

#[wasm_bindgen(raw_module = "../../src/components/Simulation.tsx")]
extern "C" {
    fn update_state();
}

// bad solution until WebWorkers work
#[wasm_bindgen(raw_module = "../../src/components/ControlBar.tsx")]
extern "C" {
    fn stop_execution();
}

#[wasm_bindgen]
pub unsafe fn load_elf_file(bytes: &[u8]) {
    let bytes: Vec<u8> = Vec::from(bytes);
    ELF_FILE = match ELFFile::load_raw(bytes) {
        Ok(elf_file) => Some(elf_file),
        Err(err_msg) => {
            print_error_to_web_console(&err_msg);
            None
        }
    }
}

#[wasm_bindgen]
pub unsafe fn init_simulator() {
    CPU = None;
    if let Some(elf_file) = &ELF_FILE {
        let mut cpu: Box<dyn SimulatedCPU<i32>> = 
            Box::new(ARMv5CPU::new(WebOutput, WebExit));
        cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
        cpu.set_register(RegNames::SP, 0x3_000_000);
        cpu.set_encoding(elf_file.get_encoding());
        if let Err(msg) = elf_file.load_memory(cpu.get_memory()) {
            print_error_to_web_console(&msg);
            return;
        }
        CPU = Some(cpu);
        update_state();
    }
    else {
        print_error_to_web_console("Need to load elf_file first!");
    }
}

#[wasm_bindgen]
pub unsafe fn step() {
    if let Some(cpu) = &mut CPU{
        cpu.step();
        update_state();
    }
    else {
        print_error_to_web_console("Need to init cpu first!");
    }
}

#[wasm_bindgen]
pub unsafe fn run_continuously() {
    if let Some(cpu) = &mut CPU{
        RUNNING = true;
        while RUNNING {
            cpu.step();
        }
    }
    else {
        print_error_to_web_console("Need to init cpu first!");
    }
}

#[wasm_bindgen]
pub unsafe fn stop() {
    RUNNING = false;
}

#[wasm_bindgen]
pub unsafe fn get_disassembly() -> String {
    if let Some(elf) = &ELF_FILE {
        if let Some(cpu) = &mut CPU{
            let (text_start, text_end): (u32, u32) = 
                elf.get_text_section_range().unwrap_or_default();
    
            let labels = elf.get_labels().unwrap_or_default();
            cpu.disassemble_memory(text_start, text_end, labels)
        }
        else {
            print_error_to_web_console("Need to init cpu first!");
            String::new()
        }
    }
    else {
        print_error_to_web_console("Need to load elf_file first!");
        String::new()
    }
}

#[wasm_bindgen]
pub unsafe fn get_registers() -> Vec<i32> {
    if let Some(cpu) = &CPU {
        Vec::from(cpu.get_registers())
    }
    else {
        print_error_to_web_console("Need to init cpu first!");
        vec![0; 16]
    }
}

//#[wasm_bindgen]
//pub unsafe fn get_registers2() -> Vec<u8> {
//    if let Some(cpu) = &CPU {
//        Vec::from(cpu.get_flags())
//    }
//    else {
//        print_error_to_web_console("Need to init cpu first!");
//        vec![0; 16]
//    }
//}

#[wasm_bindgen]
pub unsafe fn get_program_counter() -> u32 { 
    if let Some(cpu) = &CPU {
        cpu.get_register(RegNames::PC) as u32
    }
    else {
        print_error_to_web_console("Need to init cpu first!");
        0
    }
}


struct WebOutput;
impl OutputDevice for WebOutput {
    fn output(&self, msg: &str) {
        print_to_web_console(msg);
    }
    fn output_err(&self, err: &str) {
        print_error_to_web_console(err);
    }
}

struct WebExit;
impl ExitBehaviour for WebExit {
    fn exit(&self, code: i32) {
        print_to_web_console(&format!("\n!> Exit Code: {code}\n\n"));
        stop_execution();
    }
}
