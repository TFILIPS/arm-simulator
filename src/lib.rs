use std::collections::HashMap;

use elf_loader::ELFFile;
use simulated_cpu::{names::ARMv5RegNames, ARMv5CPU, SimulatedCPU};
pub use simulated_cpu::{SimulationEvent, SimulationException};
use utils::Memory;

mod elf_loader;
mod simulated_cpu;
pub mod utils;

const DEFAULT_STACK_POINTER: u32 = 0x40000;

pub struct ARMSimulator {
    elf_file: Option<ELFFile>,
    simulated_cpu: ARMv5CPU,
    dynamic_breakpoints: HashMap<u32, bool>,
}
impl ARMSimulator {
    pub fn new() -> ARMSimulator {
        ARMSimulator{ 
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

    pub fn get_flag(&self, flag: u32) -> bool {
        self.simulated_cpu.get_flag(flag.into())
    }

    pub fn set_flag(&mut self, flag: u32, value: bool) {
        self.simulated_cpu.set_flag(flag.into(), value);
    }

    pub fn get_flags(&self) -> Vec<bool> {
        Vec::from(self.simulated_cpu.get_flags())
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
    

// impl<CPU, R, RN, FN> ARMSimulator<CPU, R, RN, FN> {
//     pub fn new() -> ARMSimulator<CPU, R, RN, FN> {
//         ARMSimulator{ simulated_cpu: CPU::new(), dynamic_breakpoints: HashMap::new() }
//     }
 
    // pub fn from_elf_file(path: &str) -> Result<ARMSimulator, String> {

    //     let elf: ELFFile = ELFFile::load(path)?;
    //     elf.check_header_values()?;

    //     let mut cpu: ARMv5CPU = ARMv5CPU::new();
    //     cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
    //     cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
    //     cpu.set_encoding(elf.get_encoding());
    //     elf.load_into_memory(&mut cpu)?;

    //     Ok(ARMSimulator { 
    //         elf_file: elf, 
    //         simulated_cpu: cpu, 
    //         breakpoints: HashMap::new() 
    //     })
    // }

    // pub fn step(&mut self) -> Result<SimulationEvent, SimulationException> {
    //     self.simulated_cpu.step()
    // }

    // pub fn get_flags(&self) -> Vec<bool> {
    //     Vec::from(self.simulated_cpu.get_flags())
    //}
//}

// #[wasm_bindgen]
// #[cfg(target_family = "wasm")]
// impl ARMSimulator {
//     #[wasm_bindgen(constructor)]
//     pub fn new(
//         elf_bytes: &[u8], output_device: WebOutput, exit_behaviour: WebExit
//     ) -> Result<ARMSimulator, String> {

//         let elf: ELFFile = ARMSimulator::get_loaded_elf_file(elf_bytes)?;

//         let mut cpu: ARMv5CPU = ARMv5CPU::new(output_device, exit_behaviour);
//         cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
//         cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
//         cpu.set_encoding(elf.get_encoding());
//         elf.load_into_memory(&mut cpu)?;

//         Ok(ARMSimulator { 
//             elf_file: elf, simulated_cpu: cpu, breakpoints: HashMap::new() 
//         })
//     }

//     pub fn step(&mut self) {
//         self.simulated_cpu.step().unwrap_or(SimulationEvent::None);
//     }

//     pub fn multiple_steps(&mut self, n: usize) -> bool { 
//         for _ in 0..n {
//             if self.hit_active_breakepoint() {
//                 return false; 
//             }
//             match self.simulated_cpu.step() {
//                 Ok(SimulationEvent::Breakpoint) => return false,
//                 Ok(SimulationEvent::Exit(_)) | Err(_) => return false,
//                 _ => ()
//             }
//         }
//         true
//     }
// }

// #[wasm_bindgen]
// impl ARMSimulator {
//     pub fn get_disassembly(&self) -> Result<String, String> {
//         let (text_start, text_end) = self.elf_file.get_text_section_range()?;
//         let labels: Vec<(u32, String)> = self.elf_file.get_labels()?;
//         Ok(self.simulated_cpu.disassemble_memory(text_start, text_end, labels))
//     }

//     pub fn get_registers(&self) -> Vec<i32> {
//         Vec::from(self.simulated_cpu.get_registers())
//     }

//     pub fn get_register(&self, reg_num: u32) -> i32 {
//         self.simulated_cpu.get_register(reg_num.into())
//     }

//     pub fn set_register(&mut self, reg: u32, value: i32) {
//         self.simulated_cpu.set_register(reg.into(), value);
//     }

//     pub fn get_program_counter(&self) -> u32 { 
//         self.simulated_cpu.get_register(ARMv5RegNames::PC) as u32
//     }

//     pub fn set_flag(&mut self, flag: u32, value: bool) {
//         self.simulated_cpu.set_flag(flag.into(), value);
//     }

//     pub fn get_flag(&self, flag: u32) -> bool {
//         self.simulated_cpu.get_flag(flag.into())
//     }

//     pub fn add_breakpoint(&mut self, address: u32) {
//         if !self.breakpoints.contains_key(&address) {
//             self.breakpoints.insert(address, true);
//         }  
//     }

//     pub fn remove_breakpoint(&mut self, address: u32) {
//         if self.breakpoints.contains_key(&address) {
//             self.breakpoints.remove(&address);
//         }
//     }

//     pub fn get_memory(&self, address: u32, size: u32) -> Result<Vec<u8>, String> {
//         match self.simulated_cpu.get_memory(address, size) {
//             Ok(bytes) => Ok(Vec::from(bytes)),
//             Err(err) => Err(err.msg)
//         }
//     }

//     pub fn set_memory(&mut self, address: u32, bytes: &[u8]) -> Result<(), String> {
//         match self.simulated_cpu.set_memory(address, bytes) {
//             Ok(()) => Ok(()),
//             Err(err) => Err(err.msg)
//         }
//     }

//     pub fn hit_active_breakepoint(&mut self) -> bool {
//         let address: u32 = self.get_program_counter();
//         if let Some(active) = self.breakpoints.get(&address) {
//             if *active {
//                 self.breakpoints.insert(address, false);
//                 return true;
//             }
//             else {
//                 self.breakpoints.insert(address, true);
//             }
//         }
//         false
//     }

//     pub fn reload_simulator(&mut self) -> Result<(), String> {
//         let cpu: &mut ARMv5CPU = &mut self.simulated_cpu;
//         let elf: &ELFFile = &self.elf_file;
//         cpu.reset();
//         cpu.set_register(ARMv5RegNames::PC, elf.get_entry_point() as i32);
//         cpu.set_register(ARMv5RegNames::SP, DEFAULT_STACK_POINTER as i32);
//         cpu.set_encoding(elf.get_encoding());
//         elf.load_into_memory(cpu)?;
//         Ok(())
//     }
// }

// impl ARMSimulator {
//     pub fn get_loaded_elf_file(elf_bytes: &[u8]) -> Result<ELFFile, String> {
//         let bytes: Vec<u8> = Vec::from(elf_bytes);
//         let elf_file: ELFFile = ELFFile::load_raw(bytes)?;
//         elf_file.check_header_values()?;
//         Ok(elf_file)
//     }
// }


// #[wasm_bindgen]
// #[cfg(target_family = "wasm")]
// pub struct WebOutput {
//     output_buffer: Vec<u8>,
//     error_buffer: Vec<u8>,
//     print: js_sys::Function,
//     print_err: js_sys::Function
// }
// #[cfg(target_family = "wasm")]
// impl OutputDevice for WebOutput {
//     fn output(&mut self, bytes: &[u8]) {
//         self.output_buffer.extend(bytes);
//         if let Ok(string) = &String::from_utf8(self.output_buffer.clone()) {
//             self.print.call1(&JsValue::NULL, &JsValue::from_str(string))
//                 .expect("Error while trying to execute js function print!");
//             self.output_buffer.clear();
//         }
//     }
//     fn output_err(&mut self, bytes: &[u8]) {
//         self.error_buffer.extend(bytes);
//         if let Ok(string) = &String::from_utf8(self.error_buffer.clone()) {
//             self.print_err.call1(&JsValue::NULL, &JsValue::from_str(string))
//                 .expect("Error while trying to execute js function print!");
//             self.error_buffer.clear();
//         }
//     }

//     fn flush(&mut self) {
//         let buffered_outputs: String = 
//             String::from_utf8_lossy(&self.output_buffer).to_string();
//         self.print.call1(&JsValue::NULL, &JsValue::from_str(&buffered_outputs))
//             .expect("Error while trying to execute js function print!");
//         self.output_buffer.clear();

//         let buffered_errors: String = 
//             String::from_utf8_lossy(&self.error_buffer).to_string();
//         self.print.call1(&JsValue::NULL, &JsValue::from_str(&buffered_errors))
//             .expect("Error while trying to execute js function print!");
//         self.error_buffer.clear();
//     }
// }
// #[cfg(target_family = "wasm")]
// #[wasm_bindgen]
// impl WebOutput {
//     #[wasm_bindgen(constructor)]
//     pub fn new(print: js_sys::Function, print_err: js_sys::Function) -> WebOutput {
//         WebOutput { 
//             output_buffer: Vec::new(), 
//             error_buffer: Vec::new(),
//             print, print_err
//         }
//     }
// }

// #[wasm_bindgen]
// pub struct WebExit {
//         print: js_sys::Function, 
//         stop: js_sys::Function 
// }
// impl ExitBehaviour for WebExit {
//     fn exit(&self, code: i32) {
//         let return_str: &str = &format!("\n!> Exit Code: {code}\n\n");
//         self.print.call1(&JsValue::NULL, &JsValue::from_str(return_str))
//             .expect("Error while trying to execute js function print!");
//         self.stop.call0(&JsValue::NULL)
//             .expect("Error while trying to execute js function stop!");
//     }
// }
// #[wasm_bindgen]
// impl WebExit {
//     #[wasm_bindgen(constructor)]
//     pub fn new(print: js_sys::Function, stop: js_sys::Function) -> WebExit {
//         WebExit { print, stop }
//     }
// }
