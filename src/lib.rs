use wasm_bindgen::prelude::*;

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames};

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
    pub fn new(elf_bytes: &[u8]) -> Result<ARMSimulator, String> {

        let elf_file: ELFFile = ARMSimulator::get_loaded_elf_file(elf_bytes)?;

        let simulated_cpu: Box<dyn SimulatedCPU<i32>> = 
            ARMSimulator::get_new_cpu(&elf_file, DEFAULT_SP)?;

        Ok(ARMSimulator { elf_file, simulated_cpu })
    }

    pub fn reload_simulator(&mut self) -> Result<(), String> {
        self.simulated_cpu = 
            ARMSimulator::get_new_cpu(&self.elf_file, DEFAULT_SP)?;
        Ok(())
    }

    pub fn step(&mut self) {
        self.simulated_cpu.step();
    }

    fn get_new_cpu(
        elf_file: &ELFFile, stack_pointer: u32
    ) -> Result<Box<dyn SimulatedCPU<i32>>, String> {

        let mut cpu: Box<dyn SimulatedCPU<i32>> = 
            Box::new(ARMv5CPU::new(ConsoleOutput, ConsoleExit));

        cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
        cpu.set_register(RegNames::SP, stack_pointer as i32);
        cpu.set_encoding(elf_file.get_encoding());
        elf_file.load_memory(cpu.get_memory())?;
        Ok(cpu)
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

    pub fn get_program_counter(&self) -> u32 { 
        self.simulated_cpu.get_register(RegNames::PC) as u32
    }

    pub fn multiple_steps(&mut self, n: usize) { 
        for _ in 0..n {
            self.simulated_cpu.step();
        }
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
    fn output(&self, msg: &str) {
        self.print.call1(&JsValue::NULL, &JsValue::from_str(msg))
            .expect("Error while trying to execute js function print!");
    }
    fn output_err(&self, err: &str) {
        self.print_err.call1(&JsValue::NULL, &JsValue::from_str(err))
            .expect("Error while trying to execute js function print_err!");
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



/*use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames};
use utils::{OutputDevice, ExitBehaviour, ConsoleOutput, ConsoleExit};

mod elf_loader;
mod simulated_cpu;
mod utils;

static mut ELF_FILE: Option<ELFFile> = None;
static mut CPU: Option<Box<dyn SimulatedCPU<i32>>> = None;

static mut PLATFORM: Platform = Platform::Default;

enum Platform { 
    Default, 
    Web {
        print: js_sys::Function, 
        print_err: js_sys::Function,
        stop: js_sys::Function,
        update: js_sys::Function
    },
    Test
}

#[wasm_bindgen]
pub unsafe fn change_platfrom_to_web(
    print: js_sys::Function, print_err: js_sys::Function, 
    stop: js_sys::Function, update: js_sys::Function
) { 
    PLATFORM = Platform::Web { print, print_err, stop, update } 
}

#[wasm_bindgen]
pub unsafe fn load_elf_file(bytes: &[u8]) -> Result<(), String> {
    ELF_FILE = None;

    let bytes: Vec<u8> = Vec::from(bytes);
    match ELFFile::load_raw(bytes) {
        Ok(elf_file) => {
            match elf_file.check_header_values() {
                Ok(()) => Ok(ELF_FILE = Some(elf_file)),
                Err(err_msg) => Err(err_msg)
            }
        },
        Err(err_msg) => Err(err_msg)
    }
}

#[wasm_bindgen]
pub unsafe fn init_simulator() -> Result<(), String>{
    CPU = None;

    if let Some(elf_file) = &ELF_FILE {
        let mut cpu: Box<dyn SimulatedCPU<i32>> = match &PLATFORM {
            Platform::Default => {
                Box::new(ARMv5CPU::new(ConsoleOutput, ConsoleExit))
            },
            Platform::Web{print, print_err, stop, ..} => {
                Box::new(ARMv5CPU::new(
                    WebOutput { print, print_err }, WebExit { print, stop }
                ))
            },
            Platform::Test => Box::new(ARMv5CPU::new(TestOutput, TestExit))
        };
            
        cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
        cpu.set_register(RegNames::SP, 0x3_000_000);
        cpu.set_encoding(elf_file.get_encoding());

        if let Err(err) = elf_file.load_memory(cpu.get_memory()) {
            Err(err)
        }
        else { 
            CPU = Some(cpu);
            if let Platform::Web { update, ..} = &PLATFORM {
                update.call0(&JsValue::NULL).expect(
                    "Error while trying to execute js function update!");
            }
            Ok(()) 
        }
    }
    else { Err(String::from("Need to load elf_file first!")) }
}

#[wasm_bindgen]
pub unsafe fn step() -> Result<(), String>{
    if let Some(cpu) = &mut CPU{
        cpu.step();
        if let Platform::Web { update, .. } = &PLATFORM {
            update.call0(&JsValue::NULL)
                .expect("Error while trying to execute js function update!");
        }
        Ok(())
    }
    else { Err(String::from("Need to init cpu first!")) }
}

#[wasm_bindgen]
pub unsafe fn get_disassembly() -> Result<String, String> {
    if let Some(elf) = &ELF_FILE {
        if let Some(cpu) = &mut CPU{
            let (text_start, text_end): (u32, u32) = 
                elf.get_text_section_range().unwrap_or_default();
    
            let labels = elf.get_labels().unwrap_or_default();
            Ok(cpu.disassemble_memory(text_start, text_end, labels))
        }
        else { Err(String::from("Need to init cpu first!")) }
    }
    else { Err(String::from("Need to load elf_file first!")) }
}



#[wasm_bindgen]
pub unsafe fn get_registers() -> Result<Vec<i32>, String> {
    if let Some(cpu) = &CPU {
        Ok(Vec::from(cpu.get_registers()))
    }
    else { Err(String::from("Need to init cpu first!")) }
}

#[wasm_bindgen]
pub unsafe fn get_program_counter() -> Result<u32, String> { 
    if let Some(cpu) = &CPU {
        Ok(cpu.get_register(RegNames::PC) as u32)
    }
    else { Err(String::from("Need to init cpu first!")) }
}

#[no_mangle]
pub extern "C" fn print_hello() {
    println!("Hello, world!");
}

struct WebOutput {
    print: &'static js_sys::Function,
    print_err: &'static js_sys::Function
}
impl OutputDevice for WebOutput {
    fn output(&self, msg: &str) {
        self.print.call1(&JsValue::NULL, &JsValue::from_str(msg))
            .expect("Error while trying to execute js function print!");
    }
    fn output_err(&self, err: &str) {
        self.print_err.call1(&JsValue::NULL, &JsValue::from_str(err))
            .expect("Error while trying to execute js function print_err!");
    }
}

struct WebExit {
        print: &'static js_sys::Function, 
        stop: &'static js_sys::Function 
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

struct TestOutput;
impl OutputDevice for TestOutput {
    fn output(&self, _: &str) { }
    fn output_err(&self, _: &str) { }
}

struct TestExit;
impl ExitBehaviour for TestExit {
    fn exit(&self, _: i32) { }
}
*/
/* 
// found better solution
use pyo3::{prelude::*, exceptions::PyValueError};


#[pymodule]
fn arm_simulator(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(load_elf_file_py, m)?)?;
    m.add_function(wrap_pyfunction!(init_simulator_py, m)?)?;
    m.add_function(wrap_pyfunction!(step_py, m)?)?;
    m.add_function(wrap_pyfunction!(get_disassembly_py, m)?)?;
    m.add_function(wrap_pyfunction!(get_registers_py, m)?)?;
    m.add_function(wrap_pyfunction!(set_register_py, m)?)?;
    m.add_function(wrap_pyfunction!(change_platfrom_to_test, m)?)?;
    Ok(())
}

#[pyfunction]
pub unsafe fn load_elf_file_py(bytes: &[u8]) -> PyResult<()> {
    match load_elf_file(bytes) {
        Ok(()) => Ok(()),
        Err(err_msg) => Err(PyValueError::new_err(err_msg))
    }
}

#[pyfunction]
pub unsafe fn init_simulator_py() -> PyResult<()> {
    match init_simulator() {
        Ok(()) => Ok(()),
        Err(err_msg) => Err(PyValueError::new_err(err_msg))
    }
}

#[pyfunction]
pub unsafe fn step_py() -> PyResult<()> {
    match step() {
        Ok(()) => Ok(()),
        Err(err_msg) => Err(PyValueError::new_err(err_msg))
    }
}

#[pyfunction]
pub unsafe fn get_disassembly_py() -> PyResult<String> {
    match get_disassembly() {
        Ok(dissasembly) => Ok(dissasembly),
        Err(err_msg) => Err(PyValueError::new_err(err_msg))
    }
}

#[pyfunction]
pub unsafe fn get_registers_py() -> PyResult<Vec<i32>> {
    match get_registers() {
        Ok(registers) => Ok(registers),
        Err(err_msg) => Err(PyValueError::new_err(err_msg))
    }
}

#[pyfunction]
pub unsafe fn set_register_py(register: usize, value: u32) -> PyResult<()> {
    if let Some(cpu) = &mut CPU {
        Ok(cpu.set_register((register as u32).into(), value as i32))
    }
    else { Err(PyValueError::new_err(String::from("Need to init cpu first!"))) }
}

#[pyfunction]
pub unsafe fn get_program_counter_py() -> PyResult<u32> {
    match get_program_counter() {
        Ok(pc) => Ok(pc),
        Err(err_msg) => Err(PyValueError::new_err(err_msg))
    }
}

#[pyfunction]
pub unsafe fn change_platfrom_to_test() {
    PLATFORM = Platform::Test;
}
*/