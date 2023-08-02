use std::{process::exit, env};

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames};
use utils::{ConsoleExit, ConsoleOutput, ExitOnError};

mod elf_loader;
mod simulated_cpu;
mod utils;

const DEFAULT_STACK_POINTER: u32 = 0x400000;

fn main() {
    let (path, disassemble): (String, bool) = parse_arguments();

    let elf_file: ELFFile = ELFFile::load(&path).unwarp_or_exit();
    elf_file.check_header_values().unwarp_or_exit();
    
    let mut cpu: ARMv5CPU = ARMv5CPU::new(ConsoleOutput, ConsoleExit);
    cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
    cpu.set_register(RegNames::SP, DEFAULT_STACK_POINTER as i32);
    cpu.set_encoding(elf_file.get_encoding());
    elf_file.load_into_memory(&mut cpu).unwarp_or_exit();

    if disassemble {
        let (text_start, text_end): (u32, u32) = 
            elf_file.get_text_section_range().unwarp_or_exit();
        let labels: Vec<(u32, String)> = 
            elf_file.get_labels().unwarp_or_exit();
        print!("{}", cpu.disassemble_memory(text_start, text_end, labels));
    }
    else { 
        loop {
            //Here we can use unwrap, because if the last executed step wasn't
            //successful, the selected ExitBehaviour will terminate the
            //simulation before the unwraping happens.
            cpu.step().unwrap() 
        } 
    }
}

fn parse_arguments() -> (String, bool) {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 {
        println!("Usage: {:} path_to_executable [--disassemble]", args[0]);
        exit(1);
    }
    let mut path: String = String::new();
    let mut disassemble: bool = false;
    for arg in &args[1..] {
        if arg.starts_with("--") {
            disassemble |= arg == "--disassemble";
        }
        else if path.is_empty() {
            path = String::from(arg);
        }
    }
    (path, disassemble)
}
