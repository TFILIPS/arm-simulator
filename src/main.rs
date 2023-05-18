use std::{process::exit, env};

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames};
use utils::{ConsoleExit, ConsoleOutput};

mod elf_loader;
mod simulated_cpu;
mod utils;

fn main() {
    let (path, disassemble): (String, bool) = parse_arguments();

    let elf_file: ELFFile = ELFFile::load(&path).unwrap_or_else(print_and_exit);
    elf_file.check_header_values().unwrap_or_else(print_and_exit);
    
    let mut cpu: Box<dyn SimulatedCPU<i32>> = 
        Box::new(ARMv5CPU::new(ConsoleOutput, ConsoleExit));
    cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
    cpu.set_register(RegNames::SP, 0x4000);
    cpu.set_encoding(elf_file.get_encoding());
    elf_file.load_memory(cpu.get_memory()).unwrap_or_else(print_and_exit);

    if disassemble {
        let (text_start, text_end): (u32, u32) = 
        elf_file.get_text_section_range().unwrap_or_else(print_and_exit);

        let labels = elf_file.get_labels().unwrap_or_else(print_and_exit);
        print!("{}", cpu.disassemble_memory(text_start, text_end, labels));
    }
    else { loop { cpu.step(); } }
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

fn print_and_exit<T>(msg: String) -> T {
    eprintln!("{msg}");
    exit(1);
}
