use std::{process::exit, env};

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, ARMv5CPU, names::RegNames};

mod elf_loader;
mod simulated_cpu;
mod utils;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: ./{:} path_to_executable", args[0]);
        exit(1);
    }
    let path: &str = &args[1];
    let elf_file: ELFFile = ELFFile::load(path).unwrap_or_else(print_and_exit);
    elf_file.check_header_values().unwrap_or_else(print_and_exit);
    
    let mut cpu: Box<dyn SimulatedCPU<i32>> = Box::new(ARMv5CPU::new());
    cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
    cpu.set_register(RegNames::SP, 0x4000);
    cpu.set_encoding(elf_file.get_encoding());

    elf_file.load_memory(cpu.get_memory()).unwrap_or_else(print_and_exit);
    //println!("{:}", cpu.disassemble_memory(elf_file.get_entry_point(), 4*100));

    loop { cpu.step(); }
}

fn print_and_exit<T>(msg: String) -> T {
    eprintln!("{msg}");
    exit(1);
}
