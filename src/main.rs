use std::{process::exit};

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, names::RegNames};

mod elf_loader;
mod simulated_cpu;
mod utils;

fn main() {
    let mut cpu: SimulatedCPU = SimulatedCPU::new();
    
    let elf_file: ELFFile = 
        ELFFile::load("data/simple_add").unwrap_or_else(print_error_exit);
    elf_file.check_header_values().unwrap_or_else(print_error_exit);

    elf_file.load_memory(cpu.get_memory()).unwrap_or_else(print_error_exit);
    cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32);
    cpu.set_register(RegNames::SP, 0x8000);
    cpu.set_encoding(elf_file.get_encoding());

    cpu.step();
}

fn print_error_exit<T>(msg: String) -> T {
    eprintln!("{msg}");
    exit(-1);
}

