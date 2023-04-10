use std::{process::exit};

use elf_loader::ELFFile;
use simulated_cpu::{SimulatedCPU, names::RegNames};

mod elf_loader;
mod simulated_cpu;
mod utils;

fn main() {
    let mut cpu: SimulatedCPU = SimulatedCPU::new();
    
    let elf_file: ELFFile = 
        ELFFile::load("data/and").unwrap_or_else(print_error_exit);
    elf_file.check_header_values().unwrap_or_else(print_error_exit);

    elf_file.load_memory(cpu.get_memory()).unwrap_or_else(print_error_exit);
    cpu.set_register(RegNames::PC, elf_file.get_entry_point() as i32 + 8);
    cpu.set_register(RegNames::SP, 0x8000);
    cpu.set_encoding(elf_file.get_encoding());

    for _ in 0..16 {
        cpu.step();
    }

    print_all_registers(&cpu);
}

fn print_all_registers(cpu: &SimulatedCPU) {
    for i in 0..16 {
        println!("Register: {:?} Value: {:x?}", RegNames::from(i), cpu.get_register(RegNames::from(i)))
    }
}

fn print_error_exit<T>(msg: String) -> T {
    eprintln!("{msg}");
    exit(-1);
}

