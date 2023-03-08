use std::process::exit;

use elf_loader::ELFFile;

mod elf_loader;
mod simulated_cpu;

fn main() {
    let mut memory: Vec<u8> = vec![0u8; 2usize.pow(26)];
    
    let elf_file: ELFFile = ELFFile::load("data/hello").unwrap_or_else(|msg| {
        eprintln!("{msg}");
        exit(-1);
    });

    elf_file.load_memory(&mut memory).unwrap_or_else(|msg| {
        eprintln!("{msg}");
        exit(-1);
    });

    elf_file.check_header_values().unwrap_or_else(|msg| {
        eprintln!("{msg}");
        exit(-1);
    });    
}
