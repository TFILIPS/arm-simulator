use std::process::exit;
use elf_loader::{ELFFile, utils::Endian};

mod elf_loader;

const MEMORY_SIZE: usize = 2usize.pow(26);

fn main() {
    let mut memory: Vec<u8> = vec![0u8; MEMORY_SIZE];
    let elf_file: ELFFile = ELFFile::load("data/hello").unwrap_or_else(|msg| {
        eprintln!("{msg}");
        exit(-1);
    });
    if let Err(msg) = elf_file.check_header_values() {
        eprintln!("{msg}");
        exit(-1);
    }
    if let Err(msg) = elf_file.load_into_memory(&mut memory) {
        eprintln!("{msg}");
        exit(-1);
    }
    let test_range = &memory[0x10000..0x10050];
    println!("Memory: {:?}", test_range);

    let enc: &Endian = elf_file.get_encoding();
    let pc: u32 = elf_file.get_entry_point();
    println!("{:?}, 0x{:x?}", enc, pc);
}
