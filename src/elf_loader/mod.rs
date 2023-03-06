use std::{fs::File, io::Read, process::exit};
use headers::ELFHeader;

mod headers;
mod utils;

pub fn load_elf(file_path: &str) {
    let mut elf_file: File = match File::open(file_path) {
        Ok(file) => file,
        Err(_) => {
            eprintln!("Unable to open ELF file!");
            exit(-1);
        }
    };

    let mut header_bytes: [u8; ELFHeader::SIZE] = [0u8; ELFHeader::SIZE];
    if let Err(_) = elf_file.read_exact(&mut header_bytes) {
        eprintln!("ELF file is to short to be valid!");
        exit(-1);
    }

    let file_header: ELFHeader = ELFHeader::from_byte_array(header_bytes);
    println!("{:#x?}", file_header);

    file_header.check_values();
}