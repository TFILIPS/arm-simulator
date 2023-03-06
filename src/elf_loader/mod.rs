use std::{fs::File, os::unix::prelude::FileExt, process::exit};

use headers::{ELFHeader, ProgramHeader};
use utils::Endian;

mod headers;
mod utils;

const MEMORY_SIZE: usize = 2usize.pow(26);

pub fn load_elf(file_path: &str) -> (Vec<u8>, u32, Endian) {
    let elf_file: File = match File::open(file_path) {
        Ok(file) => file,
        Err(_) => {
            eprintln!("Unable to open ELF file!");
            exit(-1);
        }
    };

    let elf_header: ELFHeader = read_elf_header(&elf_file);
    elf_header.check_values();

    let encoding: Endian = elf_header.encoding();

    let program_headers: Vec<ProgramHeader> =
        read_program_headers(&elf_file, &elf_header, &encoding);

    let memory: Vec<u8> = create_memory(&elf_file, &program_headers);

    return (memory, *elf_header.entry_point(), encoding);
}

fn read_elf_header(elf_file: &File) -> ELFHeader{
    let mut header_bytes: [u8; ELFHeader::SIZE] = [0u8; ELFHeader::SIZE];
    if let Err(_) = elf_file.read_exact_at(&mut header_bytes, 0) {
        eprintln!("ELF file is to short to be valid!");
        exit(-1);
    }

    return ELFHeader::from_byte_array(header_bytes);
}

fn read_program_headers(
    elf_file: &File,
    elf_header: &ELFHeader,
    encoding: &Endian
) -> Vec<ProgramHeader> {
    
    let num_prog_headers: usize = *elf_header.num_program_headers() as usize;

    let mut program_headers: Vec<ProgramHeader> =
        Vec::with_capacity(num_prog_headers);

    let mut offset: u64 = *elf_header.program_table_offset() as u64;
    for _ in 0..num_prog_headers {
        let mut buffer: [u8; ProgramHeader::SIZE] = [0u8; ProgramHeader::SIZE];

        if let Err(_) = elf_file.read_exact_at(&mut buffer, offset) {
            eprintln!("Error while reading program headers!");
            exit(-1);
        };
        program_headers.push(ProgramHeader::from_byte_array(buffer, &encoding));

        offset += ProgramHeader::SIZE as u64;
    }

    return program_headers;
}

fn create_memory(
    elf_file: &File, 
    program_headers: &Vec<ProgramHeader>
) -> Vec<u8> {

    let mut memory: Vec<u8> = vec![0; MEMORY_SIZE];

    for header in program_headers {
        let offset: u64 = *header.offset() as u64;
        let start: usize = *header.virtual_address() as usize;
        let end: usize = start + *header.file_size() as usize; 

        if let Err(_) = 
            elf_file.read_exact_at(&mut memory[start..end], offset) {
            
            eprintln!("Error while initializing memory from ELF file!");
            exit(-1);
        }
    }
    return memory;
}
