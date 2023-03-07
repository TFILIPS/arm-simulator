use std::fs;

use headers::{ELFHeader, ProgramHeader};
use utils::Endian;

mod headers;
pub mod utils;

pub struct ELFFile {
    elf_header: ELFHeader,
    raw_data: Vec<u8>,
    encoding: Endian
}

impl ELFFile {
    pub fn load(file_path: &str) -> Result<Self, String> {
        let raw_data: Vec<u8> = match fs::read(file_path) {
            Ok(file_content) => file_content,
            Err(_) => return Err(String::from("Failed to read the ELF file!"))
        };

        if raw_data.len() < ELFHeader::SIZE {
            return Err(String::from("ELF file is to short to be valid!"));
        }
        
        let mut header_bytes: [u8; ELFHeader::SIZE] = [0u8; ELFHeader::SIZE];
        header_bytes.copy_from_slice(&raw_data[0..ELFHeader::SIZE]);
        let (elf_header, encoding) = ELFHeader::new(header_bytes)?;

        Ok(Self { elf_header, raw_data, encoding })
    }

    pub fn check_header_values(&self) -> Result<(), String> {
        self.elf_header.check_values()
    }

    pub fn load_into_memory(&self, memory: &mut Vec<u8>) -> Result<(), String> {
        let headers: Vec<ProgramHeader> = self.read_program_headers()?;
        for header in headers {
            // toDo: check for errors
            let file_start: usize = header.offset as usize;
            let file_end: usize = file_start + header.file_size as usize;
            let mem_start: usize = header.virtual_address as usize;
            let mem_end: usize = mem_start + header.memory_size as usize;

            memory[mem_start..mem_end].copy_from_slice(
                &self.raw_data[file_start..file_end]
            );
        }
        Ok(())
    }

    pub fn get_entry_point(&self) -> u32 {
        self.elf_header.entry_point
    }

    pub fn get_encoding(&self) -> &Endian {
        &self.encoding
    }

    fn read_program_headers(&self) -> Result<Vec<ProgramHeader>, String> {
        const HEADER_SIZE: usize = ProgramHeader::SIZE;

        let num_headers: usize = self.elf_header.num_program_headers as usize;
        let mut headers: Vec<ProgramHeader> = Vec::with_capacity(num_headers);

        let mut start: usize = self.elf_header.program_table_offset as usize;
        let mut stop: usize = start + HEADER_SIZE;
        for _ in 0..num_headers {
            if self.raw_data.len() < stop {
                return {
                    Err(String::from("Error while reading program headers!"))
                };
            }

            let mut header_bytes: [u8; HEADER_SIZE] = [0u8; HEADER_SIZE];
            header_bytes.copy_from_slice(&self.raw_data[start..stop]);
            headers.push(ProgramHeader::new(header_bytes, &self.encoding));

            start += HEADER_SIZE;
            stop += HEADER_SIZE;
        }
        Ok(headers)
    }
}