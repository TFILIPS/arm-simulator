use std::fs;

use headers::{
    ELFHeader, InternalHeader, ProgramHeader, SectionHeader, ValueError
};
use crate::utils::Endian;

mod headers;

// Defined values every ELF file header has to contain
const ELF_ID: [u8; 4] = [0x7f, 0x45, 0x4c, 0x46];
const HEADER_VERSION_KEY: u8 = 0x1;
const ELF_VERSION_KEY: u32 = 0x1;

// Modifiable values: For this project we want an 32-bit ARM executable
const CLASS_KEY: u8 = 0x1; // 32-Bit
const MACHINE_KEY: u16 = 0x28; // ARM
const ELF_TYPE_KEY: u16 = 0x2; // executeable

pub struct ELFFile {
    elf_header: ELFHeader,
    raw_data: Vec<u8>,
    encoding: Endian
}

impl ELFFile {
    pub fn load(file_path: &str) -> Result<Self, String> {
        let raw_data: Vec<u8> = match fs::read(file_path) {
            Ok(file_content) => file_content,
            Err(_) => return Err(String::from("Failed to read the ELF file!")),
        };

        if raw_data.len() < ELFHeader::SIZE {
            return Err(String::from("ELF file is to short to be valid!"));
        }

        let (elf_header, encoding) =
            ELFHeader::new(raw_data[0..ELFHeader::SIZE].try_into().unwrap())?;

        Ok(Self{ elf_header, raw_data, encoding })
    }

    pub fn load_memory(&self, memory: &mut Vec<u8>) -> Result<(), String> {
        let headers: Vec<ProgramHeader> = self.read_program_headers()?;
        for header in headers {
            if header.program_type != 1 { continue; }
            let file_start: usize = header.offset as usize;
            let file_end: usize = file_start + header.file_size as usize;
            let mem_start: usize = header.virtual_address as usize;
            let mem_end: usize = mem_start + header.file_size as usize;

            if memory.len() < mem_start + header.memory_size as usize {
                return Err(String::from("ELF file expects a larger memory!"));
            }
            if self.raw_data.len() < file_end {
                return Err(String::from(
                    "Error while loading memory! Check ELF file."
                ));
            }

            memory[mem_start..mem_end]
                .copy_from_slice(&self.raw_data[file_start..file_end]);

            if header.memory_size > header.file_size {
                let exp_end: usize = mem_start + header.memory_size as usize;
                for byte in memory[mem_end..exp_end].iter_mut() { *byte = 0 };
            }
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub fn print_string_table(&self) {
        for header in self.read_section_header().unwrap() {
            println!("{header:#?}");
        }
        
    }

    pub fn check_header_values(&self) -> Result<(), String> {
        match self.elf_header.check_values(
            ELF_VERSION_KEY, 
            ELF_TYPE_KEY, 
            MACHINE_KEY, 
            CLASS_KEY
        ) {
            Err(ValueError::Version) => {
                Err(String::from("The ELF file has an invalid version!"))
            },
            Err(ValueError::Type) => {
                Err(String::from("The ELF file is not an executable file!"))
            },
            Err(ValueError::Machine) => {
                Err(String::from("The ELF file is not ARM compatible!"))
            },
            Err(ValueError::Class) => {
                Err(String::from("The ELF file is not 32-bit compatible!"))
            },
            Ok(_) => Ok(())
        }
    }

    pub fn get_entry_point(&self) -> u32 {
        self.elf_header.entry_point
    }

    pub fn get_encoding(&self) -> Endian {
        self.encoding
    }

    fn read_internal_header<H: InternalHeader>(
        &self, num_headers: u16, table_offset: u32
    ) -> Result<Vec<H>, String>  {

        let  header_size: usize = H::SIZE;
        let num_headers: usize = num_headers as usize;
        let mut headers: Vec<H> = Vec::with_capacity(num_headers);

        let mut start: usize = table_offset as usize;
        let mut stop: usize = start + header_size;
        for _ in 0..num_headers {
            if self.raw_data.len() < stop {
                return Err(String::from(
                    "Error while reading internal headers! Check ELF file.",
                ));
            }

            let header_bytes: &[u8] = &self.raw_data[start..stop];
            headers.push(H::new(header_bytes, &self.encoding));

            start += header_size;
            stop += header_size;
        }
        Ok(headers)
    }

    fn read_program_headers(&self) -> Result<Vec<ProgramHeader>, String> {
        self.read_internal_header::<ProgramHeader>(
            self.elf_header.num_program_headers, 
            self.elf_header.program_table_offset
        )
    }

    fn read_section_header(&self) -> Result<Vec<SectionHeader>, String> {
        self.read_internal_header::<SectionHeader>(
            self.elf_header.num_section_headers, 
            self.elf_header.section_table_offset
        )
    }
}
