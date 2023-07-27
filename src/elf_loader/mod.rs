use std::fs;

use headers::{
    ELFHeader, ELFTableEntry, ProgramHeader, 
    SectionHeader, SymbolTableEntry, ValueError
};
use crate::{utils::Endian, simulated_cpu::SimulatedCPU};

mod headers;

// Defined values every ELF file header has to contain
const ELF_ID: [u8; 4] = [0x7f, 0x45, 0x4c, 0x46];
const HEADER_VERSION_KEY: u8 = 0x1;
const ELF_VERSION_KEY: u32 = 0x1;
const SECTION_SYMTAB_KEY: u32 = 0x2;
// 0x0: STT_NOTYPE, 0x1: STT_OBJECT, 0x2: STT_FUNC
const LABEL_TYPES: [u8; 3] = [0x0, 0x1, 0x2];

// Has to be UNIX - System V for OS specific operation in get_labels
const OSABI_KEY: u8 = 0x0; // UNIX - System V

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
            Err(_) => return Err(String::from("Failed to read the ELF file!"))
        };

        ELFFile::load_raw(raw_data)
    }

    pub fn load_raw(raw_data: Vec<u8>) -> Result<Self, String> {
        if raw_data.len() < ELFHeader::SIZE {
            return Err(String::from("ELF file is to short to be valid!"));
        }

        let (elf_header, encoding) =
            ELFHeader::new(raw_data[0..ELFHeader::SIZE].try_into().unwrap())?;

        Ok(Self{ elf_header, raw_data, encoding })
    }

    pub fn load_cpu_memory<S>(
        &self, target: &mut dyn SimulatedCPU<S>
    ) -> Result<(), String> {
        let headers: Vec<ProgramHeader> = self.read_program_headers()?;
        for header in headers {
            if header.program_type != 1 { continue; }
            let file_start: usize = header.offset as usize;
            let file_end: usize = file_start + header.file_size as usize;
            let mut address: u32 = header.virtual_address;

            if self.raw_data.len() < file_end {
                return Err(String::from(
                    "Error while loading memory! Check ELF file."
                ));
            }

            if let Err(_) = target
                .set_memory(address, &self.raw_data[file_start..file_end]) {
                return Err(String::from("ELF file expects larger memory!"));
            }

            if header.memory_size > header.file_size {
                let padding: u32 = header.memory_size - header.file_size;
                address += header.file_size;

                if let Err(_) = target
                    .set_memory(address, &vec![0; padding as usize]) {
                    return Err(String::from("ELF file expects larger memory!"));
                }
            }
        }
        Ok(())
    }

    pub fn get_labels(&self) -> Result<Vec<(u32, String)>, String> {
        let mut labels: Vec<(u32, String)> = Vec::new();
        let headers: Vec<SectionHeader> = self.read_section_headers()?;

        for header in 
            headers.iter().filter(|h| h.section_type == SECTION_SYMTAB_KEY) {

            // This use of the link data field is Unix - System V specific. 
            let str_header: &SectionHeader = &headers[header.link as usize];
            let str_table: String = self.load_string_table(str_header)?;

            let entries: Vec<SymbolTableEntry> = self.read_table_entries(
                (header.size / header.entrie_size) as u16, header.offset)?;

            for entry in entries[1..].iter()
                .filter(|e| LABEL_TYPES.contains(&(e.info & 0xf))) {

                let str_start: usize = entry.name as usize;
                if let Some((s, _)) = str_table[str_start..].split_once('\0') {
                    if !s.starts_with("$") {
                        labels.push((entry.value, String::from(s)));
                    }
                }
            }
        }
        Ok(labels)
    }

    // todo make struct for relevant sections
    pub fn get_text_section_range(&self) -> Result<(u32, u32), String> {
        let headers: Vec<SectionHeader> = self.read_section_headers()?;

        let str_header: &SectionHeader = 
            &headers[self.elf_header.section_str_table_idx as usize];
        let str_table: String = self.load_string_table(str_header)?;

        for header in headers {
            let str_start: usize = header.section_name as usize;
            if let Some((s, _)) = str_table[str_start..].split_once('\0') {
                if s == ".text" {
                    return Ok((header.address, header.address + header.size));
                }
            }
        }
        Err(String::from("No .text section found!"))
    }

    pub fn check_header_values(&self) -> Result<(), String> {
        match self.elf_header.check_values(
            ELF_VERSION_KEY, ELF_TYPE_KEY, MACHINE_KEY, 
            CLASS_KEY, OSABI_KEY
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
            Err(ValueError::OsAbi) => {
                Err(String::from("The ELF files traget is not Unix-SystemV!"))
            }
            Ok(_) => Ok(())
        }
    }

    pub fn get_entry_point(&self) -> u32 {
        self.elf_header.entry_point
    }

    pub fn get_encoding(&self) -> Endian {
        self.encoding
    }

    fn read_table_entries<H: ELFTableEntry>(
        &self, num_entries: u16, table_offset: u32
    ) -> Result<Vec<H>, String>  {

        let header_size: usize = H::SIZE;
        let num_headers: usize = num_entries as usize;
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
        self.read_table_entries::<ProgramHeader>(
            self.elf_header.num_program_headers, 
            self.elf_header.program_table_offset
        )
    }

    fn read_section_headers(&self) -> Result<Vec<SectionHeader>, String> {
        self.read_table_entries::<SectionHeader>(
            self.elf_header.num_section_headers, 
            self.elf_header.section_table_offset
        )
    }

    fn load_string_table(&self, table_header: &SectionHeader) 
    -> Result<String, String> {
        let start = table_header.offset as usize;
        let stop = start + table_header.size as usize;
        if self.raw_data.len() < stop {
            return Err(String::from(
                "Error while reading string_table! Check ELF file.",
            ));
        }
        Ok(String::from_utf8_lossy(&self.raw_data[start..stop]).to_string())
    }
}
