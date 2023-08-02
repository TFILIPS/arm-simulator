use std::fs;

use crate::utils::{Endian, Memory};
use headers::{
    ELFHeader, ELFTableEntry, ProgramHeader, 
    SectionHeader, SymbolTableEntry, ValueError
};

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
const ELF_TYPE_KEY: u16 = 0x2; // executable

pub struct ELFFile {
    elf_header: ELFHeader,
    raw_data: Vec<u8>,
    encoding: Endian
}

impl ELFFile {
    pub fn load(file_path: &str) -> Result<Self, &'static str> {
        let raw_data = fs::read(file_path).or(
            Err("Failed to read the ELF file!")
        )?;

        ELFFile::load_raw(raw_data)
    }

    pub fn load_raw(raw_data: Vec<u8>) -> Result<Self, &'static str> {
        if raw_data.len() < ELFHeader::SIZE {
            return Err("ELF file is to short to be valid!");
        }

        let (elf_header, encoding) =
            ELFHeader::new(&raw_data[0..ELFHeader::SIZE])?;

        Ok(Self{ elf_header, raw_data, encoding })
    }

    pub fn load_into_memory(
        &self, target: &mut dyn Memory
    ) -> Result<(), &'static str> {
        
        let headers: Vec<ProgramHeader> = self.read_program_headers()?;

        for header in headers {
            if header.program_type != 1 { continue; }

            let file_start: usize = header.offset as usize;
            let file_end: usize = file_start + header.file_size as usize;
            let mut address: u32 = header.virtual_address;

            if self.raw_data.len() < file_end {
                return Err("Error while loading memory! Check ELF file.");
            }

            target.set_memory(address, &self.raw_data[file_start..file_end]).or(
                Err("ELF file expects larger memory!")
            )?;

            if header.memory_size > header.file_size {
                let padding: u32 = header.memory_size - header.file_size;
                address += header.file_size;

                target.set_memory(address, &vec![0; padding as usize]).or(
                    Err("ELF file expects larger memory!")
                )?;
            }
        }
        Ok(())
    }

    pub fn get_labels(&self) -> Result<Vec<(u32, String)>, &'static str> {
        let mut labels: Vec<(u32, String)> = Vec::new();
        let headers: Vec<SectionHeader> = self.read_section_headers()?;

        for header in headers
            .iter().filter(|h| h.section_type == SECTION_SYMTAB_KEY) {

            // This use of the link data field is Unix - System V specific. 
            let str_header: &SectionHeader = &headers[header.link as usize];
            let str_table: String = self.load_string_table(str_header)?;

            let entries: Vec<SymbolTableEntry> = self.read_table_entries(
                (header.size / header.entry_size) as u16, header.offset)?;

            // Starting at 1 to skip the reserved undefined symbol entry.
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

    // ToDo: Make a struct for relevant sections
    pub fn get_text_section_range(&self) -> Result<(u32, u32), &'static str> {
        let headers: Vec<SectionHeader> = self.read_section_headers()?;

        let str_header: &SectionHeader = 
            &headers[self.elf_header.section_str_table_idx as usize];
        let str_table: String = self.load_string_table(str_header)?;

        for header in headers {
            let str_start: usize = header.section_name as usize;
            if let Some((s, _)) = str_table[str_start..].split_once('\0') {
                if s == ".text" {
                    return Ok((header.address, header.address + header.size));
                    // ALEX: can we have multilpe .text sections?
                    // The specification says yes. But I don't think this will
                    // happen in our use cases. Definitely a thing to keep in
                    // mind though.
                }
            }
        }
        Err("No .text section found!")
    }

    pub fn check_header_values(&self) -> Result<(), &'static str> {
        match self.elf_header.check_values(
            ELF_VERSION_KEY, ELF_TYPE_KEY, MACHINE_KEY, 
            CLASS_KEY, OSABI_KEY
        ) {
            Err(ValueError::Version) => {
                Err("The ELF file has an invalid version!")
            },
            Err(ValueError::Type) => {
                Err("The ELF file is not an executable file!")
            },
            Err(ValueError::Machine) => {
                Err("The ELF file is not ARM compatible!")
            },
            Err(ValueError::Class) => {
                Err("The ELF file is not 32-bit compatible!")
            },
            Err(ValueError::OsAbi) => {
                Err("The ELF files traget is not Unix-SystemV!")
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
    ) -> Result<Vec<H>, &'static str>  {

        let header_size: usize = H::SIZE;
        let num_headers: usize = num_entries as usize;
        let mut headers: Vec<H> = Vec::with_capacity(num_headers);

        let mut start: usize = table_offset as usize;
        let mut stop: usize = start + header_size;
        for _ in 0..num_headers {
            if self.raw_data.len() < stop {
                return Err(
                    "Error while reading internal headers! Check ELF file."
                );
            }

            let header_bytes: &[u8] = &self.raw_data[start..stop];
            headers.push(H::new(header_bytes, &self.encoding));

            start += header_size;
            stop += header_size;
        }
        Ok(headers)
    }

    fn read_program_headers(&self) -> Result<Vec<ProgramHeader>, &'static str> {
        self.read_table_entries::<ProgramHeader>(
            self.elf_header.num_program_headers, 
            self.elf_header.program_table_offset
        )
    }

    fn read_section_headers(&self) -> Result<Vec<SectionHeader>, &'static str> {
        self.read_table_entries::<SectionHeader>(
            self.elf_header.num_section_headers, 
            self.elf_header.section_table_offset
        )
    }

    fn load_string_table(
        &self, table_header: &SectionHeader
    ) -> Result<String, &'static str> {
        let start = table_header.offset as usize;
        let stop = start + table_header.size as usize;
        if self.raw_data.len() < stop {
            return Err("Error while reading string_table! Check ELF file.");
        }
        Ok(String::from_utf8_lossy(&self.raw_data[start..stop]).to_string())
    }
}
