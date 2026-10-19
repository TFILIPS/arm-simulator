use std::{fs, error::Error, fmt::Display};

use crate::utils::{Endian, Memory};
use headers::{
    ELFHeader, ELFTableEntry, ProgramHeader, 
    SectionHeader, SymbolTableEntry, HeaderError
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

// Error while loading or reading an ELF file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ElfError(&'static str);
impl From<&'static str> for ElfError {
    fn from(message: &'static str) -> Self {
        ElfError(message)
    }
}
impl Error for ElfError {}
impl Display for ElfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
            return Err("ELF file is too short to be valid!");
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

            let bytes: &[u8] = self.get_bytes(header.offset, header.file_size)
                .ok_or("Error while loading memory! Check ELF file.")?;

            target.set_memory(header.virtual_address, bytes).or(
                Err("ELF file expects larger memory!")
            )?;

            // The rest of the segment is not stored in the file (e.g. .bss)
            if header.memory_size > header.file_size {
                let padding: u32 = header.memory_size - header.file_size;
                let address: u32 = header.virtual_address
                    .checked_add(header.file_size)
                    .ok_or("ELF file expects larger memory!")?;

                // Check the range first, so a corrupted size can not
                // cause a huge allocation
                target.get_memory(address, padding).or(
                    Err("ELF file expects larger memory!")
                )?;
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

            if header.entry_size as usize != SymbolTableEntry::SIZE {
                return Err("Invalid symbol table entry size! Check ELF file.");
            }

            // This use of the link data field is Unix - System V specific. 
            let str_header: &SectionHeader = headers.get(header.link as usize)
                .ok_or("Symbol string table not found! Check ELF file.")?;
            let str_table: &[u8] = self.load_string_table(str_header)?;

            let entries: Vec<SymbolTableEntry> = self.read_table_entries(
                (header.size / header.entry_size) as u16, header.offset)?;

            // Skipping the reserved undefined symbol entry.
            for entry in entries.iter().skip(1)
                .filter(|e| LABEL_TYPES.contains(&(e.info & 0xf))) {

                if let Some(name) = get_string(str_table, entry.name) {
                    if !name.starts_with("$") {
                        labels.push((entry.value, name));
                    }
                }
            }
        }
        Ok(labels)
    }

    // ToDo: Make a struct for relevant sections
    pub fn get_text_section_range(&self) -> Result<(u32, u32), &'static str> {
        let headers: Vec<SectionHeader> = self.read_section_headers()?;

        let str_header: &SectionHeader = headers
            .get(self.elf_header.section_str_table_idx as usize)
            .ok_or("Section name table not found! Check ELF file.")?;
        let str_table: &[u8] = self.load_string_table(str_header)?;

        // ELF files can have several .text sections, but the programs for
        // this simulator have one. Only the first one is used.
        for header in &headers {
            if let Some(name) = get_string(str_table, header.section_name) {
                if name == ".text" {
                    // The section has to be stored in the file, which also
                    // limits its size
                    self.get_bytes(header.offset, header.size)
                        .ok_or("Error while reading .text section! Check ELF file.")?;
                    let end: u32 = header.address.checked_add(header.size)
                        .ok_or("Error while reading .text section! Check ELF file.")?;
                    return Ok((header.address, end));
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
            Err(HeaderError::Version) => {
                Err("The ELF file has an invalid version!")
            },
            Err(HeaderError::Type) => {
                Err("The ELF file is not an executable file!")
            },
            Err(HeaderError::Machine) => {
                Err("The ELF file is not ARM compatible!")
            },
            Err(HeaderError::Class) => {
                Err("The ELF file is not 32-bit compatible!")
            },
            Err(HeaderError::OsAbi) => {
                Err("The target of the ELF file is not Unix-SystemV!")
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

        let table_size: u32 = H::SIZE as u32 * num_entries as u32;
        let table: &[u8] = self.get_bytes(table_offset, table_size)
            .ok_or("Error while reading internal headers! Check ELF file.")?;

        Ok(table.chunks_exact(H::SIZE)
            .map(|bytes| H::new(bytes, &self.encoding))
            .collect())
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
    ) -> Result<&[u8], &'static str> {
        self.get_bytes(table_header.offset, table_header.size)
            .ok_or("Error while reading string_table! Check ELF file.")
    }

    // Offsets and sizes come from the file itself, so every access is checked
    fn get_bytes(&self, offset: u32, size: u32) -> Option<&[u8]> {
        let end: u32 = offset.checked_add(size)?;
        self.raw_data.get(offset as usize..end as usize)
    }
}

// Reads the null terminated string at offset from a string table
fn get_string(table: &[u8], offset: u32) -> Option<String> {
    let tail: &[u8] = table.get(offset as usize..)?;
    let length: usize = tail.iter().position(|&byte| byte == 0)?;
    Some(String::from_utf8_lossy(&tail[..length]).into_owned())
}

#[cfg(test)]
mod tests {
    use crate::simulated_cpu::ARMv5CPU;
    use super::ELFFile;

    // 32-bit little endian ARM executable
    const HALLO: &[u8] = include_bytes!("../../sample_programs/hallo");

    // Section indices in hallo
    const TEXT: usize = 1;
    const SYMTAB: usize = 4;
    const SHSTRTAB: usize = 6;

    // Field offsets in the ELF header, section headers and program headers
    const E_PHOFF: usize = 28;
    const E_SHSTRNDX: usize = 50;
    const SH_NAME: usize = 0;
    const SH_OFFSET: usize = 16;
    const SH_SIZE: usize = 20;
    const SH_LINK: usize = 24;
    const SH_ENTSIZE: usize = 36;
    const P_MEMSZ: usize = 20;

    fn read_u32(offset: usize) -> u32 {
        u32::from_le_bytes(HALLO[offset..offset + 4].try_into().unwrap())
    }

    fn section_header(index: usize) -> usize {
        read_u32(32) as usize + index * 40
    }

    fn first_load_segment_header() -> usize {
        let num_headers: usize = u16::from_le_bytes([HALLO[44], HALLO[45]]) as usize;
        (0..num_headers)
            .map(|i| read_u32(E_PHOFF) as usize + i * 32)
            .find(|&header| read_u32(header) == 1)
            .unwrap()
    }

    fn corrupted(offset: usize, value: &[u8]) -> ELFFile {
        let mut bytes: Vec<u8> = HALLO.to_vec();
        bytes[offset..offset + value.len()].copy_from_slice(value);
        ELFFile::load_raw(bytes).unwrap()
    }

    #[test]
    fn valid_elf_file() {
        let elf: ELFFile = ELFFile::load_raw(HALLO.to_vec()).unwrap();

        assert!(elf.check_header_values().is_ok());
        assert!(elf.load_into_memory(&mut ARMv5CPU::new()).is_ok());
        assert_eq!(Ok((0x10074, 0x10098)), elf.get_text_section_range());
        assert!(elf.get_labels().unwrap().iter().any(|(_, name)| name == "_start"));
    }

    #[test]
    fn invalid_section_name_table_index() {
        let elf: ELFFile = corrupted(E_SHSTRNDX, &0xFFFFu16.to_le_bytes());
        assert!(elf.get_text_section_range().is_err());
    }

    #[test]
    fn section_name_outside_string_table() {
        let elf: ELFFile = corrupted(
            section_header(TEXT) + SH_NAME, &0x00FF_FFFFu32.to_le_bytes()
        );
        assert_eq!(Err("No .text section found!"), elf.get_text_section_range());
    }

    #[test]
    fn section_name_table_offset_overflow() {
        let elf: ELFFile = corrupted(
            section_header(SHSTRTAB) + SH_OFFSET, &0xFFFF_FFF0u32.to_le_bytes()
        );
        assert!(elf.get_text_section_range().is_err());
    }

    #[test]
    fn text_section_larger_than_file() {
        let elf: ELFFile = corrupted(
            section_header(TEXT) + SH_SIZE, &0xFFFF_FFF0u32.to_le_bytes()
        );
        assert!(elf.get_text_section_range().is_err());
    }

    #[test]
    fn symbol_table_entry_size_zero() {
        let elf: ELFFile = corrupted(
            section_header(SYMTAB) + SH_ENTSIZE, &0u32.to_le_bytes()
        );
        assert!(elf.get_labels().is_err());
    }

    #[test]
    fn empty_symbol_table() {
        let elf: ELFFile = corrupted(
            section_header(SYMTAB) + SH_SIZE, &0u32.to_le_bytes()
        );
        assert_eq!(Ok(Vec::new()), elf.get_labels());
    }

    #[test]
    fn invalid_symbol_string_table_index() {
        let elf: ELFFile = corrupted(
            section_header(SYMTAB) + SH_LINK, &0xFFFFu32.to_le_bytes()
        );
        assert!(elf.get_labels().is_err());
    }

    #[test]
    fn huge_bss_segment() {
        let elf: ELFFile = corrupted(
            first_load_segment_header() + P_MEMSZ, &0xFFFF_FFF0u32.to_le_bytes()
        );
        assert!(elf.load_into_memory(&mut ARMv5CPU::new()).is_err());
    }

    #[test]
    fn program_header_offset_overflow() {
        let elf: ELFFile = corrupted(E_PHOFF, &0xFFFF_FFF0u32.to_le_bytes());
        assert!(elf.load_into_memory(&mut ARMv5CPU::new()).is_err());
    }
}
