use std::mem::size_of;
use super::utils::{slice_to_u16, slice_to_u32, Endian};

// Defined values every ELF file header has to contain
const ELF_ID: [u8; 4] = [0x7f, 0x45, 0x4c, 0x46];
const HEADER_VERSION_KEY: u8 = 0x1;
const ELF_VERSION_KEY: u32 = 0x1;

// Modifiable values: For this program we want an 32-bit ARM executable
const CLASS_KEY: u8 = 0x1; // 32-Bit
const MACHINE_KEY: u16 = 0x28; // ARM
const ELF_TYPE_KEY: u16 = 0x2; // executeable

#[repr(C)]
#[derive(Debug)]
pub struct ELFHeader {
    pub(super) elf_id: [u8; 16],
    pub(super) elf_type: u16,
    pub(super) machine: u16,
    pub(super) elf_version: u32,
    pub(super) entry_point: u32,
    pub(super) program_table_offset: u32,
    pub(super) section_table_offset: u32,
    pub(super) flags: u32,
    pub(super) elf_header_size: u16,
    pub(super) program_header_size: u16,
    pub(super) num_program_headers: u16,
    pub(super) section_header_size: u16,
    pub(super) num_section_headers: u16,
    pub(super) string_table_idx: u16
}

impl ELFHeader {
    pub const SIZE: usize = size_of::<ELFHeader>();

    pub fn new(bytes: [u8; ELFHeader::SIZE]) -> Result<(Self, Endian), String> {
        let elf_id: [u8; 16] = bytes[0..16].try_into().unwrap();

        if elf_id[0..4] != ELF_ID || elf_id[6] != HEADER_VERSION_KEY {
            return Err(String::from("The ELF file is not valid!"));
        }

        let enc: Endian = match elf_id[5] {
            1 => Endian::Little,
            2 => Endian::Big,
            _ => return {
                Err(String::from("The ELF file has an invalid encoding!"))
            }
        };

        Ok((Self {
            elf_id,
            elf_type: slice_to_u16(&bytes[16..18], &enc),
            machine: slice_to_u16(&bytes[18..20], &enc),
            elf_version: slice_to_u32(&bytes[20..24], &enc),
            entry_point: slice_to_u32(&bytes[24..28], &enc),
            program_table_offset: slice_to_u32(&bytes[28..32], &enc),
            section_table_offset: slice_to_u32(&bytes[32..36], &enc),
            flags: slice_to_u32(&bytes[36..40], &enc),
            elf_header_size: slice_to_u16(&bytes[40..42], &enc),
            program_header_size: slice_to_u16(&bytes[42..44], &enc),
            num_program_headers: slice_to_u16(&bytes[44..46], &enc),
            section_header_size: slice_to_u16(&bytes[46..48], &enc),
            num_section_headers: slice_to_u16(&bytes[48..50], &enc),
            string_table_idx: slice_to_u16(&bytes[50..52], &enc)
        }, enc))
    }

    pub fn check_values(&self) -> Result<(), String> {
        if self.elf_version != ELF_VERSION_KEY {
            return Err(String::from("The ELF file has an invalid version!"));
        }
        if self.elf_type != ELF_TYPE_KEY {
            return Err(String::from("The ELF file is not an executable file!"));
        }
        if self.machine != MACHINE_KEY {
            return Err(String::from("The ELF file is not ARM compatible!"));
        }
        if self.elf_id[4] != CLASS_KEY {
            return Err(String::from("The ELF file is not 32-bit compatible!"));
        }
        Ok(())
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct ProgramHeader {
    pub(super) program_type: u32,
    pub(super) offset: u32,
    pub(super) virtual_address: u32,
    pub(super) physical_address: u32,
    pub(super) file_size: u32,
    pub(super) memory_size: u32,
    pub(super) flags: u32,
    pub(super) align: u32
}

impl ProgramHeader {
    pub const SIZE: usize = size_of::<ProgramHeader>();

    pub fn new(array: [u8; ProgramHeader::SIZE], encoding: &Endian) -> Self {
        Self {
            program_type: slice_to_u32(&array[0..4], encoding),
            offset: slice_to_u32(&array[4..8], encoding),
            virtual_address: slice_to_u32(&array[8..12], encoding),
            physical_address: slice_to_u32(&array[12..16], encoding),
            file_size: slice_to_u32(&array[16..20], encoding),
            memory_size: slice_to_u32(&array[20..24], encoding),
            flags: slice_to_u32(&array[24..28], encoding),
            align: slice_to_u32(&array[28..32], encoding)
        }
    }
}
