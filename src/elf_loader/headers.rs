use std::mem::size_of;
use crate::utils::{slice_to_u16, slice_to_u32, Endian};
use super::{ELF_ID, HEADER_VERSION_KEY};

pub enum ValueError { Version, Type, Machine, Class }

#[repr(C)]
#[derive(Debug)]
pub struct ELFHeader {
    pub elf_id: [u8; 16],
    pub elf_type: u16,
    pub machine: u16,
    pub elf_version: u32,
    pub entry_point: u32,
    pub program_table_offset: u32,
    pub section_table_offset: u32,
    pub flags: u32,
    pub elf_header_size: u16,
    pub program_header_size: u16,
    pub num_program_headers: u16,
    pub section_header_size: u16,
    pub num_section_headers: u16,
    pub string_table_idx: u16,
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
            _ => {
                return {
                    Err(String::from("The ELF file has an invalid encoding!"))
                }
            }
        };

        Ok((
            Self {
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
                string_table_idx: slice_to_u16(&bytes[50..52], &enc),
            },
            enc
        ))
    }

    pub fn check_values(
        &self,
        elf_version: u32,
        elf_type: u16,
        machine: u16,
        class: u8
    ) -> Result<(), ValueError> {
        
        if self.elf_version != elf_version {
            return Err(ValueError::Version);
        }
        if self.elf_type != elf_type {
            return Err(ValueError::Type);
        }
        if self.machine != machine {
            return Err(ValueError::Machine);
        }
        if self.elf_id[4] != class {
            return Err(ValueError::Class);
        }
        Ok(())
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct ProgramHeader {
    pub program_type: u32,
    pub offset: u32,
    pub virtual_address: u32,
    pub physical_address: u32,
    pub file_size: u32,
    pub memory_size: u32,
    pub flags: u32,
    pub align: u32,
}

impl ProgramHeader {
    pub const SIZE: usize = size_of::<ProgramHeader>();

    pub fn new(bytes: [u8; ProgramHeader::SIZE], encoding: &Endian) -> Self {
        Self {
            program_type: slice_to_u32(&bytes[0..4], encoding),
            offset: slice_to_u32(&bytes[4..8], encoding),
            virtual_address: slice_to_u32(&bytes[8..12], encoding),
            physical_address: slice_to_u32(&bytes[12..16], encoding),
            file_size: slice_to_u32(&bytes[16..20], encoding),
            memory_size: slice_to_u32(&bytes[20..24], encoding),
            flags: slice_to_u32(&bytes[24..28], encoding),
            align: slice_to_u32(&bytes[28..32], encoding),
        }
    }
}
