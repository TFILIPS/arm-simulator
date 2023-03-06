use std::{process::exit, mem};
use super::utils::{Endian, slice_to_u16, slice_to_u32};

// Defined values every ELF file header has to contain
const ELF_ID: [u8; 4] = [0x7f, 0x45, 0x4c, 0x46];
const HEADER_VERSION_KEY: u8 = 1;

#[repr(C)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct ELFHeader {
    elf_id: [u8; 16], 
    elf_type: u16,
    machine: u16,
    elf_version: u32,
    entry_point: u32,
    program_table_offset: u32,
    section_table_offset: u32,
    flags: u32,
    elf_header_size: u16,
    program_header_size: u16,
    num_program_headers: u16,
    section_header_size: u16,
    num_section_headers: u16,
    string_table_idx: u16
}

impl ELFHeader {
    pub const SIZE: usize = mem::size_of::<ELFHeader>();

    pub fn from_byte_array(array: [u8; ELFHeader::SIZE]) -> Self {
        let mut elf_id: [u8; 16] = [0; 16];
        elf_id.copy_from_slice(&array[0..16]);

        if elf_id[0..4] != ELF_ID || elf_id[6] != HEADER_VERSION_KEY {
            eprintln!("The ELF file is not valid!");
            exit(-1);
        }

        let encoding: Endian = match elf_id[5] {
            1 => Endian::Little,
            2 => Endian::Big,
            _ => {
                eprintln!("The ELF file has an invalid data encoding!");
                exit(-1);
            }
        };

        Self {
            elf_id,
            elf_type: slice_to_u16(&array[16..18], &encoding),
            machine: slice_to_u16(&array[18..20], &encoding),
            elf_version: slice_to_u32(&array[20..24], &encoding),
            entry_point: slice_to_u32(&array[24..28], &encoding),
            program_table_offset: slice_to_u32(&array[28..32], &encoding),
            section_table_offset: slice_to_u32(&array[32..36], &encoding),
            flags: slice_to_u32(&array[36..40], &encoding),
            elf_header_size: slice_to_u16(&array[40..42], &encoding),
            program_header_size: slice_to_u16(&array[42..44], &encoding),
            num_program_headers: slice_to_u16(&array[44..46], &encoding),
            section_header_size: slice_to_u16(&array[46..48], &encoding),
            num_section_headers: slice_to_u16(&array[48..50], &encoding),
            string_table_idx: slice_to_u16(&array[50..52], &encoding)
        }
    }
}