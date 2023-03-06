#[derive(Debug)]
pub enum Endian {
    Little,
    Big,
}

pub(super) fn slice_to_u16(slice: &[u8], endianess: &Endian) -> u16 {
    let buffer: [u8; 2] = slice
        .try_into()
        .expect("No conversion to u16 possible! Given slice has length != 2.");

    match endianess {
        Endian::Little => u16::from_le_bytes(buffer),
        Endian::Big => u16::from_be_bytes(buffer),
    }
}

pub(super) fn slice_to_u32(slice: &[u8], endianess: &Endian) -> u32 {
    let buffer: [u8; 4] = slice
        .try_into()
        .expect("No conversion to u32 possible! Given slice has length != 4.");

    match endianess {
        Endian::Little => u32::from_le_bytes(buffer),
        Endian::Big => u32::from_be_bytes(buffer),
    }
}
