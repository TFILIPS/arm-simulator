use std::collections::VecDeque;

use crate::{
    SimulationEvent,
    simulated_cpu::{ARMv5CPU, SimulatedCPU, SimulationExceptionKind, names::ARMv5RegNames},
    utils::Memory
};

// -EBADF: the file descriptor is not open
const EBADF: i32 = -9;

// Input of the program (stdin). Like a terminal it hands out whole lines:
// a read returns at most up to the end of the next line.
#[derive(Default)]
pub struct Input {
    bytes: VecDeque<u8>,
    // End of input (EOF): reads return 0 once the remaining bytes are read
    closed: bool
}

impl Input {
    pub fn push(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes);
    }

    pub fn close(&mut self) {
        self.closed = true;
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    pub fn clear(&mut self) {
        self.bytes.clear();
        self.closed = false;
    }

    // Number of bytes the next read of at most `max` bytes returns
    fn next_read_length(&self, max: usize) -> usize {
        let line_end: usize = self.bytes.iter().position(|&b| b == b'\n')
            .map_or(self.bytes.len(), |i| i + 1);
        line_end.min(max)
    }
}

// Linux system calls (EABI): number in r7, arguments and result in r0-r2
pub fn handle_syscall(cpu: &mut ARMv5CPU, input: &mut Input) -> Result<SimulationEvent, SimulationExceptionKind> {
    match cpu.get_register(ARMv5RegNames::R7) {
        1 => {
            // Linux only keeps the lowest 8 bits of the exit code
            let exit_code: i32 = cpu.get_register(ARMv5RegNames::R0) & 0xFF;
            Ok(SimulationEvent::Exit{exit_code})
        },
        3 => read(cpu, input),
        4 => {
            let stream: i32 = cpu.get_register(ARMv5RegNames::R0);
            if stream != 1 && stream != 2 {
                // Only stdout and stderr are open for writing
                cpu.set_register(ARMv5RegNames::R0, EBADF);
                return Ok(SimulationEvent::None);
            }

            let address: u32 = cpu.get_register(ARMv5RegNames::R1) as u32;
            let length: u32 = cpu.get_register(ARMv5RegNames::R2) as u32;

            let message: Vec<u8> = cpu.get_memory(address, length)?.to_vec();
            // Return the number of written bytes
            cpu.set_register(ARMv5RegNames::R0, length as i32);
            Ok(SimulationEvent::ConsoleOutput{stream, message})
        },
        number => Err(SimulationExceptionKind::UnsupportedSyscall { number })
    }
}

// read(fd = r0, buffer = r1, count = r2): returns the number of read bytes
// in r0, 0 at the end of input. Without input the program waits.
fn read(cpu: &mut ARMv5CPU, input: &mut Input) -> Result<SimulationEvent, SimulationExceptionKind> {
    if cpu.get_register(ARMv5RegNames::R0) != 0 {
        // Only stdin is open for reading
        cpu.set_register(ARMv5RegNames::R0, EBADF);
        return Ok(SimulationEvent::None);
    }
    let address: u32 = cpu.get_register(ARMv5RegNames::R1) as u32;
    let count: usize = cpu.get_register(ARMv5RegNames::R2) as u32 as usize;

    let length: usize = input.next_read_length(count);
    if length == 0 && count > 0 && !input.closed {
        return Ok(SimulationEvent::WaitingForInput);
    }
    let bytes: Vec<u8> = input.bytes.iter().take(length).copied().collect();
    // Bytes are only taken from the input when they fit into memory
    cpu.set_memory(address, &bytes)?;
    input.bytes.drain(..length);
    cpu.set_register(ARMv5RegNames::R0, length as i32);
    Ok(SimulationEvent::None)
}

#[cfg(test)]
mod tests {
    use crate::{
        SimulationEvent,
        simulated_cpu::{ARMv5CPU, SimulatedCPU, SimulationExceptionKind, names::ARMv5RegNames},
        utils::Memory
    };
    use super::{handle_syscall, Input};

    fn syscall(cpu: &mut ARMv5CPU) -> Result<SimulationEvent, SimulationExceptionKind> {
        handle_syscall(cpu, &mut Input::default())
    }

    #[test]
    fn write_returns_number_of_bytes() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_memory(0x2000, b"abc").unwrap();
        cpu.set_register(ARMv5RegNames::R7, 4);
        cpu.set_register(ARMv5RegNames::R0, 1);
        cpu.set_register(ARMv5RegNames::R1, 0x2000);
        cpu.set_register(ARMv5RegNames::R2, 3);

        match syscall(&mut cpu) {
            Ok(SimulationEvent::ConsoleOutput { stream: 1, message }) => {
                assert_eq!(b"abc".to_vec(), message)
            },
            _ => panic!("Expected output on stdout!")
        }
        assert_eq!(3, cpu.get_register(ARMv5RegNames::R0));
    }

    #[test]
    fn write_to_closed_file_descriptor_is_ebadf() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_register(ARMv5RegNames::R7, 4);
        cpu.set_register(ARMv5RegNames::R0, 5);
        cpu.set_register(ARMv5RegNames::R2, 3);

        assert!(matches!(syscall(&mut cpu), Ok(SimulationEvent::None)));
        assert_eq!(-9, cpu.get_register(ARMv5RegNames::R0));
    }

    #[test]
    fn write_outside_memory_is_data_abort() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_register(ARMv5RegNames::R7, 4);
        cpu.set_register(ARMv5RegNames::R0, 1);
        cpu.set_register(ARMv5RegNames::R1, 0xFFFF_FF00u32 as i32);
        cpu.set_register(ARMv5RegNames::R2, 0x200);

        assert!(matches!(
            syscall(&mut cpu), 
            Err(SimulationExceptionKind::DataAbort { memory_address: 0xFFFF_FF00, size: 0x200 })
        ));
    }

    #[test]
    fn unknown_syscall_is_error() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_register(ARMv5RegNames::R7, 5);

        assert!(matches!(
            syscall(&mut cpu), Err(SimulationExceptionKind::UnsupportedSyscall { number: 5 })
        ));
    }

    // read(0, 0x2000, count)
    fn read_call(count: i32) -> ARMv5CPU {
        let mut cpu = ARMv5CPU::new();
        cpu.set_register(ARMv5RegNames::R7, 3);
        cpu.set_register(ARMv5RegNames::R0, 0);
        cpu.set_register(ARMv5RegNames::R1, 0x2000);
        cpu.set_register(ARMv5RegNames::R2, count);
        cpu
    }

    fn input(text: &[u8]) -> Input {
        let mut input = Input::default();
        input.push(text);
        input
    }

    #[test]
    fn read_returns_one_line() {
        let mut input = input(b"abc\nxy\n");
        let mut cpu = read_call(100);

        assert!(matches!(handle_syscall(&mut cpu, &mut input), Ok(SimulationEvent::None)));
        assert_eq!(4, cpu.get_register(ARMv5RegNames::R0));
        assert_eq!(b"abc\n", cpu.get_memory(0x2000, 4).unwrap());

        assert!(matches!(handle_syscall(cpu_again(&mut cpu), &mut input), Ok(SimulationEvent::None)));
        assert_eq!(3, cpu.get_register(ARMv5RegNames::R0));
        assert_eq!(b"xy\n", cpu.get_memory(0x2000, 3).unwrap());
    }

    // The same read call again, after r0 was overwritten by the result
    fn cpu_again(cpu: &mut ARMv5CPU) -> &mut ARMv5CPU {
        cpu.set_register(ARMv5RegNames::R0, 0);
        cpu
    }

    #[test]
    fn read_with_small_count_leaves_rest_of_line() {
        let mut input = input(b"abcdef\n");
        let mut cpu = read_call(4);

        handle_syscall(&mut cpu, &mut input).unwrap();
        assert_eq!(4, cpu.get_register(ARMv5RegNames::R0));
        assert_eq!(b"abcd", cpu.get_memory(0x2000, 4).unwrap());

        handle_syscall(cpu_again(&mut cpu), &mut input).unwrap();
        assert_eq!(3, cpu.get_register(ARMv5RegNames::R0));
        assert_eq!(b"ef\n", cpu.get_memory(0x2000, 3).unwrap());
    }

    #[test]
    fn read_without_input_waits() {
        let mut cpu = read_call(10);
        assert!(matches!(syscall(&mut cpu), Ok(SimulationEvent::WaitingForInput)));
        // r0 is unchanged, the call is repeated once there is input
        assert_eq!(0, cpu.get_register(ARMv5RegNames::R0));
    }

    #[test]
    fn read_at_end_of_input_returns_zero() {
        let mut input = input(b"last");
        input.close();
        let mut cpu = read_call(10);

        handle_syscall(&mut cpu, &mut input).unwrap();
        assert_eq!(4, cpu.get_register(ARMv5RegNames::R0));
        handle_syscall(cpu_again(&mut cpu), &mut input).unwrap();
        assert_eq!(0, cpu.get_register(ARMv5RegNames::R0));
    }

    #[test]
    fn read_of_zero_bytes_returns_zero_without_waiting() {
        let mut cpu = read_call(0);
        assert!(matches!(syscall(&mut cpu), Ok(SimulationEvent::None)));
        assert_eq!(0, cpu.get_register(ARMv5RegNames::R0));
    }

    #[test]
    fn read_from_other_file_descriptor_is_ebadf() {
        let mut cpu = read_call(10);
        cpu.set_register(ARMv5RegNames::R0, 1);
        assert!(matches!(syscall(&mut cpu), Ok(SimulationEvent::None)));
        assert_eq!(-9, cpu.get_register(ARMv5RegNames::R0));
    }

    #[test]
    fn read_outside_memory_is_data_abort_and_keeps_input() {
        let mut input = input(b"abc\n");
        let mut cpu = read_call(10);
        cpu.set_register(ARMv5RegNames::R1, 0xFFFF_FFFEu32 as i32);

        assert!(matches!(
            handle_syscall(&mut cpu, &mut input),
            Err(SimulationExceptionKind::DataAbort { memory_address: 0xFFFF_FFFE, size: 4 })
        ));
        cpu.set_register(ARMv5RegNames::R1, 0x2000);
        handle_syscall(&mut cpu, &mut input).unwrap();
        assert_eq!(4, cpu.get_register(ARMv5RegNames::R0));
    }

    macro_rules! exit_code_tests {
        ($($test_name:ident: $test_values:expr),*) 
        => {$(
            #[test]
            fn $test_name() {
                let (r0, exp_exit_code): (i32, i32) = $test_values;

                let mut cpu = ARMv5CPU::new();
                cpu.set_register(ARMv5RegNames::R7, 1);
                cpu.set_register(ARMv5RegNames::R0, r0);

                match syscall(&mut cpu) {
                    Ok(SimulationEvent::Exit { exit_code }) => {
                        assert_eq!(exp_exit_code, exit_code)
                    },
                    _ => panic!("Expected exit!")
                }
            }
        )*}
    }

    exit_code_tests! {
        exit_code_in_range: (42, 42),
        exit_code_keeps_lowest_8_bits: (300, 44),
        exit_code_negative: (-1, 255)
    }
}
