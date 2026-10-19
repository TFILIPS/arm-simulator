use crate::{
    SimulationEvent,
    simulated_cpu::{ARMv5CPU, SimulatedCPU, SimulationExceptionKind, names::ARMv5RegNames},
    utils::Memory
};

// Linux system calls (EABI): number in r7, arguments and result in r0-r2
pub fn handle_syscall(cpu: &mut ARMv5CPU) -> Result<SimulationEvent, SimulationExceptionKind> {
    match cpu.get_register(ARMv5RegNames::R7) {
        1 => {
            // Linux only keeps the lowest 8 bits of the exit code
            let exit_code: i32 = cpu.get_register(ARMv5RegNames::R0) & 0xFF;
            Ok(SimulationEvent::Exit{exit_code})
        },
        4 => {
            let stream: i32 = cpu.get_register(ARMv5RegNames::R0);
            if stream != 1 && stream != 2 {
                // -EBADF: only stdout and stderr are open
                cpu.set_register(ARMv5RegNames::R0, -9);
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

#[cfg(test)]
mod tests {
    use crate::{
        SimulationEvent,
        simulated_cpu::{ARMv5CPU, SimulatedCPU, SimulationExceptionKind, names::ARMv5RegNames},
        utils::Memory
    };
    use super::handle_syscall;

    #[test]
    fn write_returns_number_of_bytes() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_memory(0x2000, b"abc").unwrap();
        cpu.set_register(ARMv5RegNames::R7, 4);
        cpu.set_register(ARMv5RegNames::R0, 1);
        cpu.set_register(ARMv5RegNames::R1, 0x2000);
        cpu.set_register(ARMv5RegNames::R2, 3);

        match handle_syscall(&mut cpu) {
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

        assert!(matches!(handle_syscall(&mut cpu), Ok(SimulationEvent::None)));
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
            handle_syscall(&mut cpu), 
            Err(SimulationExceptionKind::DataAbort { memory_address: 0xFFFF_FF00, size: 0x200 })
        ));
    }

    #[test]
    fn unknown_syscall_is_error() {
        let mut cpu = ARMv5CPU::new();
        cpu.set_register(ARMv5RegNames::R7, 3);

        assert!(matches!(
            handle_syscall(&mut cpu), Err(SimulationExceptionKind::UnsupportedSyscall { number: 3 })
        ));
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

                match handle_syscall(&mut cpu) {
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
