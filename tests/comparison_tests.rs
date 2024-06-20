extern crate arm_simulator;

use std::{fs::{File, read_to_string}, io::BufReader};
use serde::{Deserialize, Serialize};
use serde_json;

use arm_simulator::{ARMSimulator, SimulationEvent, ARMv5RegNames};

#[derive(Serialize, Deserialize, Debug)]
struct State {
    registers: Vec<i32>,
    flags: Vec<bool>
}

#[derive(Serialize, Deserialize, Debug)]
struct Behavior(Vec<State>);


//ToDo: Implement Error for custom error types and get rid of unwraps
macro_rules! behavior_tests {
    ($($test_name:ident: (
        executable: $executable:expr, behavior: $behavior:expr
    )),*)  => ($(
        #[test]
        fn $test_name() {
            let behavior_file = File::open($behavior)
                .expect("Behavior File not found!");
            let reader = BufReader::new(behavior_file);
            let expected_behavior: Behavior = serde_json::from_reader(reader)
                .expect("Could not parse behavior file!");
            // The struct is only needed for parsing the JSON
            let expected_behavior = expected_behavior.0;

            let mut sim = ARMSimulator::new();
            sim.load_elf_file($executable).unwrap();
            // Synchronize the stack pointer of simulator and behavior
            sim.set_register(ARMv5RegNames::SP, expected_behavior[0].registers[13]);
            
            for expected_state in expected_behavior {
                // We produce the expected behavior with QEMU and
                // GDB. Sometimes two instructions are combined into one
                // so sometimes we need to step twice
                if sim.get_register(ARMv5RegNames::PC) != expected_state.registers[15] {
                    sim.step().unwrap();
                }

                assert_eq!(
                    expected_state.registers, sim.get_registers(),
                    "Registers do not match at 0x{:X}!", 
                    sim.get_register(ARMv5RegNames::PC)
                );
                assert_eq!(
                    expected_state.flags, sim.get_flags(),
                    "Flags do not match at 0x{:}!", 
                    sim.get_register(ARMv5RegNames::PC)
                );
                
                sim.step().unwrap();
            }
        }
    )*)
}

behavior_tests! {
    hallo_test: (
        executable: "./sample_programs/hallo",
        behavior: "./tests/behaviors/hallo-expected-behavior.json"
    ),
    // Detected mistake: C flag behaves differently in subtractions
    fib_test: (
        executable: "./sample_programs/fib",
        behavior: "./tests/behaviors/fib-expected-behavior.json"
    ),

    scalar_test: (
        executable: "./sample_programs/scalar",
        behavior: "./tests/behaviors/scalar-expected-behavior.json"
    ),
    
    functions_test: (
        executable: "./sample_programs/functions",
        behavior: "./tests/behaviors/functions-expected-behavior.json"
    ),

    fold_test: (
        executable: "./sample_programs/fold",
        behavior: "./tests/behaviors/fold-expected-behavior.json"
    )
}

macro_rules! output_tests {
    ($($test_name:ident: (
        executable: $executable:expr, output: $output:expr
    )),*)  => ($(
        #[test]
        fn $test_name() {
            let mut expected_output = read_to_string($output)
                .expect("Could not read output file!");
            expected_output = expected_output.replace("\r\n", "\n");

            let mut sim = ARMSimulator::new();
            sim.load_elf_file($executable).unwrap();
            
            let mut buffer: Vec<u8> = Vec::new();
            loop {
                match sim.step().unwrap() {
                    SimulationEvent::ConsoleOutput { stream: 1, message } => {
                        buffer.extend(message);
                    },
                    SimulationEvent::Exit { .. } => break,
                    _ => {}
                }
            }

            let output = String::from_utf8_lossy(&buffer);
            assert_eq!(expected_output, output);
        }
    )*)
}

output_tests! {
    hallo_output_test: (
        executable: "./sample_programs/hallo",
        output: "./tests/outputs/hallo-expected-output.txt"
    ),
    // Detected mistake: C flag behaves differently in subtractions
    fib_output_test: (
        executable: "./sample_programs/fib",
        output: "./tests/outputs/fib-expected-output.txt"
    ),

    scalar_output_test: (
        executable: "./sample_programs/scalar",
        output: "./tests/outputs/scalar-expected-output.txt"
    ),
    
    functions_output_test: (
        executable: "./sample_programs/functions",
        output: "./tests/outputs/functions-expected-output.txt"
    ),

    fold_output_test: (
        executable: "./sample_programs/fold",
        output: "./tests/outputs/fold-expected-output.txt"
    )
}