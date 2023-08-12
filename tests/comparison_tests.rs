use std::{fs::{File, read_to_string}, io::BufReader};

//use std::{fs::{read_dir, File}, io::BufReader};
use arm_simulator::{ARMSimulator, utils::{ConsoleOutput, ConsoleExit, OutputDevice}};
use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct State {
    registers: Vec<i32>,
    flags: Vec<bool>
}

#[derive(Serialize, Deserialize, Debug)]
struct Behaviour(Vec<State>);


//ToDo: Implement Error for custom error types and get rid of unwraps
macro_rules! behaviour_tests {
    ($($test_name:ident (
        executable: $executable:expr, behaviour: $behaviour:expr
    )),*)  => ($(
        #[test]
        fn $test_name() {
            let behaviour_file = File::open($behaviour)
                .expect("Behaviour File not found!");
            let reader = BufReader::new(behaviour_file);
            let expected_behaviour: Behaviour = serde_json::from_reader(reader)
                .expect("Could not parse behaviour file!");
            // The struct is only needed for parsing the JSON
            let expected_behaviour = expected_behaviour.0;

            let mut sim = ARMSimulator::from_elf_file(
                $executable, ConsoleOutput::new(), ConsoleExit
            ).unwrap();
            // Syncronize the stack pointer of simulator and behaviour
            sim.set_register(13, expected_behaviour[0].registers[13]);
            
            for expected_state in expected_behaviour {
                // We produce the expected behaviour with QEMU and
                // GDB. Sometimes two instructions are combined into one
                // so sometimes we need to step twice
                if sim.get_register(15) != expected_state.registers[15] {
                    sim.step().unwrap();
                }

                assert_eq!(
                    expected_state.registers, sim.get_registers(),
                    "Registers do not match at 0x{:X}!", 
                    sim.get_register(15)
                );
                assert_eq!(
                    expected_state.flags, sim.get_flags(),
                    "Flags do not match at 0x{:}!", 
                    sim.get_register(15)
                );
                sim.step().unwrap();
            }
        }
    )*)
}

behaviour_tests! {
    hallo_test (
        executable: "./sample_programs/hallo",
        behaviour: "./tests/behaviours/hallo-expected-behaviour.json"
    ),
    // Detected misstake: C flag behaves differently in subtractions
    fib_test (
        executable: "./sample_programs/fib",
        behaviour: "./tests/behaviours/fib-expected-behaviour.json"
    ),

    scalar_test (
        executable: "./sample_programs/scalar",
        behaviour: "./tests/behaviours/scalar-expected-behaviour.json"
    ),
    
    functions_test (
        executable: "./sample_programs/functions",
        behaviour: "./tests/behaviours/functions-expected-behaviour.json"
    ),

    fold_test (
        executable: "./sample_programs/fold",
        behaviour: "./tests/behaviours/fold-expected-behaviour.json"
    )
}

struct OutputComperator {
    expected_output: String,
    output_buffer: Vec<u8>
}
impl OutputDevice for OutputComperator {
    fn output(&mut self, bytes: &[u8]) {
        self.output_buffer.extend(bytes);
    }

    fn output_err(&mut self, bytes: &[u8]) {
        self.output_buffer.extend(bytes);
    }

    fn flush(&mut self) {
        assert_eq!(
            self.expected_output, 
            String::from_utf8_lossy(&self.output_buffer)
        );
    }
}
impl OutputComperator {
    fn new(expected_output: String) -> OutputComperator {
        OutputComperator {
            expected_output,
            output_buffer: Vec::new()
        }
    }
}

macro_rules! output_tests {
    ($($test_name:ident (
        executable: $executable:expr, output: $output:expr
    )),*)  => ($(
        #[test]
        fn $test_name() {
            let mut expected_output = read_to_string($output)
                .expect("Could not read output file!");
            expected_output = expected_output.replace("\r\n", "\n");

            let mut sim = ARMSimulator::from_elf_file(
                $executable, 
                OutputComperator::new(expected_output), 
                ConsoleExit
            ).unwrap();

            loop {
                sim.step().unwrap();
            }
        }
    )*)
}

output_tests! {
    hallo_output_test (
        executable: "./sample_programs/hallo",
        output: "./tests/outputs/hallo-expected-output.txt"
    ),
    // Detected misstake: C flag behaves differently in subtractions
    fib_output_test (
        executable: "./sample_programs/fib",
        output: "./tests/outputs/fib-expected-output.txt"
    ),

    scalar_output_test (
        executable: "./sample_programs/scalar",
        output: "./tests/outputs/scalar-expected-output.txt"
    ),
    
    functions_output_test (
        executable: "./sample_programs/functions",
        output: "./tests/outputs/functions-expected-output.txt"
    ),

    fold_output_test (
        executable: "./sample_programs/fold",
        output: "./tests/outputs/fold-expected-output.txt"
    )
}