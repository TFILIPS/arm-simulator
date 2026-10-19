use std::{env, fmt::Display, io::{self, Write}, process::exit};

use arm_simulator::{ARMSimulator, SimulationEvent};

fn main() {
    let (path, disassemble): (String, bool) = parse_arguments();

    let mut simulator: ARMSimulator = ARMSimulator::new();
    simulator.load_elf_file(&path).unwrap_or_exit();

    if disassemble {
        let disassembly: String = simulator.get_disassembly().unwrap_or_exit();
        write_unbuffered(io::stdout(), disassembly.as_bytes());
    }
    else {
        loop {
            match simulator.step() {
                Ok(SimulationEvent::ConsoleOutput { stream: 1, message }) => {
                    write_unbuffered(io::stdout(), &message);
                },
                Ok(SimulationEvent::ConsoleOutput { stream: 2, message }) => {
                    write_unbuffered(io::stderr(), &message);
                },
                Ok(SimulationEvent::Exit { exit_code }) => {
                    exit(exit_code);
                },
                Err(exception) => {
                    eprintln!("{exception}");
                    exit(1);
                },
                _ => {}
            }
        }
    }
}

// Like the write system call: the bytes are passed on unchanged and errors
// (e.g. a closed pipe) do not stop the simulation
fn write_unbuffered(mut stream: impl Write, bytes: &[u8]) {
    let _ = stream.write_all(bytes).and_then(|_| stream.flush());
}

fn parse_arguments() -> (String, bool) {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 {
        eprintln!("Usage: {} path_to_executable [-d | --disassemble]", args[0]);
        exit(1);
    }
    let mut path: String = String::new();
    let mut disassemble: bool = false;
    for arg in &args[1..] {
        if arg.starts_with("--") {
            disassemble |= arg == "--disassemble";
        }
        else if arg.starts_with("-") {
            disassemble |= arg == "-d";
        }
        else if path.is_empty() {
            path = String::from(arg);
        }
    }
    (path, disassemble)
}

trait ExitOnError<T> {
    fn unwrap_or_exit(self) -> T;
}

impl<T, E: Display> ExitOnError<T> for Result<T, E> {
    fn unwrap_or_exit(self) -> T {
        self.unwrap_or_else(|error| {
            eprintln!("{error}");
            exit(1);
        })
    }
}
