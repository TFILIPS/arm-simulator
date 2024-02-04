use std::{process::exit, env};

use arm_simulator::{
    ARMSimulator, SimulationEvent, SimulationException,
    utils::{ConsoleOutput, ExitOnError, OutputDevice}
};

fn main() {
    let (path, disassemble): (String, bool) = parse_arguments();

    let mut output_devie: ConsoleOutput = ConsoleOutput::new();
    let mut simulator: ARMSimulator = ARMSimulator::new();
    simulator.load_elf_file(&path).unwarp_or_exit();

    if disassemble {
        let disassembly:String = simulator.get_disassembly().unwarp_or_exit();
        print!("{disassembly}");
    }
    else {
        loop {
            match simulator.step() {
                Ok(SimulationEvent::ConsoleOutput { stream, message }) => {
                    if stream == 1 { output_devie.output(&message) }
                    else if stream == 2 { output_devie.output_err(&message) }
                },
                Ok(SimulationEvent::Exit { exit_code }) => {
                    output_devie.flush();
                    exit(exit_code);
                },
                Err(SimulationException { msg, .. }) => {
                    output_devie.flush();
                    eprintln!("{msg}");
                    exit(1);
                },
                _ => {}
            }
        }
    }
}

fn parse_arguments() -> (String, bool) {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 {
        println!("Usage: {:} path_to_executable [--disassemble]", args[0]);
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
