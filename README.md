# ARM-Simulator
This repository contains my version of a simple ARM simulator written in Rust. I created it for my bachelor project in computer science at the University of Innsbruck. 

## Features
The simulator takes an ELF file as an input. In order to execute a program you just have to provide the path to the executable:
	
``` ./arm-simulator path/to/executable ```

If you do not want to execute the program but just disassemble it, you can add the --disassemble option:

``` ./arm-simulator path/to/executable --disassemble```

It supports the ARMv5 instruction set with a few exceptions:
- coprocessor instructions
- instructions that modify the status register (plans to add these later)

It also supports a simple simulation of the Linux system calls Write and Exit.

## Library
In addition to the binary file, the repository also contains a library file, that allows the simulator to be embedded into other projects. For a UI version that I wrote in typescript using react I also included WASM support. In case you are interested, you can find the UI repository [here](https://github.com/TFILIPS/arm-simulator-webapp).


