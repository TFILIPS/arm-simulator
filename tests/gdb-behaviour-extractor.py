import os
import sys
import json
import pygdbmi
import subprocess

from pygdbmi.gdbcontroller import GdbController

DEFAULT_PORT = 4545
BEHAVIOUR_FILE_FORMAT = "./%s-expected-behaviour.json"
OUTPUT_FILE_FORMAT = "./%s-expected-output.txt"

REGISTER_NAMES = [
    " R0", " R1", " R2", " R3", " R4", " R5", " R6", " R7",
    " R8", " R9", "R10", "R11", "R12", " SP", " lR", " PC"
]

FLAG_NAMES = [" N", " Z", " C", " V"]


def initialize_gdb_controller(program_file_path, qemu_port):
    gdb_controller = GdbController(["gdb-multiarch", "--nx", "--quiet", "--interpreter=mi3"])
    gdb_controller.write("-file-exec-and-symbols " + program_file_path)
    gdb_controller.write("-gdb-set architecture armv5")
    gdb_controller.write("-target-select remote localhost:" + str(qemu_port))
    for register_index in range(13):
        gdb_controller.write(f"-gdb-set $r{register_index}=0")

    response = gdb_controller.write('-data-list-register-values d 13')
    response_stack_pointer = response[0]['payload']['register-values'][0]
    return gdb_controller, int(response_stack_pointer['value'])


def print_register_list(register_values):
    for i in range(len(register_values)):
        print(REGISTER_NAMES[i] + ":" + hex(register_values[i] & 0xFFFFFFFF)[2:].zfill(8) + " ", end="")
        print() if (i + 1) % 8 == 0 else None


def get_qemu_state(gdb_controller):
    response = gdb_controller.write('-data-list-register-values d 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 25')
    response_values = response[0]['payload']['register-values']
    status_register_value = int(response_values.pop()['value'])
    register_value_list = [int(item['value']) for item in response_values]
    flag_value_list = [
        (status_register_value >> 31) & 1 == 1,
        (status_register_value >> 30) & 1 == 1,
        (status_register_value >> 29) & 1 == 1,
        (status_register_value >> 28) & 1 == 1
    ]
    return register_value_list, flag_value_list


def print_flags(flag_values):
    for i in range(len(FLAG_NAMES)):
        print(FLAG_NAMES[i] + ":" + str(flag_values[i]) + " ", end="")
    print()


def create_qemu_process(program_file_path, program_output_file, debug_port):
    return subprocess.Popen(
        ["qemu-arm", "-g", str(debug_port), "-R", "0x4000000", program_file_path],
        stdout=program_output_file, stderr=program_output_file
    )


if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.stderr.write(f"Usage: {sys.argv[0]} test_program [port]\n")
        exit(1)

    test_program_path = sys.argv[1]
    test_program_name = os.path.basename(test_program_path)

    port = sys.argv[2] if len(sys.argv) > 2 else DEFAULT_PORT

    gdb = qemu = output_file = None
    try:
        output_file = open(OUTPUT_FILE_FORMAT % test_program_name, "w+")
        qemu = create_qemu_process(test_program_path, output_file, port)

        gdb, sp = initialize_gdb_controller(test_program_path, port)

        step_counter = 0
        behaviour_data = []
        while qemu.poll() is None:
            reg_vals_qemu, flag_vals_qemu = get_qemu_state(gdb)

            print(f"Step: {step_counter}")
            print_register_list(reg_vals_qemu)
            print_flags(flag_vals_qemu)

            behaviour_data.append({
                "registers": reg_vals_qemu,
                "flags": flag_vals_qemu
            })

            gdb.write("-exec-step-instruction")
            step_counter += 1

    except pygdbmi.IoManager.GdbTimeoutError as e:
        sys.stderr.write(str(e) + "\n")

    finally:
        if gdb is not None:
            gdb.exit()
        if qemu is not None:
            qemu.kill()
        if output_file is not None:
            output_file.close()

    print("\nWriting behaviour file...")
    with open(BEHAVIOUR_FILE_FORMAT % test_program_name, "w+") as behaviour_file:
        json.dump(behaviour_data, behaviour_file)
    print("Done!")
