use elf_loader::load_elf;

mod elf_loader;

fn main() {
    load_elf("data/hello");
}
