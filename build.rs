use std::process::Command;

fn main() {
    std::env::set_current_dir("./runtime").unwrap();
    Command::new("rustc")
        .args([
              "--crate-type", "lib", "--emit=llvm-ir", "-o", "../target/runtime.ll", "src/lib.rs",
        ])
        .status()
        .unwrap();
}
