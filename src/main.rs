mod compiler;
use std::io::{self, Write};
fn main() {
    println!("PIC16F84 Emulator - Assembly Compiler");
    println!("Type your PIC assembly code below. Type 'RUN' on a new line to compile and execute.");

    let mut input_code = String::new();

    loop {
        print!(">> "); // Display prompt
        if let Err(e) = io::stdout().flush() {
            eprintln!("Error flushing stdout: {}", e)
        }
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                let line = line.trim();
                if line.eq_ignore_ascii_case("RUN") {
                    break;
                }
                if !line.chars().all(|c| c.is_ascii()) {
                    eprintln!("Erro: Inout contains non-ASCII characters");
                    continue;
                }

                input_code.push_str(line);
                input_code.push('\n');
            }
            Err(e) => {
                eprintln!("ERROR: can't read input: {}", e);
                continue;
            }
        }
    }

    println!("\nCompiling...\n");

    if let Err(e) = compiler::compiler::Compiler::run_compiler(&input_code) {
        eprintln!("CompilationErr {}", e)
    }
}
