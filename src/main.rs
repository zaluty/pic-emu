mod compiler;

use std::fs;
use std::io::{self, Write};

fn main() {
    println!("🔹 📦 PIC16F84 Emulator  - Assembly Compiler");
    println!("🔹 Type or paste your PIC assembly code below.");
    println!("🔹 Type 'RUN' to compile, 'HELP' for instructions, or 'LOAD <file.asm>' to load code from a file.");

    let mut input_code = String::new();
    let instructions = "
    // Byte-oriented File register operations
    MOVWF f      ; Move W to file register
    NOP          ; No operation
    ADDWF f, d   ; Add W and f
    ANDWF f, d   ; AND W with F
    CLRF f       ; Clear f
    CLRW         ; Clear W
    COMF f, d    ; Complement f
    DECF f, d    ; Decrement f
    DECFSZ f, d  ; Decrement f, skip if zero
    INCF f, d    ; Increment f
    INCFSZ f, d  ; Increment f, skip if zero
    IORWF f, d   ; Inclusive OR W with f
    RLF f, d     ; Rotate left through Carry
    RRF f, d     ; Rotate right through Carry
    SUBWF f, d   ; Subtract W from f
    SWAPF f, d   ; Swap nibbles in f
    XORWF f, d   ; Exclusive OR W with f

    // Bit-oriented File register operations
    BSF f, b     ; Set bit in register
    BCF f, b     ; Clear bit in register
    BTFSC f, b   ; Bit test f, skip if clear
    BTFSS f, b   ; Bit test f, skip if set

    // Literal and control operations
    ADDLW k      ; Add literal to W
    CALL addr    ; Call subroutine
    MOVLW k      ; Load literal into W
    RETURN       ; Return from subroutine
    ANDLW k      ; AND literal with W
    CLRWDT       ; Clear watchdog timer
    GOTO addr    ; Go to address
    IORLW k      ; Inclusive OR literal with W
    RETFIE       ; Return from interrupt
    RETLW k      ; Return with literal in W
    SLEEP        ; Go into standby mode
    SUBLW k      ; Subtract W from literal
    XORLW k      ; Exclusive OR literal with W
    ";

    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();
        let line = line.trim();
        if line.eq_ignore_ascii_case("HELP") {
            println!("{}", instructions);
            continue;
        }

        if line.eq_ignore_ascii_case("RUN") {
            break;
        }
        if line.to_uppercase().starts_with("LOAD ") {
            let filename = line.split_whitespace().nth(1);
            if let Some(file) = filename {
                match fs::read_to_string(file) {
                    Ok(contents) => {
                        input_code = contents; // Overwrite existing input with file content
                        println!("✅ Successfully loaded code from '{}'.", file);
                    }
                    Err(e) => {
                        eprintln!("❌ Error loading file '{}': {}", file, e);
                    }
                }
            } else {
                eprintln!("⚠️ Usage: LOAD <filename.asm>");
            }
            continue;
        }

        // ✅ Collect manual input
        input_code.push_str(line);
        input_code.push('\n');
    }

    println!("\n🔹 Compiling...\n");

    match compiler::compiler::Compiler::run_compiler(&input_code) {
        Ok(_) => println!("✅ Compilation successful!"),
        Err(_) => eprintln!(""),
    }
}
