use std::collections::HashMap;
use std::str::FromStr;
use thiserror::Error;

/// ✅ Instruction Set
#[derive(Debug, Clone)]
enum Instruction {
    // Byte-oriented File Register Operations
    Movwf(u8),
    Addwf(u8, u8),
    Andwf(u8, u8),
    Clrf(u8),
    Clrw,
    Comf(u8, u8),
    Decf(u8, u8),
    Decfsz(u8, u8),
    Incf(u8, u8),
    Incfsz(u8, u8),
    Iorwf(u8, u8),
    Rlf(u8, u8),
    Rrf(u8, u8),
    Subwf(u8, u8),
    Swapf(u8, u8),
    Xorwf(u8, u8),
    Nop,

    // Bit-oriented File Register Operations
    Bsf(u8, u8),
    Bcf(u8, u8),
    Btfsc(u8, u8),
    Btfss(u8, u8),

    // Literal and Control Operations
    Addlw(u8),
    Call(u16),
    Movlw(u8),
    Return,
    Andlw(u8),
    Clrwdt,
    Goto(u16),
    Iorlw(u8),
    Retfie,
    Retlw(u8),
    Sleep,
    Sublw(u8),
    Xorlw(u8),
}

/// ✅ AST Representation
#[derive(Debug, Clone)]
struct ASTNode {
    instruction: Instruction,
    line_number: usize,
}

#[derive(Debug, Clone)]
struct AST {
    nodes: Vec<ASTNode>,
}

/// ✅ Error Handling
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Invalid instruction '{0}' at line {1}")]
    InvalidInstruction(String, usize),
    #[error("{0} at line {1} found {2}")]
    ArgumentCountMismatchWithHint(String, usize, String),
    #[error("Unexpected number of arguments for '{0}' at line {1}")]
    ArgumentCountMismatch(String, usize),
    #[error("Failed to parse number at line {0}")]
    NumberParseError(usize),
    #[error("Expected the operand to be either 0 or 1")]
    InvalidOperand(usize),
    #[error("Unknown register {0} at line {1}")]
    InvalidRegister(String, usize),
}

/// ✅ Compiler Structure
pub struct Compiler;

impl Compiler {
    pub fn run_compiler(input: &str) -> Result<Vec<u16>, ParseError> {
        match Self::parse_assembly(input) {
            Ok(ast) => {
                println!("Parsed AST: {:#?}", ast);
                let machine_code = Self::compile_to_machine_code(&ast);
                println!("Machine Code: {:?}", machine_code);
                Ok(machine_code)
            }
            Err(e) => {
                eprintln!("❌ Error parsing assembly: {}", e);
                Err(e)
            }
        }
    }

    /// ✅ Parsing PIC Assembly
    fn parse_assembly(input: &str) -> Result<AST, ParseError> {
        let mut nodes = Vec::new();

        for (line_number, line) in input.lines().enumerate() {
            let line = line.split(";").next().unwrap().trim();
            let split = line.replace(",", "");
            let tokens: Vec<&str> = split.split_whitespace().collect();
            if tokens.is_empty() {
                continue;
            }
            print!("{:?}", tokens);
            if tokens.len() != Self::match_tokens_len(&tokens).unwrap_or(0) {
                let hint = Self::generate_hint(tokens[0])
                    .unwrap_or_else(|| "No hints available.".to_string());
                return Err(ParseError::ArgumentCountMismatchWithHint(
                    tokens[0].to_string(),
                    line_number + 1,
                    hint,
                ));
            }

            let instruction = match tokens[0].to_uppercase().as_str() {
                // Byte-oriented Operations
                "MOVWF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Movwf(Self::parse_hex(tokens[1], line_number)?)
                }
                "ADDWF" => {
                    Self::check_arg(&tokens, line_number)?;
                    Instruction::Addwf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "ANDWF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Andwf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "CLRF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Clrf(Self::parse_hex(tokens[1], line_number)?)
                }
                "CLRW" => {
                    print!("{:?}", tokens.len());
                    Instruction::Clrw
                }
                "COMF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Comf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "DECF" => {
                    Self::check_arg(&tokens, line_number + 1)?;

                    Instruction::Decf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "DECFSZ" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Decfsz(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "INCF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Incf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "INCFSZ" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Incfsz(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "IORWF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Iorwf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "RLF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Rlf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "RRF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Rrf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "SUBWF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Subwf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "SWAPF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Swapf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "XORWF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Xorwf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, true)?,
                    )
                }
                "NOP" => Instruction::Nop,

                // Bit-oriented Operations
                "BSF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Bsf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, false)?,
                    )
                }
                "BCF" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Bcf(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, false)?,
                    )
                }
                "BTFSC" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Btfsc(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, false)?,
                    )
                }
                "BTFSS" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Btfss(
                        Self::parse_hex(tokens[1], line_number)?,
                        Self::parse_dec(tokens[2], line_number, false)?,
                    )
                }

                // Literal and Control Operations
                "ADDLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Addlw(Self::parse_hex(tokens[1], line_number)?)
                }
                "CALL" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Call(Self::parse_hex16(tokens[1], line_number)?)
                }
                "MOVLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Movlw(Self::parse_hex(tokens[1], line_number)?)
                }
                "RETURN" => Instruction::Return,
                "ANDLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Andlw(Self::parse_hex(tokens[1], line_number)?)
                }
                "CLRWDT" => Instruction::Clrwdt,
                "GOTO" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Goto(Self::parse_hex16(tokens[1], line_number)?)
                }
                "IORLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Iorlw(Self::parse_hex(tokens[1], line_number)?)
                }
                "RETFIE" => Instruction::Retfie,
                "RETLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Retlw(Self::parse_hex(tokens[1], line_number)?)
                }
                "SLEEP" => Instruction::Sleep,
                "SUBLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Sublw(Self::parse_hex(tokens[1], line_number)?)
                }
                "XORLW" => {
                    Self::check_arg(&tokens, line_number + 1)?;
                    Instruction::Xorlw(Self::parse_hex(tokens[1], line_number)?)
                }

                _ => {
                    return Err(ParseError::InvalidInstruction(
                        tokens[0].to_string(),
                        line_number + 1,
                    ))
                }
            };

            nodes.push(ASTNode {
                instruction,
                line_number: line_number + 1,
            });
        }

        Ok(AST { nodes })
    }

    /// ✅ Machine Code Generation (encoding fixes applied)
    fn compile_to_machine_code(ast: &AST) -> Vec<u16> {
        ast.nodes
            .iter()
            .map(|node| match node.instruction {
                // Byte-oriented Instructions
                Instruction::Movwf(f) => 0x0080 | (f as u16),
                Instruction::Addwf(f, d) => 0x0700 | ((d as u16) << 8) | (f as u16),
                Instruction::Andwf(f, d) => 0x0500 | ((d as u16) << 8) | (f as u16),
                Instruction::Clrf(f) => 0x0180 | (f as u16),
                Instruction::Clrw => 0x0100,
                Instruction::Comf(f, d) => 0x0900 | ((d as u16) << 8) | (f as u16),
                Instruction::Decf(f, d) => 0x0300 | ((d as u16) << 8) | (f as u16),
                Instruction::Decfsz(f, d) => 0x0B00 | ((d as u16) << 8) | (f as u16),
                Instruction::Incf(f, d) => 0x0A00 | ((d as u16) << 8) | (f as u16),
                Instruction::Incfsz(f, d) => 0x0F00 | ((d as u16) << 8) | (f as u16),
                Instruction::Iorwf(f, d) => 0x0400 | ((d as u16) << 8) | (f as u16),
                Instruction::Rlf(f, d) => 0x0D00 | ((d as u16) << 8) | (f as u16),
                Instruction::Rrf(f, d) => 0x0C00 | ((d as u16) << 8) | (f as u16),
                Instruction::Subwf(f, d) => 0x0200 | ((d as u16) << 8) | (f as u16),
                Instruction::Swapf(f, d) => 0x0E00 | ((d as u16) << 8) | (f as u16),
                Instruction::Xorwf(f, d) => 0x0600 | ((d as u16) << 8) | (f as u16),
                Instruction::Nop => 0x0000,

                // Bit-oriented Instructions
                Instruction::Bsf(f, b) => 0x1400 | ((f as u16) << 5) | (b as u16),
                Instruction::Bcf(f, b) => 0x1000 | ((f as u16) << 5) | (b as u16),
                Instruction::Btfsc(f, b) => 0x1800 | ((f as u16) << 5) | (b as u16),
                Instruction::Btfss(f, b) => 0x1C00 | ((f as u16) << 5) | (b as u16),

                // Literal & Control Operations
                Instruction::Addlw(lit) => 0x3E00 | (lit as u16),
                Instruction::Call(addr) => 0x2000 | (addr & 0x7FF),
                Instruction::Movlw(lit) => 0x3000 | (lit as u16),
                Instruction::Return => 0x0008,
                Instruction::Andlw(lit) => 0x3900 | (lit as u16),
                Instruction::Clrwdt => 0x0064,
                Instruction::Goto(addr) => 0x2800 | (addr & 0x7FF),
                Instruction::Iorlw(lit) => 0x3800 | (lit as u16),
                Instruction::Retfie => 0x0009,
                Instruction::Retlw(lit) => 0x3400 | (lit as u16),
                Instruction::Sleep => 0x0063,
                Instruction::Sublw(lit) => 0x3C00 | (lit as u16),
                Instruction::Xorlw(lit) => 0x3A00 | (lit as u16),
            })
            .collect()
    }

    /// Register Map for Special Function Registers
    fn register_map() -> HashMap<&'static str, u8> {
        HashMap::from([
            ("PORTA", 0x05),
            ("PORTB", 0x06),
            ("TRISA", 0x85),
            ("TRISB", 0x86),
            ("STATUS", 0x03),
            ("FSR", 0x04),
            ("PCL", 0x02),
            ("PCLATH", 0x0A),
            ("INTCON", 0x0B),
            ("OPTION_REG", 0x81),
            ("TMR0", 0x01),
            ("TMR1L", 0x0E),
            ("TMR1H", 0x0F),
            ("T1CON", 0x10),
            ("TMR2", 0x11),
            ("T2CON", 0x12),
            ("EEDATA", 0x08),
            ("EEADR", 0x09),
            ("EECON1", 0x88),
            ("EECON2", 0x89),
            ("EEADRH", 0x10),
            ("EEDATH", 0x11),
        ])
    }

    /// ✅ Pin Map for PORTA and PORTB
    /// ✅ Pin Map for PORTA, PORTB, and Special Bits
    fn pin_map() -> HashMap<&'static str, u8> {
        HashMap::from([
            ("RA0", 0),
            ("RA1", 1),
            ("RA2", 2),
            ("RA3", 3),
            ("RA4", 4),
            ("RB0", 0),
            ("RB1", 1),
            ("RB2", 2),
            ("RB3", 3),
            ("RB4", 4),
            ("RB5", 5),
            ("RB6", 6),
            ("RB7", 7),
            ("IRP", 7),
            ("RP1", 6),
            ("RP0", 5),
            ("TO", 4),
            ("PD", 3),
            ("Z", 2),
            ("DC", 1),
            ("C", 0),
            ("GIE", 7),
            ("PEIE", 6),
            ("T0IE", 5),
            ("INTE", 4),
            ("RBIE", 3),
            ("T0IF", 2),
            ("INTF", 1),
            ("RBIF", 0),
            ("PSA", 3),
            ("T0SE", 4),
            ("T0CS", 5),
            ("INTEDG", 6),
            ("RBPU", 7),
            ("MCLR", 0),
            ("VPP", 1),
            ("OSC1", 2),
            ("OSC2", 3),
            ("PGC", 4),
            ("PGD", 5),
            ("PGM", 6),
        ])
    }

    /// ✅ Helper Functions
    fn parse_hex(value: &str, line: usize) -> Result<u8, ParseError> {
        let reg_map = Self::register_map();
        let pin_map = Self::pin_map();

        // 1️⃣ Try parsing as a hexadecimal number
        if let Ok(num) = u8::from_str_radix(value.trim_start_matches("0x"), 16) {
            return Ok(num);
        }

        // 2️⃣ Check if it's a known pin (e.g., RA0, RB7)
        if let Some(&pin_addr) = pin_map.get(value.to_uppercase().as_str()) {
            return Ok(pin_addr);
        }

        // 3️⃣ Check if it's a known register (e.g., STATUS, TRISA)
        if let Some(&reg_addr) = reg_map.get(value.to_uppercase().as_str()) {
            return Ok(reg_addr);
        }

        // ❌ Unknown value
        Err(ParseError::InvalidRegister(value.to_string(), line + 1))
    }

    fn parse_hex16(value: &str, line: usize) -> Result<u16, ParseError> {
        u16::from_str_radix(value.trim_start_matches("0x"), 16)
            .map_err(|_| ParseError::NumberParseError(line))
    }

    fn parse_dec(value: &str, line: usize, restrict_to_0_1: bool) -> Result<u8, ParseError> {
        let pin_map = Self::pin_map();

        // ✅ Check if the value matches a pin/bit name (e.g., "RP0", "RB7")
        if let Some(&pin) = pin_map.get(value.to_uppercase().as_str()) {
            return Ok(pin);
        }

        // ✅ Parse as a decimal number
        let num = u8::from_str(value).map_err(|_| ParseError::NumberParseError(line))?;

        // ✅ Restrict to 0 or 1 when required
        if restrict_to_0_1 && num > 1 {
            return Err(ParseError::InvalidOperand(line));
        }

        // ✅ Ensure valid range for bit positions (0-7)
        if !restrict_to_0_1 && num > 7 {
            return Err(ParseError::InvalidOperand(line));
        }

        Ok(num)
    }

    fn generate_hint(instruction: &str) -> Option<String> {
        match instruction.to_uppercase().as_str() {
            // Byte-oriented File Register Operations
            "MOVWF" => Some("\nHint: Use 'MOVWF <file_register>'. Example: MOVWF 0x0C".to_string()),
            "ADDWF" => Some("\nHint: Use 'ADDWF <f>, <d>'. Example: ADDWF 0x1F, 1".to_string()),
            "CLRF" => Some("\nHint: Use 'CLRF <file_register>'. Example: CLRF 0x0D".to_string()),

            // Bit-oriented File Register Operations
            "BSF" => {
                Some("\nHint: Use 'BSF <file_register>, <bit>'. Example: BSF 0x06, 3".to_string())
            }
            "BTFSC" => Some(
                "\nHint: Use 'BTFSC <file_register>, <bit>'. Example: BTFSC 0x05, 2".to_string(),
            ),

            // Literal and Control Operations
            "MOVLW" => Some("\nHint: Use 'MOVLW <literal>'. Example: MOVLW 0x55".to_string()),
            "CALL" => Some("\nHint: Use 'CALL <address>'. Example: CALL 0x200".to_string()),
            "GOTO" => Some("\nHint: Use 'GOTO <address>'. Example: GOTO 0x300".to_string()),

            // Control Instructions (No operands)
            "RETURN" | "NOP" | "SLEEP" | "CLRWDT" => Some(format!(
                "\nHint: '{}' does not require any operands.",
                instruction
            )),

            // Default case for unknown instructions
            _ => None,
        }
    }

    fn match_tokens_len(tokens: &[&str]) -> Option<usize> {
        match tokens[0].to_uppercase().as_str() {
            // Byte-oriented File Register Operations
            "MOVWF" | "CLRF" | "CLRW" | "NOP" => Some(2),
            "ADDWF" | "ANDWF" | "RLF" | "RRF" | "DECFSZ" | "INCFSZ" | "XORWF" | "IORWF"
            | "COMF" | "DECF" | "SUBWF" | "SWAPF" | "INCF" => Some(3),

            // Bit-oriented File Register Operations
            "BSF" | "BCF" | "BTFSC" | "BTFSS" => Some(3),

            // Literal and Control Operations
            "MOVLW" | "ADDLW" | "ANDLW" | "IORLW" | "XORLW" | "SUBLW" | "RETLW" | "CALL"
            | "GOTO" => Some(2),
            "RETURN" | "RETFIE" | "CLRWDT" | "SLEEP" => Some(1),

            _ => None,
        }
    }

    fn check_arg(tokens: &[&str], line: usize) -> Result<(), ParseError> {
        if let Some(expected_len) = Self::match_tokens_len(tokens) {
            if tokens.len() != expected_len {
                return Err(ParseError::ArgumentCountMismatch(
                    tokens[0].to_string(),
                    line,
                ));
            }
        } else {
            return Err(ParseError::InvalidInstruction(tokens[0].to_string(), line));
        }
        Ok(())
    }
}
