use std::str::FromStr;
use thiserror::Error;

/// Represents the different PIC16F84 instructions
#[derive(Debug, Clone)]
enum Instruction {
    // Byte-oriented File register operations
    Movwf(u8), // Move W to file register DONE
    Nop,       // No operation DONE
    // TO-DO
    Addwf(u8, u8),  // Add W and f
    Andwf(u8, u8),  // AND W with F
    Clrf(u8),       // CLear f
    Clearw,         // Clear W
    Comf(u8, u8),   // Complement f
    Decf(u8, u8),   // Decrement f
    Decfsz(u8, u8), // Decrement f skip if 0
    Incf(u8, u8),   // Increment f
    Incfsz(u8, u8), // Increment f skip if zero
    Iorwf(u8, u8),  // Inclusive OR W with f
    Rlf(u8, u8),    // Rotate left f through Carry
    Rrf(u8, u8),    // ROtate right f through Carry
    Subwf(u8, u8),  // Substract W from f
    Swapf(u8, u8),  // Swap nibbles in f
    Xorwf(u8, u8),  // Exclusive OR  W with f

    // Bit-oriented File registre
    Bsf(u8, u8),   // Set bit in register DONE
    Bcf(u8, u8),   // Clear bit in register  DONE
    Btfsc(u8, u8), // Bit test f skip if CLear
    Btfss(u8, u8), // bit test f skip if Set

    // Literal and control operations
    Addlw(u8), // Add literal to W DONE
    Call(u16), // Call subroutine  DONE
    Movlw(u8), // Load literal into W DONE
    Return,    // Return from subroutine DONE
    Andlw(u8), // And literal with W
    Clrwdt,    // Clear watchdog timer
    Goto(u8),  // GO to Address
    Iorlw(u8), // inclusive OR literal with W
    Retfie,    // Return form interrupt
    Retlw(u8), // Return with literal in W
    Sleep,     // Go into standby mode
    Sublw(u8), // Substract W from literal
    Xorlw(u8), // Exclusive OR Literal With W
}

/// Represents a single node in the AST
#[derive(Debug, Clone)]
struct ASTNode {
    instruction: Instruction,
    line_number: usize,
}

/// Holds the full AST of a program
#[derive(Debug, Clone)]
struct AST {
    nodes: Vec<ASTNode>,
}

/// Error handling for parsing issues
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Invalid instruction '{0}' at line {1}")]
    InvalidInstruction(String, usize),

    #[error("Missing or invalid argument at line {0}")]
    InvalidArgument(usize),

    #[error("Unexpected number of arguments for '{0}' at line {1}")]
    ArgumentCountMismatch(String, usize),

    #[error("Failed to parse number at line {0}")]
    NumberParseError(usize),
}

/// Struct-based approach for the compiler
pub struct Compiler;

impl Compiler {
    /// Runs the compiler (main function to be called)
    pub fn run_compiler(input: &str) -> Result<Vec<u16>, ParseError> {
        match Self::parse_assembly(input) {
            Ok(ast) => {
                println!("Parsed AST: {:#?}", ast);
                let machine_code = Self::compile_to_machine_code(&ast);
                println!("Machine Code: {:?}", machine_code);
                Ok(machine_code) // Return the machine code on success
            }
            Err(e) => {
                eprintln!("Error parsing assembly: {}", e); // Use eprintln for errors
                Err(e) // Return the error
            }
        }
    }

    /// Parses PIC assembly into an AST
    fn parse_assembly(input: &str) -> Result<AST, ParseError> {
        let mut nodes = Vec::new();

        for (line_number, line) in input.lines().enumerate() {
            let tokens: Vec<&str> = line.split_whitespace().collect();

            if tokens.is_empty() {
                continue;
            } // Skip empty lines

            let instruction = match tokens[0] {
                "MOVLW" if tokens.len() == 2 => {
                    let value = Self::parse_hex(tokens[1], line_number)?;
                    Instruction::Movlw(value)
                }
                "MOVWF" if tokens.len() == 2 => {
                    let value = Self::parse_hex(tokens[1], line_number)?;
                    Instruction::Movwf(value)
                }
                "ADDLW" if tokens.len() == 2 => {
                    let value = Self::parse_hex(tokens[1], line_number)?;
                    Instruction::Addlw(value)
                }
                "BSF" if tokens.len() == 3 => {
                    let reg = Self::parse_hex(tokens[1], line_number)?;
                    let bit = Self::parse_dec(tokens[2], line_number)?;
                    Instruction::Bsf(reg, bit)
                }
                "BCF" if tokens.len() == 3 => {
                    let reg = Self::parse_hex(tokens[1], line_number)?;
                    let bit = Self::parse_dec(tokens[2], line_number)?;
                    Instruction::Bcf(reg, bit)
                }
                "CALL" if tokens.len() == 2 => {
                    let addr = Self::parse_hex16(tokens[1], line_number)?;
                    Instruction::Call(addr)
                }
                "ADDWF" if tokens.len() == 3 => {
                    let reg = Self::parse_hex(tokens[1], line_number)?; // File register
                    let dest = Self::parse_dec(tokens[2], line_number)?; // Destination (0 = W, 1 = f)
                    Instruction::Addwf(reg, dest)
                }
                "CLRF" if tokens.len() == 2 => {
                    let f = Self::parse_hex(tokens[1], line_number)?;
                    Instruction::Clrf(f)
                }
                "ANDLW" if tokens.len() == 2 => {
                    let f = Self::parse_hex(tokens[1], line_number)?;
                    Instruction::Andlw(f)
                }
                "RETURN" if tokens.len() == 1 => Instruction::Return,
                "NOP" if tokens.len() == 1 => Instruction::Nop,

                _ if tokens.len() < 2 => return Err(ParseError::InvalidArgument(line_number + 1)),
                _ if tokens.len() > 3 => {
                    return Err(ParseError::ArgumentCountMismatch(
                        tokens[0].to_string(),
                        line_number + 1,
                    ))
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

    /// Converts AST into machine code
    fn compile_to_machine_code(ast: &AST) -> Vec<u16> {
        let mut machine_code = Vec::new();

        for node in &ast.nodes {
            let opcode = match node.instruction {
                Instruction::Movlw(lit) => 0x3000 | (lit as u16),
                Instruction::Movwf(f) => 0x0080 | (f as u16),
                Instruction::Addlw(lit) => 0x3E00 | (lit as u16),
                Instruction::Bsf(f, b) => 0x1400 | ((f as u16) << 5) | (b as u16),
                Instruction::Bcf(f, b) => 0x1000 | ((f as u16) << 5) | (b as u16),
                Instruction::Call(addr) => 0x2000 | (addr & 0x7FF),
                Instruction::Return => 0x0008,
                Instruction::Nop => 0x0000,
                Instruction::Addwf(f, d) => 0x0700 | ((d as u16) << 8) | (f as u16),
                Instruction::Clrf(f) => 0x180 | (f as u16),
                Instruction::Andlw(f) => 0x3900 | (f as u16),
                _ => todo!(),
            };

            machine_code.push(opcode);
        }

        machine_code
    }

    /// Parses an 8-bit hexadecimal value
    fn parse_hex(value: &str, line: usize) -> Result<u8, ParseError> {
        u8::from_str_radix(value.trim_start_matches("0x"), 16)
            .map_err(|_| ParseError::NumberParseError(line))
    }

    /// Parses a 16-bit hexadecimal value
    fn parse_hex16(value: &str, line: usize) -> Result<u16, ParseError> {
        u16::from_str_radix(value.trim_start_matches("0x"), 16)
            .map_err(|_| ParseError::NumberParseError(line))
    }

    /// Parses a decimal value
    fn parse_dec(value: &str, line: usize) -> Result<u8, ParseError> {
        u8::from_str(value).map_err(|_| ParseError::NumberParseError(line))
    }
}
