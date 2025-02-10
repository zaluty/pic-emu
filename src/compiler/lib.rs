use std::collections::HashMap;

use crate::{Compiler, Instruction, ParseError};

#[test]
fn test_parse_assembly_valid() -> Result<(), ParseError> {
    let input = "
        MOVLW 0x50
        MOVWF 0x0C
        ADDWF 0x1F, 1
        ANDWF 0x20, 0
        CLRF 0x0D
        CLRW
        COMF 0x0E, 1
        DECF 0x0F, 0
        DECFSZ 0x10, 1
        INCF 0x11, 0
        INCFSZ 0x12, 1
        IORWF 0x13, 0
        RLF 0x14, 1
        RRF 0x15, 0
        SUBWF 0x16, 1
        SWAPF 0x17, 0
        XORWF 0x18, 1
        NOP
        BSF 0x06, 3
        BCF 0x07, 2
        BTFSC 0x05, 1
        BTFSS 0x04, 0
        ADDLW 0x20
        CALL 0x250
        RETURN
        ANDLW 0x30
        CLRWDT
        GOTO 0x3A0
        IORLW 0x40
        RETFIE
        RETLW 0x50
        SLEEP
        SUBLW 0x60
        XORLW 0x70
    ";

    let ast = Compiler::parse_assembly(input)?;

    // You could add assertions here to check the structure of the AST
    // For example, check the number of nodes, the instructions, etc.
    assert_eq!(ast.nodes.len(), 34); // Check if all instructions are parsed

    Ok(())
}

#[test]
fn test_parse_assembly_invalid_instruction() {
    let input = "INVALID_INSTRUCTION 0x00";
    let result = Compiler::parse_assembly(input);
    assert!(matches!(result, Err(ParseError::InvalidInstruction(_, _))));
}

#[test]
fn test_parse_assembly_argument_mismatch() {
    let input = "MOVLW 0x00 0x01"; // Too many arguments
    let result = Compiler::parse_assembly(input);
    assert!(matches!(
        result,
        Err(ParseError::ArgumentCountMismatch(_, _))
    ));

    let input = "ADDWF 0x00"; // Too few arguments
    let result = Compiler::parse_assembly(input);
    assert!(matches!(
        result,
        Err(ParseError::ArgumentCountMismatch(_, _))
    ));

    let input = "RETURN 0x00"; // Too many arguments
    let result = Compiler::parse_assembly(input);
    assert!(matches!(
        result,
        Err(ParseError::ArgumentCountMismatch(_, _))
    ));
}

#[test]
fn test_parse_assembly_number_parse_error() {
    let input = "MOVLW 0xZZ"; // Invalid hex number
    let result = Compiler::parse_assembly(input);
    assert!(matches!(result, Err(ParseError::NumberParseError(_))));

    let input = "ADDWF 0x00, 2"; // Invalid decimal number (2 for 'd' operand)
    let result = Compiler::parse_assembly(input);
    assert!(matches!(result, Err(ParseError::InvalidOperand(_))));
}

#[test]
fn test_compile_to_machine_code() -> Result<(), ParseError> {
    let input = "
        MOVLW 0x50
        MOVWF 0x0C
        ADDWF 0x1F, 1
        RETURN
    ";
    let ast = Compiler::parse_assembly(input)?;
    let machine_code = Compiler::compile_to_machine_code(&ast);

    assert_eq!(machine_code.len(), 4);
    assert_eq!(machine_code[0], 0x3050); // MOVLW 0x50
    assert_eq!(machine_code[1], 0x008C); // MOVWF 0x0C
    assert_eq!(machine_code[2], 0x079F); // ADDWF 0x1F, 1
    assert_eq!(machine_code[3], 0x0008); // RETURN

    Ok(())
}

#[test]
fn test_run_compiler_valid() -> Result<(), ParseError> {
    let input = "MOVLW 0x50";
    let machine_code = Compiler::run_compiler(input)?;
    assert_eq!(machine_code.len(), 1);
    assert_eq!(machine_code[0], 0x3050);
    Ok(())
}

#[test]
fn test_run_compiler_invalid() {
    let input = "INVALID_INSTRUCTION";
    let result = Compiler::run_compiler(input);
    assert!(matches!(result, Err(ParseError::InvalidInstruction(_, _))));
}

#[test]
fn test_generate_hint() {
    assert_eq!(
        Compiler::generate_hint("MOVLW"),
        Some("\nHint: Use 'MOVLW <literal>'. Example: MOVLW 0x55".to_string())
    );
    assert_eq!(Compiler::generate_hint("INVALID"), None); // No hint for invalid instructions
    assert_eq!(
        Compiler::generate_hint("RETURN"),
        Some("\nHint: 'RETURN' does not require any operands.".to_string())
    );
}

#[test]
fn test_match_tokens_len() {
    assert_eq!(Compiler::match_tokens_len(&vec!["MOVLW", "0x00"]), Some(2));
    assert_eq!(
        Compiler::match_tokens_len(&vec!["ADDWF", "0x00", "0"]),
        Some(3)
    );
    assert_eq!(Compiler::match_tokens_len(&vec!["RETURN"]), Some(1));
    assert_eq!(Compiler::match_tokens_len(&vec!["INVALID"]), None);
}

#[test]
fn test_check_arg_valid() -> Result<(), ParseError> {
    Compiler::check_arg(&vec!["MOVLW", "0x00"], 1)?;
    Compiler::check_arg(&vec!["ADDWF", "0x00", "0"], 1)?;
    Compiler::check_arg(&vec!["RETURN"], 1)?;
    Ok(())
}

#[test]
fn test_check_arg_invalid() {
    let result = Compiler::check_arg(&vec!["MOVLW"], 1); // Missing argument
    assert!(matches!(
        result,
        Err(ParseError::ArgumentCountMismatch(_, _))
    ));

    let result = Compiler::check_arg(&vec!["MOVLW", "0x00", "0x01"], 1); // Too many arguments
    assert!(matches!(
        result,
        Err(ParseError::ArgumentCountMismatch(_, _))
    ));

    let result = Compiler::check_arg(&vec!["INVALID"], 1); // Invalid instruction
    assert!(matches!(result, Err(ParseError::InvalidInstruction(_, _))));
}
