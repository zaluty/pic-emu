.intel_syntax noprefix
.section .data
    message: .string "Hello, World!"
    len = . - message

.section .text
    .global _start

_start:
    mov rax, 1
    mov rdi, 1
    lea rsi, [rip + message]
    mov rdx, len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall
