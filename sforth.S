### Scouarn Forth
# Non standard bootstrap system

## Refs :
# - https://compilercrim.es/bootstrap/miniforth/#parsing
# - https://notryan.com/snippets/forth/forth-tux.html
# - https://www.forth.com/starting-forth/9-forth-execution/
# - https://forth-standard.org/standard/core
# - https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/
# - https://dacvs.neocities.org/SF/


## System constants

.equ DSTACK_SZ, 0x4000 # TODO: dynamic allocation using mmap syscall
.equ DICT_SZ,   0x4000 # TODO: dynamic allocation using mmap syscall

.equ SOURCE_SZ, 256
.equ WORD_SZ,   256

.equ FLAG_IMM,    0x01
.equ FLAG_HIDDEN, 0x02

.equ TRUE_VAL,  (~0)
.equ FALSE_VAL, 0


## Syscall constants

#include <asm/unistd.h>

.equ SYS_READ,  __NR_read
.equ SYS_WRITE, __NR_write
.equ SYS_EXIT,  __NR_exit

.equ STDIN,  0
.equ STDOUT, 1

.equ EXIT_SUCCESS, 0


## Data stack
        .bss
dtack_end:
        .skip   DSTACK_SZ
dstack_init:

.macro DPUSH src
        leaq    -8(%rbp), %rbp
        movq    \src, (%rbp)
.endm

.macro DPOP dst
        movq    (%rbp), \dst
        leaq    8(%rbp), %rbp
.endm


## Return stack
        .bss
rstack_init:    .quad


## Entrypoint

        .text
        .globl _start
_start:
        cld
        movq %rsp, rstack_init          # Return stack
        movq $dstack_init, %rbp         # Data stack pointer
        jmp  xt_QUIT                    # No return


## Dictionary

.equ _LAST_DEF, 0
.macro DEFINE_WORD label, name, flags=0
word_\label:
        .quad   _LAST_DEF
        .byte   \flags
        .byte   (2f - 1f)
1:
        .ascii  "\name"
2:
xt_\label:
.equ _LAST_DEF, word_\label
.endm


.macro DEFINE_CONST label, name, value, flags=0
DEFINE_WORD \label, \name, \flags
        DPUSH   $\value
        ret
.endm


.macro DEFINE_VAR label, name, value, flags=0
DEFINE_WORD \label, \name, \flags
        DPUSH   $3f
        ret
3:      .quad   \value
.endm



        .section .dictionary, "awx", @progbits


## IO

DEFINE_WORD EMIT, "EMIT"        # ( char -- )
        movq    $SYS_WRITE, %rax
        movq    $STDOUT,    %rdi
        movq    %rbp,       %rsi
        movq    $1,         %rdx
        syscall
        addq    $8, %rbp        # Drop
        ret


DEFINE_WORD CR, "CR"            # ( -- )
        DPUSH   $'\n'
        call    xt_EMIT
        ret


DEFINE_WORD TYPE, "TYPE"        # ( c-addr u -- )
        DPOP    %rdx
        DPOP    %rsi
_TYPE:
        movq    $SYS_WRITE, %rax
        movq    $STDOUT, %rdi
        syscall
        ret


## Parser

_REFILL: # ( -- ) Update source_buf, source_idx and source_cnt
        movq    $SYS_READ,    %rax
        movq    $STDIN,       %rdi
        movq    $source_buf,  %rsi
        movq    $SOURCE_SZ,   %rdx
        syscall
        movq    $0,   source_idx
        movq    %rax, source_cnt
        ret

source_idx: .quad 0 # >IN
source_cnt: .quad 0
source_buf:
        .skip SOURCE_SZ
        .byte 0


_WORD: # ( "<spaces>ccc<space>" -- ) Update source_idx, word_buf and word_cnt
        movq    source_idx, %rsi # Index in source_buf
        xorq    %rdi, %rdi       # Index in word_cnt

.Lword_skip: # Skip leading spaces
        cmpq    source_cnt, %rsi
        jge     .Lword_end
        movb    source_buf(%rsi), %al
        incq    %rsi

        cmpb    $' ', %al
        je      .Lword_skip
        cmpb    $'\n', %al
        je      .Lword_end

.Lword_loop: # Copy to word_buf until a space is found
        cmpq    $WORD_SZ, %rdi
        jge     .Lword_end
        movb    %al, word_buf(%rdi)
        incq    %rdi

        cmpq    source_cnt, %rsi
        jge     .Lword_end
        movb    source_buf(%rsi), %al
        incq    %rsi

        cmpb    $' ', %al
        je      .Lword_end
        cmpb    $'\n', %al
        je      .Lword_end
        jmp     .Lword_loop

.Lword_end: # Set source_idx and word_cnt
        movq    %rsi, source_idx
        movq    %rdi, word_cnt
        ret

word_cnt: .quad 0
word_buf:
        .skip WORD_SZ
        .byte 0


_NUMBER: # ( -- ) Parse hex number in word_buf, %rax := n|u, %rbx := flag
        xorq    %rsi, %rsi      # Index in word_buf
        xorq    %rax, %rax      # Result
        movq    $0, %rbx        # FALSE (failure)

.Lnumber_loop:
        movzxb  source_buf(%rsi), %rcx
        incq    %rsi
        cmpb    $'A', %cl
        jb      .Lnumber_digit
        subb    $('A'-'0'-10), %cl
.Lnumber_digit:
        subb    $'0', %cl
        cmpb    $0x0f, %cl
        ja      .Lnumber_fail

        shlq    $4, %rax        # Multiply by 15
        addq    %rcx, %rax      # Add digit

        cmpq    word_cnt, %rsi
        jl      .Lnumber_loop

        decq    %rbx            # TRUE (ok)
.Lnumber_fail:
        ret



## Interpreter

DEFINE_WORD BYE, "BYE"
        movq    $SYS_EXIT, %rax
        movq    $EXIT_SUCCESS, %rdi
        syscall


DEFINE_WORD QUIT, "QUIT"
        movq    rstack_init, %rsp       # Empty the return stack
.Lquit_loop:
        call    _REFILL
        test    %rax, %rax
        jz      xt_BYE                  # No return
        call    xt_INTERPRET
        movq    $.Lquit_msg, %rsi
        movq    $QUIT_MSG_SZ, %rdx
        call    _TYPE
        jmp     .Lquit_loop

.Lquit_msg:
        .ascii  "ok\n"
        .equ QUIT_MSG_SZ, (.-.Lquit_msg)


DEFINE_WORD ABORT, "ABORT"
        movq    $dstack_init, %rbp      # Empty data stack
        jmp     xt_QUIT                 # No return


DEFINE_WORD STATE, "STATE"
        DPUSH   $state
        ret
state: .quad 0


DEFINE_WORD Lsq, "[", FLAG_IMM
        incq    state   # State becomes 0 = FALSE
        ret


DEFINE_WORD Rsq, "]"
        decq    state   # State becomes -1 = TRUE
        ret


_FIND: # ( -- ) Find word in word_buf, %rbx := defptr, %rdi := xt, %dl := flags
        movq    lastest, %rbx
        jmp     .Lfind_begin

.Lfind_loop:
        movq    (%rbx), %rbx    # Next
.Lfind_begin:
        test    %rbx, %rbx
        jz      .Lfind_not_found

        movb    8(%rbx), %dl    # Flags

        movzxb  9(%rbx), %rcx   # Length
        cmpb    word_cnt, %cl
        jne     .Lfind_loop     # Next word if the length doesn't match

        leaq    10(%rbx), %rdi  # String compare
        leaq    word_buf, %rsi
        repe    cmpsb
        jne     .Lfind_loop     # Next word if not equal

        # Found ! After repe, %rdi contains the xt

.Lfind_not_found:
        ret


DEFINE_WORD INTERPRET, "INTERPRET"
.Linterpret_loop:
        call    _WORD
        cmpq    $0, word_cnt
        jg      .Linterpret_try_find
        ret

.Linterpret_try_find:
        call    _FIND
        test    %rbx, %rbx
        jz      .Linterpret_try_number
        test    $FLAG_IMM, %dl
        jnz     .Linterpret_exec_word
        cmpq    $0, state
        jz      .Linterpret_exec_word

        # TODO: Compile word
        jmp     .Linterpret_loop

.Linterpret_exec_word:
        call    *%rdi           # Interpreting: call xt
        jmp     .Linterpret_loop


.Linterpret_try_number:
        call    _NUMBER
        test    %rbx, %rbx
        jz      .Linterpret_not_found
        cmpq    $0, state
        jz      .Linterpret_exec_number

        # TODO: Compile number
        jmp     .Linterpret_loop

.Linterpret_exec_number:
        DPUSH   %rax            # Interpreting: push number
        jmp     .Linterpret_loop


.Linterpret_not_found:
        movq    $.Linterpret_msg, %rsi
        movq    $.Linterpret_msg_sz, %rdx
        call    _TYPE           # Print message
        movq    $word_buf, %rsi
        movq    word_cnt,  %rdx
        call    _TYPE           # Print word
        call    xt_CR
        jmp     xt_ABORT        # No return
        ret

.Linterpret_msg:
        .ascii  "**NOT FOUND** "
        .equ .Linterpret_msg_sz, (.-.Linterpret_msg)


lastest:   .quad _LAST_DEF
current:   .quad user_dict
user_dict: .skip DICT_SZ
