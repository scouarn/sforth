### Scouarn Forth

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

.equ FLAG_IMM, 0x01

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
        movq %rsp, rstack_init
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
        movq    $STDOUT, %rdi
        syscall
        addq    $8, %rbp        # Drop
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

_REFILL:
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


_WORD: # ( "<spaces>ccc<space>" -- )
        movq    $source_buf, %rsi
        addq    source_idx,  %rsi # Pointer in source_buf
        movq    $word_buf,   %rdi

.Lword_skip: # Skip leading spaces
        cmpq    $(source_buf+SOURCE_SZ), %rsi
        jge     .Lword_end
        lodsb
        cmpb    $' ', %al
        je      .Lword_skip

.Lword_loop: # Copy to word_buf until a space is found
        cmpq    $(word_buf+WORD_SZ), %rdi
        jge     .Lword_end
        stosb

        cmpq    $(source_buf+SOURCE_SZ), %rsi
        jge     .Lword_end
        lodsb

        cmpb    $' ', %al
        jne      .Lword_loop

.Lword_end: # Compute source_idx and word_cnt
        subq    $source_buf, %rsi
        movq    %rsi, source_idx
        subq    $word_buf, %rdi
        movq    %rdi, word_cnt
        ret


word_cnt: .quad 0
word_buf:
        .skip WORD_SZ
        .byte 0


_NUMBER: # ( -- ) Parses word_buf, %rax := n|u, %rbx := flag
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


DEFINE_WORD INTERPRET, "INTERPRET"
.Linterpret_loop:
        call    _WORD
        cmpq    $0, word_cnt
        jg      .Linterpret_not_empty
        ret
.Linterpret_not_empty:
        movq    $word_buf, %rsi
        movq    word_cnt,  %rdx
        call    _TYPE
        jmp     .Linterpret_loop
        ret


lastest:   .quad _LAST_DEF
current:   .quad user_dict
user_dict: .skip DICT_SZ
