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


DEFINE_WORD BYE, "BYE"
        movq    $SYS_EXIT, %rax
        movq    $EXIT_SUCCESS, %rdi
        syscall


DEFINE_WORD REFILL, "REFILL"
        movq    $TRUE_VAL, %rax
        ret


DEFINE_WORD QUIT, "QUIT"
        movq    rstack_init, %rsp  # Empty return stack
.Lquit_loop:
        call    xt_REFILL
        DPOP    %rax
        test    %rax, %rax
        jz      xt_BYE
        call    xt_INTERPRET
        DPUSH   $.Lquit_msg
        DPUSH   $QUIT_MSG_SZ
        call    xt_TYPE
        jmp     .Lquit_loop

.Lquit_msg:
        .ascii  "ok\r\n"
        .equ QUIT_MSG_SZ, (.-.Lquit_msg)


DEFINE_WORD INTERPRET, "INTERPRET"
        ret


DEFINE_WORD TYPE, "TYPE"        # ( c-addr u -- )
        DPOP    %rdx
        DPOP    %rsi
        movq    $SYS_WRITE, %rax
        movq    $STDOUT, %rdi
        syscall
        ret


# Counted string
word_cnt:
        .byte 0
word_buff:
        .skip WORD_SZ
        .byte 0

# Counted string
source_cnt:
        .byte 0
source_buff:
        .skip SOURCE_SZ
        .byte 0

lastest:   .quad _LAST_DEF
current:   .quad user_dict
user_dict: .skip DICT_SZ
