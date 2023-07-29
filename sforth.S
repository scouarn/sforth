### Scouarn Forth Bootstrap system
# %rbp: data stack pointer
# %r8:  top of the stack


## System constants

.equ DSTACK_SZ,         0x04000 # TODO: dynamic allocation using mmap syscall
.equ DICT_SZ,           0x10000 # TODO: dynamic allocation using mmap syscall
.equ TIB_SZ,            256
.equ FLAG_IMM,          0x01
.equ FLAG_HIDDEN,       0x02

#include <asm/unistd.h>
#define STDIN  0
#define STDOUT 1


## Stacks

        .bss
rstack_init:  .quad 0 # Return stack initial value
dstack_end:
        .skip DSTACK_SZ
dstack_start:

.macro DPUSH src
        leaq    -8(%rbp), %rbp
        movq    \src, (%rbp)
.endm

.macro DPOP dst
        movq    (%rbp), \dst
        leaq    8(%rbp), %rbp
.endm


## Entrypoint

        .text
        .globl _start
_start:
        cld
        xorq    %r8, %r8
        movq    $dstack_start, %rbp        # Data stack
        movq    %rsp, rstack_init          # Return stack
        jmp     xt_QUIT                    # No return


## Dictionary

.equ _LAST_DEF, 0
.macro DEFINE_WORD label, name, flags=0
1:
        .quad   _LAST_DEF
        .byte   \flags
        .byte   (3f - 2f)
2:
        .ascii  "\name"
3:
xt_\label:
.equ _LAST_DEF, 1b
.endm


.macro DEFINE_CONST label, name, value, flags=0
DEFINE_WORD \label, \name, \flags
        DPUSH   %r8
        mov     $\value, %r8
        ret
.endm

        .section .dictionary, "awx", @progbits
dict_start:


## Consts and variables

DEFINE_CONST    SZero,          "S0",           dstack_start
DEFINE_CONST    STATE,          "STATE",        state
DEFINE_CONST    LATEST,         "LATEST",       last_def
DEFINE_CONST    CP,             "CP",           curr_def
DEFINE_CONST    CPZero,         "CP0",          dict_start
DEFINE_CONST    CPOne,          "CP1",          user_dict_start
DEFINE_CONST    brk,            "brk",          dict_end
dict_end:       .quad (user_dict_start+DICT_SZ)
curr_def:       .quad user_dict_start
state:          .quad 0

DEFINE_WORD RSP0, "RSP0"        # ( -- addr )
        DPUSH   %r8
        mov     rstack_init, %r8
        ret

DEFINE_CONST    IN,             "IN",           source
DEFINE_CONST    SupIN,          ">IN",          source_idx
DEFINE_CONST    HashIN,         "\#IN",         source_cnt
DEFINE_CONST    TIB,            "TIB",          tib
source:         .quad tib
source_idx:     .quad 0
source_cnt:     .quad 0
tib:            .skip TIB_SZ


# Compilation

DEFINE_WORD Lsq, "[", FLAG_IMM
        movq    $0, state
        ret


DEFINE_WORD Rsq, "]"
        movq    $-1, state
        ret


DEFINE_WORD Comma, ","          # ( x -- )
        movq    curr_def, %rdi
        movq    %r8, (%rdi)
        addq    $8, curr_def
        DPOP    %r8
        ret


DEFINE_WORD CComma, "C,"        # ( char -- )
        movq    curr_def, %rdi
        movq    %r8, %rax
        DPOP    %r8
        movb    %al, (%rdi)
        incq    curr_def
        ret


DEFINE_WORD COMPILEComma, "COMPILE," # ( xt -- )
        movq    %r8, %rax # xt
        DPOP    %r8
_COMPILE: # ( -- ) %rax : xt
        # Compile: call REL
        # Where REL is xt relative to the next instruction:
        # REL = xt - (curr_def + 5) = %rax - 5 - %rdx
        movq    curr_def, %rdx
        movb    $0xe8, (%rdx)   # Append opcode of call
        subq    $5,    %rax
        subq    %rdx,  %rax
        movl    %eax,  1(%rdx)  # Append offset
        addq    $5, curr_def    # Update compilation pointer
        ret


DEFINE_WORD LITERAL, "LITERAL", FLAG_IMM # ( x -- )
        movq    %r8, %rax # x
        DPOP    %r8
_LITERAL: # ( -- ) %rax : x
        movq    curr_def, %rdx

        # TODO: use sign extended for more compact code

        # Compile: leaq -8(%rbp), %rbp
        # 48 8d 6d f8
        movl    $0xf86d8d48, (%rdx)

        # Compile: movq %r8, (%rbp)
        # 4c 89 45 00
        movl    $0x0045894c, 4(%rdx)

        # Compile: movq $IMM, %r8
        # 49 b8 xx xx xx xx xx xx xx xx
        movw    $0xb849, 8(%rdx)
        movq    %rax, 10(%rdx)

        addq    $18, curr_def
        ret


DEFINE_WORD Colon, ":"          # ( "<spaces>ccc<space>" -- )
        movq    last_def, %rax
        movq    curr_def, %rdi
        movq    %rdi, last_def  # Update last_def
        stosq                   # Put link field

        movb    $FLAG_HIDDEN, %al
        stosb                   # Put hidden flag

        push    %rdi
        movb    $' ', %bl
        call    _SKIP
        call    _PARSE          # FIXME: Allow empty word ?
        movq    %rdi, %rsi      # c-addr
        mov     %rcx, %rax      # u
        pop     %rdi

        stosb                   # Put size
        repe    movsb           # Put name

        movq    $-1, state      # Enter compilation mode

        movq    %rdi, curr_def  # Update CP
        ret


DEFINE_WORD Semi, ";", FLAG_IMM
        movq    curr_def, %rdi
        movb    $0xc3, (%rdi)   # Compile a ret instruction
        incq    curr_def
        movq    last_def, %rdi  # Make the word visible
        andb    $(~FLAG_HIDDEN), 8(%rdi)
        movq    $0, state       # Exit compilation mode
        ret


DEFINE_WORD IMMEDIATE, "IMMEDIATE", # ( -- )
        # Toggle imm flag
        movq    last_def, %rdi
        xorb    $FLAG_IMM, 8(%rdi)
        ret


## Parsing

_REFILL: # ( -- ) Read line in source buff, update source_idx and source_cnt
        movq    $0, source_idx
        mov     $tib, %rdi
.Lrefill_loop:
        push    %rdi

        pushw   $0              # Input one char
        movq    $__NR_read, %rax
        movq    $STDIN,     %rdi
        movq    %rsp,       %rsi
        movq    $1,         %rdx
        syscall

        test    %rax, %rax
        popw    %ax
        pop     %rdi
        jl      .Lrefill_err    # Error
        je      .Lrefill_loop   # Nothing read

        cmpb    $'\r', %al      # Line terminator
        je      .Lrefill_end
        cmpb    $'\n', %al
        je      .Lrefill_end

        stosb
        cmp     $(tib+TIB_SZ), %rdi
        jb      .Lrefill_loop

.Lrefill_end:
        sub     $tib, %rdi
        movq    %rdi, source_cnt
        ret

.Lrefill_err:
        movq    $-1, source_cnt
        ret


DEFINE_WORD SCAN_UNTIL, "scan-until"    # ( char "ccc<char>" -- )
        mov     %r8, %rbx
        DPOP    %r8
_PARSE:  # bl:char -- rdi:c-addr rcx:u
        movq    source_idx, %rsi
        movq    source_cnt, %rdx
        movq    source, %r15
        leaq    (%r15,%rsi), %rdi  # c-addr
        xorq    %rcx, %rcx              # u

.Lparse_loop: # Scan until end of buffer or <char> is found
        cmpq    %rdx, %rsi
        jge     .Lparse_end

        movb    (%r15,%rsi), %al
        inc     %rsi
        inc     %rcx
        cmpb    %al, %bl
        jne     .Lparse_loop
        dec     %rcx

.Lparse_end:
        movq    %rsi, source_idx
        ret


DEFINE_WORD SCAN_WHILE, "scan-while"    # ( char "<chars>ccc" -- "ccc" )
        mov     %r8, %rbx
        DPOP    %r8
_SKIP:  # bl:char
        movq    source_idx, %rsi
        movq    source_cnt, %rdx
        movq    source, %r15
.Lskip_loop:
        cmpq    %rdx, %rsi
        jge     .Lskip_end
        cmpb    $' ', (%r15,%rsi)
        jne     .Lskip_end
        incq    %rsi
        jmp     .Lskip_loop
.Lskip_end:
        movq    %rsi, source_idx
        ret


DEFINE_WORD PARSE "PARSE"       # ( char "ccc<char>" -- c-addr u )
        call    xt_SCAN_UNTIL
        DPUSH   %r8
        DPUSH   %rdi            # c-addr
        movq    %rcx, %r8       # u
        ret


_NUMBER: # ( rdi:c-addr rdx:u -- rax:u rbx:flag ) Parse hex number
        xorq    %rsi, %rsi      # Index in word_buf
        xorq    %rax, %rax      # Result
        movq    $0, %rbx        # FALSE (failure)

.Lnumber_loop:
        movzxb  (%rdi,%rsi), %rcx
        incq    %rsi
        cmpb    $'9', %cl
        jbe     .Lnumber_digit
        cmpb    $'A', %cl
        jb      .Lnumber_fail
        subb    $('A'-'0'-10), %cl
.Lnumber_digit:
        subb    $'0', %cl
        cmpb    $0x0f, %cl
        ja      .Lnumber_fail

        shlq    $4, %rax        # Multiply by 15
        addq    %rcx, %rax      # Add digit

        cmpq    %rdx, %rsi
        jb      .Lnumber_loop

        decq    %rbx            # TRUE (ok)
.Lnumber_fail:
        ret



## Interpreter

DEFINE_WORD BYE, "BYE"
        movq    $__NR_exit, %rax
        movq    $0, %rdi
        syscall

DEFINE_WORD QUIT, "(quit)"
        movq    rstack_init, %rsp       # Empty the return stack
        movq    $0, state               # Interpreter mode
.Lquit_loop:
        call    _REFILL
        cmpq    $0, source_cnt
        jl      xt_BYE
        call    _INTERPRET
        jmp     .Lquit_loop


_FIND: # ( rsi:c-addr rax:u -- rbx:0|nt rdi:xt dl:flags )
        movq    last_def, %rbx
        jmp     .L_find_begin

.L_find_loop:
        movq    (%rbx), %rbx    # Next
.L_find_begin:
        test    %rbx, %rbx
        jz      .L_find_not_found

        movzxb  8(%rbx), %rdx   # Flags
        test    $FLAG_HIDDEN, %dl
        jnz     .L_find_loop    # Hidden word

        movzxb  9(%rbx), %rcx   # Length
        # jrcxz   .L_find_loop    # Ignore if length is zero (:NONAME) ## Is it even useful to check for that ?
        cmp     %rax, %rcx
        jne     .L_find_loop    # Next word if the length doesn't match

        leaq    10(%rbx), %rdi
        push    %rsi
        repe    cmpsb           # String compare
        pop     %rsi
        jne     .L_find_loop    # Next word if not equal

        # Found ! After repe, %rdi contains the xt
.L_find_not_found:
        ret


DEFINE_WORD find, "find"   # ( c-addr u -- xt flags 0|nt )
        movq    %r8, %rax
        movq    (%rbp), %rsi
        call    _FIND
        lea     -8(%rbp), %rbp
        movq    %rdi, 8(%rbp)   # xt
        movq    %rdx, (%rbp)    # flag
        movq    %rbx, %r8       # nt
        ret


_INTERPRET:
.Linterpret_loop:
        movb    $' ', %bl
        call    _SKIP
        call    _PARSE
        test    %rcx, %rcx
        jnz     .Linterpret_try_find
        ret

.Linterpret_try_find:
        push    %rdi            # c-addr
        push    %rcx            # u

        movq    %rdi, %rsi      # c-addr
        movq    %rcx, %rax      # u
        call    _FIND
        test    %rbx, %rbx
        jz      .Linterpret_try_number

        addq    $16, %rsp       # Drop

        test    $FLAG_IMM, %dl
        jnz     .Linterpret_exec_word
        cmpq    $0, state
        jz      .Linterpret_exec_word

        movq    %rdi, %rax
        call    _COMPILE        # Compile xt
        jmp     .Linterpret_loop

.Linterpret_exec_word:
        call    *%rdi           # Execute xt
        jmp     .Linterpret_loop


.Linterpret_try_number:
        movq    0(%rsp), %rdx   # u
        movq    8(%rsp), %rdi   # c-addr
        call    _NUMBER

        test    %rbx, %rbx
        jz      .Linterpret_not_found

        addq    $16, %rsp       # Drop

        cmpq    $0, state
        jz      .Linterpret_exec_number

        call    _LITERAL        # Compile number
        jmp     .Linterpret_loop

.Linterpret_exec_number:
        DPUSH   %r8
        movq    %rax, %r8       # Push number
        jmp     .Linterpret_loop


.Linterpret_not_found:
        movq    $__NR_write, %rax       # Print error message
        movq    $STDOUT, %rdi
        movq    $.Linterpret_msg, %rsi
        movq    $.Linterpret_msg_sz, %rdx
        syscall

        movq    $__NR_write, %rax       # Print word
        movq    $STDOUT, %rdi
        pop     %rdx            # u
        pop     %rsi            # c-addr
        syscall

        jmp     xt_BYE          # No return

.Linterpret_msg:
        .ascii  "**NOT FOUND** "
        .equ .Linterpret_msg_sz, (.-.Linterpret_msg)


last_def: .quad _LAST_DEF
user_dict_start:
        .skip DICT_SZ
