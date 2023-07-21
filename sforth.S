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
dstack_end:
        .skip   DSTACK_SZ
dstack_start:

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
rstack_init:    .quad 0


## Entrypoint

        .text
        .globl _start
_start:
        cld
        movq    %rsp, rstack_init          # Return stack
        movq    $dstack_start, %rbp        # Data stack pointer
        jmp     xt_QUIT                    # No return


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

## Stack manipulation

DEFINE_WORD DROP, "DROP"        # ( x -- )
        leaq    8(%rbp), %rbp
        ret


DEFINE_WORD NIP, "NIP"          # ( x1 x2 -- x2 )
        DPOP    %rax
        movq    %rax, (%rbp)
        ret


DEFINE_WORD DUP, "DUP"          # ( x -- x x )
        movq    (%rbp), %rax
        DPUSH   %rax
        ret


DEFINE_WORD SWAP, "SWAP"        # ( x1 x2 -- x2 x1 )
        movq    8(%rbp), %rax   # x1
        movq    0(%rbp), %rbx   # x2
        movq    %rbx, 8(%rbp)   # x2
        movq    %rax, 0(%rbp)   # x1
        ret


DEFINE_WORD OVER, "OVER"        # ( x1 x2 -- x1 x2 x1 )
        movq    8(%rbp), %rax
        DPUSH   %rax
        ret


DEFINE_WORD ROT, "ROT"          # ( x1 x2 x3 -- x2 x3 x1 )
        movq    16(%rbp), %rax  # x1
        movq     8(%rbp), %rbx  # x2
        movq     0(%rbp), %rcx  # x3
        movq    %rbx, 16(%rbp)  # x2
        movq    %rcx,  8(%rbp)  # x3
        movq    %rax,  0(%rbp)  # x1
        ret


DEFINE_WORD MinusROT, "-ROT"    # ( x1 x2 x3 -- x3 x1 x2 )
        movq    16(%rbp), %rax  # x1
        movq     8(%rbp), %rbx  # x2
        movq     0(%rbp), %rcx  # x3
        movq    %rcx, 16(%rbp)  # x3
        movq    %rax,  8(%rbp)  # x1
        movq    %rbx,  0(%rbp)  # x2
        ret


DEFINE_WORD TwoDROP, "2DROP"    # ( x1 x2 -- )
        leaq    16(%rbp), %rbp
        ret


DEFINE_WORD TwoDUP, "2DUP"      # ( x1 x2 -- x1 x2 x1 x2 )
        movq    8(%rbp), %rax   # x1
        movq    0(%rbp), %rbx   # x2
        DPUSH   %rax
        DPUSH   %rbx
        ret


DEFINE_WORD TwoSWAP, "2SWAP"    # ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
        movq    24(%rbp), %rax  # x1
        movq    16(%rbp), %rbx  # x2
        movq     8(%rbp), %rcx  # x3
        movq     0(%rbp), %rdx  # x4
        movq    %rcx, 24(%rbp)  # x3
        movq    %rdx, 16(%rbp)  # x4
        movq    %rax,  8(%rbp)  # x1
        movq    %rbx,  0(%rbp)  # x2
        ret


DEFINE_WORD QuesDUP, "?DUP"     # ( x -- 0 | x x )
        movq    (%rbp), %rax    # x
        test    %rax, %rax
        jz      .Lquesdup
        DPUSH   %rax
.Lquesdup:
        ret


DEFINE_WORD SupR, ">R"          # ( x -- ) ( R: -- x )
        popq    %rax            # Return addr
        DPOP    %rbx            # x
        pushq   %rbx
        jmp    *%rax            # Ret


DEFINE_WORD RSup, "R>"          # ( -- x ) ( R: x -- )
        popq    %rax            # Return addr
        popq    %rbx            # x
        DPUSH   %rbx
        jmp    *%rax            # Ret


DEFINE_WORD RAt, "R@"           # ( -- x ) ( R: x -- x )
        movq    8(%rsp), %rax   # x
        DPUSH   %rax
        ret


DEFINE_WORD SZero, "S0"         # ( -- addr )
        DPUSH   $dstack_start
        ret


DEFINE_WORD SPAt, "SP@"         # ( -- addr )
        mov     %rbp, %rax
        DPUSH   %rax
        ret


## Memory

DEFINE_WORD At, "@"             # ( addr -- x )
        DPOP    %rax
        movq    (%rax), %rax
        DPUSH   %rax
        ret


DEFINE_WORD Bang, "!"           # ( x addr -- )
        DPOP    %rax            # addr
        DPOP    %rbx            # x
        movq    %rbx, (%rax)
        ret


DEFINE_WORD HBang, "H!"         # ( x addr -- ) Write half cell
        DPOP    %rax            # addr
        DPOP    %rbx            # x
        movl    %ebx, (%rax)
        ret


DEFINE_WORD PlusBang, "+!"      # ( x addr -- )
        DPOP    %rax            # addr
        DPOP    %rbx            # x
        addq    %rbx, (%rax)
        ret


DEFINE_WORD MinusBang, "-!"     # ( x addr -- )
        DPOP    %rax            # addr
        DPOP    %rbx            # x
        subq    %rbx, (%rax)
        ret


DEFINE_WORD CELLPlus, "CELL+"   # ( c-addr1 -- c-addr2 )
        addq    $8, (%rbp)
        ret


DEFINE_WORD CELLS, "CELLS"      # ( c-addr1 -- c-addr2 )
        shlq    $3, (%rbp)
        ret


DEFINE_WORD CAt, "C@"           # ( c-addr -- c )
        DPOP    %rax            # c-addr
        movzxb  (%rax), %rax    # c
        DPUSH   %rax
        ret


DEFINE_WORD CBang, "C!"         # ( char c-addr -- )
        DPOP    %rax            # c-addr
        DPOP    %rbx            # char
        movb    %bl, (%rax)
        ret


DEFINE_WORD CHARS, "CHARS"      # ( c-addr1 -- c-addr2 )
        ret                     # NOP


DEFINE_WORD CHARPlus, "CHAR+"   # ( c-addr1 -- c-addr2 )
        incq    (%rbp)
        ret


## Arithmetic

DEFINE_WORD Plus, "+"           # ( n1 | u1 n2 | u2 -- n3 | u3 )
        DPOP    %rax
        addq    %rax, (%rbp)
        ret


DEFINE_WORD Minus, "-"          # ( n1 | u1 n2 | u2 -- n3 | u3 )
        DPOP    %rax
        subq    %rax, (%rbp)
        ret


DEFINE_WORD NEGATE, "NEGATE"    # ( n1 -- n2 )
        negq    (%rbp)
        ret


DEFINE_WORD OnePlus, "1+"       # ( n1 | u1 -- n2 | u2 )
        incq    (%rbp)
        ret


DEFINE_WORD OneMinus, "1-"      # ( n1 | u1 -- n2 | u2 )
        decq    (%rbp)
        ret


DEFINE_WORD Star, "*"           # ( n1 n2 -- n3 )
        DPOP    %rcx            # n2
        DPOP    %rax            # n1
        xorq    %rdx, %rdx
        imul    %rcx
        DPUSH   %rax            # n3
        ret


DEFINE_WORD MStar, "M*"         # ( n1 n2 -- d )
        DPOP    %rcx            # n2
        DPOP    %rax            # n1
        xorq    %rdx, %rdx
        imul    %rcx
        DPUSH   %rax            # d_lo
        DPUSH   %rdx            # d_hi
        ret


DEFINE_WORD SlashMOD, "/MOD"    # ( n1 n2 -- rem quot )
        DPOP    %rcx            # n1
        DPOP    %rax            # n2
        xorq    %rdx, %rdx
        idiv    %rcx            # %rax := quotient, %rdx := rem
        DPUSH   %rdx            # rem
        DPUSH   %rax            # quot
        ret


DEFINE_WORD ABS, "ABS"          # ( n -- u )
        # https://stackoverflow.com/questions/2639173/x86-assembly-abs-implementation
        # u = ( n ^ s ) - s     Where s = ( n > 0 ) = n >> 63
        DPOP    %rax            # n
        movq    %rax, %rbx
        sarq    $63,  %rbx      # s
        xorq    %rbx, %rax      # n ^ s
        subq    %rbx, %rax      # (n^s) - s
        DPUSH   %rax
        ret


DEFINE_WORD MAX, "MAX", # ( n1 n2 -- n3 )
        DPOP    %rax            # n2
        DPOP    %rbx            # n1
        cmpq    %rbx, %rax
        jg      .Lmax_end
        movq    %rbx, %rax
.Lmax_end:
        DPUSH   %rax            # n3
        ret


# Logical

DEFINE_WORD INVERT, "INVERT"    # ( x1 -- x2 )
        notq    (%rbp)
        ret


DEFINE_WORD AND, "AND"          # ( x1 x2 -- x3 )
        DPOP    %rax            # x2
        andq    %rax, (%rbp)
        ret


DEFINE_WORD OR, "OR"            # ( x1 x2 -- x3 )
        DPOP    %rax            # x2
        orq     %rax, (%rbp)
        ret


DEFINE_WORD XOR, "XOR"          # ( x1 x2 -- x3 )
        DPOP    %rax            # x2
        xorq    %rax, (%rbp)
        ret


# Comparisons

DEFINE_WORD ZeroEq, "0="        # ( n | u -- flag )
        DPOP    %rax            # n
        test    %rax, %rax
        setnz   %al             # if      0 then 1 - 1 = 0  = FALSE
        dec     %al             # if not 0  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD ZeroInfSup, "0<>"   # ( n | u -- flag )
        DPOP    %rax            # n
        test    %rax, %rax
        setz    %al             # if not 0 then 1 - 1 = 0  = FALSE
        dec     %al             # if     0  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD ZeroSup, "0>"       # ( n -- flag )
        DPOP    %rax            # n
        cmp     $0, %rax
        setng   %al             # if <= 0 then 1 - 1 = 0  = FALSE
        dec     %al             # if >  0 then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD ZeroInf, "0<"       # ( n -- flag )
        DPOP    %rax            # n
        cmp     $0, %rax
        setnl   %al             # if >= 0 then 1 - 1 = 0  = FALSE
        dec     %al             # if <  0 then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD ZeroInfEq, "0<="    # ( n -- flag )
        DPOP    %rax            # n
        cmp     $0, %rax
        setnle  %al             # if >   0 then 1 - 1 = 0  = FALSE
        dec     %al             # if <=  0 then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD ZeroSupEq, "0>="       # ( n -- flag )
        DPOP    %rax            # n
        cmp     $0, %rax
        setnge  %al             # if <  0 then 1 - 1 = 0  = FALSE
        dec     %al             # if >= 0 then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD Eq, "="             # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setne   %al             # if neq then 1 - 1 = 0  = FALSE
        dec     %al             # if eq  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD InfSup, "<>"        # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        sete    %al             # if eq  then 1 - 1 = 0  = FALSE
        dec     %al             # if neq then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD Inf, "<"            # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setnl   %al             # if >= then 1 - 1 = 0  = FALSE
        dec     %al             # if <  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD Sup, ">"            # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setng   %al             # if <= then 1 - 1 = 0  = FALSE
        dec     %al             # if >  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD InfEq, "<="         # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setnle  %al             # if >  then 1 - 1 = 0  = FALSE
        dec     %al             # if <= then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD SupEq, ">="         # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setnge  %al             # if <  then 1 - 1 = 0  = FALSE
        dec     %al             # if >= then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD UInf, "U<"          # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setnb   %al             # if >= then 1 - 1 = 0  = FALSE
        dec     %al             # if <  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD USup, "U>"          # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setna   %al             # if <= then 1 - 1 = 0  = FALSE
        dec     %al             # if >  then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD UInfEq, "U<="       # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setnbe  %al             # if >  then 1 - 1 = 0  = FALSE
        dec     %al             # if <= then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


DEFINE_WORD USupEq, "U>="       # ( n1 | u1  n2 | u2 -- flag )
        DPOP    %rbx            # n2
        DPOP    %rax            # n1
        cmp     %rbx, %rax
        setnae  %al             # if <  then 1 - 1 = 0  = FALSE
        dec     %al             # if >= then 0 - 1 = -1 = TRUE
        movsx   %al, %rax       # Sign extend
        DPUSH   %rax
        ret


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
        DPUSH   $'\r'
        call    xt_EMIT
        DPUSH   $'\n'
        call    xt_EMIT
        ret


DEFINE_WORD TYPE, "TYPE"        # ( c-addr u -- )
        DPOP    %rdx
        DPOP    %rsi
_TYPE: # ( -- ) %rsi : c-addr, %rdx : u
        movq    $SYS_WRITE, %rax
        movq    $STDOUT, %rdi
        syscall
        ret


DEFINE_WORD KEY, "KEY"          # ( -- char )
        DPUSH   $0 # Alloc
        movq    $SYS_READ, %rax
        movq    $STDIN,    %rdi
        movq    %rbp,      %rsi
        movq    $1,        %rdx
        syscall
        ret


DEFINE_WORD Dot, "."            # ( u -- ) Print hex unsigned with leading 0s
        movq    $16, %rcx
        DPOP    %rax
.Ldot_loop:
        rolq    $4, %rax
        movq    %rax, %rbx
        andq    $0x0f, %rbx
        addb    $'0', %bl
        cmpb    $'9', %bl
        jbe     .Ldot_emit
        addb    $('A'-'0'-10), %bl
.Ldot_emit:
        DPUSH   %rbx
        push    %rax
        push    %rcx
        call    xt_EMIT
        pop     %rcx
        pop     %rax
        dec     %rcx
        jnz     .Ldot_loop
.Ldot_end:
        DPUSH   $' '
        call    xt_EMIT
        ret


# Compilation

DEFINE_WORD CP, "CP"            # ( -- addr )
        DPUSH   $curr_def
        ret
curr_def: .quad user_dict


DEFINE_WORD LASTEST, "LASTEST"  # ( -- addr )
        DPUSH   $last_def
        ret


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


DEFINE_WORD Comma, ","          # ( x -- )
        DPOP    %rax
        movq    curr_def, %rdi
        movq    %rax, (%rdi)
        addq    $8, curr_def
        ret


DEFINE_WORD CComma, "C,"        # ( char -- )
        DPOP    %rax
        movq    curr_def, %rdi
        movb    %al, (%rdi)
        incq    curr_def
        ret


DEFINE_WORD COMPILEComma, "COMPILE," # ( xt -- )
        DPOP    %rax # xt
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
        DPOP    %rax
_LITERAL: # ( -- ) %rax : x
        movq    curr_def, %rdx

        # Compile: leaq -8(%rbp), %rbp
        # 48 8d 6d f8
        movl    $0xf86d8d48, (%rdx)

        # FIXME: doesn't work for words that don't fit on 32bits

        # Compile: movq $IMM, (%rbp)
        # Where IMM is what's in %eax, sign extended
        # 48 c7 45 00 xx xx xx xx
        movl    $0x0045c748, 4(%rdx)
        movl    %eax, 8(%rdx)

        addq    $12, curr_def
        ret


DEFINE_WORD HEAD, "HEAD"        # ( "<spaces>ccc<space>" -- ) Add header for ccc
        movq    last_def, %rax
        movq    curr_def, %rdi
        movq    %rdi, last_def  # Update last_def
        stosq                   # Put link field

        movb    $FLAG_HIDDEN, %al
        stosb                   # Put hidden flag

        push    %rdi
        call    _PARSE_NAME     # FIXME: Ignore empty word ? Allows for :NONAME
        movq    %rdi, %rsi      # c-addr
        mov     %rcx, %rax      # u
        pop     %rdi

        stosb                   # Put size
        repe    movsb           # Put name

        movq    %rdi, curr_def
        ret


DEFINE_WORD HIDE, "HIDE", # ( -- )
        # Interpretation: toggle the visibility flag of the last definition
        movq    last_def, %rdi
        xorb    $FLAG_HIDDEN, 8(%rdi)
        ret


DEFINE_WORD IMMEDIATE, "IMMEDIATE", # ( -- )
        # Toggle imm flag
        movq    last_def, %rdi
        xorb    $FLAG_IMM, 8(%rdi)
        ret


## Parsing

DEFINE_WORD SupIN, ">IN"
        DPUSH   $source_idx
        ret
source_idx: .quad 0


DEFINE_WORD SOURCE, "SOURCE" # ( -- c-addr u )
        DPUSH   $source_buf
        movq    source_cnt, %rax
        DPUSH   %rax
        ret
source_cnt: .quad 0 # Size of the current line
source_buf:
        .skip SOURCE_SZ
        .byte 0


_REFILL: # ( -- ) Read line in source_buf, update source_idx and source_cnt
        # %rbx := Number of non special chars read (one newline -> 1)
        DPUSH   $'>'            # Prompt
        call    xt_EMIT
        DPUSH   $' '
        call    xt_EMIT

        xorq    %rbx, %rbx      # Total number of chars read
        movq    $source_buf, %rdi

.Lrefill_loop:
        push    %rdi            # Read one char
        call    xt_KEY
        pop     %rdi
        DPOP    %rax

        test    %al, %al        # Stop on zero (nothing read)
        jz      .Lrefill_end

        cmpb    $0x04, %al      # ^D: Restart
        jz      .Lrefill_end

        cmpb    $0x03, %al      # ^C: Restart
        je      _REFILL

        cmpb    $0x7f, %al      # DEL
        jne     .Lrefill_not_del
        cmpq    $source_buf, %rdi # Ignore DEL on first col
        je      .Lrefill_loop
        dec     %rdi
        push    %rdi
        DPUSH   $'\b'
        call    xt_EMIT
        DPUSH   $' '
        call    xt_EMIT
        DPUSH   $'\b'
        call    xt_EMIT
        pop     %rdi
        jmp     .Lrefill_loop
.Lrefill_not_del:

        cmpb    $0x1b, %al      # Ignore escape printable
        je      .Lrefill_loop

        incq    %rbx

        cmpb    $'\r', %al      # Stop on newline
        je      .Lrefill_end
        cmpb    $'\n', %al
        je      .Lrefill_end

        stosb                   # Add to buffer

        push    %rdi
        DPUSH   %rax
        call    xt_EMIT         # Echo
        pop     %rdi

        cmpq    $(source_buf+SOURCE_SZ), %rdi
        jl      .Lrefill_loop   # Stop on end of buffer

.Lrefill_end:
        movq    $0, source_idx
        subq    $source_buf, %rdi
        movq    %rdi, source_cnt
        ret


DEFINE_WORD REFILL, "REFILL"
        call    _REFILL
        test    %rbx, %rbx
        setz    %al             # 0 if rbx <> 0
        movzxb  %al, %rax
        decq    %rax            # -1 (TRUE) if rbx <> 0
        DPUSH   %rax
        ret


_WORD:  # ( "<chars>ccc<char>" -- ) %dl: char
        # Update source_idx, word_buf and word_cnt
        movq    source_idx, %rsi # Index in source_buf
        xorq    %rdi, %rdi       # Index in word_cnt

.Lword_skip: # Skip leading spaces
        cmpq    source_cnt, %rsi
        jge     .Lword_end
        movb    source_buf(%rsi), %al
        incq    %rsi

        cmpb    %dl, %al
        je      .Lword_skip

.Lword_loop: # Copy to word_buf until a space is found
        cmpq    $WORD_SZ, %rdi
        jge     .Lword_end
        movb    %al, word_buf(%rdi)
        incq    %rdi

        cmpq    source_cnt, %rsi
        jge     .Lword_end
        movb    source_buf(%rsi), %al
        incq    %rsi

        cmpb    %dl, %al
        jne     .Lword_loop

.Lword_end: # Set source_idx and word_cnt
        movq    %rsi, source_idx
        movq    %rdi, %rax
        movb    %al,  word_cnt
        ret

word_cnt: .byte 0
word_buf:
        .skip WORD_SZ
        .byte 0


_PARSE:  # ( bl:char "ccc<char>" -- rdi:c-addr rcx:u )
        movq    source_idx, %rsi
        movq    source_cnt, %rdx
        leaq    source_buf(%rsi), %rdi  # c-addr
        xorq    %rcx, %rcx              # u

.Lparse_loop: # Scan until end of buffer or <char> is found
        cmpq    %rdx, %rsi
        jge     .Lparse_end

        movb    source_buf(%rsi), %al
        inc     %rsi
        inc     %rcx
        cmpb    %al, %bl
        jne     .Lparse_loop
        dec     %rcx

.Lparse_end:
        movq    %rsi, source_idx
        ret


DEFINE_WORD PARSE "PARSE"       # ( char "ccc<char>" -- c-addr u )
        DPOP    %rbx
        call    _PARSE
        DPUSH   %rdi
        DPUSH   %rcx
        ret


_PARSE_NAME: # ( "<spaces>ccc<space>" -- rdi:c-addr rcx:u )
        movq    source_idx, %rsi
        movq    source_cnt, %rdx

.Lparse_name_skip: # Skip leading spaces
        cmpq    %rdx, %rsi
        jge     .Lparse_name_end
        cmpb    $' ', source_buf(%rsi)
        jne     .Lparse_name_end
        incq    %rsi
        jmp     .Lparse_name_skip

.Lparse_name_end:
        movq    %rsi, source_idx
        movb    $' ', %bl
        call    _PARSE
        ret


DEFINE_WORD PARSEMinusNAME "PARSE-NAME" # ( "<spaces>ccc<space>" -- c-addr u )
        call    _PARSE_NAME
        DPUSH   %rdi
        DPUSH   %rcx
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
        movq    $SYS_EXIT, %rax
        movq    $EXIT_SUCCESS, %rdi
        syscall


DEFINE_WORD QUIT, "QUIT"
        movq    rstack_init, %rsp       # Empty the return stack
        movq    $0, state               # Interpreter
.Lquit_loop:
        call    _REFILL
        test    %rbx, %rbx
        jz      xt_BYE                  # No return
        DPUSH   $' '
        call    xt_EMIT
        call    xt_INTERPRET

        cmpq    $0, state
        jnz     .Lquit_compiling
        movq    $.Lquit_msg, %rsi
        movq    $QUIT_MSG_SZ, %rdx
        jmp     .Lquit_ok
.Lquit_compiling:
        movq    $.Lquit_msg2, %rsi
        movq    $QUIT_MSG2_SZ, %rdx
        jmp     .Lquit_ok
.Lquit_ok:
        call    _TYPE
        jmp     .Lquit_loop

.Lquit_msg:
        .ascii  "ok\r\n"
        .equ QUIT_MSG_SZ, (.-.Lquit_msg)
.Lquit_msg2:
        .ascii  "ok (compiling)\r\n"
        .equ QUIT_MSG2_SZ, (.-.Lquit_msg2)


DEFINE_WORD ABORT, "ABORT"
        movq    $dstack_start, %rbp     # Empty data stack
        jmp     xt_QUIT                 # No return


_FIND: # ( rsi:c-addr rax:u -- rbx:nt rdi:xt dl:flags )
        movq    last_def, %rbx
        jmp     .L_find_begin

.L_find_loop:
        movq    (%rbx), %rbx    # Next
.L_find_begin:
        test    %rbx, %rbx
        jz      .L_find_not_found

        movb    8(%rbx), %dl    # Flags
        test    $FLAG_HIDDEN, %dl
        jnz     .L_find_loop    # Hidden word

        movzxb  9(%rbx), %rcx   # Length
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


DEFINE_WORD FIND, "FIND"        # ( c-addr u -- 0|nt xt )
        DPOP    %rax
        DPOP    %rsi
        call    _FIND
        DPUSH   %rbx            # nt
        DPUSH   %rdi            # xt
        ret


DEFINE_WORD INTERPRET, "INTERPRET"
.Linterpret_loop:
        call    _PARSE_NAME
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
        DPUSH   %rax            # Push number
        jmp     .Linterpret_loop


.Linterpret_not_found:
        movq    $.Linterpret_msg, %rsi
        movq    $.Linterpret_msg_sz, %rdx
        call    _TYPE           # Print message
        pop     %rdx            # u
        pop     %rsi            # c-addr
        call    _TYPE           # Print word
        call    xt_CR
        jmp     xt_ABORT        # No return
        ret

.Linterpret_msg:
        .ascii  "**NOT FOUND** "
        .equ .Linterpret_msg_sz, (.-.Linterpret_msg)


last_def:  .quad _LAST_DEF
user_dict: .skip DICT_SZ
