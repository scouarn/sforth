### Scouarn Forth
# Bootstrap system

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

# %rbp: data stack pointer
# %r8:  top of the stack

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
        xorq    %r8, %r8
        movq    $dstack_start, %rbp        # Data stack
        movq    %rsp, rstack_init          # Return stack
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
        DPOP    %r8
        ret


DEFINE_WORD NIP, "NIP"          # ( x1 x2 -- x2 )
        addq    $8, %rbp
        ret


DEFINE_WORD DUP, "DUP"          # ( x -- x x )
        DPUSH   %r8
        ret


DEFINE_WORD SWAP, "SWAP"        # ( x1 x2 -- x2 x1 )
        xchg    %r8, (%rbp)
        ret


DEFINE_WORD OVER, "OVER"        # ( x1 x2 -- x1 x2 x1 )
        movq    (%rbp), %rax
        DPUSH   %r8
        movq    %rax, %r8
        ret


DEFINE_WORD SupR, ">R"          # ( x -- ) ( R: -- x )
        popq    %rax            # Return addr
        movq    %r8, %rbx       # x
        DPOP    %r8
        pushq   %rbx            # x
        jmp    *%rax            # Ret


DEFINE_WORD RSup, "R>"          # ( -- x ) ( R: x -- )
        popq    %rax            # Return addr
        DPUSH   %r8
        popq    %r8             # x
        jmp    *%rax            # Ret


DEFINE_WORD RAt, "R@"           # ( -- x ) ( R: x -- x )
        DPUSH   %r8
        movq    8(%rsp), %r8    # x
        ret


DEFINE_WORD TwoSupR, "2>R"      # ( x1 x2 -- ) ( R: -- x1 x2 )
        popq    %rax            # Return addr
        push    (%rbp)          # x1
        push    %r8             # x2
        movq    8(%rbp), %r8
        leaq    16(%rbp), %rbp
        jmp    *%rax            # Ret


DEFINE_WORD TwoRSup, "2R>"      # ( -- x1 x2 ) ( R: x1 x2 -- )
        leaq    -16(%rbp), %rbp
        movq    %r8, 8(%rbp)
        popq    %rax            # Return addr
        popq    %r8             # x1
        popq    (%rbp)          # x2
        jmp    *%rax            # Ret



DEFINE_WORD SZero, "S0"         # ( -- addr )
        DPUSH   %r8
        movq    $dstack_start, %r8
        ret


DEFINE_WORD SPAt, "SP@"         # ( -- addr )
        movq    %r8, %rax
        movq    %rbp, %r8
        DPUSH   %rax
        ret


## Memory

DEFINE_WORD At, "@"             # ( addr -- x )
        movq    (%r8), %r8
        ret


DEFINE_WORD Bang, "!"           # ( x addr -- )
        DPOP    %rax            # x
        movq    %rax, (%r8)
        DPOP    %r8
        ret


DEFINE_WORD HBang, "H!"         # ( x addr -- ) Write half cell
        DPOP    %rax            # x
        movl    %eax, (%r8)
        DPOP    %r8
        ret


DEFINE_WORD PlusBang, "+!"      # ( x addr -- )
        DPOP    %rax            # x
        addq    %rax, (%r8)
        DPOP    %r8
        ret


DEFINE_WORD CAt, "C@"           # ( c-addr -- c )
        movzxb  (%r8), %r8    # c
        ret


DEFINE_WORD MOVE, "MOVE"        # ( addr1 addr2 u -- ) # TODO: optimize better
        movq    %r8, %rcx       # u
        DPOP    %r8
        movq    %r8, %rdi       # addr2
        DPOP    %r8
        movq    %r8, %rsi       # addr1
        DPOP    %r8
        repe    movsb
        ret




# Logical

DEFINE_WORD INVERT, "INVERT"    # ( x1 -- x2 )
        notq    %r8
        ret


DEFINE_WORD AND, "AND"          # ( x1 x2 -- x3 )
        andq    (%rbp), %r8
        leaq    8(%rbp), %rbp
        ret



DEFINE_WORD OR, "OR"            # ( x1 x2 -- x3 )
        orq     (%rbp), %r8
        leaq    8(%rbp), %rbp
        ret


DEFINE_WORD XOR, "XOR"          # ( x1 x2 -- x3 )
        xorq    (%rbp), %r8
        leaq    8(%rbp), %rbp
        ret


# Comparisons

DEFINE_WORD ZeroSup, "0>"       # ( n -- flag )
        cmp     $0, %r8
        setng   %al             # if <= 0 then 1 - 1 = 0  = FALSE
        dec     %al             # if >  0 then 0 - 1 = -1 = TRUE
        movsx   %al, %r8        # Sign extend
        ret


DEFINE_WORD ZeroEq, "0="        # ( n -- flag )
        cmp     $0, %r8
        setnz   %al             # if <= 0 then 1 - 1 = 0  = FALSE
        dec     %al             # if >  0 then 0 - 1 = -1 = TRUE
        movsx   %al, %r8        # Sign extend
        ret


## IO

DEFINE_WORD EMIT, "EMIT"        # ( char -- )
        movq    %r8, %rax
        DPOP    %r8
_EMIT:  # %al:char
        movb    %al, .Lemit_buf
        movq    $SYS_WRITE,  %rax
        movq    $STDOUT,     %rdi
        movq    $.Lemit_buf, %rsi
        movq    $1,          %rdx
        syscall
        ret
.Lemit_buf: .byte 0


DEFINE_WORD CR, "CR"            # ( -- )
        movb    $'\r', %al
        call    _EMIT
        movb    $'\n', %al
        call    _EMIT
        ret


DEFINE_WORD TYPE, "TYPE"        # ( c-addr u -- )
        movq    %r8, %rdx
        movq    0(%rbp), %rsi
        movq    8(%rbp), %r8
        addq    $16, %rbp
_TYPE: # ( -- ) %rsi : c-addr, %rdx : u
        movq    $SYS_WRITE, %rax
        movq    $STDOUT, %rdi
        syscall
        ret


DEFINE_WORD KEY, "KEY"          # ( -- char )
        DPUSH   %r8
        DPUSH   $0,             # Alloc
        movq    $SYS_READ,  %rax
        movq    $STDIN,     %rdi
        movq    %rbp,       %rsi
        movq    $1,         %rdx
        syscall
        DPOP    %r8
        ret


DEFINE_WORD Dot, "."            # ( u -- ) Print hex unsigned with leading 0s
        movq    $16, %rcx
        movq    %r8, %rax
        DPOP    %r8
.Ldot_loop:
        rolq    $4, %rax
        movq    %rax, %rbx
        andq    $0x0f, %rbx
        addb    $'0', %bl
        cmpb    $'9', %bl
        jbe     .Ldot_emit
        addb    $('A'-'0'-10), %bl
.Ldot_emit:
        push    %rax
        push    %rcx
        movb    %bl, %al
        call    _EMIT
        pop     %rcx
        pop     %rax
        dec     %rcx
        jnz     .Ldot_loop
.Ldot_end:
        movb    $' ', %al
        call    _EMIT
        ret


# Compilation

DEFINE_WORD CONST_FLAG_IMM, "FLAG-IMM"
        DPUSH   %r8
        movq    $FLAG_IMM, %r8
        ret


DEFINE_WORD CONST_FLAG_HIDDEN, "FLAG-HIDDEN"
        DPUSH   %r8
        movq    $FLAG_HIDDEN, %r8
        ret


DEFINE_WORD CP, "CP"            # ( -- addr )
        DPUSH   %r8
        movq    $curr_def, %r8
        ret
curr_def: .quad user_dict


DEFINE_WORD LATEST, "LATEST"  # ( -- addr )
        DPUSH   %r8
        movq    $last_def, %r8
        ret


DEFINE_WORD STATE, "STATE"
        DPUSH   %r8
        movq    $state, %r8
        ret
state: .quad 0


DEFINE_WORD Lsq, "[", FLAG_IMM
        incq    state   # State becomes 0 = FALSE
        ret


DEFINE_WORD Rsq, "]"
        decq    state   # State becomes -1 = TRUE
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
        call    _PARSE_NAME     # FIXME: Ignore empty word ? Allows for :NONAME
        movq    %rdi, %rsi      # c-addr
        mov     %rcx, %rax      # u
        pop     %rdi

        stosb                   # Put size
        repe    movsb           # Put name

        decq    state           # Enter compilation mode

        movq    %rdi, curr_def
        ret


DEFINE_WORD Semi, ";", FLAG_IMM
        movq    curr_def, %rdi
        movb    $0xc3, (%rdi)   # Compile a ret instruction
        incq    curr_def
        call    xt_RECURSE      # Make the word visible
        incq    state           # Exit compilation mode
        ret


DEFINE_WORD RECURSE, "RECURSE", FLAG_IMM
        movq    last_def, %rdi
        andb    $(~FLAG_HIDDEN), 8(%rdi)
        ret



DEFINE_WORD IMMEDIATE, "IMMEDIATE", # ( -- )
        # Toggle imm flag
        movq    last_def, %rdi
        xorb    $FLAG_IMM, 8(%rdi)
        ret


## Parsing

DEFINE_WORD SupIN, ">IN"
        DPUSH   %r8
        movq    $source_idx, %r8
        ret
source_idx: .quad 0


DEFINE_WORD SOURCE, "SOURCE" # ( -- c-addr u )
        DPUSH   %r8
        movq    $source_buf, %r8
        DPUSH   %r8
        movq    source_cnt, %r8
        ret
source_cnt: .quad 0 # Size of the current line
source_buf:
        .skip SOURCE_SZ
        .byte 0


_REFILL: # ( -- ) Read line in source_buf, update source_idx and source_cnt
        # %rbx := Number of non special chars read (one newline -> 1)

        movb    $'>', %al       # Prompt TODO
        call    _EMIT
        movb    $' ', %al
        call    _EMIT

        xorq    %rbx, %rbx      # Total number of chars read
        movq    $source_buf, %rdi

.Lrefill_loop:
        push    %rdi            # Read one char
        call    xt_KEY
        pop     %rdi
        movq    %r8, %rax
        DPOP    %r8

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
        movb    $'\b', %al
        call    _EMIT
        movb    $' ', %al
        call    _EMIT
        movb    $'\b', %al
        call    _EMIT
        pop     %rdi
        jmp     .Lrefill_loop
.Lrefill_not_del:

        cmpb    $0x1b, %al      # Ignore escape
        je      .Lrefill_loop

        incq    %rbx

        cmpb    $'\r', %al      # Stop on newline
        je      .Lrefill_end
        cmpb    $'\n', %al
        je      .Lrefill_end

        stosb                   # Add to buffer

        push    %rdi
        call    _EMIT         # Echo
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
        movq    %r8, %rbx
        call    _PARSE
        movq    %rcx, %r8       # u
        DPUSH   %rdi            # c-addr
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
        call    xt_CR
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
        DPUSH   %r8
        movq    $' ', %r8
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


_FIND: # ( rsi:c-addr rax:u -- rbx:0|nt rdi:xt dl:flags )
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
        movq    %r8, %rax
        movq    (%rbp), %rsi
        call    _FIND
        movq    %rdi, %r8       # xt
        movq    %rbx, (%rbp)    # nt
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
        DPUSH   %r8
        movq    %rax, %r8       # Push number
        jmp     .Linterpret_loop


.Linterpret_not_found:
        movq    $.Linterpret_msg, %rsi
        movq    $.Linterpret_msg_sz, %rdx
        call    _TYPE           # Print message
        pop     %rdx            # u
        pop     %rsi            # c-addr
        call    _TYPE           # Print word
        call    xt_CR
        call    xt_ABORT        # No return
        ret

.Linterpret_msg:
        .ascii  "**NOT FOUND** "
        .equ .Linterpret_msg_sz, (.-.Linterpret_msg)


last_def:  .quad _LAST_DEF
user_dict: .skip DICT_SZ
