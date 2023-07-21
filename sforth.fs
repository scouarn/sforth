HEAD : ] HEAD ] [ C3 C, HIDE
HEAD ; ] C3 C, HIDE 0 STATE ! [ C3 C, HIDE IMMEDIATE

: \ 0A WORD DROP ; IMMEDIATE \ Now we can comment
: ( 29 WORD DROP ; IMMEDIATE ( Now we have both types of comments )

\ We compiled a word called ':' that calls HEAD and enters compilation mode,
\ then we appended a ret instruction and rendered it visible with HIDE.
\ We compiled ';' that compiles a ret instruction, calls HIDE and disable
\ compilation. We added a ret and set it visible and immediate.

\ 48 C, 83 C, C5 C, 08 C, \ SUB $8, %rbp      ADD r/m64, imm8
\ 48 C, 83 C, ED C, 08 C, \ SUB $8, %rbp      SUB r/m64, imm8
\ 48 C, 8b C, 45 C, 00 C, \ MOV (%rbp), %rax  MOV r64, r/m64

: HERE CP @ ;

: BL 20 ;

: SEE BL WORD FIND . . ;

: IF ( C: -- ori ) ( flag -- )
    48 C, 83 C, C5 C, 08 C, \ ADD $8, %rbp      SUB r/m64, imm8
    48 C, 8B C, 45 C, 00 C, \ MOV (%rbp), %rax  MOV r64, r/m64
    48 C, 85 C, C0 C,       \ TEST %rax, %rax   TEST r/m64, r64
    0F C, 84 C,             \ JZ rel32
    HERE
    99 C, 99 C, 99 C, 99 C, \ rel32
; IMMEDIATE


: THEN ( C: ori -- )
    DUP HERE SWAP - 4 - ( ori rel32 ) HERE .
    SWAP H! \ write rel32 to ori
; IMMEDIATE


