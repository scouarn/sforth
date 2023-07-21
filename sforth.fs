HEAD : ] HEAD ] [ C3 C, HIDE
HEAD ; ] C3 C, HIDE 0 STATE ! [ C3 C, HIDE IMMEDIATE

: \ 0A PARSE 2DROP ; IMMEDIATE \ Now we can comment
: ( 29 PARSE 2DROP ; IMMEDIATE ( Now we have both types of comments )

\ We compiled a word called ':' that calls HEAD and enters compilation mode,
\ then we appended a ret instruction and rendered it visible with HIDE.
\ We compiled ';' that compiles a ret instruction, calls HIDE and disable
\ compilation. We added a ret and set it visible and immediate.



\ 48 C, 83 C, C5 C, 08 C, \ SUB $8, %rbp      ADD r/m64, imm8
\ 48 C, 83 C, ED C, 08 C, \ SUB $8, %rbp      SUB r/m64, imm8
\ 48 C, 8b C, 45 C, 00 C, \ MOV (%rbp), %rax  MOV r64, r/m64

: HERE CP @ ;

: BL 20 ;
: SPACE 20 . ;
: CR 0D EMIT 0A EMIT ;

: TRUE FFFFFFFFFFFFFFFF ;
: FALSE 0 ;

: PAD ( -- addr ) SP@ 100 + ;

: COUNT ( c-addr1 -- c-addr2 u ) DUP CHAR+ SWAP C@ ;



: IF ( C: -- ori ) ( x -- )
    48 C, 8B C, 45 C, 00 C, \ MOV (%rbp), %rax  MOV r64, r/m64
    48 C, 83 C, C5 C, 08 C, \ ADD $8, %rbp      SUB r/m64, imm8
    48 C, 85 C, C0 C,       \ TEST %rax, %rax   TEST r/m64, r64
    0F C, 84 C,             \ JZ rel32
    HERE
    99 C, 99 C, 99 C, 99 C, \ rel32
; IMMEDIATE

: THEN ( C: ori -- )
    DUP HERE SWAP - 4 - ( ori rel32 )
    SWAP H! \ write rel32 to ori
; IMMEDIATE

: ELSE ( C: ori1 -- ori2 ) ( -- )
    >R

    E9 C,                   \ JMP rel32
    HERE                    \ ori2
    99 C, 99 C, 99 C, 99 C, \ rel32

    R>

    DUP HERE SWAP - 4 - ( ori2 ori1 rel32 )
    SWAP H! \ write rel32 to ori1
; IMMEDIATE


: BEGIN ( C: -- dest ) ( -- )
    HERE
; IMMEDIATE

: AGAIN ( C: dest -- ) ( -- )
    E9 C,                   \ JMP rel32
    HERE 4 + -              ( rel32 )
    HERE H! 4 CP +!         \ write rel32
; IMMEDIATE

: UNTIL ( C: dest -- ) ( x -- )
    48 C, 8B C, 45 C, 00 C, \ MOV (%rbp), %rax  MOV r64, r/m64
    48 C, 83 C, C5 C, 08 C, \ ADD $8, %rbp      SUB r/m64, imm8
    48 C, 85 C, C0 C,       \ TEST %rax, %rax   TEST r/m64, r64
    0F C, 84 C,             \ JZ rel32

    HERE 4 + -  ( rel32 )
    HERE H!     \ write rel32
    4 CP +!
; IMMEDIATE

: WHILE ( C: dest -- orig dest ) ( x -- )
    48 C, 8B C, 45 C, 00 C, \ MOV (%rbp), %rax  MOV r64, r/m64
    48 C, 83 C, C5 C, 08 C, \ ADD $8, %rbp      SUB r/m64, imm8
    48 C, 85 C, C0 C,       \ TEST %rax, %rax   TEST r/m64, r64
    0F C, 84 C,             \ JZ rel32
    HERE SWAP               ( dest -- orig dest )
    99 C, 99 C, 99 C, 99 C, \ rel32
; IMMEDIATE

: REPEAT ( C: dest -- orig dest ) ( -- )

    \ Compile a jump to dest
    E9 C,                   \ JMP rel32
    HERE 4 + -              ( rel32 )
    HERE H! 4 CP +!         \ write rel32

    \ Resolve backward reference to orig
    DUP HERE SWAP - 4 - ( orig rel32 )
    SWAP H!             \ write rel32 to orig
; IMMEDIATE


: / ( x1 x2 -- x3 ) /MOD NIP ;
: DEPTH ( -- +n ) SP@ S0 SWAP - 8 / ;

: ? ( addr -- ) @ . ;
: SEE ( "<spaces>ccc<space>" -- ) PARSE-NAME FIND . . ;
: .S ( x * i -- ) BEGIN DEPTH 0> WHILE CR . REPEAT ;

: .( 29 PARSE TYPE ;


