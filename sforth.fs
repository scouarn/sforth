HEAD : ] HEAD ] [ C3 C, HIDE
HEAD ; ] C3 C, HIDE 0 STATE ! [ C3 C, HIDE IMMEDIATE

: \ 0A PARSE 2DROP ; IMMEDIATE \ Now we can comment
: ( 29 PARSE 2DROP ; IMMEDIATE ( Now we have both types of comments )


\ : IMMEDIATE ( -- ) LATEST @ 8 + DUP C@ FLAG-IMM    OR SWAP C! ;
\ : RECURSE   ( -- ) LATEST @ 8 + DUP C@ FLAG-HIDDEN OR SWAP C! ; IMMEDIATE
: RECURSE HIDE ; IMMEDIATE


: HERE  (   -- addr ) CP @ ;
: ALLOT ( n --      ) CP +! ;
: PAD   (   -- addr ) SP@ 100 + ;
: COUNT ( c-addr1 -- c-addr2 u ) DUP CHAR+ SWAP C@ ;

: TRUE FFFFFFFFFFFFFFFF ;
: FALSE 0 ;
: BL 20 ;
: SPACE 20 . ;
: CR 0D EMIT 0A EMIT ;


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
: DEPTH ( -- +n ) SP@ S0 SWAP - 8 / ; \ TODO: use 2/ 2/ 2/ instead

: ? ( addr -- ) @ . ;
: SEE ( "<spaces>ccc<space>" -- ) PARSE-NAME FIND . . ;
: .S ( x * i -- ) BEGIN DEPTH 0> WHILE CR . REPEAT ;

: .( 29 PARSE TYPE ;


: ' ( "<spaces>name" -- xt ) PARSE-NAME FIND NIP ;

: POSTPONE ( "<spaces>name" -- )
    PARSE-NAME FIND SWAP ( xt nt )
    8 + C@ FLAG-IMM AND \ Is imm?
    IF \ Compile code that executes xt
        COMPILE,
    ELSE \ Compile code that compiles xt (default compilation behaviour)

        [ ' LITERAL  COMPILE, ]          \ Compile DPUSH $xt
        [ ' COMPILE, LITERAL  ] COMPILE, \ Compile COMPILE,
    THEN
; IMMEDIATE


: ['] ( C: "<spaces>name" -- ) ( -- xt ) ' POSTPONE LITERAL ; IMMEDIATE
: [COMPILE] ( C: "<spaces>name" -- ) ' COMPILE, ; IMMEDIATE

: CHAR PARSE-NAME DROP C@ ;
: [CHAR] POSTPONE CHAR ; IMMEDIATE


: GT1  AAAA CR . ;
: GT4 POSTPONE GT1 ; IMMEDIATE
: GT5 GT4 ;
GT5

: GT6 BBBB CR . ; IMMEDIATE
: GT7 POSTPONE GT6 ;
GT7

