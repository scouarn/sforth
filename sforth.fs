HEAD : ] HEAD ] [ C3 C, HIDE
HEAD ; ] C3 C, HIDE 0 STATE ! [ C3 C, HIDE IMMEDIATE

: \ 0A PARSE DROP DROP ; IMMEDIATE \ Now we can comment
: ( 29 PARSE DROP DROP ; IMMEDIATE ( Now we have both types of comments )


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


: AHEAD ( C: -- ori ) ( -- )
    E9 C,                   \ JMP rel32
    HERE                    \ ori
    99 C, 99 C, 99 C, 99 C, \ rel32
; IMMEDIATE

: IF ( C: -- ori ) ( x -- )
    4D C, 85 C, C0 C,       \ TEST %r8, %r8
    4C C, 8B C, 45 C, 00 C, \ MOV (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ LEA 8(%rbp), %rbp
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
    4D C, 85 C, C0 C,       \ TEST %r8, %r8
    4C C, 8B C, 45 C, 00 C, \ MOV (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ LEA 8(%rbp), %rbp
    0F C, 84 C,             \ JZ rel32
    HERE 4 + -  ( rel32 )
    HERE H!     \ write rel32
    4 CP +!
; IMMEDIATE

: WHILE ( C: dest -- orig dest ) ( x -- )
    4D C, 85 C, C0 C,       \ TEST %r8, %r8
    4C C, 8B C, 45 C, 00 C, \ MOV (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ LEA 8(%rbp), %rbp
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

: CHAR   ( "<spaces>name" -- char )           PARSE-NAME DROP C@ ;
: [CHAR] ( C: "<spaces>name" -- ) ( -- char ) CHAR POSTPONE LITERAL ; IMMEDIATE



: SLITERAL ( C: c-addr1 u ) ( -- c-addr2 u )
    DUP >R
    POSTPONE AHEAD          \ Skip string data
    R> HERE >R ALLOT        \ Allocate string
    POSTPONE THEN
    R@  POSTPONE LITERAL    \ Push c-addr2
    DUP POSTPONE LITERAL    \ Push u
    ( c-addr1 u ) R> SWAP ( c-addr1 c-addr2 u ) MOVE \ Copy string data
; IMMEDIATE

: C" ( C: "ccc<quote>" -- ) ( -- c-addr ) \ Counted string !
    [CHAR] " PARSE
    DUP >R
    POSTPONE AHEAD          \ Skip string data
    R> HERE >R
    DUP C,                  \ Store length
    ALLOT                   \ Allocate string
    POSTPONE THEN
    R@ POSTPONE LITERAL     \ Push c-addr2
    R> CHAR+ SWAP ( c-addr1 c-addr3 u ) MOVE \ Copy string data
; IMMEDIATE

: S" ( C: "ccc<quote>" -- ) ( -- c-addr u ) [CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE
: ." ( C: "ccc<quote>" -- ) ( -- )          POSTPONE S" POSTPONE TYPE        ; IMMEDIATE
: .( (    "ccc<quote>" -- ) ( -- )          [CHAR] ) PARSE TYPE ;

: ABORT" ( C: "ccc<quote>" -- ) ( i*x x -- | i*x ) ( R: j*x -- | j*x )
    POSTPONE IF
    POSTPONE ."
    POSTPONE TYPE
    POSTPONE CR
    POSTPONE ABORT
    POSTPONE THEN
; IMMEDIATE

