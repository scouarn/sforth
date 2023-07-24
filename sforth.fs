: \ 0A PARSE DROP DROP ; IMMEDIATE \ Now we can comment
: ( 29 PARSE DROP DROP ; IMMEDIATE ( Now we have both types of comments )

\ TODO
\ Primitives / Arithmetic / Comparisons
\ For loops
\ CASE
\ DEFER
\ CONSTANT / VARIABLE / CREATE / DOES> ...
\ Numeric output


: HERE  (   -- addr ) CP @ ;
: ALLOT ( n --      ) CP +! ;
: PAD   (   -- addr ) SP@ 100 + ;
: COUNT ( c-addr1 -- c-addr2 u ) DUP CHAR+ SWAP C@ ;


: <resolve ( C: ori      --     ) (   -- ) >R HERE 4 - R@ - R> H! ;
: resolve> ( C: dest ori --     ) (   -- ) >R HERE - R> H! ;
: branch   ( C:          -- ori ) (   -- ) E9 C, HERE 99 C, 99 C, 99 C, 99 C, ;
: branch0  ( C:          -- ori ) ( x -- )
    4D C, 85 C, C0 C,       \ TEST %r8, %r8
    4C C, 8B C, 45 C, 00 C, \ MOV (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ LEA 8(%rbp), %rbp
    0F C, 84 C,             \ JZ rel32
    HERE 4 ALLOT            \ rel32
;

: AHEAD  ( C:      -- ori      ) (   -- ) branch                    ; IMMEDIATE
: IF     ( C:      -- ori      ) ( x -- ) branch0                   ; IMMEDIATE
: ELSE   ( C: ori1 -- ori2     ) (   -- ) >R branch R> <resolve     ; IMMEDIATE
: THEN   ( C: ori  --          ) (   -- ) <resolve                  ; IMMEDIATE
: BEGIN  ( C:      -- dest     ) (   -- ) HERE                      ; IMMEDIATE
: AGAIN  ( C: dest --          ) (   -- ) branch  resolve>          ; IMMEDIATE
: UNTIL  ( C: dest --          ) ( x -- ) branch0 resolve>          ; IMMEDIATE
: WHILE  ( C: dest -- ori dest ) ( x -- ) branch0 SWAP              ; IMMEDIATE
: REPEAT ( C: ori dest --      ) (   -- ) branch  resolve> <resolve ; IMMEDIATE


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
    >R
    POSTPONE AHEAD          \ Skip string data
    R@ HERE >R ALLOT        \ Allocate string and save it's addr
    POSTPONE THEN
    R@ POSTPONE LITERAL R>  \ Push c-addr2
    R@ POSTPONE LITERAL R>  \ Push u
    ( c-addr1 c-addr2 u ) MOVE \ Copy string data
; IMMEDIATE

: S" ( C: "ccc<quote>" -- ) ( -- c-addr u ) [CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE
: ." ( C: "ccc<quote>" -- ) ( --          ) POSTPONE S" POSTPONE TYPE        ; IMMEDIATE
: .( (    "ccc<quote>" -- ) ( --          ) [CHAR] ) PARSE TYPE ;
: C" ( C: "ccc<quote>" -- ) ( -- c-addr   )
    [CHAR] " PARSE >R
    POSTPONE AHEAD          \ Skip string data
    R@ HERE >R              \ Keep addr of counted string
    DUP C, ALLOT            \ Store length and allocate chars
    POSTPONE THEN
    R@ POSTPONE LITERAL     \ Push c-addr2
    R> CHAR+ R> ( c-addr1 c-addr3 u ) MOVE \ Copy string data
; IMMEDIATE

: ABORT" ( "ccc<quote>" -- ) ( i*x x -- | i*x ) ( R: j*x -- | j*x )
    POSTPONE IF
    POSTPONE ."
    POSTPONE TYPE
    POSTPONE CR
    POSTPONE ABORT
    POSTPONE THEN
; IMMEDIATE


: / ( x1 x2 -- x3 ) /MOD NIP ;
: DEPTH ( -- +n ) SP@ S0 SWAP - 8 / ; \ TODO: use 2/ 2/ 2/ instead
: ? ( addr -- ) @ . ;
: SEE ( "<spaces>ccc<space>" -- ) PARSE-NAME FIND . . ;
: .S ( x * i -- ) DEPTH IF BEGIN DEPTH 0> WHILE CR . REPEAT ELSE ." EMPTY " THEN ;


: docreate ( -- a-addr ) R> 1+ ; \ 1+ to skip the ret
    \ NOTE: The ret is never executed and we return to child's caller

: CREATE   ( "<spaces>name" -- ) ( -- a-addr )
    \ parent: <parent-code> <ret>
    \ docreate: Push caller's PFA <ret>
    \ <child-header> <call docreate> <data>  (call doesn't return)
    : ['] docreate COMPILE, POSTPONE ;
;

: >BODY ( xt -- a-addr ) 6 + ;
: >code ( nt -- xt     ) 9 + ( len-addr ) DUP C@ ( len-addr len ) + 1+ ;

: dodoes ( -- ) ( R: ret -- )
    R> \ Start of child-code, popping the return address will exit from parent
    CP @ >R
    LATEST @ >code CP ! COMPILE, \ Redirect child's call
    R> CP !
;

: DOES> ( -- ) ( R: ret -- )
    \ <parent-header> <parent-code> <dodoes> <child-code> <ret>
    \                                        ^
    \                   +--------------------+
    \                   |
    \ <child-header> <call child-code> <data> (call doesn't return)
    POSTPONE dodoes
    \ Start of child-code
    POSTPONE R> POSTPONE 1+ \ Compile code to get child's PFA (inlined DOCREATE)
; IMMEDIATE


\ : CONSTANT ( x "<spaces>name" -- ) ( -- x ) CREATE , DOES> @ ;
: CONSTANT ( x "<spaces>name" -- ) ( -- x ) : POSTPONE LITERAL POSTPONE ; ;

: VARIABLE (   "<spaces>name" -- ) ( -- a-addr ) CREATE 0 , ;
\ : VARIABLE (   "<spaces>name" -- ) ( -- a-addr ) : HERE 13 + POSTPONE LITERAL POSTPONE ; 0 , ;

0 1 - CONSTANT TRUE
00 CONSTANT FALSE
20 CONSTANT BL
: SPACE BL EMIT ;

VARIABLE BASE
: HEX     ( -- ) 10 BASE ! ;
: DECIMAL ( -- ) 0A BASE ! ;
HEX

\ Tests

CREATE CR0 .S
' CR0 >BODY HERE .S

: DOES1 DOES> @ 1 + ; .S
: DOES2 DOES> @ 2 + ; .S
CREATE CR1 .S
CR1 HERE .S
1 , .S
CR1 @ .S
DOES1 .S
CR1 .S
DOES2 .S
CR1 .S

: WEIRD: CREATE DOES> 1 + DOES> 2 + ; .S
WEIRD: W1 .S
' W1 >BODY HERE .S
W1 HERE 1 + .S
W1 HERE 2 + .S
