: \ 0A PARSE DROP DROP ; IMMEDIATE
: ( 29 PARSE DROP DROP ; IMMEDIATE

\ TODO
\ DEFER
\ Primitives / Arithmetic / Comparisons
\ Double
\ For loops
\ CASE
\ Numeric output <# # #S #>
\ Numeric input >NUMBER and standard number parser
\ ENVIRONMENT?


\ Utilities ====================================================================

: /   ( x1 x2 -- x3 ) /MOD NIP  ;
: MOD ( x1 x2 -- x3 ) /MOD DROP ;

: =   ( x1 x2 -- flag ) - 0= ;

: CELL+ 8 + ;
: CELLS 8 * ;
: CHARS ;
: CHAR+ 1+ ;

: HERE  (   -- addr ) CP @ ;
: ALLOT ( n --      ) CP +! ;
: PAD   (   -- addr ) SP@ 100 + ;
: COUNT ( c-addr1 -- c-addr2 u ) DUP CHAR+ SWAP C@ ;

: 2DROP ( x1 x2 --             ) DROP DROP ;
: 2DUP  ( x1 x2 -- x1 x2 x1 x2 ) OVER OVER ;


\ Control flow =================================================================

: <resolve ( C: ori      --     ) (   -- ) >R HERE 4 - R@ - R> H! ;
: resolve> ( C: dest ori --     ) (   -- ) >R HERE - R> H! ;
: branch   ( C:          -- ori ) (   -- ) E9 C, HERE 99 C, 99 C, 99 C, 99 C, ;
: branch0  ( C:          -- ori ) ( x -- )
    4D C, 85 C, C0 C,       \ test %r8, %r8
    4C C, 8B C, 45 C, 00 C, \ mov (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ lea 8(%rbp), %rbp
    \ 48 C, 83 C, C5 C, 08 C, \ add $8, %rbp
    0F C, 84 C,             \ jz rel32
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


\ POSTPONE =====================================================================

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



\ DO loops =====================================================================

: DO? ( C: -- dest ) ( l i -- ) ( R: -- l i ) ; IMMEDIATE

: DO  ( C: -- dest ) ( l i -- ) ( R: -- l i )
    POSTPONE SWAP
    POSTPONE >R
    POSTPONE >R
    HERE
; IMMEDIATE

: I ( -- idx ) ( R: lim idx ret -- lim idx ) [
    48 C, 83 C, ED C, 08 C,       \ lea    -8(%rbp), %rbp
    4C C, 89 C, 45 C, 00 C,       \ mov    r8, (%rbp)
    4C C, 8B C, 44 C, 24 C, 08 C, \ mov    8(%rsp), %r8
] ;

: J ( -- i1 ) ( R: l1 i1 l2 i2 ret -- l1 i1 l2 i2 ret ) [
    48 C, 83 C, ED C, 08 C,       \ lea    -8(%rbp), %rbp
    4C C, 89 C, 45 C, 00 C,       \ mov    r8, (%rbp)
    4C C, 8B C, 44 C, 24 C, 18 C, \ mov    24(%rsp), %r8
] ;


: LOOP  ( C: dest -- ) ( -- ) ( R: lim i1 -- | lim i2 )
        POSTPONE R> POSTPONE 1+ POSTPONE R> ( i2 lim )
        POSTPONE 2DUP ( i2 lim i2 lim )
        POSTPONE >R POSTPONE >R ( i2 lim ) ( R: lim i2 )
        POSTPONE =              ( flag )
        branch0 resolve>
        POSTPONE R> POSTPONE DROP
        POSTPONE R> POSTPONE DROP
; IMMEDIATE

: LOOP+ ( C: do -- ) ( n -- ) ( R: loop1 -- |loop2 ) ; IMMEDIATE

: LEAVE  ( -- ) ( R: loop -- ) ;
: UNLOOP ( -- ) ( R: loop -- ) ;



\ Strings ======================================================================

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

: S" ( C: "ccc<quote>" -- ) ( -- c-addr u )
    [CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE

: ." ( C: "ccc<quote>" -- ) ( --          )
    POSTPONE S" POSTPONE TYPE ; IMMEDIATE

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


\ CREATE DOES> =================================================================

: >BODY ( xt -- a-addr ) 6 + ;

: >code ( nt -- xt     ) 9 + ( len-addr ) DUP C@ ( len-addr len ) + 1+ ;

: docreate ( -- a-addr ) R> 1+ ; \ 1+ to skip the ret
    \ NOTE: The ret is never executed and we return to child's caller

: dodoes ( -- ) ( R: ret -- )
    R> \ Start of child-code, popping the return address will exit from parent
    CP @ >R
    LATEST @ >code CP ! COMPILE, \ Redirect child's call
    R> CP !
;

: CREATE   ( "<spaces>name" -- ) ( -- a-addr )
    \ parent: <parent-code> <ret>
    \ docreate: Push caller's PFA <ret>
    \ <child-header> <call docreate> <data>  (call doesn't return)
    : ['] docreate COMPILE, POSTPONE ;
;

: DOES> ( -- ) ( R: ret -- )
    \ <parent-header> <parent-code> <dodoes> <child-code> <ret>
    \                                        ^
    \                   +--------------------+
    \                   |
    \ <child-header> <call child-code> <data> (call doesn't return)
    POSTPONE dodoes
    \ child-code starts here
    POSTPONE R> POSTPONE 1+ \ Compile code to get child's PFA
; IMMEDIATE

: CONSTANT ( x "<spaces>name" -- ) ( -- x      ) : POSTPONE LITERAL POSTPONE ; ;
: VARIABLE (   "<spaces>name" -- ) ( -- a-addr ) CREATE 0 , ;


\ DEFER ========================================================================

: EXECUTE ( i*x xt -- j*x ) [
    4C C, 89 C, C0 C,       \ mov   %r8, %rax
    4C C, 8B C, 45 C, 00 C, \ mov   (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ lea   8(%rbp), %rbp
    FF C, D0 C,             \ call  *%rax
] ;

: DEFER  (    "<spaces>name" --     ) CREATE 0 , DOES> @ EXECUTE ;
: DEFER@ (            xt1    -- xt2 ) >BODY @ ;
: DEFER! (        xt2 xt1    --     ) >BODY ! ;

: IS
    STATE @ IF
        POSTPONE ['] POSTPONE DEFER!
    ELSE
        ' DEFER!
    THEN
; IMMEDIATE

: ACTION-OF
    STATE @ IF
        POSTPONE ['] POSTPONE DEFER@
    ELSE
        ' DEFER@
    THEN
; IMMEDIATE


\ Tools ========================================================================

: DEPTH ( -- +n ) SP@ S0 SWAP - 8 / ; \ TODO: use 2/ 2/ 2/ instead
: .S ( -- )
    DEPTH IF
        DEPTH BEGIN S0 OVER 1+ CELLS - @ CR . 1- DUP 0= UNTIL DROP
    ELSE
       CR ." EMPTY "
    THEN
;
        DEPTH .

: ? ( addr -- ) @ . ;
: NAME>STRING ( nt -- c-addr u ) 9 + DUP 1+ SWAP C@ ;
: SEE ( "<spaces>ccc<space>" -- )
    PARSE-NAME FIND ( xt nt )
        [ DEPTH . ]
    CR ." prev:   " OVER @ NAME>STRING TYPE
    CR ." imm:    " OVER 8 + C@ FLAG-IMM AND IF
        ." yes"
        [ DEPTH . ]
        ELSE
        ." no"
        [ DEPTH . ]
        THEN
    CR ." xt:     " .
    CR ." nt:     " .
;

: BREAK CC C, 90 C, ; IMMEDIATE \ Int3 / Breakpoint / Sigtrap on Linux


\ Consts and vars  =============================================================

0 1 - CONSTANT TRUE
00 CONSTANT FALSE
20 CONSTANT BL
: SPACE BL EMIT ;


\ Number IO ====================================================================

VARIABLE BASE
: HEX     ( -- ) 10 BASE ! ;
: DECIMAL ( -- ) 0A BASE ! ;
DECIMAL


\ ==============================================================================


\ Interpreter ==================================================================


: TEST 10 0 DO CR ." I: " I . ." D: " DEPTH . LOOP ;
