: DROP [ 4C C, 8B C, 45 C, 00 C, 48 C, 8D C, 6D C, 08 C, ] ;
: \ 0A PARSE DROP DROP ; IMMEDIATE
: ( 29 PARSE DROP DROP ; IMMEDIATE

\ TODO
\ SEE when not found...
\ Memory and stack primitives
\ Double
\ For loops
\ CASE
\ Numeric output <# # #S #>
\ Numeric input >NUMBER and standard number parser
\ ENVIRONMENT?

\ Stack ========================================================================

: sp+ 48 C, 8D C, 6D C, 08 C, ; \ lea    8(%rbp), %rbp
: sp- 48 C, 8D C, 6D C, F8 C, ; \ lea   -8(%rbp), %rbp

\ TODO how is the reg encoded ?
\ : >reg ( off reg ) [ ?? C, 8B C, 45 C, C, ] ; \ mov off(%rbp), reg
\ : reg> ( off reg ) [ ?? C, 89 C, 45 C, C, ] ; \ mov reg, off(%rbp)

\ : DROP ( x  --   ) [ 4C C, 8B C, 45 C, 00 C, sp+ ] ; \ mov (%rbp), %r8
: DUP  ( x     -- x x      ) [ sp- 4C C, 89 C, 45 C, 00 C, ] ; \ mov %r8, (%rbp)
: SWAP ( x1 x2 -- x2 x1    ) [ 4C C, 87 C, 45 C, 00 C, ] ; \ xchg %r8, (%rbp)
: NIP  ( x1 x2   -- x2     ) [ sp+ ] ;
: OVER ( x1 x2 -- x1 x2 x1 ) [
    sp- 4C C, 89 C, 45 C, 00 C, \ mov %r8, (%rbp)
        4C C, 8B C, 45 C, 08 C, \ mov 8(%rbp), %r8
] ;

: TUCK ( x1 x2   -- x2     ) [
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax
    4C C, 89 C, 45 C, 00 C, \ mov     %r8, (%rbp)
    sp-
    48 C, 89 C, 45 C, 00 C, \ mov     %rax, (%rbp)
] ;

: ROT  ( x1 x2 x3 -- x2 x3 x1 ) [
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax
    4C C, 89 C, 45 C, 00 C, \ mov     %r8, (%rbp)
    4C C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %r8
    48 C, 89 C, 45 C, 08 C, \ mov     %rax, 8(%rbp)
] ;

: -ROT ( x1 x2 x3 -- x2       ) [
    48 C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %rax
    4C C, 89 C, 45 C, 08 C, \ mov     %r8, 8(%rbp)
    4C C, 8B C, 45 C, 00 C, \ mov     (%rbp), %r8
    48 C, 89 C, 45 C, 00 C, \ mov     %rax, (%rbp)
] ;




\ Arithmetic ===================================================================

: + ( n1 | u1 n2 | u2 -- n3 | u3 ) [
    4C C, 03 C, 45 C, 00 C,     \ add   (%rbp), %r8
    48 C, 8D C, 6D C, 08 C,     \ lea   8(%rbp), %rbp
] ;

: - ( n1 | u1 n2 | u2 -- n3 | u3 ) [
    4C C, 2B C, 45 C, 00 C,     \ sub   0x0(%rbp),%r8
    48 C, 8D C, 6D C, 08 C,     \ lea   0x8(%rbp),%rbp
    49 C, F7 C, D8 C,           \ neg   %r8
] ;

: NEGATE ( n1 -- n2 ) [ 49 C, F7 C, D8 C, ] ; \ neg   %r8

: 1+ ( n1 | u1 -- n2 | u2 ) [ 49 C, FF C, C0 C, ] ; \ incq    %r8
: 1- ( n1 | u1 -- n2 | u2 ) [ 49 C, FF C, C8 C, ] ; \ dec     %r8

: * ( n1 n2 -- n3 ) [
    4C C, 0F C, AF C, 45 C, 00 C,   \ imul  (%rbp), %r8     %r8 := n1 * n2
    48 C, 8D C, 6D C, 08 C,         \ leaq  8(%rbp), %rbp
] ;

: /MOD ( n1 n2 -- rem quot ) [
    48 C, 8B C, 45 C, 00 C,     \ movq  (%rbp), %rax        n1
    48 C, 99 C,                 \ cqto
    49 C, F7 C, F8 C,           \ idiv  %r8
    49 C, 89 C, C0 C,           \ movq  %rax, %r8           quot
    48 C, 89 C, 55 C, 00 C,     \ movq  %rdx, (%rbp)        rem
] ;

: /   ( x1 x2 -- x3 ) /MOD NIP  ;
: MOD ( x1 x2 -- x3 ) /MOD DROP ;


\ Logical ======================================================================

: INVERT ( x1 -- x2 ) [ 49 C, F7 C, D0 C, ] ; \ not %r8

: AND ( x1 x2 -- x3 ) [
    4C C, 23 C, 45 C, 00 C,     \ and   (%rbp), %r8
    48 C, 8D C, 6D C, 08 C,     \ lea   8(%rbp), %rbp
] ;


: OR ( x1 x2 -- x3 ) [
    4C C, 0B C, 45 C, 00 C,     \ or    (%rbp), %r8
    48 C, 8D C, 6D C, 08 C,     \ lea   8(%rbp), %rbp
] ;


: XOR ( x1 x2 -- x3 ) [
    4C C, 33 C, 45 C, 00 C,     \ xor   (%rbp), %r8
    48 C, 8D C, 6D C, 08 C,     \ lea   8(%rbp), %rbp
] ;


\ Comparisons ==================================================================

: cc-o  0 ; : cc-no 1 ; : cc-b  2 ; : cc-ae 3 ;
: cc-e  4 ; : cc-ne 5 ; : cc-be 6 ; : cc-a  7 ;
: cc-s  8 ; : cc-ns 9 ; : cc-pe A ; : cc-po B ;
: cc-l  C ; : cc-ge D ; : cc-le E ; : cc-g  F ;

: 0cmp, ( cond -- ) ( n -- flag )
    \ Invert cond and dec: if false then -1 (true) else 0 (false)
    1 XOR 90 OR
    49 C, 83 C, F8 C, 00 C,     \ cmp     $0, %r8
    0F C,    C, C0 C,           \ setcc   %al
    FE C, C8 C,                 \ dec     %al
    4C C, 0F C, BE C, C0 C,     \ movsx   %al, %r8  # Sign extend
;

: cmp, ( cond -- ) ( n1 n2 -- flag )
    \ Invert cond and dec: if false then -1 (true) else 0 (false)
    1 XOR 90 OR
    4C C, 39 C, 45 C, 00 C,     \ cmp     %r8, (%rbp)
    0F C,    C, C0 C,           \ setcc   %al
    FE C, C8 C,                 \ dec     %al
    4C C, 0F C, BE C, C0 C,     \ movsx   %al, %r8  # Sign extend
    48 C, 8D C, 6D C, 08 C,     \ lea     8(%rbp), %rbp
;

: 0=  (     n -- flag ) [ cc-e  0cmp, ] ;
: 0<> (     n -- flag ) [ cc-ne 0cmp, ] ;
: 0>  (     n -- flag ) [ cc-g  0cmp, ] ;
: 0<  (     n -- flag ) [ cc-l  0cmp, ] ;
: =   ( x1 x2 -- flag ) [ cc-e   cmp, ] ;
: <>  ( x1 x2 -- flag ) [ cc-ne  cmp, ] ;
: >   ( x1 x2 -- flag ) [ cc-g   cmp, ] ;
: <   ( x1 x2 -- flag ) [ cc-l   cmp, ] ;
: U>  ( x1 x2 -- flag ) [ cc-a   cmp, ] ;
: U<  ( x1 x2 -- flag ) [ cc-b   cmp, ] ;
: U>= ( x1 x2 -- flag ) [ cc-ae  cmp, ] ;
: U<= ( x1 x2 -- flag ) [ cc-be  cmp, ] ;
: >=  ( x1 x2 -- flag ) [ cc-ge  cmp, ] ;
: <=  ( x1 x2 -- flag ) [ cc-le  cmp, ] ;
: 0>= (     n -- flag ) [ cc-ge 0cmp, ] ;
: 0<= (     n -- flag ) [ cc-le 0cmp, ] ;


\ Utilities ====================================================================

: CELL+ 8 + ;
: CELLS 8 * ;
: CHARS ;
: CHAR+ 1+ ;

: HERE  (   -- addr ) CP @ ;
: ALLOT ( n --      ) CP +! ;
: PAD   (   -- addr ) SP@ 100 - ;
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
    48 C, 8D C, 6D C, 08 C, \ lea 8(%rbp), %rbp     -> Use lea to keep flags
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
    48 C, 83 C, ED C, 08 C,       \ sub    $8, %rbp
    4C C, 89 C, 45 C, 00 C,       \ mov    r8, (%rbp)
    4C C, 8B C, 44 C, 24 C, 08 C, \ mov    8(%rsp), %r8
] ;

: J ( -- i1 ) ( R: l1 i1 l2 i2 ret -- l1 i1 l2 i2 ret ) [
    48 C, 83 C, ED C, 08 C,       \ add    $8, %rbp
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

: ." ( C: "ccc<quote>" -- ) ( -- ) POSTPONE S" ['] TYPE COMPILE, ; IMMEDIATE
\ FIXME POSTPONE vs ['] XX COMPILE,
: .( (    "ccc<quote>" -- ) ( -- ) [CHAR] ) PARSE TYPE ;

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
    48 C, 83 C, C5 C, 08 C, \ add   $8, %rbp
    FF C, D0 C,             \ call  *%rax
] ;

: EXIT ( -- ) R> ;

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
    DEPTH 0> IF DEPTH BEGIN S0 OVER 1+ CELLS - @ CR .X 1- DUP 0= UNTIL DROP
    ELSE DEPTH 0= IF ." EMPTY " ELSE ." UNDERFLOW " THEN THEN
;

: ? ( addr -- ) @ .X ;


: NAME>STRING ( nt -- c-addr u ) 9 + DUP 1+ SWAP C@ ;


: SEE ( "<spaces>ccc<space>" -- )
    PARSE-NAME FIND ( xt nt )
    CR ." prev:   " OVER @ NAME>STRING TYPE
    CR ." imm:    " OVER 8 + C@ FLAG-IMM AND IF ." yes" ELSE ." no" THEN
    CR ." xt:     " .X
    CR ." nt:     " .X
;

: BREAK CC C, ; IMMEDIATE \ Int3 / Breakpoint / Sigtrap on Linux


\ Consts and vars  =============================================================

0 INVERT CONSTANT TRUE
0 CONSTANT FALSE
20 CONSTANT BL
: SPACE BL EMIT ;


\ Number IO ====================================================================

VARIABLE BASE
: HEX     ( -- ) 10 BASE ! ;
: DECIMAL ( -- ) 0A BASE ! ;
DECIMAL


\ ==============================================================================


\ Interpreter ==================================================================

