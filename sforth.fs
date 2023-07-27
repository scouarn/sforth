: \ 0A scan-until ; IMMEDIATE
: ( 29 scan-until ; IMMEDIATE

\ TODO:
\ SOURCE vs TIB -> EVALUATE (read https://www.forth.com/starting-forth/10-input-output-operators/)
\ CASE ENDCASE OF ENDOF (see https://forth-standard.org/standard/rationale)
\ >NUMBER and standard number parser
\ ENVIRONMENT?
\ TO and VALUE
\ When we have the new interpreter : Remove echo from bootstrap
\ Dyn alloc with mmap when ALLOT which becomes a primitive called by , and C,
\ Write the system label
\ Run the test suite


\ Stack ========================================================================

: sp+ 48 C, 8D C, 6D C, 08 C, ; \ lea    8(%rbp), %rbp
: sp- 48 C, 8D C, 6D C, F8 C, ; \ lea   -8(%rbp), %rbp

\ TODO how is the reg encoded ?
\ : >reg ( off reg ) [ ?? C, 8B C, 45 C, C, ] ; \ mov off(%rbp), reg
\ : reg> ( off reg ) [ ?? C, 89 C, 45 C, C, ] ; \ mov reg, off(%rbp)
\ : >top ; mov xx, %r8
\ : top> ; mov %r8, xx

: DROP (    x  --       ) [ 4C C, 8B C, 45 C, 00 C, sp+ ] ; \ mov (%rbp), %r8
: DUP  (    x  -- x  x  ) [ sp- 4C C, 89 C, 45 C, 00 C, ] ; \ mov %r8, (%rbp)
: SWAP ( x1 x2 -- x2 x1 ) [ 4C C, 87 C, 45 C, 00 C, ] ; \ xchg %r8, (%rbp)
: NIP  ( x1 x2 -- x2    ) [ sp+ ] ;

: PICK ( xu...x1 x0 u -- xu...x1 x0 xu ) [
     4E C, 8B C, 44 C, C5 C, 00 C, \ mov (%rbp, %r8, 8), %r8
] ;

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

: -ROT ( x1 x2 x3 -- x3 x1 x2 ) [
    48 C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %rax
    4C C, 89 C, 45 C, 08 C, \ mov     %r8, 8(%rbp)
    4C C, 8B C, 45 C, 00 C, \ mov     (%rbp), %r8
    48 C, 89 C, 45 C, 00 C, \ mov     %rax, (%rbp)
] ;

: ?DUP ( x -- 0 | x x ) [
    49 C, 83 C, F8 C, 00 C, \ cmp    $0x0,%r8
    74 C, 08 C,             \ je     rel+8
    sp-
    4C C, 89 C, 45 C, 00 C, \ mov    %r8,0x0(%rbp)
] ;


: rdrop ( -- ) ( R: x -- ) 48 C, 83 C, C4 C, 08 C, ; IMMEDIATE \ add $8, %rsp

: >R ( -- x ) ( R: x -- )
    41 C, 50 C,             \ push   %r8
    4C C, 8B C, 45 C, 00 C, \ mov   (%rbp), %r8
    sp+
; IMMEDIATE

: R> ( -- x ) ( R: x -- )
    sp-
    4C C, 89 C, 45 C, 00 C, \ mov   %r8, (%rbp)
    41 C, 58 C,             \ pop   %r8
; IMMEDIATE

: R@ ( -- x ) ( R: x -- x )
    sp-
    4C C, 89 C, 45 C, 00 C, \ mov    %r8, (%rbp)
    4C C, 8B C, 04 C, 24 C, \ mov   (%rsp), %r8
; IMMEDIATE

: 2>R ( x1 x2 -- )   ( R: -- x1 x2 )
    FF C, 75 C, 00 C,       \ push  (%rbp)
    41 C, 50 C,             \ push  %r8
    4C C, 8B C, 45 C, 08 C, \ mov   8(%rbp), %r8
    48 C, 8D C, 6D C, 10 C, \ lea   16(%rbp), %rbp
; IMMEDIATE

: 2R> ( -- x1 x2 )   ( R: x1 x2 -- )
    48 C, 8D C, 6D C, F0 C, \ lea   -16(%rbp), %rbp
    4C C, 89 C, 45 C, 00 C, \ mov   %r8, 8(%rbp)
    8F C, 45 C, 00 C,       \ pop  (%rbp)
    41 C, 58 C,             \ pop  %r8
; IMMEDIATE

: 2R@ ( -- x1 x2 )   ( R: x1 x2 -- x1 x2 )
    48 C, 8D C, 6D C, F0 C,       \ lea   -16(%rbp), %rbp
    4C C, 89 C, 45 C, 00 C,       \ mov   %r8, 8(%rbp)
    4C C, 8B C, 44 C, 24 C, 08 C, \ mov   8(%rsp), %r8
    4C C, 89 C, 45 C, 00 C,       \ mov   %r8, (%rbp)
    4C C, 8B C, 04 C, 24 C,       \ mov   (%rsp), %r8
; IMMEDIATE

: 2DROP (       x1 x2 --                   ) DROP DROP ;
: 2DUP  (       x1 x2 -- x1 x2 x1 x2       ) OVER OVER ;
: 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2       ) >R -ROT R> -ROT ;
: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) 3 PICK 3 PICK ;


\ Memory =======================================================================

: ALIGNED ( addr -- a-addr ) ; \ Byte aligned

: @  (   addr -- x ) [ 4D C, 8B C, 00 C, ] ; \ movq (%r8), %r8
: !  ( x addr --   ) [
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax    # x
    49 C, 89 C, 00 C,       \ movq    %rax, (%r8)
    4C C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %r8
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
] ;

: H! ( x addr -- ) [ \ Store Half cell (4bytes)
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax    # x
    41 C, 89 C, 00 C,       \ movq    %rax, (%r8)
    4C C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %r8
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
] ;

: +! ( n addr -- ) [
    48 C, 8B C, 45 C, 00 C, \ movq    (%rbp), %rax
    49 C, 01 C, 00 C,       \ addq    %rax, (%r8)
    4C C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %r8
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
] ;

: -! ( n addr -- ) [
    48 C, 8B C, 45 C, 00 C, \ movq    (%rbp), %rax
    49 C, 29 C, 00 C,       \ subq    %rax, (%r8)
    4C C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %r8
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
] ;

: 1+! ( addr -- ) [
    49 C, FF C, 00 C,       \ incq    (%r8)
    4C C, 8B C, 45 C, 00 C, \ mov     (%rbp), %r8
    sp+
] ;

: 1-! ( addr -- ) [
    49 C, FF C, 08 C,       \ decq    (%r8)
    4C C, 8B C, 45 C, 00 C, \ mov     (%rbp), %r8
    sp+
] ;

: C@ ( c addr -- c ) [ 4D C, 0F C, B6 C, 00 C, ] ;  \ movzxb  (%r8), %r8
: C! ( c addr --   ) [
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax    # x
    41 C, 88 C, 00 C,       \ movb    %al (%r8)
    4C C, 8B C, 45 C, 08 C, \ mov     8(%rbp), %r8
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
] ;


: ON  ( addr -- ) [
    49 C, C7 C, 00 C, FF C, FF C, FF C, FF C, \ mov $-1, (%r8)
    4C C, 8B C, 45 C, 00 C, sp+               \ mov   (%rbp), %r8
] ;

: OFF ( addr -- ) [
    49 C, C7 C, 00 C, 00 C, 00 C, 00 C, 00 C, \ mov $0, (%r8)
    4C C, 8B C, 45 C, 00 C, sp+               \ mov   (%rbp), %r8
] ; 

: CHARS (    n1 -- n2    ) ; \ Do nothing
: CELLS (    n1 -- n2    ) [ 49 C, C1 C, E0 C, 03 C, ] ; \ shl  $3, %r8
: CHAR+ ( addr1 -- addr2 ) [ 49 C, FF C, C0 C,       ] ; \ incq %r8
: CELL+ ( addr1 -- addr2 ) [ 49 C, 83 C, C0 C, 08 C, ] ; \ add  $8, %r8

: 2@ (   addr -- d ) DUP CELL+ @ SWAP @ ;
: 2! ( d addr --   ) SWAP OVER ! CELL+ ! ;


\ Arithmetic ===================================================================

: NEGATE (      n1 -- n2      ) [ 49 C, F7 C, D8 C, ] ; \ neg   %r8
: 1+     ( n1 | u1 -- n2 | u2 ) [ 49 C, FF C, C0 C, ] ; \ incq    %r8
: 1-     ( n1 | u1 -- n2 | u2 ) [ 49 C, FF C, C8 C, ] ; \ dec     %r8

: + ( n1 | u1 n2 | u2 -- n3 | u3 ) [
    4C C, 03 C, 45 C, 00 C, sp+ \ add   (%rbp), %r8
] ;

: - ( n1 | u1 n2 | u2 -- n3 | u3 ) [
    4C C, 2B C, 45 C, 00 C, sp+ \ sub   (%rbp),%r8
    49 C, F7 C, D8 C,           \ neg   %r8
] ;

: * ( n1 n2 -- n3 ) [
    4C C, 0F C, AF C, 45 C, 00 C,   \ imul  (%rbp), %r8     %r8 := n1 * n2
    sp+
] ;

: S>D ( n -- d ) [
    sp- 4C C, 89 C, 45 C, 00 C, \ mov %r8, (%rbp)
        49 C, C1 C, F8 C, 3F C, \ sar $63, %r8      if r8<0 then -1 else 0
] ;

: D>S ( d -- n ) DROP ; \ Easy on 2's complement machine

: M* ( n1 n2 -- d ) [
    48 C, 8B C, 45 C, 00 C, \ mov   (%rbp), %rax
    49 C, F7 C, E8 C,       \ imul  %r8
    48 C, 89 C, 45 C, 00 C, \ mov   %rax, (%rbp)
    49 C, 89 C, D0 C,       \ mov   %rdx, %r8
] ;

: UM* ( u1 u2 -- ud ) [
    48 C, 8B C, 45 C, 00 C, \ mov   (%rbp), %rax
    49 C, F7 C, E0 C,       \ mul   %r8
    48 C, 89 C, 45 C, 00 C, \ mov   %rax, (%rbp)
    49 C, 89 C, D0 C,       \ mov   %rdx, %r8
] ;

: /MOD ( n1 n2 -- rem quot ) [
    48 C, 8B C, 45 C, 00 C,     \ movq  (%rbp), %rax        n1
    48 C, 99 C,                 \ cqto
    49 C, F7 C, F8 C,           \ idiv  %r8
    49 C, 89 C, C0 C,           \ movq  %rax, %r8           quot
    48 C, 89 C, 55 C, 00 C,     \ movq  %rdx, (%rbp)        rem
] ;

: UM/MOD ( ud u -- rem quot ) [
    48 C, 8B C, 45 C, 08 C,     \ movq  8(%rbp), %rax       u_low
    48 C, 8B C, 55 C, 00 C,     \ movq  0(%rbp), %rdx       u_high
    49 C, F7 C, F0 C,           \ div   %r8
    49 C, 89 C, C0 C,           \ movq  %rax, %r8           quot
    sp+
    48 C, 89 C, 55 C, 00 C,     \ movq  %rdx, (%rbp)        rem
] ;

: SM/REM ( d n -- rem quot ) [
    48 C, 8B C, 45 C, 08 C,     \ movq  8(%rbp), %rax       u_low
    48 C, 8B C, 55 C, 00 C,     \ movq  0(%rbp), %rdx       u_high
    49 C, F7 C, F8 C,           \ idiv  %r8
    49 C, 89 C, C0 C,           \ movq  %rax, %r8           quot
    sp+
    48 C, 89 C, 55 C, 00 C,     \ movq  %rdx, (%rbp)        rem
] ;

\ : FM/MOD ( u n -- rem quot ) ; \ TODO

: /     (   x1 x2  -- x3    ) /MOD NIP  ;
: MOD   (   x1 x2  -- x3    ) /MOD DROP ;
: */MOD ( n1 n2 n3 -- n4 n5 ) >R M* R> ( d n3 ) SM/REM ;
: */    ( n1 n2 n3 -- n4    ) */MOD NIP ;


: shl-imm, ( u -- ) ( x -- x<<u ) 49 C, C1 C, E0 C, C, ; \ shl $u, %r8
: shr-imm, ( u -- ) ( x -- x<<u ) 49 C, C1 C, E8 C, C, ; \ shr $u, %r8
: 2* ( x1 -- x2 ) [ 1 shl-imm, ] ;
: 4* ( x1 -- x2 ) [ 2 shl-imm, ] ;
: 8* ( x1 -- x2 ) [ 3 shl-imm, ] ;
: 2/ ( x1 -- x2 ) [ 1 shr-imm, ] ;
: 4/ ( x1 -- x2 ) [ 2 shr-imm, ] ;
: 8/ ( x1 -- x2 ) [ 3 shr-imm, ] ;

: LSHIFT ( x1 u -- x2 ) [
    48 C, 8B C, 4D C, 00 C, \ mov (%rbp), %rcx
    sp+
    49 C, 87 C, C8 C,       \ xchg  %rcx, %r8
    49 C, D3 C, E0 C,       \ shl   %cl, %r8
] ;

: RSHIFT ( x1 u -- x2 ) [
    48 C, 8B C, 4D C, 00 C, \ mov (%rbp), %rcx
    sp+
    49 C, 87 C, C8 C,       \ xchg  %rcx, %r8
    49 C, D3 C, E8 C,       \ shl   %cl, %r8
] ;


\ Logical ======================================================================

: INVERT ( x1 -- x2 ) [ 49 C, F7 C, D0 C, ] ;           \ not %r8
: AND ( x1 x2 -- x3 ) [ 4C C, 23 C, 45 C, 00 C, sp+ ] ; \ and   (%rbp), %r8
: OR  ( x1 x2 -- x3 ) [ 4C C, 0B C, 45 C, 00 C, sp+ ] ; \ or    (%rbp), %r8
: XOR ( x1 x2 -- x3 ) [ 4C C, 33 C, 45 C, 00 C, sp+ ] ; \ xor   (%rbp), %r8


\ Comparisons ==================================================================

\ x86 condition codes
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
    sp+
;

: 0=  (     n -- flag ) [ cc-e  0cmp, ] ;
: 0<> (     n -- flag ) [ cc-ne 0cmp, ] ;
: 0>  (     n -- flag ) [ cc-g  0cmp, ] ;
: 0<  (     n -- flag ) [ cc-l  0cmp, ] ; \ TODO SAR $63, %r8
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

\ TODO  WITHIN

: D0= ( h l -- flag ) 0= ( h flag ) SWAP 0= ( flag flag ) AND ;


\ Compilation/utility ==========================================================

: BREAK ( -- SIGTRAP ) CC C, ; IMMEDIATE
: HERE   (   -- addr ) CP @ ;
: ALLOT  ( n --      ) CP +! ;
: ALIGN  (   --      ) CP @ ALIGNED CP ! ;
: PAD    (   -- addr ) HERE 100 + ;
: UNUSED (   -- u    ) brk @ HERE - ;

: FLAG-IMM 01 ;
: FLAG-HIDDEN 02 ;

: EXECUTE ( i*x xt -- j*x ) [
    4C C, 89 C, C0 C,       \ mov   %r8, %rax
    4C C, 8B C, 45 C, 00 C, \ mov   (%rbp), %r8
    sp+
    FF C, D0 C,             \ call  *%rax
] ;

: EXIT ( -- ) [ 48 C, 83 C, C4 C, 08 C, ] ; \ add $8, %rsp


\ Control flow =================================================================

: <resolve ( C: ori      --     ) (   -- ) >R HERE 4 - R@ - R> H! ;
: resolve> ( C: dest ori --     ) (   -- ) >R HERE - R> H! ;
: branch   ( C:          -- ori ) (   -- ) E9 C, HERE 99 C, 99 C, 99 C, 99 C, ;
: branch0  ( C:          -- ori ) ( x -- )
    4D C, 85 C, C0 C,       \ test %r8, %r8
    4C C, 8B C, 45 C, 00 C, \ mov (%rbp), %r8
    48 C, 8D C, 6D C, 08 C, \ lea 8(%rbp), %rbp -> We have to lea to keep flags
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

: BL 20 ;
: PARSE-NAME ( char "<chars>ccc<char>" -- c-addr u ) BL scan-while BL PARSE ;
: ' ( "<spaces>name" -- xt ) PARSE-NAME find IF DROP ELSE 2DROP 0 THEN ;

: POSTPONE ( "<spaces>name" -- )
    PARSE-NAME find ( xt flags nt )
    0= IF BYE THEN \ TODO handle error

    ( flags ) FLAG-IMM AND IF \ Compile code that executes xt
        COMPILE,
    ELSE \ Compile code that compiles xt (default compilation behaviour)
        [ ' LITERAL  COMPILE, ]          \ Compile DPUSH $xt
        [ ' COMPILE, LITERAL  ] COMPILE, \ Compile COMPILE,
    THEN
; IMMEDIATE

: ['] ( C: "<spaces>name" -- ) ( -- xt ) ' POSTPONE LITERAL ; IMMEDIATE
: [COMPILE] ( C: "<spaces>name" -- ) ' COMPILE, ; IMMEDIATE


\ DO loops =====================================================================

\ leave: jump addr to quit the loop (at LOOP just before an UNLOOP)
\ leave-orig: addr of the cell containing leave, written by LOOP at compilation
\ dest: jump addr to loop (after DO)

: UNLOOP ( R: leave lim idx -- )
    48 C, 83 C, C4 C, 18 C, \ add $24, %rsp
; IMMEDIATE

: LEAVE ( R: leave lim idx -- ) \ UNLOOP performed after the jump
    FF C, 64 C, 24 C, 10 C, \ jmp *16(%rsp)
; IMMEDIATE

\ TODO Refactor DO ?DO and LOOP +LOOP

: DO  ( C: -- leave-orig dest ) ( lim idx -- ) ( R: -- leave lim idx )
    \ Reserve a cell for the leave address
    \ TODO Kinda hacky, similar to S" -> better solution for this kind of storage?
    POSTPONE AHEAD HERE >R 99 , POSTPONE THEN

    \ At execution, push the value inside the cell, which was set by LOOP
    R@ ( leave-orig ) POSTPONE LITERAL POSTPONE @ ( leave ) POSTPONE >R
    POSTPONE 2>R \ Also push lim and idx

    \ At compilation, pass leave-orig for LOOP to resolve and dest so LOOP can
    \ jump back here
    R> HERE ( leave-orig dest )
; IMMEDIATE

: ?DO ( C: -- leave-orig dest ) ( lim idx -- ) ( R: -- leave lim idx )
    POSTPONE AHEAD HERE >R 99 , POSTPONE THEN
    R@ POSTPONE LITERAL POSTPONE @ ( leave ) POSTPONE >R
    POSTPONE 2>R

    \ Same as DO but LEAVE at execution if lim = idx
    POSTPONE 2R@ POSTPONE = POSTPONE IF POSTPONE LEAVE POSTPONE THEN

    R> HERE
; IMMEDIATE

\ FIXME : If the loop index did not cross the boundary between the loop limit
\ minus one and the loop limit, continue execution at the beginning of the loop.
: +LOOP ( C: leave-orig dest -- ) ( n -- ) ( R: leave lim i1 -- | leave lim i2 )
        POSTPONE R> POSTPONE + POSTPONE R> ( i2 lim )
        POSTPONE 2DUP ( i2 lim i2 lim )
        POSTPONE >R POSTPONE >R ( i2 lim ) ( R: lim i2 )
        POSTPONE =              ( flag ) \ FIXME
        branch0 resolve>        \ Resolve dest: 0branch to DO
        HERE SWAP ( here leave-orig ) ! \ Resolve leave-orig
        POSTPONE UNLOOP
; IMMEDIATE

: LOOP  ( C: leave-orig dest -- ) ( -- ) ( R: leave lim i1 -- | lim i2 )
        POSTPONE R> POSTPONE 1+ POSTPONE R> ( i2 lim )
        POSTPONE 2DUP ( i2 lim i2 lim )
        POSTPONE >R POSTPONE >R ( i2 lim ) ( R: lim i2 )
        POSTPONE =              ( flag )
        branch0 resolve>        \ Resolve dest: 0branch to DO
        HERE SWAP ( here leave-orig ) ! \ Resolve leave-orig
        POSTPONE UNLOOP
; IMMEDIATE

: I ( -- idx ) ( R: leave lim idx -- leave lim idx )
    sp-   4C C, 89 C, 45 C, 00 C, \ mov    r8, (%rbp)
    4C C, 8B C, 04 C, 24 C,       \ mov    (%rsp), %r8
; IMMEDIATE

: J ( -- i1 ) ( R: leave1 lim1 i1 leave2 lim2 i2 -- <same> )
    sp-   4C C, 89 C, 45 C, 00 C, \ mov    r8, (%rbp)
    4C C, 8B C, 44 C, 24 C, 18 C, \ mov    24(%rsp), %r8
; IMMEDIATE


\ Defining words ===============================================================

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

: CREATE ( "<spaces>name" -- ) ( -- a-addr )
    \ parent: <parent-code> <ret>
    \ docreate: Push caller's PFA <ret>
    \ <child-header> <call docreate> <data>  (call doesn't return)
    : POSTPONE docreate POSTPONE ;
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
: BUFFER:  ( u "<spaces>name" -- ) ( -- a-addr ) CREATE ALLOT ;

: :NONAME ( -- xt ) ( -- xt )
    HERE LATEST DUP @ ( here latest prev-addr ) , \ Set previous field
    ( here latest ) ! \ Update latest
    0 C, 0 C, \ Flags and length (empty name)
    ] \ Start compiling
    HERE \ xt
;

: RECURSE ( -- ) LATEST @ >code COMPILE, ; IMMEDIATE


\ FIXME State does matter but the rationale isn't well explained
\ https://forth-standard.org/standard/double/TwoVALUE
\ : VALUE ( x "<spaces>name" -- ) CREATE , DOES> ( -- x ) @ ;
\ : TO    ( x "<spaces>name" -- ) ' >BODY ! ;

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


\ Arithmetic/ Utility ==========================================================

\ TODO Redo in machine code (?)
: ABS (    n1 -- u    ) DUP 0< IF NEGATE THEN ;
: MIN ( n1 n2-- n1|n2 ) 2DUP < IF DROP ELSE NIP THEN ;
: MAX ( n1 n2-- n1|n2 ) 2DUP > IF DROP ELSE NIP THEN ;

: CMOVE ( c-addr1 c-addr2 u -- ) [
    4C C, 89 C, C1 C,       \ mov   %r8, %rcx
    48 C, 8B C, 7D C, 00 C, \ mov   0(%rbp), %rdi
    48 C, 8B C, 75 C, 08 C, \ mov   8(%rbp), %rsi
    F3 C, A4 C,             \ rep   movsb
    4C C, 8B C, 45 C, 10 C, \ mov   16(%rbp), %r8
    48 C, 8D C, 6D C, 18 C, \ lea   24(%rbp), %rbp
] ;

: FILL  ( c-addr u char -- ) [
    4C C, 89 C, C0 C,       \ mov   %r8, %rax
    48 C, 8B C, 4D C, 00 C, \ mov   0(%rbp), %rcx
    48 C, 8B C, 7D C, 08 C, \ mov   8(%rbp), %rdi
    F3 C, AA C,             \ rep   stosb
    4C C, 8B C, 45 C, 10 C, \ mov   16(%rbp), %r8
    48 C, 8D C, 6D C, 18 C, \ lea   24(%rbp), %rbp
] ;

: CMOVE> ( c-addr1 c-addr2 u -- )
    >R R@ + 1- SWAP R@ + 1- SWAP ( c-addr1+u-1 c-addr2+u-1 )
    [ FD C, ] R> CMOVE [ FC C, ] \ opcodes for std and cld (movsb direction)
;

: MOVE  ( c-addr1 c-addr2 u -- ) >R 2DUP > IF R> CMOVE ELSE R> CMOVE> THEN ;
: ERASE (         c-addr  u -- ) 0 FILL ;

\ TODO Optimized versions that start writing cell by cell until u < 8
\ Something like (~7) AND FILL-CELLS (with rep movsq)
\ followed by 7 AND FILL


\ Strings ======================================================================

: SPACE  (   -- ) BL EMIT ;
: SPACES ( u -- ) 0 DO SPACE LOOP ;
: CR     (   -- ) 0D EMIT 0A EMIT ;

: COUNT   ( c-addr1   -- c-addr2 u ) DUP CHAR+ SWAP C@ ;

100 BUFFER: counted-buf \ 1 + 255 (minimal length in the standard)

: counted ( c-addr1 u -- c-addr2 ) \ String to counted-string (transient region)
    FF MIN \ Limit size to 255
    DUP counted-buf ( c-addr1 u u buf ) C! \ len
    counted-buf 1+ SWAP ( c-addr1 buf+1 u ) CMOVE \ chars
    counted-buf
;

: WORD ( char "<chars>ccc<char>" -- c-addr ) DUP scan-while PARSE counted ;

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

: ." ( C: "ccc<quote>" -- ) ( -- ) POSTPONE S" POSTPONE TYPE ; IMMEDIATE
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


\ Number IO ====================================================================

VARIABLE BASE
: HEX     ( -- ) 10 BASE ! ;
: DECIMAL ( -- ) 0A BASE ! ;
DECIMAL

100 CONSTANT #sz
#sz BUFFER: #buf
VARIABLE #idx

: <# ( -- ) #sz #idx ! ;

: #> ( xd -- c-addr u )
    2DROP
    #idx @ #buf + ( c-addr )
    #sz #idx @ - ( c-addr u )
;

: HOLD ( char -- )
    1 #idx -! \ dec idx
    #buf #idx @ + ( char c-addr ) C! \ write char
;

: HOLDS ( c-addr u -- )
    \ #sz #idx - MIN \ TODO Limit to the remaining space in the buffer
    DUP #idx -! \ dec idx
    #buf #idx @ + ( c-addr1 u c-addr2 ) SWAP MOVE \ write data
;

: >digit ( u -- char ) DUP 9 > IF [CHAR] A + 0A - ELSE [CHAR] 0 + THEN ;

: # ( ud1 -- ud2 )
    BASE @ UM/MOD ( rem quot ) \ TODO Handle double division
    SWAP >digit HOLD ( quot )
    0 ( ud2 )
;

: #S ( ud1 -- ud2 )
    BEGIN # 2DUP D0= UNTIL
;

: SIGN (   n -- ) 0< IF [CHAR] - HOLD THEN ;
: #pad (   n -- ) #sz #idx @ - - BEGIN DUP 0> WHILE BL HOLD 1- REPEAT ;
: U.   (   u -- )           0 ( ud ) <# #S          #> TYPE SPACE ;
: .    (   u -- ) DUP ABS S>D (  d ) <# #S ROT SIGN #> TYPE SPACE ;
: U.R  ( u w -- ) >R           0 ( ud ) <# #S          R> #pad #> TYPE ;
: .R   ( n w -- ) >R DUP ABS S>D (  d ) <# #S ROT SIGN R> #pad #> TYPE ;
: .X   (   u -- ) BASE @ SWAP HEX 0 <# 10 0 DO # LOOP #> TYPE SPACE BASE ! ;


\ Tools ========================================================================

: DEPTH ( -- +n ) SP@ S0 SWAP - 8/ ;

: ? ( addr -- ) @ . ;

: .S ( -- )
    DEPTH 0> IF DEPTH BEGIN S0 OVER 1+ CELLS - @ CR . 1- DUP 0<= UNTIL DROP
    ELSE DEPTH 0= IF ." EMPTY " ELSE ." UNDERFLOW " THEN THEN
;

: NAME>STRING ( nt -- c-addr u ) 9 + DUP 1+ SWAP C@ ;
: SEE ( "<spaces>ccc<space>" -- )
    PARSE-NAME find ( xt flag 0|nt )
    ?DUP 0= IF ." NOT FOUND " 2DROP EXIT THEN ( xt flag 0|nt )

    CR ." prev:   " DUP @ ?DUP IF NAME>STRING TYPE ELSE ." (none)" THEN
    CR ." imm:    " SWAP FLAG-IMM AND IF ." yes" ELSE ." no" THEN
    CR ." nt:     " .X
    CR ." xt:     " .X
;

: .byte (   char -- ) 0 <# # # #> SPACE TYPE ;
: .addr (   addr -- ) 0 <# 10 0 DO # LOOP #> CR TYPE [CHAR] : EMIT ;
: .char (   char -- ) DUP 20 < OVER 7E > OR IF DROP [CHAR] . THEN EMIT ;
: DUMP  ( addr u -- )
    BASE @ >R HEX
    BEGIN DUP WHILE
        OVER .addr
        8 0 DO DUP I U> IF OVER I + C@ .byte ELSE 3 SPACES THEN LOOP \ bytes
        2 SPACES
        8 0 DO DUP I U> IF OVER I + C@ .char ELSE LEAVE    THEN LOOP \ chars
        DUP 8 U< IF DROP 0 ELSE 8 - THEN \ dec u
        SWAP 8 + SWAP \ inc addr
    REPEAT
    DROP R> BASE ! CR
;

: WORDS ( -- )
    CR LATEST @
    BEGIN DUP WHILE DUP NAME>STRING TYPE SPACE @ REPEAT
    CR
;


: FORGET-NAME ( nt -- ) DUP @ LATEST ! CP ! ;

: FORGET ( "<spaces>name" -- )
    PARSE-NAME find ( xt flags 0|nt )
    DUP IF FORGET-NAME THEN 2DROP
;

: MARKER HERE CREATE , DOES> @ FORGET-NAME ;


\ ==============================================================================
\ Interpreter ==================================================================


0 1 - CONSTANT -ONE
0 INVERT CONSTANT TRUE
0 CONSTANT FALSE

: ABORT ( i*x -- ) S0 [ 4C C, 89 C, C5 C, ] QUIT ; \ mov %r8, %rbp

: ABORT" ( "ccc<quote>" -- ) ( i*x x -- | i*x ) ( R: j*x -- | j*x )
    POSTPONE IF
    POSTPONE ."
    POSTPONE CR
    POSTPONE ABORT
    POSTPONE THEN
; IMMEDIATE


: FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
    DUP COUNT find ( c-addr xt flags 0|nt )
    0= IF 2DROP 0 ( c-addr 0 ) EXIT THEN
    ( c-addr xt flags ) ROT DROP ( xt flags )
    FLAG-IMM AND IF 1 ELSE -ONE THEN ;
;

: greet ." Welcome to Scouarn Forth"
    CR ." used: "
    HERE CP0 - . ." total, "
    CP1  CP0 - . ." kernel, "
    HERE CP1 - . ." user "
    UNUSED CR ." free: " .
    CR CR
;

\ Start ========================================================================

greet
echo ON
