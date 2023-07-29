: \ 0A scan-until ; IMMEDIATE
: ( 29 scan-until ; IMMEDIATE

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

: TUCK ( x1 x2   -- x2 x1 x2 ) [
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


: rdrop  ( -- ) ( R: x -- ) 48 C, 83 C, C4 C, 08 C, ; IMMEDIATE \ add $8,  %rsp
: 2rdrop ( -- ) ( R: x -- ) 48 C, 83 C, C4 C, 10 C, ; IMMEDIATE \ add $16, %rsp

: SP@   ( -- addr ) [
    sp-  4C C, 89 C, 45 C, 00 C, \ mov  %r8, (%rbp)
         4C C, 8D C, 45 C, 08 C, \ lea  8(%rbp), %r8
] ;

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
    4C C, 89 C, 45 C, 08 C, \ mov   %r8, 8(%rbp)
    41 C, 58 C,             \ pop  %r8
    8F C, 45 C, 00 C,       \ pop  (%rbp)
; IMMEDIATE

: 2R@ ( -- x1 x2 )   ( R: x1 x2 -- x1 x2 )
    48 C, 8D C, 6D C, F0 C,       \ lea   -16(%rbp), %rbp
    4C C, 89 C, 45 C, 08 C,       \ mov   %r8, 8(%rbp)
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

: L! ( x addr -- ) [ \ Store long word (32bits)
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

: D+ ( d1 d2 -- d3 ) [ ( l1:16 h1:8 l2:0 h2:r8 -- l3:16 h3:r8 )
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax      l2
    48 C, 01 C, 45 C, 10 C, \ addq    %rax, 16(%rbp)    l1 += l2 -> l3
    4C C, 13 C, 45 C, 08 C, \ adcq    8(%rbp), %r8      h2 += h1 -> h3
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
] ;

: D- ( d1 d2 -- d3 ) [ ( l1:16 h1:8 l2:0 h2:r8 -- l3:16 h3:r8 )
    48 C, 8B C, 45 C, 00 C, \ mov     (%rbp), %rax      l2
    48 C, 29 C, 45 C, 10 C, \ subq    %rax, 16(%rbp)    l1 -= l2 -> l3
    4C C, 1B C, 45 C, 08 C, \ sbbq    8(%rbp), %r8      h2 -= h1 -> -h3
    49 C, F7 C, D8 C,       \ neg     %r8               h3
    48 C, 8D C, 6D C, 10 C, \ lea     16(%rbp), %rbp
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


: M+     (   d1   n -- d2    ) S>D D+ ;
: M-     (   d1   n -- d2    ) S>D D- ;
: U/MOD  (   u1 u2  -- u3 u4 ) 0 SWAP UM/MOD ;
: U*     (   u1 u2  -- u3    ) UM* DROP ;
: /      (   x1 x2  -- x3    ) /MOD NIP ;
: MOD    (   x1 x2  -- x3    ) /MOD DROP ;
: */MOD  ( n1 n2 n3 -- n4 n5 ) >R M*  R> ( d  n3 ) SM/REM ;
: U*/MOD ( u1 u2 u3 -- u4 u5 ) >R UM* R> ( ud u3 ) UM/MOD ;
: */     ( n1 n2 n3 -- n4    ) */MOD NIP ;


: shl-imm, ( u -- ) ( x -- x<<u ) 49 C, C1 C, E0 C, C, ; \ shl $u, %r8
: sar-imm, ( u -- ) ( x -- x<<u ) 49 C, C1 C, F8 C, C, ; \ shr $u, %r8
: 2* ( x1 -- x2 ) [ 1 shl-imm, ] ;
: 4* ( x1 -- x2 ) [ 2 shl-imm, ] ;
: 8* ( x1 -- x2 ) [ 3 shl-imm, ] ;
: 2/ ( x1 -- x2 ) [ 1 sar-imm, ] ;
: 4/ ( x1 -- x2 ) [ 2 sar-imm, ] ;
: 8/ ( x1 -- x2 ) [ 3 sar-imm, ] ;

: D2* ( xd1 -- xd2 ) [
    48 C, D1 C, 65 C, 00 C, \ salq  (%rbp)  Low
    49 C, D1 C, D0 C,       \ rclq  %8      High
] ;

: D2/ ( xd1 -- xd2 ) [
    49 C, D1 C, F8 C,       \ sarq  %r8     High
    48 C, D1 C, 5D C, 00 C, \ rclq  (%rbp)  Low
] ;

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


\ Unsigned double mixed multiplication
\ Double times single -> double product
\ 
\      Y  X
\      0  Z
\ *  ______
\      B  A     B:A = X Z UM*
\      C  0     C   = X Z U*
\ +  ______
\     t2 t1     t2 t3 = A B 0 C D+
: UDM* ( ud1 u -- ud2 ) ( X Y Z -- t1 t2 )
    TUCK    ( X Z Y Z   )
    U* >R   ( X Z       ) ( R: C )
    UM*     ( A B       )
    R>      ( A B C   )
    +       ( t1  t2    )
;

\ Verion yielding triple product
\ 
\       Y  X
\       0  Z
\ *  _______
\     0  B  A     B:A = X Z UM*
\     D  C  0     D:C = X Z U*
\ +  ________
\     t3 t2 t1    t3:t2:t1 = A B 0 C D+
\ 
\ : UDM* ( ud1 u -- t ) ( X Y Z -- t1 t2 t3 )
\     TUCK        ( X Z Y Z   )
\     UM* 2>R     ( X Z       ) ( R: C D )
\     UM*         ( A B       )
\     0 2R>       ( A B 0 C D )
\     D+          ( t1 t2 t3  )
\ ;

\ Unsigned double mixed division
\ Double dividend, single divisor, single remainder, double quotient
\ Can be modified for triple over single division
\ 
\   W  Y   X  | Z
\   Z*Qh     |______    \ 1: W:Y = Z*Qh + Rh      Divide Y (0 extended) by Z
\ - ____     | Qh Ql
\     Rh  X  |          \ 2: Rh:X = Z*Ql + Rl   Divide X (Rh extended) by Z
\      Ql*Z  |
\   - _____  |
\       Rl   |
: UD/MOD ( t n -- rem dquot ) ( X Y Z -- Rl Ql Qh ) \ Same as in Gforth
    TUCK        ( X  Z  Y  Z )
    U/MOD  >R   ( X  Z  Rh   ) ( R: Qh )    \ 1
    SWAP        ( X  Rh Z    )
    UM/MOD R>   ( Rl Ql Qh   )              \ 2
;


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


\ Reference implementation for 2's complement machines
\    (lo < hi and (lo <= x and x < hi))
\ or (lo > hi and (lo <= x or  x < hi))
: WITHIN ( x lo hi -- flag ) OVER - ( x lo hi-lo ) >R - R> ( x-lo hi-lo ) U< ;


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

: <resolve ( C: ori      --     ) (   -- ) >R HERE 4 - R@ - R> L! ;
: resolve> ( C: dest ori --     ) (   -- ) >R HERE - R> L! ;
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
    POSTPONE AHEAD HERE >R 0 , POSTPONE THEN

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


\ Implementation of AntonErtl's comment
\ https://forth-standard.org/standard/core/PlusLOOP
: (loop-stop?) ( n -- flag ) ( R: lim i1 ret -- lim i1 ret )
    R> 2R@ ( n ret lim i1 ) ROT >R
    ( n lim i1 )
    SWAP - ( n i-l )
    [ 1 3F LSHIFT ] LITERAL ( n i-l ) \ MIN-N
    + ( n x )

    [ ( n1 n2 -- 0|1 ) \ n1 + n2 overflowed?
        4C C, 03 C, 45 C, 00 C, \ add    (%rbp), %r8
        0F C, 90 C, C0 C,       \ seto   %al
        4C C, 0F C, B6 C, C0 C, \ movzxb %al, %r8
        sp+
    ]
;

: +LOOP ( C: leave-orig dest -- ) ( n -- ) ( R: leave lim i1 -- | leave lim i2 )
        POSTPONE DUP POSTPONE (loop-stop?) POSTPONE SWAP ( n flag ) \ Exit cond
        POSTPONE R> POSTPONE + POSTPONE >R ( flag ) ( R: lim i2 ) \ Increment
        branch0 resolve> \ Resolve dest: 0branch to DO
        HERE SWAP ( here leave-orig ) ! \ Resolve leave-orig
        POSTPONE UNLOOP
; IMMEDIATE

: LOOP  ( C: leave-orig dest -- ) ( -- ) ( R: leave lim i1 -- | lim i2 )
        POSTPONE R> POSTPONE 1+ POSTPONE >R ( R: lim i2 )
        POSTPONE 2R@ POSTPONE = ( flag )
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


\ CASE =========================================================================

: CASE ( C: -- num ) ( -- ) 0 ; IMMEDIATE \ Initial number of cases

: ENDCASE ( C: ori1 ori2 ... oriu u -- ) ( x -- )
    POSTPONE DROP
    0 ?DO POSTPONE THEN LOOP
; IMMEDIATE

\ I was about to call it "OF-COND" like "cond" in Lisp but "?of" is in Gforth
: ?OF ( C: u1 -- ori u2 ) ( flag -- ) POSTPONE IF SWAP 1+ ; IMMEDIATE

: OF ( C: u1 -- ori u2 ) ( x1 x2 -- | x1 )
    POSTPONE OVER
    POSTPONE =
    POSTPONE ?OF
    POSTPONE DROP
; IMMEDIATE

: ENDOF ( C: ori1 u -- ori2 u ) ( -- ) SWAP POSTPONE ELSE SWAP ; IMMEDIATE


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

: 2LITERAL ( C: x1 x2 -- ) ( -- x1 x2 )
    SWAP POSTPONE LITERAL POSTPONE LITERAL
; IMMEDIATE

: 2CONSTANT ( xd "<spaces>name" -- ) ( -- xd ) : POSTPONE 2LITERAL POSTPONE ; ;
: 2VARIABLE (    "<spaces>name" -- ) ( -- a-addr ) CREATE 0 , 0 , ;

0 1 -           CONSTANT -ONE
0 INVERT        CONSTANT TRUE
0               CONSTANT FALSE
-ONE 1 RSHIFT   CONSTANT MAX-N
1 3F   LSHIFT   CONSTANT MIN-N
-ONE            CONSTANT MAX-U
-ONE MAX-N     2CONSTANT MAX-D
-ONE -ONE      2CONSTANT MAX-UD


: :NONAME ( -- xt ) ( -- xt )
    HERE LATEST DUP @ ( here latest prev-addr ) , \ Set previous field
    ( here latest ) ! \ Update latest
    0 C, 0 C, \ Flags and length (empty name)
    ] \ Start compiling
    HERE \ xt
;

: RECURSE ( -- ) LATEST @ >code COMPILE, ; IMMEDIATE


\ NOTE: For the double ext set, TO has to work with 2VALUE aswell
\ https://forth-standard.org/standard/double/TwoVALUE
: VALUE ( x "<spaces>name" -- ) CREATE , DOES> ( -- x ) @ ; IMMEDIATE
: TO    ( x "<spaces>name" -- )
    STATE @ IF POSTPONE ['] POSTPONE >BODY POSTPONE ! ELSE ' >BODY ! THEN
; IMMEDIATE


: DEFER  ( "<spaces>name" --     ) CREATE 0 , DOES> @ EXECUTE ;
: DEFER@ (         xt1    -- xt2 ) >BODY @ ;
: DEFER! (     xt2 xt1    --     ) >BODY ! ;

: IS
    STATE @ IF POSTPONE ['] POSTPONE DEFER!  ELSE ' DEFER!  THEN
; IMMEDIATE

: ACTION-OF
    STATE @ IF POSTPONE ['] POSTPONE DEFER@ ELSE ' DEFER@ THEN
; IMMEDIATE


\ More Arithmetic ==============================================================

\ TODO Redo in machine code (?)
: ABS (    n1 -- u    ) DUP 0< IF NEGATE THEN ;
: MIN ( n1 n2-- n1|n2 ) 2DUP < IF DROP ELSE NIP THEN ; \ TODO use CMOVcc
: MAX ( n1 n2-- n1|n2 ) 2DUP > IF DROP ELSE NIP THEN ;

: DINVERT ( xd1 -- xd2 ) SWAP INVERT SWAP INVERT ;
: DNEGATE (  d1 --  d2 ) DINVERT 1 M+ ; \ 2's complement

: D0= (      xd -- flag ) 0= SWAP 0= AND ;
: D0< (       d -- flag ) 0< NIP ;
: D0> (       d -- flag ) 0> NIP ;
: D=  ( xd1 xd2 -- flag ) ROT ( l1 l2 h2 h1 ) = >R = R> AND ;
: D<  (  d1  d2 -- flag ) D- D0< ;
: D>  (  d1  d2 -- flag ) D- D0> ;
: DU< ( ud1 ud2 -- flag ) ROT ( l1 l2 h2 h1 ) 2DUP U> >R D= >R U< R> AND R> OR ;
: 2ROT ( xd1 xd2 xd3 -- xd2 xd3 xd1 ) 2>R 2SWAP 2R> 2SWAP ;

: DABS (    d  -- ud    ) DUP 0< IF DNEGATE THEN ;
: DMIN ( d1 d2 -- d1|d2 ) 2OVER 2OVER D> IF 2SWAP THEN 2DROP ;
: DMAX ( d1 d2 -- d1|d2 ) 2OVER 2OVER D< IF 2SWAP THEN 2DROP ;

\ Floored division
\ https://www.nimblemachines.com/symmetric-division-considered-harmful/
: FM/MOD ( d n -- rem quot )
    DUP >R \ Save divisor
    2DUP XOR 63 RSHIFT ( u n 0|1 ) >R \ 1 -> Signs differ ?
    SM/REM ( rem quot ) ( R: n signs? )
    OVER 0= R> 0= OR IF rdrop EXIT THEN \ rem = 0 or same signs -> No adjust
    1- \ Adjust quotient
    SWAP R> + SWAP \ Adjust remainder
;


\ More memory operations =======================================================

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

: ROLL ( xu xu-1 xu-2 ... x0 u -- xu-1 xu-2 ... x0 x1 ) [
    4C C, 89 C, C1 C,             \ mov  %r8, %rcx            u
    48 C, 8D C, 7C C, CD C, 00 C, \ lea  (%rbp,%rcx,8), %rdi  addr of xu
    48 C, 8D C, 77 C, F8 C,       \ lea  -8(%rdi), %rsi       addr of xu-1
    4C C, 8B C, 07 C, sp+         \ mov  (%rdi), %r8          pick u
    FD C, F3 C, 48 C, A5 C, FC C, \ std; rep movsq; cld       offset stack
] ;


\ OS ===========================================================================

0 CONSTANT sys-read
1 CONSTANT sys-write
0 CONSTANT stdin
1 CONSTANT stdout
2 CONSTANT stderr

: syscall3 ( num arg1 arg2 arg3 -- n ) [
    48 C, 8B C, 45 C, 10 C, \ movq    16(%rbp), %rax
    48 C, 8B C, 7D C, 08 C, \ movq    8(%rbp),  %rdi
    48 C, 8B C, 75 C, 00 C, \ movq    0(%rbp),  %rsi
    4C C, 89 C, C2 C,       \ movq    %r8,      %rdx
    0F C, 05 C,             \ syscall
    49 C, 89 C, C0 C,       \ movq    %rax, %r8
    48 C, 8D C, 6D C, 18 C, \ lea     24(%rbp), %rbp
] ;


VARIABLE key-buf
: KEY ( -- -1|char ) \ -1 on failure
    sys-read stdin key-buf 1 syscall3
    0<= IF -ONE ELSE key-buf @ THEN
;

VARIABLE emit-buf
: EMIT ( char -- ) emit-buf ! sys-write stdout emit-buf 1 syscall3 DROP ;

: TYPE ( c-addr u -- ) 2>R sys-write stdout 2R> syscall3 DROP ;


\ Strings ======================================================================

00 CONSTANT #nul
07 CONSTANT #bel
08 CONSTANT #bs
09 CONSTANT #ht
0A CONSTANT #lf
0B CONSTANT #vt
0C CONSTANT #ff
0D CONSTANT #cr
1B CONSTANT #esc
7F CONSTANT #del

: SPACE  (   -- ) BL EMIT ;
: SPACES ( u -- ) 0 ?DO SPACE LOOP ;
: CR     (   -- ) #cr EMIT #lf EMIT ;
: graph? ( char -- flag ) DUP 20 >= SWAP 7E <= AND ;


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

: /STRING ( c-addr1 u1 n -- c-addr2 u2 ) TUCK - >R CHARS + R> ;

: s= ( c-addr1 u1 c-addr2 u2 -- flag ) \ String equality
    2 PICK <> IF DROP 2DROP FALSE EXIT THEN \ <>len -> false
    ( c-addr1 u c-addr2 )
    2 PICK OVER = IF DROP 2DROP TRUE EXIT THEN \ =addr and =len -> true
    SWAP ( c-addr1 c-addr2 u ) 0 ?DO \ Compare chars
        2DUP C@ SWAP C@ <> IF 2DROP FALSE UNLOOP EXIT THEN
        CHAR+ SWAP CHAR+ \ Don't care if addr1 and addr2 are swapped
    LOOP
    2DROP
    TRUE
;


\ Pictured numeric output ======================================================

VARIABLE BASE
: HEX     ( -- ) 10 BASE ! ;
: DECIMAL ( -- ) 0A BASE ! ;
HEX

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
    BASE @ UD/MOD ( rem ud2 )
    ROT >digit HOLD ( ud2 )
;

: #S ( ud1 -- ud2 )
    BEGIN # 2DUP D0= UNTIL
;

: SIGN (    n -- ) 0< IF [CHAR] - HOLD THEN ;
: #pad (    n -- ) #sz #idx @ - - BEGIN DUP 0> WHILE BL HOLD 1- REPEAT DROP ;

: U.   (    u -- )              0 <# #S                  #> TYPE SPACE ;
: .    (    u -- )    DUP ABS   0 <# #S ROT SIGN         #> TYPE SPACE ;
: U.R  (  u w -- ) >R           0 <# #S          R> #pad #> TYPE ;
: .R   (  n w -- ) >R DUP ABS   0 <# #S ROT SIGN R> #pad #> TYPE ;
: UD.  ( ud   -- )                <# #S                  #> TYPE SPACE ;
: UD.R ( ud w -- ) >R             <# #S          R> #pad #> TYPE SPACE ;
: D.   (    d -- )    DUP >R DABS <# #S  R> SIGN         #> TYPE SPACE ;
: D.R  (  d w -- ) >R DUP >R DABS <# #S 2R> SIGN    #pad #> TYPE SPACE ;

\ Hex with leading zeros
: .X   (   u -- ) BASE @ SWAP HEX 0 <# 10 0 DO # LOOP #> TYPE SPACE BASE ! ;


\ Number parser ================================================================

: digit> ( char -- -1|u )
    DUP  [CHAR] 0 U<  IF DROP -ONE  EXIT THEN
    DUP  [CHAR] 9 U<= IF [CHAR] 0 - EXIT THEN
    DUP  [CHAR] A U<  IF DROP -ONE  EXIT THEN
    DUP  [CHAR] Z U<= IF [ 0A CHAR A - ] LITERAL + EXIT THEN
    DUP  [CHAR] a U<  IF DROP -ONE  EXIT THEN
    DUP  [CHAR] z U<= IF [ 0A CHAR a - ] LITERAL + EXIT THEN
    DROP -ONE
;

: >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 ) \ Standard number parsing word
    BEGIN DUP WHILE
        OVER C@ \ Get char
        digit> DUP 0<  IF DROP EXIT THEN \ Convert digit to number
        DUP BASE @ U>= IF DROP EXIT THEN \ Stop if incompatible with base

        -ROT 1 /STRING 2>R ( ud digit ) \ Next char
        >R BASE @ UDM* R> M+ \ Mult by base and add digit  TODO error on overflow (Using a triple product)
        2R> ( ud2 c-addr2 u2 )
    REPEAT
;

\ TODO return ( -- ud 2 | u 1 | x 0 ) to allow for the 'xxx.' notation
: number ( c-addr u1 -- u2 flag ) \ Parse unsigned number
    DUP 0= IF 2DROP 0 FALSE EXIT THEN
    0 0 2SWAP >NUMBER ( ud c-addr u )
    0= >R ( ud c-addr ) 2DROP ( u ) R>
;

\ TODO: return ud
: number ( c-addr u -- n flag ) \ Parse signed number
    DUP 0= IF 2DROP 0 FALSE EXIT THEN
    OVER C@ [CHAR] - = DUP >R \ is-signed flag
    IF 1 /STRING THEN \ Next char if signed
    number ( u flag ) SWAP R> IF NEGATE THEN SWAP ( n flag )
;

: number ( c-addr u -- n flag ) \ Parse signed number with base
    DUP 0= IF 2DROP 0 FALSE EXIT THEN
    BASE @ >R

    OVER C@ CASE
        [CHAR] # OF DECIMAL  ENDOF
        [CHAR] $ OF HEX      ENDOF
        [CHAR] % OF 2 BASE ! ENDOF
        ( default ) >R -ONE /STRING R> \ If not base is present ignore
    ENDCASE

    1 /STRING number

    R> BASE !
;

: charlit ( c-addr u -- -1|char ) \ Check for '<char>' notation
                            3 <> IF DROP -ONE EXIT THEN
    DUP           C@ [CHAR] ' <> IF DROP -ONE EXIT THEN
    DUP 2 CHARS + C@ [CHAR] ' <> IF DROP -ONE EXIT THEN
    CHAR+ C@
;

: number ( c-addr u -- n flag ) \ Parse char literal or signed number with base
    2DUP charlit DUP 0> IF NIP NIP TRUE ELSE DROP number THEN
;


\ Tools ========================================================================

: CS-ROLL ROLL ;
: CS-PICK PICK ;

: N>R ( i*x n -- ) ( R: -- j*x n )
    DUP \ n -> stored last
    BEGIN DUP WHILE ( i*x n idx )
        ROT R> ( j*x n idx x ret ) 2>R
    1-
    REPEAT ( n 0 ) DROP
    R> ( n ret ) 2>R
;

: NR> ( -- i*x n ) ( R: j*x n )
    2R> ( n ret ) >R
    DUP ( n idx )
    BEGIN DUP WHILE
        2R> ( n idx x ret ) >R -ROT ( x n idx )
    1-
    REPEAT ( i*x n 0 ) DROP
;

: [DEFINED]   ( "<spaces>name" -- flag ) PARSE-NAME find NIP NIP 0<> ; IMMEDIATE
: [UNDEFINED] ( "<spaces>name" -- flag ) PARSE-NAME find NIP NIP 0=  ; IMMEDIATE

: DEPTH ( -- +n ) SP@ S0 SWAP - 8 / ; \ NOTE: Not using 8/ because it's unsigned

: ? ( addr -- ) @ . ;

: .S ( -- )
    DEPTH 0< IF ." UNDERFLOW " EXIT THEN
    DEPTH 0= IF ." EMPTY "     EXIT THEN
    DEPTH 1 ?DO S0 I 1+ CELLS - @ . LOOP
    DUP . \ Make shure %r8 is printed and not -8(%rbp)
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


\ Not really from tools word set
: MARKER HERE CREATE , DOES> @ FORGET-NAME ;

: ENVIRONMENT? ( c-addr u -- false | i*x true )
    2>R
    2R@ S" /COUNTED-STRING"     s= IF 2rdrop FF         TRUE  EXIT THEN
    2R@ S" /HOLD"               s= IF 2rdrop #sz        TRUE  EXIT THEN
    2R@ S" /PAD"                s= IF 2rdrop            FALSE EXIT THEN \ TODO About one page using dyn-alloc ?
    2R@ S" ADDRESS-UNIT-BITS"   s= IF 2rdrop 8          TRUE  EXIT THEN
    2R@ S" FLOORED"             s= IF 2rdrop FALSE      TRUE  EXIT THEN
    2R@ S" MAX-CHAR"            s= IF 2rdrop FF         TRUE  EXIT THEN
    2R@ S" MAX-D"               s= IF 2rdrop MAX-D      TRUE  EXIT THEN
    2R@ S" MAX-N"               s= IF 2rdrop MAX-N      TRUE  EXIT THEN
    2R@ S" MAX-U"               s= IF 2rdrop MAX-U      TRUE  EXIT THEN
    2R@ S" MAX-UD"              s= IF 2rdrop MAX-UD     TRUE  EXIT THEN
    2R@ S" RETURN-STACK-CELLS"  s= IF 2rdrop            FALSE EXIT THEN \ TODO sys_getrlim
    2R@ S" STACK-CELLS"         s= IF 2rdrop 4000       TRUE  EXIT THEN
    2rdrop FALSE
;


\ Text IO ======================================================================

VARIABLE ECHO
0 VALUE SOURCE-ID
: SOURCE ( -- c-addr u ) IN @ #IN @ ;

: ACCEPT ( c-addr +n1 -- +n2 ) \ -1 On failure
    2>R 0 ( n2 )
    BEGIN KEY CASE
        DUP 0< ?OF 2rdrop DROP -ONE  EXIT ENDOF \ Error char

        #cr OF 2rdrop            EXIT ENDOF \ OK
        #lf OF 2rdrop            EXIT ENDOF \ OK
        03  OF 2rdrop DROP 0     EXIT ENDOF \ C^C Cancel
        04  OF 2rdrop ?DUP 0= IF -ONE THEN EXIT ENDOF \ C^D Terminate

        #del OF ( n2 ) DUP 0> IF \ Ignore on first col
                ECHO @ IF #bs EMIT SPACE #bs EMIT THEN
                2R> -ONE /STRING 2>R \ Prev char
                1-
        THEN ENDOF

        R@ 0<= ?OF DROP ENDOF \ No more space, ignore the rest

        DUP graph? ?OF
            ECHO @ IF DUP EMIT THEN
            2R@ DROP C! \ Write
            2R> 1 /STRING 2>R \ Next char
            1+ ( n2 )
        ENDOF 0

        ( default ) >R DROP R>
    ENDCASE AGAIN
;

: REFILL ( -- flag )
    SOURCE-ID IF FALSE ELSE
        0 >IN !
        TIB 100 ACCEPT
        DUP #IN !
        0>=
    THEN
;

: SAVE-INPUT ( -- xn ... x1 n )
    >IN @ 1 \ Works for TIB and EVALUATE string
;

: RESTORE-INPUT ( xn ... x1 n -- flag )
    DUP 1 <> IF \ We did not put that there
        0 ?DO DROP LOOP
    ELSE
        DROP >IN !
    THEN
;


\ Interpreter ==================================================================

: FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
    DUP COUNT find ( c-addr xt flags 0|nt )
    0= IF 2DROP 0 ( c-addr 0 ) EXIT THEN
    ( c-addr xt flags ) ROT DROP ( xt flags )
    FLAG-IMM AND IF 1 ELSE -ONE THEN ;
;

DEFER INTERPRET \ Allow user-defined interpreter
VARIABLE PROMPT

: QUIT ( -- ) ( R: i*x -- )
    RSP0 [ 4C C, 89 C, C4 C, ] DROP \ mov %r8, %rsp
    0 TO SOURCE-ID
    FALSE STATE !

    BEGIN
        ECHO   @ IF CR THEN
        PROMPT @ IF STATE @ IF ." C " ELSE ." > " THEN THEN
        REFILL
        PROMPT @ IF SPACE THEN
    WHILE
        INTERPRET
        PROMPT @ IF ." ok" THEN
    REPEAT

    ." Bye!" CR BYE
;

: EVALUATE ( i*x c-addr u -- j*x )
    SOURCE 2>R >IN @ SOURCE-ID 2>R \ Save input
    -ONE TO SOURCE-ID 0 >IN ! #IN ! IN !  \ Set source to the string
    INTERPRET
    2R> TO SOURCE-ID >IN ! 2R> #IN ! IN ! \ Restore input
;


: ABORT ( i*x -- ) S0 [ 4C C, 89 C, C5 C, ] QUIT ; \ mov %r8, %rbp

: ABORT" ( "ccc<quote>" -- ) ( i*x x -- | i*x ) ( R: j*x -- | j*x )
    POSTPONE IF
    POSTPONE ."
    POSTPONE CR
    POSTPONE ABORT
    POSTPONE THEN
; IMMEDIATE


: (INTERPRET) ( i*x -- j*x ) \ Main interpreter
    BEGIN
        PARSE-NAME DUP
    WHILE
        2DUP 2>R find ( xt flags 0|nt )
        IF 2rdrop ( c-addr u xt flags )
            FLAG-IMM AND STATE @ 0= OR IF
                EXECUTE
            ELSE
                COMPILE,
            THEN
        ELSE ( xt flags ) 2DROP
            2R@ number IF ( n )
                STATE @ IF POSTPONE LITERAL THEN
                2rdrop
            ELSE
                ." **NOT FOUND** " 2R> TYPE ABORT
            THEN
        THEN
    REPEAT
    ( c-addr u ) 2DROP
;


: RESET
    DECIMAL
    ." Welcome to Scouarn Forth"
    CR ." used: "
    HERE CP0 - . ." total, "
    CP1  CP0 - . ." kernel, "
    HERE CP1 - . ." user "
    UNUSED CR ." free: " . CR

    ['] (PROMPT)    IS PROMPT
    ['] (INTERPRET) IS INTERPRET
    ECHO    ON
    PROMPT  ON
    ABORT \ No return
;

RESET


\ TODO Read that http://www.forth200x.org/escaped-strings.html
\ : S\" ( C: "ccc<quote>" -- ) ( -- c-addr u )


\ Let's try a simpler problem first : print escaped in interpreter
\ Then we replace EMIT by C, and keep track of total length
\ -> by comparing HERE before and after ?
\ TODO \x00 notation ?

: esc-seq ( c-addr1 u1 -- c-addr2 u2 )
    OVER C@ CASE
        [CHAR] a OF #bel EMIT ENDOF
        [CHAR] b OF #bs  EMIT ENDOF
        [CHAR] e OF #esc EMIT ENDOF
        [CHAR] f OF #ff  EMIT ENDOF
        [CHAR] l OF #lf  EMIT ENDOF
        [CHAR] m OF #cr  EMIT #lf EMIT ENDOF
        [CHAR] n OF #cr  EMIT #lf EMIT ENDOF
        [CHAR] q OF [CHAR] " EMIT ENDOF
        [CHAR] r OF #cr  EMIT ENDOF
        [CHAR] t OF #ht  EMIT ENDOF
        [CHAR] v OF #vt  EMIT ENDOF
        [CHAR] z OF #nul EMIT ENDOF
        [CHAR] " OF [CHAR] " EMIT 2DROP [CHAR] " PARSE ENDOF \ Parse more
        [CHAR] x OF [CHAR] X ENDOF \ TODO
        [CHAR] \ OF [CHAR] \ EMIT ENDOF
        ( default ) \ Ignore
    ENDCASE
;

: .\( ( "ccc<quote>" -- )
    [CHAR] " PARSE ( c-addr u )

    BEGIN DUP WHILE
        OVER C@
        DUP [CHAR] \ = IF
            DROP 1 /STRING esc-seq
        ELSE
            EMIT
        THEN
        1 /STRING
    REPEAT
    2DROP
;


HEX CR .\( B\qlabla"

