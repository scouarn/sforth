\ :NONAME

VARIABLE nn1
VARIABLE nn2
T{ :NONAME 1234 ; nn1 ! -> }T
T{ :NONAME 9876 ; nn2 ! -> }T
T{ nn1 @ EXECUTE -> 1234 }T
T{ nn2 @ EXECUTE -> 9876 }T

\ ?DO
DECIMAL

: qd ?DO I LOOP ;
T{   789   789 qd -> }T
T{ -9876 -9876 qd -> }T
T{     5     0 qd -> 0 1 2 3 4 }T

: qd1 ?DO I 10 +LOOP ;
T{ 50 1 qd1 -> 1 11 21 31 41 }T
T{ 50 0 qd1 -> 0 10 20 30 40 }T

: qd2 ?DO I 3 > IF LEAVE ELSE I THEN LOOP ;
T{ 5 -1 qd2 -> -1 0 1 2 3 }T

: qd3 ?DO I 1 +LOOP ;
T{ 4  4 qd3 -> }T
T{ 4  1 qd3 ->  1 2 3 }T
T{ 2 -1 qd3 -> -1 0 1 }T

: qd4 ?DO I -1 +LOOP ;
T{  4 4 qd4 -> }T
T{  1 4 qd4 -> 4 3 2  1 }T
T{ -1 2 qd4 -> 2 1 0 -1 }T

: qd5 ?DO I -10 +LOOP ;
T{   1 50 qd5 -> 50 40 30 20 10   }T
T{   0 50 qd5 -> 50 40 30 20 10 0 }T
T{ -25 10 qd5 -> 10 0 -10 -20     }T

VARIABLE qditerations
VARIABLE qdincrement

: qd6 ( limit start increment -- )    qdincrement !
   0 qditerations !
   ?DO
     1 qditerations +!
     I
     qditerations @ 6 = IF LEAVE THEN
     qdincrement @
   +LOOP qditerations @
; 

T{  4  4 -1 qd6 ->                   0  }T
T{  1  4 -1 qd6 ->  4  3  2  1       4  }T
T{  4  1 -1 qd6 ->  1  0 -1 -2 -3 -4 6  }T
T{  4  1  0 qd6 ->  1  1  1  1  1  1 6  }T
T{  0  0  0 qd6 ->                   0  }T
T{  1  4  0 qd6 ->  4  4  4  4  4  4 6  }T
T{  1  4  1 qd6 ->  4  5  6  7  8  9 6  }T
T{  4  1  1 qd6 ->  1  2  3          3  }T
T{  4  4  1 qd6 ->                   0  }T
T{  2 -1 -1 qd6 -> -1 -2 -3 -4 -5 -6 6  }T
T{ -1  2 -1 qd6 ->  2  1  0 -1       4  }T
T{  2 -1  0 qd6 -> -1 -1 -1 -1 -1 -1 6  }T
T{ -1  2  0 qd6 ->  2  2  2  2  2  2 6  }T
T{ -1  2  1 qd6 ->  2  3  4  5  6  7 6  }T
T{  2 -1  1 qd6 -> -1  0  1          3  }T

HEX


\ ACTION-OF
T{ DEFER defer1 -> }T
T{ : action-defer1 ACTION-OF defer1 ; -> }T

T{ ' * ' defer1 DEFER! ->   }T
T{          2 3 defer1 -> 6 }T
T{ ACTION-OF defer1 -> ' * }T
T{    action-defer1 -> ' * }T

T{ ' + IS defer1 ->   }T
T{    1 2 defer1 -> 3 }T
T{ ACTION-OF defer1 -> ' + }T
T{    action-defer1 -> ' + }T


\ BUFFER:
DECIMAL
T{ 127 CHARS BUFFER: TBUF1 -> }T
T{ 127 CHARS BUFFER: TBUF2 -> }T

\ Buffer is aligned
T{ TBUF1 ALIGNED -> TBUF1 }T

\ Buffers do not overlap
T{ TBUF2 TBUF1 - ABS 127 CHARS < -> <FALSE> }T

\ Buffer can be written to
1 CHARS CONSTANT /CHAR
: TFULL? ( c-addr n char -- flag )
   TRUE 2SWAP CHARS OVER + SWAP ?DO
     OVER I C@ = AND
   /CHAR +LOOP NIP
;

T{ TBUF1 127 CHAR * FILL   ->        }T
T{ TBUF1 127 CHAR * TFULL? -> <TRUE> }T

T{ TBUF1 127 0 FILL   ->        }T
T{ TBUF1 127 0 TFULL? -> <TRUE> }T
HEX


\ C"
T{ : cq1 C" 123" ; -> }T
T{ : cq2 C" " ;    -> }T
T{ cq1 COUNT EVALUATE -> 123 }T
T{ cq2 COUNT EVALUATE ->     }T


\ CASE
: cs1 CASE 1 OF 111 ENDOF
   2 OF 222 ENDOF
   3 OF 333 ENDOF
   >R 999 R>
   ENDCASE
;

T{ 1 cs1 -> 111 }T
T{ 2 cs1 -> 222 }T
T{ 3 cs1 -> 333 }T
T{ 4 cs1 -> 999 }T
: cs2 >R CASE
   -1 OF CASE R@ 1 OF 100 ENDOF
                2 OF 200 ENDOF
                >R -300 R>
        ENDCASE
     ENDOF
   -2 OF CASE R@ 1 OF -99 ENDOF
                >R -199 R>
        ENDCASE
     ENDOF
     >R 299 R>
   ENDCASE R> DROP ;

T{ -1 1 cs2 ->  100 }T
T{ -1 2 cs2 ->  200 }T
T{ -1 3 cs2 -> -300 }T
T{ -2 1 cs2 ->  -99 }T
T{ -2 2 cs2 -> -199 }T
T{  0 2 cs2 ->  299 }T


\ COMPILE,
:NONAME DUP + ; CONSTANT dup+
T{ : q dup+ COMPILE, ; -> }T
T{ : as [ q ] ; -> }T
T{ 123 as -> 246 }T


\ DEFER
T{ DEFER defer2 ->   }T
T{ ' * ' defer2 DEFER! -> }T
T{   2 3 defer2 -> 6 }T

T{ ' + IS defer2 ->   }T
T{    1 2 defer2 -> 3 }T


\ DEFER!
T{ DEFER defer3 -> }T

T{ ' * ' defer3 DEFER! -> }T
T{ 2 3 defer3 -> 6 }T

T{ ' + ' defer3 DEFER! -> }T
T{ 1 2 defer3 -> 3 }T


\ DEFER@
T{ DEFER defer4 -> }T

T{ ' * ' defer4 DEFER! -> }T
T{ 2 3 defer4 -> 6 }T
T{ ' defer4 DEFER@ -> ' * }T

T{ ' + IS defer4 -> }T
T{ 1 2 defer4 -> 3 }T
T{ ' defer4 DEFER@ -> ' + }T


\ FALSE
T{ FALSE -> 0 }T
T{ FALSE -> <FALSE> }T


\ HOLDS \ Modified to work with Scouarn Forth
\ T{ 0. <# S" Test" HOLDS #> S" Test" COMPARE -> 0 }T
: hold-str S" Test" ;
T{ 0 0 <# hold-str HOLDS #> hold-str S= -> <TRUE> }T


\ IS
T{ DEFER defer5 -> }T
T{ : is-defer5 IS defer5 ; -> }T

T{ ' * IS defer5 -> }T
T{ 2 3 defer5 -> 6 }T

T{ ' + is-defer5 -> }T
T{ 1 2 defer5 -> 3 }T


\ PARSE-NAME
: parse-s1 S" abcd" ; \ Modified, S" isn't interpreted
: parse-s2 S" abcde" ;
T{ PARSE-NAME abcd  parse-s1 S= -> <TRUE> }T
T{ PARSE-NAME abcde parse-s2 S= -> <TRUE> }T

\ test empty parse area
T{ PARSE-NAME 
   NIP -> 0 }T    \ empty line
T{ PARSE-NAME    
   NIP -> 0 }T    \ line with white space

T{ : parse-name-test ( "name1" "name2" -- n ) 
   PARSE-NAME PARSE-NAME S= ; -> }T

T{ parse-name-test abcd abcd -> <TRUE> }T
T{ parse-name-test  abcd   abcd   -> <TRUE> }T
T{ parse-name-test abcde abcdf -> <FALSE> }T
T{ parse-name-test abcdf abcde -> <FALSE> }T
T{ parse-name-test abcde abcde 
    -> <TRUE> }T
T{ parse-name-test abcde abcde  
    -> <TRUE> }T    \ line with white space


\ SAVE-INPUT    TODO [IF] [ELSE]... are needed for this test

\ \ Testing with a file source
\ VARIABLE siv -1 siv !
\ 
\ : NeverExecuted
\    ." This should never be executed" ABORT
\ ;
\ 
\ 11111 SAVE-INPUT
\ 
\ siv @
\ 
\ [IF]
\    0 siv !
\    RESTORE-INPUT
\    NeverExecuted
\ [ELSE]
\    \ Testing the ELSE part is executed
\    22222
\ [THEN]
\ 
\ T{ -> 11111 0 22222 }T    \ 0 comes from RESTORE-INPUT
\ 
\ Testing with a string source
\ VARIABLE si_inc 0 si_inc !
\ 
\ : si1
\    si_inc @ >IN +!
\    15 si_inc !
\ ;
\ 
\ : s$ S" SAVE-INPUT si1 RESTORE-INPUT 12345" ;
\ 
\ T{ s$ EVALUATE si_inc @ -> 0 2345 15 }T
\ 
\ \ Testing nesting
\ : read_a_line
\    REFILL 0=
\    ABORT" REFILL failed"
\ ;
\ 
\ 0 si_inc !
\ 2VARIABLE 2res -1. 2res 2!
\ 
\ : si2
\    read_a_line
\    read_a_line
\    SAVE-INPUT
\    read_a_line
\    read_a_line
\    s$ EVALUATE 2res 2!
\    RESTORE-INPUT
\ ;
\ 
\ \ WARNING: do not delete or insert lines of text after si2 is called otherwise
\ \ the next test will fail
\ 
\ si2
\ 33333                  \ This line should be ignored
\ 2res 2@ 44444        \ RESTORE-INPUT should return to this line
\ 
\ 55555
\ 
\ T{ -> 0 0 2345 44444 55555 }T


\ TRUE
T{ TRUE -> <TRUE> }T
T{ TRUE -> 0 INVERT }T 


\ VALUE
T{  111 VALUE v1 -> }T
T{ -999 VALUE v2 -> }T
T{ v1 ->  111 }T
T{ v2 -> -999 }T
T{ 222 TO v1 -> }T
T{ v1 -> 222 }T

T{ : vd1 v1 ; -> }T
T{ vd1 -> 222 }T

T{ : vd2 TO v2 ; -> }T
T{ v2 -> -999 }T
T{ -333 vd2 -> }T
T{ v2 -> -333 }T
T{ v1 ->  222 }T


\ [COMPILE]
\ With default compilation semantics
T{ : [c1] [COMPILE] DUP ; IMMEDIATE -> }T
T{ 123 [c1] -> 123 123 }T

\ With an immediate word
T{ : [c2] [COMPILE] [c1] ; -> }T
T{ 234 [c2] -> 234 234 }T

\ With special compilation semantics
T{ : [cif] [COMPILE] IF ; IMMEDIATE -> }T
T{ : [c3]  [cif] 111 ELSE 222 THEN ; -> }T
T{ -1 [c3] -> 111 }T
T{  0 [c3] -> 222 }T



\ S\"
\ From http://www.forth200x.org/escaped-strings.html

HEX
T{ : GC5 S\" \a\b\e\f\l\m\q\r\t\v\x0F0\x1Fa\xaBx\z\"\\" ; -> }T
T{ GC5 SWAP DROP          -> 14 }T \ String length
T{ GC5 DROP            C@ -> 07 }T \ \a   BEL  Bell
T{ GC5 DROP  1 CHARS + C@ -> 08 }T \ \b   BS   Backspace
T{ GC5 DROP  2 CHARS + C@ -> 1B }T \ \e   ESC  Escape
T{ GC5 DROP  3 CHARS + C@ -> 0C }T \ \f   FF   Form feed
T{ GC5 DROP  4 CHARS + C@ -> 0A }T \ \l   LF   Line feed
T{ GC5 DROP  5 CHARS + C@ -> 0D }T \ \m        CR of CR/LF pair
T{ GC5 DROP  6 CHARS + C@ -> 0A }T \           LF of CR/LF pair
T{ GC5 DROP  7 CHARS + C@ -> 22 }T \ \q   "    Double Quote
T{ GC5 DROP  8 CHARS + C@ -> 0D }T \ \r   CR   Carriage Return
T{ GC5 DROP  9 CHARS + C@ -> 09 }T \ \t   TAB  Horizontal Tab
T{ GC5 DROP  A CHARS + C@ -> 0B }T \ \v   VT   Vertical Tab
T{ GC5 DROP  B CHARS + C@ -> 0F }T \ \x0F      Given Char
T{ GC5 DROP  C CHARS + C@ -> 30 }T \ 0    0    Digit follow on
T{ GC5 DROP  D CHARS + C@ -> 1F }T \ \x1F      Given Char
T{ GC5 DROP  E CHARS + C@ -> 61 }T \ a    a    Hex follow on
T{ GC5 DROP  F CHARS + C@ -> AB }T \ \xaB      Insensitive Given Char
T{ GC5 DROP 10 CHARS + C@ -> 78 }T \ x    x    Non hex follow on
T{ GC5 DROP 11 CHARS + C@ -> 00 }T \ \z   NUL  No Character
T{ GC5 DROP 12 CHARS + C@ -> 22 }T \ \"   "    Double Quote
T{ GC5 DROP 13 CHARS + C@ -> 5C }T \ \\   \    Back Slash

