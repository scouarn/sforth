\ Largest Palindrome Product

\ No smart trick, we just test if the string representation is a palindrome.

: STR-PAL? ( c-addr u -- flag )
    DUP 0 ?DO
        \ Exit when s[I] <> s[u-1-I]
        OVER I CHARS + C@ ( c-addr u s[I] )
        2 PICK 2 PICK 1- I - CHARS + C@ ( c-addr u s[I] s[u-1-I] )
        <> IF 2DROP UNLOOP FALSE EXIT THEN 
    LOOP
    2DROP TRUE
;

: INT-PAL? ( u -- flag ) 0 <# #S #> STR-PAL? ;


\ Find and print factor pairs that make palindromes.
\ We are looking for the biggest palindrome, start by only searching
\ with factors in the range 900-999. This is not rigorous.
: EULER4 ( -- )
    900 999 DO
    900  I  DO

        I J * DUP INT-PAL? IF
            CR 6 .R ."  = "
            I  3 .R ."  * "
            J  3 .R
        ELSE
            DROP
        THEN

    -1 +LOOP
    -1 +LOOP
    CR ." No palindrome found "
;

EULER4
