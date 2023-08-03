\ Even Fibonacci numbers

: EULER2 ( -- sum )
    0 >R 1 2 ( n n+1 ) ( R: sum1 )
    BEGIN OVER 4000000 U< WHILE
        OVER 1 AND 0= IF OVER R> + >R THEN ( R: sum2 )
        TUCK ( n n+1 n ) + ( n+1 n+2 )
    REPEAT
    2DROP R> ( sum )
;

EULER2 .
