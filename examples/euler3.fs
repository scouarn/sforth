\ Largest Prime Factor


: FACTORS ( u -- ) \ Print the prime factors in order
    2 ( u1 u2 )
    BEGIN OVER 1 U> WHILE
        2DUP /MOD ( u1 u2 rem quot )
        SWAP 0= IF
            -ROT NIP ( quot u2 )
            DUP .
        ELSE
            DROP 1+
        THEN
    REPEAT
    2DROP
;

600851475143 FACTORS
