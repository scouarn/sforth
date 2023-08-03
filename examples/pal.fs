: PAL? ( c-addr u -- flag )
    DUP 0 ?DO
        \ exit if s[I] !<> s[u-1-I]
        OVER I + C@ ( c-addr u c1 )
        2 PICK I - 1- 2 PICK + C@ ( c-addr u c1 c2 )
        <> IF 2DROP UNLOOP FALSE EXIT THEN
    LOOP
    2DROP TRUE
;


: S1 S" abc" ;
: S2 S" aba" ;
: S3 S" abba" ;
: S4 S" abbb" ;

S1 PAL? . .S
S2 PAL? . .S
S3 PAL? . .S
S4 PAL? . .S
