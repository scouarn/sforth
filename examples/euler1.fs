\ Multiples of 3 or 5

\ This solution is very slow since we are doing 2000 divisions
\ We could have two variable: I5 and I3 which we increment by 5 or 3 to get the
\ next multiple of 5 or 3

: EULER1 ( -- sum )
    0 ( sum ) 1000 0 DO
        I 3 MOD 0=
        I 5 MOD 0=
        OR IF I + THEN
    LOOP
;

EULER1 .
