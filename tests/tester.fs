\ This is the source for the ANS test harness, it is based on the
\ harness originally developed by John Hayes

\ (C) 1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY
\ MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE REMAINS.
\ VERSION 1.1

\ Revision history and possibly newer versions can be found at
\ http://www.forth200x/tests/ttester.fs


\ Modified by Scouarn to test Scouarn Forth:
\ - Removed floating point code (no [IF] anyway)
\ - Changed tab to spaces, words can't be terminated by tabs

BASE @
HEX

VARIABLE ACTUAL-DEPTH \ stack record
CREATE ACTUAL-RESULTS 20 CELLS ALLOT
VARIABLE START-DEPTH
VARIABLE XCURSOR \ for ...}T
VARIABLE ERROR-XT

: ERROR ERROR-XT @ EXECUTE ; \ for vectoring of error reporting

: EMPTY-FSTACK ;
: F{ ;
: F-> ;
: F} ;
: F...}T ;

: EMPTY-STACK \ ( ... -- ) empty stack; handles underflowed stack too.
   DEPTH START-DEPTH @ < IF
     DEPTH START-DEPTH @ SWAP DO 0 LOOP
   THEN
   DEPTH START-DEPTH @ > IF
     DEPTH START-DEPTH @ DO DROP LOOP
   THEN
   EMPTY-FSTACK ;

: ERROR1 \ ( C-ADDR U -- ) display an error message
           \ followed by the line that had the error.
   TYPE SOURCE TYPE CR \ display line corresponding to error
   EMPTY-STACK \ throw away everything else
;

' ERROR1 ERROR-XT !

: T{ \ ( -- ) record the pre-test depth.
   DEPTH START-DEPTH ! 0 XCURSOR ! F{ ;

: -> \ ( ... -- ) record depth and contents of stack.
   DEPTH DUP ACTUAL-DEPTH ! \ record depth
   START-DEPTH @ > IF \ if there is something on the stack
     DEPTH START-DEPTH @ - 0 DO \ save them
       ACTUAL-RESULTS I CELLS + !
     LOOP
   THEN
   F-> ;

: }T \ ( ... -- ) comapre stack (expected) contents with saved
   \ (actual) contents.
   DEPTH ACTUAL-DEPTH @ = IF          \ if depths match
     DEPTH START-DEPTH @ > IF          \ if something on the stack
       DEPTH START-DEPTH @ - 0 DO     \ for each stack item
         ACTUAL-RESULTS I CELLS + @    \ compare actual with expected
         <> IF S" INCORRECT RESULT: " ERROR LEAVE THEN
       LOOP
     THEN
   ELSE                                    \ depth mismatch
     S" WRONG NUMBER OF RESULTS: " ERROR
   THEN
   F} ;

: ...}T ( -- )
   XCURSOR @ START-DEPTH @ + ACTUAL-DEPTH @ <> IF
     S" NUMBER OF CELL RESULTS BEFORE '->' DOES NOT MATCH ...}T "
     S" SPECIFICATION: " ERROR
   ELSE DEPTH START-DEPTH @ = 0= IF
     S" NUMBER OF CELL RESULTS BEFORE AND AFTER '->' DOES NOT MATCH: "
     ERROR
   THEN THEN
   F...}T ;

: XTESTER ( X -- )
   DEPTH 0= ACTUAL-DEPTH @ XCURSOR @ START-DEPTH @ + 1+ < OR IF
     S" NUMBER OF CELL RESULTS AFTER '->' BELOW ...}T SPECIFICATION: "
     ERROR EXIT
   ELSE ACTUAL-RESULTS XCURSOR @ CELLS + @ <> IF
     S" INCORRECT CELL RESULT: " ERROR
   THEN THEN
   1 XCURSOR +! ;

: X}T XTESTER ...}T ;
: XX}T XTESTER XTESTER ...}T ;
: XXX}T XTESTER XTESTER XTESTER ...}T ;
: XXXX}T XTESTER XTESTER XTESTER XTESTER ...}T ;

\ Set the following flag to TRUE for more verbose output; this may
\ allow you to tell which test caused your system to hang.
VARIABLE VERBOSE
FALSE VERBOSE !

: TESTING \ ( -- ) TALKING COMMENT.
   SOURCE VERBOSE @
   IF DUP >R TYPE CR R> >IN !
   ELSE >IN ! DROP
   THEN ;

BASE !
