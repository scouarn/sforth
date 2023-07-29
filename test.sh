#!/bin/sh
(
    cat ./sforth.fs
    echo "ECHO OFF PROMPT OFF HEX"
    cat tests/tester.fs
    cat tests/core.fs
    echo "CR .( Tests done) CR"
) | ./sforth

