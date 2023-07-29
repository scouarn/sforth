#!/bin/sh
(
    cat ./sforth.fs
    echo "ECHO OFF PROMPT OFF HEX"
    cat tests/tester.fs
    echo "CR .( Testing the Core words) CR"
    cat tests/core.fs
    echo "CR .( Testing the Core extension words) CR"
    cat tests/core-ext.fs
    echo "CR .( Tests done) CR"
) | ./sforth

