#!/bin/bash
stty raw -echo
cat ./sforth.fs - | ./sforth
stty cooked echo
