#!/bin/bash
stty raw -echo #isig
cat ./sforth.fs - | ./sforth
stty cooked echo
