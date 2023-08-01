# Scouarn Forth

Scouarn Forth is a Forth system for x86\_64 Linux written in assembly and
mostly in Forth itself. It is the result of a personal project, it was made for
recreational and learning purposes. It is not intended to actually be used,
there are a lot of things I'm not satisfied with (See [TODO](#todo)).

Scouarn Forth is a Forth-2012 System :
- Providing the Core Extensions word set
- Providing the Programming-Tools word set
- Providing `AHEAD` `BYE` `CS-PICK` `CS-ROLL` `FORGET` `NAME>STRING` `[DEFINED]` `[UNDEFINED]` from the Programming-Tools Extensions word set

Most of the Double word set is also implemented but `M*/`, `2VALUE` and the
`123.` notation are missing.

It passes the [forth-standard test
suite](https://forth-standard.org/standard/testsuite) but of course that doesn't
mean it is 100% compliant with the standard as there still may are bugs and
misunderstandings from my part.

More detailed documentation in the [System
documentation](#system-documentation) section.


## Quick start

Building the bootstrap system (requires `as` and `ld`):
```console
make

# Alternatively:
as sforth.S -o sforth.o && ld sforth.o -o sforth
```

Running the system in interactive mode:
```console
./run.sh
```
> NOTE: The system expects user input to come from a TTY in raw mode with no
echo. In the `run.sh` script, `stty` is used to do that setup and go back to
cooked mode at exit.

Running files:
```console
cat sforth.fs <file1> <file2> <file3> | ./sforth
```

Running the test suite:
```console
make test
```


## About

### Bootstrap process

There are actually 2 systems implemented: a very basic bootstrap system written
in assembly (`sforth.S`) that compiles and run a second system written in
this limited Forth dialect (`sforth.fs`) which implement Forth-2012.
In the future I may add words to generate ELF and self-host the bootstrap.

When you run the bootstrap directly (`./sforth`) you won't even get a prompt.
Only upper case unsigned hex literals are supported and only a few words for
parsing, compiling and dictionary lookup are defined. This is why `sforth.fs`
starts by defining all the primitives in machine code using `C,` (provided by
the bootstrap). At the end of `sforth.fs` a new outer interpreter is compiled
and executed, at this point the bootstrap interpreter may be considered
unreachable code (I should put it in a section that gets deallocated).


### Memory map

Everything is statically allocated by the assembler when building the
bootstrap.  The data stack is limited to 16k and the dictionary to 64k, these
arbitrary sizes are defined in `sforth.S`, the system uses about 32k of the
dictionary when loaded.

I may implement dynamic allocation in the future, I don't know how to use
the `mmap` and `brk` syscalls yet and I think it will require to tinker with
the locations of the sections.


## System documentation

### Implementation-defined options
- Aligned address requirements: none
- Behavior of EMIT for non-graphic characters: like all characters,
    sent to the OS to be displayed by the terminal
- Character editing of ACCEPT:
    - return: send
    - backspace: delete
    - C^D: return -1 if nothing was written, else treat like return
    - C^C: cancel and return 0 (only if isig is disabled, otherwise it will interrupt the process)
- Character set (EMIT, KEY): ASCII
- Character-aligned address requirements: none
- Character-set-extensions matching characteristics: none
- Conditions under which control characters match a space delimiter: never
- Format of the control-flow stack: data stack
- Conversion of digits larger than thirty-five: won't be recognized as digits
- Display after input terminates ACCEPT: the line terminator is not echoed, nothing is written, the echoed text remains as is
- Exception abort sequence (ABORT"): type the message, CR and ABORT
- Input line terminator: either carriage return (C^M, return key) or line feed (C^J, keypad enter)
- Maximum size of a counted string, in characters: 255
- Maximum size of a parsed string: there is no hard limit, PARSE can
    potentially parse the entire input source but a line from user input is
    limited to 256 characters
- Maximum size of a definition name, in characters: 256
- Maximum string length for ENVIRONMENT?, in characters: no limit
- Method of selecting User input device: fixed to stdin
- Method of selecting User output device: fixed to stdout
- Methods of dictionary compilation: a dictionary entry is a word header
    followed directly by machine code, an execution token points to the address
    of the machine code block. The header contains the address of the previous
    entry, a flag byte, a length byte and characters of the name without padding
- Number of bits in one address unit: 8
- Number representation and arithmetic: 64bit 2's complement little endian
- Ranges for n, +n, u, d, +d, and ud:
    -  n: -2^63 -> 2^63-1
    - +n: 0 -> 2^63-1
    -  u: 0 -> 2^64-1
    -  d: -2^127 -> 2^127-1
    - +d: 0 -> 2^127-1
    - ud: 0 -> 2^128-1
- Read-only data-space regions: none
- Size of buffer at WORD: 256 characters
- Size of one cell in address units: 8
- Size of one character in address units: 1
- Size of the keyboard terminal input buffer: 256 characters
- Size of the pictured numeric output string buffer: 256 characters
- Size of the scratch area whose address is returned by PAD: indeterminate, from HERE+512 to the end of the allocated dictionary space (UNUSED-512), a minimum size will be guarantied when dynamic allocated is implemented
- System case-sensitivity characteristics: case sensitive
- System prompt: when user input is accepted, a space is emitted just after the echoed input and it is interpreted. `ok` is then written followed by `> ` (or `C ` in compilation mode) on a new line to indicate the system is ready to accept more input.
- Type of division rounding: symmetric by default
- Values of STATE when true: all 1 bits (as in TRUE)
- Values returned after arithmetic overflow: the 'incorrect' result is kept
- Whether the current definition can be found after DOES>: no


### Ambiguous conditions
- name is neither a valid definition name nor a valid number during text interpretation: `**NOT FOUND** <word>` is written and the interpreter aborts
- a definition name exceeded the maximum length allowed: the size will be mod 256 but the entire name will be written to the definition, this is not a
    problem using user input since the TIB size is the same as the max length
- addressing a region not listed in 3.3.3 Data space: the system crashes with `Segmentation fault`

- ? argument type incompatible with specified input parameter, e.g., passing a flag to a word expecting an n (3.1 Data types):
- ? attempting to obtain the execution token, (e.g., with 6.1.0070 ', 6.1.1550 FIND, etc.) of a definition with undefined interpretation semantics:

- dividing by zero (6.1.0100 \*/, 6.1.0110 \*/MOD, 6.1.0230 /, 6.1.0240 /MOD, 6.1.1561 FM/MOD, 6.1.1890 MOD, 6.1.2214 SM/REM, 6.1.2370 UM/MOD, 8.6.1.1820 M\*/):
    the system crashes with `Floating point exception`
- insufficient data-stack space or return-stack space (stack overflow): segmentation fault
- insufficient space for loop-control parameters: segmentation fault
- insufficient space in the dictionary: segmentation fault

- interpreting a word with undefined interpretation semantics:
    - ?

- modifying the contents of the input buffer or a string literal (3.3.3.4 Text-literal regions, 3.3.3.5 Input buffers):
    - addresses of string literal point inside the definition they are compiled into, their contents can be changed but not their length, the definition will be executed with the new contents
    - ? input buffer

- overflow of a pictured numeric output string: corruption of the buffer used by WORD

- parsed string overflow:
    - Using WORD: corruption of the pictured numeric output
    - ? Using PARSE and PARSE-NAME

- producing a result out of range, e.g., multiplication (using \*) results in a value too big to be represented by a single-cell integer (6.1.0090 \*, 6.1.0100 \*/, 6.1.0110 \*/MOD, 6.1.0570 \>NUMBER, 6.1.1561 FM/MOD, 6.1.2214 SM/REM, 6.1.2370 UM/MOD, 8.6.1.1820 M\*/):
    - during a division: system crash with Floating Point Exception
    - during a multiplication: most significant cell ignored
- reading from an empty data stack or return stack (stack underflow): segmentation fault (a lot has to be popped until segfault)
- unexpected end of input buffer, resulting in an attempt to use a zero-length string as a name:
    - `:` will create a definition with an empty name
    - Will likely find a name defined by :NONAME
    - ? other conditions ?
- ? \>IN greater than size of input buffer (3.4.1 Parsing);
- ? 6.1.2120 RECURSE appears after 6.1.1250 DOES>;
- ? argument input source different than current input source for 6.2.2148 RESTORE-INPUT:
- ? data space containing definitions is de-allocated (3.3.3.2 Contiguous regions);
- data space read/write with incorrect alignment (3.3.3.1 Address alignment): no alignment required
- data-space pointer not properly aligned (6.1.0150 ,, 6.1.0860 C,): no alignment required
- ? less than u+2 stack items (6.2.2030 PICK, 6.2.2150 ROLL);
- ? loop-control parameters not available (6.1.0140 +LOOP, 6.1.1680 I, 6.1.1730 J, 6.1.1760 LEAVE, 6.1.1800 LOOP, 6.1.2380 UNLOOP);
- ? most recent definition does not have a name (6.1.1710 IMMEDIATE);
- ? 6.2.2295 TO not followed directly by a name defined by a word with "TO name runtime" semantics (6.2.2405 VALUE and 13.6.1.0086 (LOCAL));
- name not found 6.1.0070 ', 6.1.2033 POSTPONE, 6.1.2510 ['], 6.2.2530 [COMPILE]):
    - `'` and `[']` will return 0
    - `POSTPONE` and `[COMPILE]` will segfault
- parameters are not of the same type 6.1.1240 DO, 6.2.0620 ?DO, 6.2.2440 WITHIN): impossible to detect, parameters are treated as they were the same type
- ? 6.1.2033 POSTPONE, 6.2.2530 [COMPILE], 6.1.0070 ' or 6.1.2510 ['] applied to 6.2.2295 TO;
- ? string longer than a counted string returned by 6.1.2450 WORD;
- ? u greater than or equal to the number of bits in a cell (6.1.1805 LSHIFT, 6.1.2162 RSHIFT);
- ? word not defined via 6.1.1000 CREATE (6.1.0550 \>BODY, 6.1.1250 DOES>);
- ? words improperly used outside 6.1.0490 <# and 6.1.0040 #> (6.1.0030 #, 6.1.0050 #S, 6.1.1670 HOLD, 6.2.1675 HOLDS, 6.1.2210 SIGN)
- access to a deferred word, a word defined by 6.2.1173 DEFER, which has yet to be assigned to an xt: segmentation fault (call to address 0)
- access to a deferred word, a word defined by 6.2.1173 DEFER, which was not defined by 6.2.1173 DEFER: call to whatever is there, likely segmentation fault
- ? 6.1.2033 POSTPONE, 6.2.2530 [COMPILE], 6.1.2510 ['] or 6.1.0070 ' applied to 6.2.0698 ACTION-OF or 6.2.1725 IS;
- \\x is not followed by two hexadecimal characters (6.2.2266 S\\"):
    - if there is no character after \\x, it is ignored
    - if there is only one character after \\x, it is ignored and the remaining character is still processed
    - else exactly 2 characters are consumed and the number is the result of \>NUMBER using base 16 (stop converting at the first non hex character: \\x5W will return 5 and \\xW5 will return 0)
- a \\ is placed before any character, other than those defined in 6.2.2266 S\\": the slash and the character are ignored


### Other system documentation


## References
- https://forth-standard.org/standard/words
- https://dacvs.neocities.org/SF/
- https://www.forth.com/starting-forth/9-forth-execution/
- https://compilercrim.es/bootstrap/miniforth/
- https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/


## TODO
- Words that parse (POSTPONE, TO, ...): issue a `**NOT FOUND**` abort
- `cat` waits for a last input to figure out the pipe is closed
- Test words longer than 128 bytes to check there's no unsigned vs signed byte bug
- Handle SIGINT, FPE
- Set the tty in raw mode in code
- Tests for Double and Tools
- Remaining from Double set : `2VALUE` (ext),  `M*/` and `123.` notation
- Self hosting: export dict as ELF (with a given entrypoint) and get rid of as/ld dep
- Remove lower case duplicates like `find` -> Case insensitivity option and use `(word)` for system words
- underflow: check in QUIT or catch with segfault
- (Using argv?) `QUIT`: echo / prompt / error reporting when inputting from `cat file.fs - `, greet message
- Dyn alloc with mmap when `ALLOT` which becomes a primitive called by `,` and `C,`
- Documentation
- Better way to load the core file ?
- Control char treated as spaces when parsing ? https://forth-standard.org/standard/file#subsection.11.3.5  https://forth-standard.org/standard/usage#subsubsection.3.4.1.1
- Interpreted string
- Interpreted `[IF] [ELSE] [THEN]` -> Test for `SAVE-INPUT`
- Better output for `SEE`
- Make `RESET` create a marker to restore the dictionary.
- Deallocate the bootstrap when starting the outer interpreter
