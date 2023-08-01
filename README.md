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


### Other system documentation


## References
- https://forth-standard.org/standard/words
- https://dacvs.neocities.org/SF/
- https://www.forth.com/starting-forth/9-forth-execution/
- https://compilercrim.es/bootstrap/miniforth/
- https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/


## TODO
- Test words longer than 128 bytes to check there's no unsigned vs signed byte bug
- Handle SIGINT
- Set the tty in raw mode in code
- Tests for Double and Tools
- Remaining from Double set : `2VALUE` (ext),  `M*/` and `123.` notation
- Self hosting: export dict as ELF (with a given entrypoint) and get rid of as/ld dep
- Remove lower case duplicates like `find` -> Case insensitivity option and use `(word)` for system words
- `QUIT`: check for stack underflow
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
