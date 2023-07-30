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

> TODO

### Implementation-defined options


### Ambiguous conditions


### Other system documentation


## References
- https://forth-standard.org/standard/words
- https://dacvs.neocities.org/SF/
- https://www.forth.com/starting-forth/9-forth-execution/
- https://compilercrim.es/bootstrap/miniforth/
- https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/


## TODO
- Set the tty in raw mode by 
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
