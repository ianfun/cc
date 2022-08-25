# cc

C Compiler written in [Nim](https://nim-lang.org/)

## About

cc is a C to LLVM IR comipler(like clang)

## Steps of process

1. source file(file or string stream, defined in *stream.nim*) => lexical tokens *lexer.nim* => C-Preprocess(CPP)*lexer.nim* => parsing, type checking
2. generate LLVM IR using LLVM-C API
3. write LLVM code to file

## Usage

cc is main program of compiler

```bash
$ nim c cc # nim c cc.nim
$ ./cc # input files
```

build C file

```bash
g++ -x c `llvm-config --cflags` test.c `llvm-config --ldflags --libs all --system-libs` -o test
```

## LLVM API

Build in **LLVM 15**, C-API

other version may not able to compile!

## References

ISO book: *Programming languages — C*
