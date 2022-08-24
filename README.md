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

## Backend

LLVM C-API

## References

ISO book: *Programming languages — C*
