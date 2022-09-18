# cc

C language Compiler, Frontend!

## About

CC is a C language Preprocessor、Frontend, can compile C to LLVM

CC is tested in Windows 64 bit/WSL Ubuntu, with LLVM-15 installed and use GNU ld linker

cc is now can compile [xc.c](https://github.com/lotabout/write-a-C-interpreter), a C small interpreter!

![](screenshots.png)

## API Documentation

generated by Nim

[https://ianfun.github.io/cc/theindex.html](https://ianfun.github.io/cc/theindex.html)

## Steps of process

1. source file(file or string stream, defined in *stream.nim*) => lexical tokens *lexer.nim* => C-Preprocess(CPP)*lexer.nim* => parsing, type checking
2. generate LLVM IR using LLVM-C API
3. write LLVM code to file

## Install

* install LLVM 15
  - [Debian/Ubuntu](https://apt.llvm.org/)
  - [Windows](https://github.com/llvm/llvm-project/releases)
* install a C++ compiler(g++, clang)
* use `llvm-config` to find LLVM binary libary path, use for linking cc

It's a difficult to build cc in windows(for you need LLVM headers and link them with LLVM C++ libary), so you can use WSL in windows to build cc!

## Build

First, build C++ API helper file

```bash
$ g++ `llvm-config --cxxflags` llvmAPI.cpp -c -o llvmAPI -O3
```

Build with Nim

```bash
$ nim c cc
$ nim -d:release cc
```

Documentation generated by

```bash
$ nim doc --project --index:on --git.url:https://github.com/ianfun/cc --git.commit:master cc.nim
```

in windows, cc can build with [llvm-mingw](https://github.com/mstorsjo/llvm-mingw)

but llvm-mingw does not register all targets

```bash
$ clang++ --print-targets
  Registered Targets:
    aarch64    - AArch64 (little endian)
    aarch64_32 - AArch64 (little endian ILP32)
    aarch64_be - AArch64 (big endian)
    arm        - ARM
    arm64      - ARM64 (little endian)
    arm64_32   - ARM64 (little endian ILP32)
    armeb      - ARM (big endian)
    thumb      - Thumb
    thumbeb    - Thumb (big endian)
    x86        - 32-bit X86: Pentium-Pro and above
    x86-64     - 64-bit X86: EM64T and AMD64
```

because in llvm-mingw source code [build-llvm.sh](https://github.com/mstorsjo/llvm-mingw/blob/master/build-llvm.sh), 

```
-DLLVM_TARGETS_TO_BUILD="ARM;AArch64;X86"
```

so you can

* modify `llvm::InitializeAllTargets` with all registered targets
* build llvm-mingw yourself

note: cc need to link with `llvm-mingw-<version>-<crt>-<arch>\\bin\\libLLVM-15.dll`

# Usage

in Windows Terminal or some terminal

help

```bash
$ ./cc --help
```

compile file to object file(default)

```bash
$ ./cc test.c # wrote to test.c.out
```

set output file path

```bash
$ ./cc test.c -o test # wrote to test.out/test.exe
```

output LLVM Assembly

```bash
$ ./cc -emit-llvm test.c # write to test.ll
```
output GNU Assembly(GAS)

```bash
$ ./cc -s test.c # wrote to test.c.s
```

output LLVM bitcode

```bash
$ ./cc -emit-bitcode test.c # wrote to test.c.bc
```

set GNU linker(ld)

```bash
$ ./cc test.c -ld
```

set LLVM linker(ld.lld)

there are some issues in lld now(maybe missing C runtime libary?)

```bash
$ ./cc test.c -lld
ld.lld: error: undefined symbol: puts                                                                                         
>>> referenced by main                                                                                                        
>>>               test.c.out.o:(main)                                                                                         
>>> referenced by main                                                                                                        
>>>               test.c.out.o:(main)                                                                                         
                                                                                                                              
ld.lld: error: undefined symbol: system                                                                                       
>>> referenced by main                                                                                                        
>>>               test.c.out.o:(main)                                                                                         
cc: error: error: ld.lld returned 256 exit status
```

run in JIT(Just-In-Time Compilation)

```bash
$ ./cc -jit test.c
run program in JIT...
```

## Cross Compiling in cc

you can use `-target` command line option to cross compiling C program to a specific machine

pass `-c` flag will not run linker

for example, in WSL or Linux bash:

```bash
$ ./cc test.c -target x86_64-pc-windows-msvc -c # cross compiling from x86_64-pc-linux-gnu to x86_64-pc-windows-msvc
$ /mnt/c/xxx/gcc.exe test.c.o # compile in windows gcc
$ ./a.exe # run!
$ ./cc test.c -target x86_64-pc-linux-gnu -c # cross compiling from xxx target to to x86_64-pc-linux-gnu
$ ./a.out
```

## LLVM API

Build in **LLVM 15**, C-API

other version will to compile because cc use [Opaque Pointers](https://llvm.org/docs/OpaquePointers.html) API.

for example, `LLVMpointerTypeInContext()`, `LLVMBuildLoad2()`, `LLVMConstInBoundsGEP2()`, `LLVMBuildCall2()` are used in cc.

However, you can disable opaque Pointer in command line options.(use `LLVMContextSetOpaquePointers()` to disable)

## GNU Readline

cc use GNU Readline library by default for reading from stdin

define `CC_NO_RAEADLINE` to disable use GNU readlines

## JIT

cc use LLVM JIT, by default, `main(argc, argv)` cc will call this function as program startup .

## Features Supported

* VLA
* `__asm__` strings

## Contribute

contribute by pull requests is wellcome

## References

ISO book: *Programming languages — C*
