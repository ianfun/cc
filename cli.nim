import core, parser, stream
import LLVMbackend

var i = 0

proc hasNext(): bool =
  return i < len(options)

proc get(): string =
  result = options[i]
  inc i

type P = proc () {.nimcall.}

proc system(command: cstring): cint {.importc: "system", header: "stdio.h".}
# we use gcc to invoke linker instead of ld commandm which require a lot of commands
# /usr/bin/ld -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crt1.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtbegin.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/usr/lib/x86_64-linux-gnu/../../lib64 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../.. -L/usr/lib/llvm-10/bin/../lib -L/lib -L/usr/lib /tmp/t-ef1090.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtend.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crtn.o
when defined(windows):
  # windows has `_execlp()`, but windows has no `fork()`
  # CreateProcess is ok, but `system()` is easy
  proc runLD*(input, path: string) =
    var cmd = "gcc \"" & $input & "\" -o \"" & $path & '"'
    let status = system(cmd.cstring)
    if status != 0:
      core.error()
      cstderr << "error: gcc returned " 
      cstderr << status
      cstderr << " exit status"
else:
  import posix
  proc runLD*(input, path: string) =
    var pid = fork()
    if pid < 0:
      core.error()
      perror("fork")
    else:
      if pid == 0:
        discard execlp("gcc", "gcc", input.cstring, "-o", path.cstring, nil)
        core.error()
        perror("execlp")
      else:
        var status: cint = 0
        discard waitpid(pid, status, 0)
        if status != 0:
            core.error()
            stderr.writeLine("error: gcc returned " & $status & " exit status")

proc runLLD*(input, output: string) =
  var cmd = "ld.lld --hash-style=gnu --no-add-needed  --build-id --eh-frame-hdr -dynamic-linker /lib/x86_64-linux-gnu/libc.so.6 /usr/lib64/ld-linux-x86-64.so.2 "
  cmd &= input
  cmd &= " -o "
  cmd &= output
  let status = system(cmd.cstring)
  if status != 0:
    core.error()
    cstderr << "error: ld.lld returned " 
    cstderr << $status
    cstderr << " exit status"

proc showVersion() =
  cstderr <<< "CC: C Compiler"
  cstderr <<< "Homepage: https://github.com/ianfun/cc.git"
  cstderr <<< "Bug report: https://github.com/ianfun/cc/issues"
  LLVMbackend.dumpVersionInfo()

proc setStdin() =
  addStdin()
  app.output = "stdin"

proc setInput(s: string) =
  case s:
  of "c", "C":
    app.input = InputC
  of "S", "s", "assembler", "asm", "Asm", "ASM":
    app.input = InputAsm
  of "ir", "IR":
    app.input = InputIR
  of "bc", "BC":
    app.input = InputBC
  else:
    core.error()
    cstderr <<< "unrecognized input language: " & s
    quit 0

proc help()

var cliOptions = [
  ("v", 0, cast[P](showVersion), "print version info"),
  ("version", 0, cast[P](showVersion), "print version info"),
  ("help", 0, help, "help"),
  ("-help", 0, help, "help"),
  ("stdin", 0, setStdin, "add stdin to files"),
  ("jit", 0, proc () = app.runJit = true, "run `main` function in LLVM JIT"),
  ("target", 1, cast[P](proc (s: string) = app.triple = s), "set target triple: e.g: x86_64-pc-linux-gnu, x86_64-pc-windows-msvc"),
  ("verbose", 0, proc () = app.verboseLevel = WVerbose, "enable verbose message"),
  ("note", 0, proc () = app.verboseLevel = WNote, "enable note message"),
  ("warning", 0, proc () = app.verboseLevel = WWarning, "enable warning message"),
  ("Wall", 0, proc () = app.verboseLevel = WWarning, "enable warning message"),
  ("?", 0, help, "help"),
  ("o", 1, cast[P](proc (s: string) = app.output = s), "set output file path"),
  ("emit-llvm", 0, cast[P](proc () = app.mode = OutputLLVMAssembly), "output LLVM Assembly"),
  ("emit-bitcode", 0, cast[P](proc () = app.mode = OutputBitcode), "output LLVM bitcode"),
  ("c", 0, cast[P](proc () = app.mode = OutputObjectFile), "output object file"),
  ("ld", 0, cast[P](proc () = app.linker = GCCLD), "use ld, The GNU linker"),
  ("lld", 0, cast[P](proc () = app.linker = LLD), "use LLD, The LLVM linker"),
  ("s", 0, cast[P](proc () = app.mode = OutputAssembly),  "output assembly"),
  ("fsyntax-only", 0, cast[P](proc () = app.mode = OutputCheck), "parse input file, type checking, emit warning and messages.Do not output a file"),
  ("no-opaque-pointers", 0, proc () = app.opaquePointerEnabled = false, "disable opaque pointer"),
  ("O0", 0, proc () = app.optLevel = 0, "no optimization(default)"),
  ("O1", 0, proc () = app.optLevel = 1, "Somewhere between -O0 and -O2"),
  ("O2", 0, proc () = app.optLevel = 2, "enables most optimizations"),
  ("O3", 0, proc () = app.optLevel = 3, "enables optimizations that take longer to perform or that may generate larger code(for example, loop unrolling)"),
  ("O4", 0, proc () = app.optLevel = 3, " = O3"),
  ("Os", 0, proc () = app.sizeLevel = 1, "reduce code size"),
  ("Oz", 0, proc () = app.sizeLevel = 2, "reduce code size further"),
  ("x", 1, cast[P](setInput), "set input langauge")
]

proc help() =
  cstderr <<< "command line options"
  for i in cliOptions:
    cstderr << '-'
    cstderr << i[0]
    cstderr << "\t\t\t\t"
    cstderr << i[3]
  stderr << '\n'
  showVersion()

proc addFile(s: string) =
  addInclude(s, false)

include "builtins.def"

proc parseCLI*() =
  var inputs = false
  var name: int
  while hasNext():
    var one = get()
    if one[0] == '-':
      var o = one[1..^1]
      var has = false
      for i in cliOptions:
        if i[0] == o:
          has = true
          if i[1] == 1:
            if hasNext() == false:
              cstderr <<< "command expect one argument"
              quit 1
            var s = get()
            cast[proc (s: string){.nimcall.}](i[2])(s)
          else:
            i[2]()
      if has == false:
        core.error()
        cstderr <<< "unrecognized command line option '-" & o & '\''
    else:
      if inputs == false:
        inputs = true
        name = i
      addFile(one)
  if p.fstack.len == 0:
    core.error()
    cstderr <<< "no input files"
    quit 1
  if app.output.len == 0:
    i = name - 1
    app.output = get()
  newBackend(app.output, app.output)
  case app.mode:
  of OutputLink:
    app.output &= (when defined(windows): ".exe" else: ".out")
  of OutputLLVMAssembly:
    app.output &= ".ll"
  of OutputBitcode:
    app.output &= ".bc"
  of OutputObjectFile:
    app.output &= ".o"
  of OutputAssembly:
    app.output &= ".s"
  of OutputCheck:
    discard
  addString(builtin_predef, "built-in")
