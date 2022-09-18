import config, core, parser, stream, constString
import LLVMbackend
import std/[editdistance, heapqueue]

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
      fstderr << "error: gcc returned " 
      fstderr << status
      fstderr << " exit status"
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
    fstderr << "error: ld.lld returned " 
    fstderr << $status
    fstderr << " exit status"

proc showVersion() =
  fstderr <<< "CC: C Compiler"
  fstderr <<< "Homepage: https://github.com/ianfun/cc.git"
  fstderr <<< "Bug report: https://github.com/ianfun/cc/issues"
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
    fstderr <<< "unrecognized input language: " & s
    quit 0

proc help()

proc hash(s: string): uint64 =
    # https://stackoverflow.com/questions/7666509/hash-function-for-string
    # https://stackoverflow.com/questions/2624192/good-hash-function-for-strings
    result = 5381
    for c in s:
        result = ((result shl 5) + result) + uint64(c)

proc hash2(s: string): uint64 =
    # https://stackoverflow.com/questions/7666509/hash-function-for-string
    # https://stackoverflow.com/questions/2624192/good-hash-function-for-strings
    result = 5381
    for i in 1..<len(s):
        result = ((result shl 5) + result) + uint64(s[i])

proc h(s: string): (ConstString, uint64) = (constStr(s), hash(s))

var cliOptions = [
  ("v".h, 0, cast[P](showVersion), "print version info"),
  ("version".h, 0, cast[P](showVersion), "print version info"),
  ("help".h, 0, help, "help"),
  ("-help".h, 0, help, "help"),
  ("print-targets".h, 0, listTargets, "print registered targets"),
  ("stdin".h, 0, setStdin, "add stdin to files"),
  ("jit".h, 0, proc () = app.runJit = true, "run `main` function in LLVM JIT"),
  ("target".h, 1, cast[P](proc (s: string) = app.triple = s), "set target triple: e.g: x86_64-pc-linux-gnu, x86_64-pc-windows-msvc"),
  ("verbose".h, 0, proc () = app.verboseLevel = WVerbose, "enable verbose message"),
  ("note".h, 0, proc () = app.verboseLevel = WNote, "enable note message"),
  ("warning".h, 0, proc () = app.verboseLevel = WWarning, "enable warning message"),
  ("Wall".h, 0, proc () = app.verboseLevel = WWarning, "enable warning message"),
  ("?".h, 0, help, "help"),
  ("o".h, 1, cast[P](proc (s: string) = app.output = s), "set output file path"),
  ("emit-llvm".h, 0, cast[P](proc () = app.mode = OutputLLVMAssembly), "output LLVM Assembly"),
  ("emit-bitcode".h, 0, cast[P](proc () = app.mode = OutputBitcode), "output LLVM bitcode"),
  ("c".h, 0, cast[P](proc () = app.mode = OutputObjectFile), "output object file"),
  ("ld".h, 0, cast[P](proc () = app.linker = GCCLD), "use ld, The GNU linker"),
  ("lld".h, 0, cast[P](proc () = app.linker = LLD), "use LLD, The LLVM linker"),
  ("s".h, 0, cast[P](proc () = app.mode = OutputAssembly),  "output assembly"),
  ("fsyntax-only".h, 0, cast[P](proc () = app.mode = OutputCheck), "parse input file, type checking, emit warning and messages.Do not output a file"),
  ("no-opaque-pointers".h, 0, proc () = app.opaquePointerEnabled = false, "disable opaque pointer"),
  ("O0".h, 0, proc () = app.optLevel = 0, "no optimization(default)"),
  ("O1".h, 0, proc () = app.optLevel = 1, "Somewhere between -O0 and -O2"),
  ("O2".h, 0, proc () = app.optLevel = 2, "enables most optimizations"),
  ("O3".h, 0, proc () = app.optLevel = 3, "enables optimizations that take longer to perform or that may generate larger code(for example, loop unrolling)"),
  ("O4".h, 0, proc () = app.optLevel = 3, " = O3"),
  ("Os".h, 0, proc () = app.sizeLevel = 1, "reduce code size"),
  ("Oz".h, 0, proc () = app.sizeLevel = 2, "reduce code size further"),
  ("x".h, 1, cast[P](setInput), "set input langauge")
]

proc help() =
  var e = newStringOfCap(20)
  fstderr <<< "command line options"
  fstderr <<< "Option                         Description"
  for i in cliOptions:
    fstderr << '-'
    fstderr << i[0][0].str
    var l = 30 - len(i[0][0].str)
    e.setLen 0
    while l > 0:
      e.add(' ')
      dec l
    fstderr << e
    fstderr <<< i[3]
  fstderr << '\n'
  showVersion()

proc addFile(s: string) =
  addInclude(s, false)

include "builtins.def"

type FixName = object
    priority: int
    name: string

proc `<`(a, b: FixName): bool = a.priority < b.priority

proc fix(name: string) =
    var fixList = initHeapQueue[FixName]()
    for i in 0..<len(cliOptions):
        var v = $ cliOptions[i][0][0]
        var d = editDistance(name, v)
        fixList.push(FixName(priority: d, name: v))
    var msg: string
    while len(fixList) > 0:
        var f = fixList.pop()
        if f.priority < 3:
            msg.add("Perhaps you meant: '-" & f.name & "'\n")
    if msg.len > 1:
        fstderr << msg

proc parseCLI*(): bool =
  var inputs = false
  var name: int
  while hasNext():
    let one = get()
    if one[0] == '-':
      var o = hash2(one)
      var has = false
      for i in cliOptions:
        if i[0][1] == o and i[0][0] == one[1..^1]:
          has = true
          if i[1] == 1:
            if hasNext() == false:
              core.error()
              fstderr << "command "
              fstderr << one 
              fstderr <<< " expect one argument"
              cc_exit(1)
            var s = get()
            cast[proc (s: string){.nimcall.}](i[2])(s)
          else:
            i[2]()
      if has == false:
        core.error()
        fstderr <<< "unrecognized command line option '" & one & '\''
        fix(one[1..^1])
    else:
      if inputs == false:
        inputs = true
        name = i
      addFile(one)
  if t.fstack.len == 0:
    core.error()
    fstderr <<< "no input files"
    return false
  if app.output.len == 0:
    i = name - 1
    app.output = get()
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
  return true
