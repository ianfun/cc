import os, core, parser
import posix
from llvm import dumpVersionInfo

var i = 0
var options = commandLineParams()

proc hasNext(): bool =
  return i < len(options)

proc get(): string =
  result = options[i]
  inc i

type P = proc () {.nimcall.}

proc perror(str: cstring) {.importc: "perror", header: "stdio.h".}

proc runLD*(input, path: cstring) =
  var pid = fork()
  if pid < 0:
    core.error()
    perror("fork")
  else:
    if pid == 0:
      discard execlp("gcc", "gcc", input, "-o", path, nil)
      core.error()
      perror("execlp")
    else:
      var status: cint
      discard waitpid(pid, status, 0)
      if status != 0:
          core.error()
          stderr.writeLine("error: gcc returned " & $status & " exit status")

proc showVersion() =
  echo "CC: C Compiler"
  echo "Homepage: https://github.com/ianfun/cc.git"
  echo "Bug report: https://github.com/ianfun/cc/issues"
  llvm.dumpVersionInfo()

proc setStdin() =
  addStdin()
  app.output = "stdin"

proc help()

var cliOptions = [
  ("v", 0, cast[P](showVersion), "print version info"),
  ("version", 0, cast[P](showVersion), "print version info"),
  ("help", 0, help, "help"),
  ("-help", 0, help, "help"),
  ("stdin", 0, setStdin, "add stdin to files"),
  ("jit", 0, proc () = app.runJit = true, "run `main` function in LLVM JIT"),
  ("verbose", 0, proc () = app.verboseLevel = WVerbose, "enable verbose message"),
  ("note", 0, proc () = app.verboseLevel = WNote, "enable note message"),
  ("warning", 0, proc () = app.verboseLevel = WWarning, "enable warning message"),
  ("Wall", 0, proc () = app.verboseLevel = WWarning, "enable warning message"),
  ("?", 0, help, "help"),
  ("o", 1, cast[P](proc (s: string) = app.output = s), "set output file path"),
  ("emit-llvm", 0, cast[P](proc () = app.mode = OutputLLVMAssembly), "output LLVM Assembly"),
  ("emit-bitcode", 0, cast[P](proc () = app.mode = OutputBitcode), "output LLVM bitcode"),
  ("c", 0, cast[P](proc () = app.mode = OutputObjectFile), "output object file"),
  ("gccld", 0, cast[P](proc () = app.linker = GCCLD), "use ld, The GNU linker"),
  ("lld", 0, cast[P](proc () = app.linker = LLD), "use LLD, The LLVM linker"),
  ("s", 0, cast[P](proc () = app.mode = OutputAssembly),  "output assembly"),
  ("fsyntax-only", 0, cast[P](proc () = app.mode = OutputCheck), "parse input file, type checking, emit warning and messages.Do not output a file"),
  ("no-opaque-pointers", 0, proc () = app.opaquePointerEnabled = false, "disable opaque pointer"),
  ("O0", 0, proc () = app.optLevel = 0, "no optimization"),
  ("O1", 0, proc () = app.optLevel = 1, "Somewhere between -O0 and -O2"),
  ("O2", 0, proc () = app.optLevel = 2, "enables most optimizations"),
  ("O3", 0, proc () = app.optLevel = 3, "enables optimizations that take longer to perform or that may generate larger code"),
  ("O4", 0, proc () = app.optLevel = 3, " = O3"),
  ("Os", 0, proc () = app.sizeLevel = 1, "reduce code size"),
  ("Oz", 0, proc () = app.sizeLevel = 2, "reduce code size further"),
  ("x", 1, cast[P](
    proc (s: string) = 
    if s != "c":
      quit("only C is supported!")), "set input language(only C is supported!)")
]

proc help() =
  echo "command line options"
  for i in cliOptions:
    echo '-', i[0], "\t\t\t\t" ,i[3]
  echo()
  showVersion()

proc addFile(s: string) =
  if addInclude(s) == false:
    core.error()
    perror(s)

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
              quit "command expect one argument"
            var s = get()
            cast[proc (s: string){.nimcall.}](i[2])(s)
          else:
            i[2]()
      if has == false:
        core.error()
        stderr.writeLine("unrecognized command line option '-" & o & '\'')
    else:
      if inputs == false:
        inputs = true
        name = i
      addFile(one)
  if p.fstack.len == 0:
    core.error()
    stderr.writeLine "no input files"
    quit 1
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
