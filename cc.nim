## Main program
##
## build in LLVM-15

when defined(windows):
    # download from https://github.com/llvm/llvm-project/releases
    # LLVM-15.0.0-rc3-win64.exe 
    # unpack and install
    {.passL: "\"C:\\Program Files\\LLVM\\lib\\LLVM-C.lib\" llvm/llvmAPI".}
else:
    {.passL: "-lLLVM-15 ./llvm/llvmAPI".}

import core, cli, stream, lexer, cpp, parser, eval, LLVMbackend

setLexer()
setCpp()
setParser()
setEval()
parseCLI()
setBackend()

proc link(opath: string = app.output) =
    case app.linker:
    of GCCLD:
        var path = opath & ".o"
        writeObjectFile(path)
        runLD(path, opath)
    of LLD:
        var path = opath & ".o"
        writeObjectFile(path)
        runLLD(path, opath)

proc output() =
    case app.mode:
    of OutputLLVMAssembly:
        writeModuleToFile(app.output)
    of OutputBitcode:
        writeBitcodeToFile(app.output)
    of OutputObjectFile:
        writeObjectFile(app.output)
    of OutputAssembly:
        writeAssemblyFile(app.output)
    of OutputCheck:
        discard
    of OutputLink:
        link()

proc c() =
    let translation_unit = runParser()
    if err():
        stderr.writeLine("compilation terminated.")
        return
    if app.mode != OutputCheck:
        gen(translation_unit)
        nimLLVMOptModule(b.module)
        optimize()
        if app.runJit:
            runJit()
            if app.mode != OutputLink:
                stderr.write("cc: warning: jit cannot combine with other output flags\njit will not write output\n")
        else:
            verify()
            output()

proc brainfuck(i: Stream) =
    var s = "int getchar(void);int putchar(int);int main(){char*ptr=malloc(1024);"
    while true:
        var x = readChar(i)
        if x == '\0':
            break
        s.add(
            case x:
            of '+': "++ptr;"
            of '-': "--ptr;"
            of '<': "++*ptr"
            of '>': "--*ptr"
            of '.': "putchar(*ptr);"
            of ',': "*ptr = getchar();"
            of '[': "while(*ptr){"
            of ']': "}"
            else: ""
        )
    s.add("free(ptr);}")
    addString(s, "<brainfuck>")

proc bf() =
    for s in p.fstack:
        brainfuck(s)
        close(s)
    c()

case app.input:
of InputC:
    c()
of InputBF:
    bf()
of InputIR, InputBC:
    var reader = if app.input == InputBC: readBitcodeToModule else: readIRToModule
    b.module = reader(p.pathstack[1].cstring)
    if b.module != nil:
        var i = 2
        while true:
            if i == len(p.pathstack):
                optimize()
                output()
                break
            var n = reader(p.pathstack[i].cstring)
            if n == nil:
                break
            if link(b.module, n):
                break
            inc i
of InputObject, InputAsm:
    warning("use gcc instead for object and assembly file")

closeParser()
shutdown_backend()
