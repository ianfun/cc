## Main program
##
## build in LLVM-15

when defined(windows):
    # download from https://github.com/llvm/llvm-project/releases
    # LLVM-15.0.0-rc3-win64.exe 
    # unpack and install
    {.passL: "\"C:\\Program Files\\LLVM\\lib\\LLVM-C.lib\"".}
else:
    {.passL: "-lLLVM-15 llvmAPI".}

import core, cli

core.init(lexer, cpp, parser, eval, llvm)
parseCLI()
initTarget()

let translation_unit = runParser()

if err():
    stderr.write("compilation terminated.")
else:
    if app.mode != OutputCheck:
        gen(translation_unit)
        optimize()
        if app.runJit:
            runJit()
            if app.mode != OutputLink:
                stderr.write("cc: warning: jit cannot combine with other output flags\njit will not write output\n")
        else:
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
                case app.linker:
                of GCCLD:
                    var path = app.output & ".o"
                    writeObjectFile(path)
                    runLD(path, app.output)
                    verbose("output executable: " & app.output)
                of LLD:
                    var path = app.output & ".o"
                    writeObjectFile(path)
                    runLLD(path, app.output)
                    verbose("output executable: " & app.output)

core.shutdown()
