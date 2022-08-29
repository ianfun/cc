## Main program
##
## build in LLVM-15

{.passL: "-lLLVM-15 llvmAPI".}

import core, cli

core.init(lexer, cpp, parser, eval, llvm)
parseCLI()

let translation_unit = runParser()

if err():
    stderr.write("compilation terminated.")
else:
    if app.mode != OutputCheck:
        gen(translation_unit)
        optimize()
        if app.runJit:
            runJit()
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
            discard

core.shutdown()
