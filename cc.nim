## Main program
##
## build in LLVM-15
##
## run `llvm-config --system-libs -libs` to get your libs

{.passL: "-lLLVM-15".}

import core

core.init(lexer, cpp, parser, llvm)
p.filename = "<stdin>"
p.path = "<stdin>"
verbose("compiling")
addStdin()

let r = runParser()

if err() == false:
    verbose("init LLVM backend")
    if newBackend():
        verbose("generate code to LLVM IR")
        gen(r)
        verbose("running LLVM optimize")
        optimize()
        verbose("verify LLVM module")
        verify()
        verbose("print module to file")
        writeModuleToFile("main.ll")
        #verbose("running jit")
        #runjit()

core.shutdown()
