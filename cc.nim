## Main program
##
## build in LLVM-15
##
## run `llvm-config --system-libs -libs` to get your libs

{.passL: "-lLLVM-15 llvmAPI".}
import core, cli

core.init(lexer, cpp, parser, eval, llvm)
parseCLI()
p.filename = "<stdin>"
p.path = "<stdin>"
verbose("compiling")


let r = runParser()

if err() == false:
    verbose("generate code to LLVM IR")
    gen(r)
    verbose("running LLVM optimize")
    optimize()
    verbose("verify LLVM module")
    verify()
    verbose("print module to file")
    writeModuleToFile("main.ll")
    verbose("running jit")
    runjit()

core.shutdown()
