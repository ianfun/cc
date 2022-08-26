## Main program
##
## build in LLVM-15
##
## run `llvm-config --system-libs -libs` to get your libs

{.passL: "-lLLVM-15".}

import "."/[token, parser, llvm, appInstance]
discard app
newParser()
p.filename = "<stdin>"
p.path = "<stdin>"
verbose("compiling")
addStdin()

when true:
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

when false:
    while p.tok.tok != TEOF:
        stdout.write($p.tok.tok)
        stdout.write(' ')
        getToken()

when false:
    let e = expression()
    if e != nil:
        echo e.k
        echo e.ty
        echo e

closeParser()
verbose("shutdown LLVM backend")
shutdown_backend()
