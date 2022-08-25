# build in LLVM-15
# run `llvm-config --system-libs -libs` to get your libs
{.passL: "-lLLVM-15".}

import "."/[token, lexer, parser, llvm]
init_backend()
newParser()
p.filename = "<stdin>"
p.path = "<stdin>"
addStdin()
getToken()

when true:
    let r = translation_unit()
    gen(r)
    if verify():
      writeModuleToFile("main.ll")
      runjit()

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
shutdown_backend()
