import "."/[token, lexer, parser, llvmbackend]

newParser()
p.filename = "<stdin>"
p.path = "<stdin>"
addStdin()
getToken()

when true:
    let r = translation_unit()
    init_backend()
    gen(r)
    writeModuleToFile("main.ll")

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
