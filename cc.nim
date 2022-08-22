import "."/[token, lexer, parser]

newParser()
addStdin()
getToken()

when false:
    translation_unit()

when false:
    while p.tok.tok != TEOF:
        stdout.write($p.tok.tok)
        stdout.write(' ')
        getToken()

when true:
    let e = expression()
    if e != nil:
        echo e.k
        echo e.ty
        echo e

closeParser()
