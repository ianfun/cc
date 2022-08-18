import "."/[token, lexer, parser]

proc test(testLex = false) =
    newParser()
    stdinParser()
    getToken()
    while true:
        let e = expression()
        if e == nil:
            break
        echo e.k
        echo e
    closeParser()
test(false)
