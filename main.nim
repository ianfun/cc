import "."/[token, lexer, parser]

proc test(testLex = false) =
    newParser()
    reset()
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
