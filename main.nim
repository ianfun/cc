import "."/[token, lexer, parser]

proc test(testLex = false) =
    var p = newParser()
    reset()
    stdinParser()
    nextTok()
    while true:
        let e = expression()
        if e == nil:
            break
        echo e
    closeParser()
test(false)
