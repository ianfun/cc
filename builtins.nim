## `__builtin` functions

proc my_p_add*(a: cstring, b: auto): cstring {.importc: "myopsadd", header: "myop.h".}

proc strncmp(str1, str2: cstring; n: csize_t): cint {.importc: "strncmp", nodecl, header: "string.h".}

proc is_builtin_name*(name: string): bool =
    if strncmp(name, "__builtin_", 10) == 0:
        true
    elif strncmp(name, "__sync_", 7) == 0:
        true
    elif strncmp(name, "__atomic_", 9) == 0:
        true
    else:
        false

#[ 
proc expand__builtin_(name: string): Expr =
    var s = $my_p_add(name, 10)
    case s:
    of "offsetof":
    of "alloca":
    of "alloca_with_align":
    of "constant_p":
    of "bit_cast":
    of "expect":
    of "expect_with_probability":
    of "trap":
    of "unreachable":
    of "LINE":
    of "FILE":
    

proc offsetof*(a: CType, b: int): culonglong =
    return app.getoffsetof(a, b)
]#
