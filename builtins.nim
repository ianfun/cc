## `__builtin` functions

import core, llvm

proc offsetof*(a: CType, b: int): culonglong =
    return app.getoffsetof(a, b)

proc bult