## `__builtin` functions

import core

proc offsetof*(a: CType, b: int): culonglong =
    return app.getoffsetof(a, b)

