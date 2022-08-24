import token

const
   sizoefint = sizeof(cint).csize_t
   sizeofpointer = sizeof(pointer).csize_t

proc isFloating*(ty: CType): bool =
    bool(ty.tags and (TYFLOAT or TYDOUBLE))

proc isSigned*(ty: CType): bool =
    bool(ty.tags and (
        TYBOOL or
        TYINT8 or
        TYINT16 or
        TYINT16 or
        TYINT32 or
        TYINT64
    ))

proc getsizeof*(ty: CType): csize_t =
    case ty.spec:
    of TYPRIM:
        if (ty.tags and TYVOID) != 0:
            type_error("cannot get size of void")
            0.csize_t
        elif (ty.tags and TYBOOL) != 0:
            1.csize_t
        elif (ty.tags and TYINT8) != 0:
            1.csize_t
        elif (ty.tags and TYUINT8) != 0:
            1.csize_t
        elif (ty.tags and TYINT16) != 0:
            2.csize_t
        elif (ty.tags and TYUINT16) != 0:
            2.csize_t
        elif (ty.tags and TYINT32) != 0:
            4.csize_t
        elif (ty.tags and TYUINT32) != 0:
            4.csize_t
        elif (ty.tags and TYINT64) != 0:
            8.csize_t
        elif (ty.tags and TYUINT64) != 0:
            8.csize_t
        elif (ty.tags and TYFLOAT) != 0:
            4.csize_t
        elif (ty.tags and TYDOUBLE) != 0:
            8.csize_t
        else:
            type_error("cannot get size of " & $ty)
            0.csize_t
    of TYPOINTER:
        sizeofpointer
    of TYSTRUCT:
        var max = 0.csize_t
        for (_, t) in ty.selems:
            let tmp = getsizeof(t)
            if tmp > max:
                max = tmp
        max
    of TYUNION:
        var sum = 0.csize_t
        for (_, t) in ty.selems:
            sum += getsizeof(t)
        sum
    of TYENUM:
        sizoefint
    of TYBITFIELD:
        getsizeof(ty.bittype)
    of TYARRAY:
        csize_t(ty.arrsize) * getsizeof(ty.arrtype)
    of TYFUNCTION:
        sizeofpointer

proc getsizeof*(e: Expr): csize_t =
    getsizeof(e.ty)

proc getAlignof*(ty: CType): csize_t =
    getsizeof(ty)

proc getAlignof*(e: Expr): csize_t =
    getAlignof(e.ty)

proc to(e: var Expr, tag: uint32) =
    if e.ty.tags != tag:
        e = Expr(k: ECast, ty: CType(tags: tag), castval: e)

proc castto*(e: var Expr, t: CType) =
    to(e, t.tags)

proc integer_promotions*(a: var Expr) =
    if a.ty.spec == TYBITFIELD or getsizeof(a) < sizoefint:
        to(a, TYINT)

proc conv*(a, b: var Expr) =
    a.ty.tags = a.ty.tags and prim
    b.ty.tags = b.ty.tags and prim
    if a.ty.spec != TYPRIM or b.ty.spec != TYPRIM:
        return
    if (a.ty.tags and TYLONGDOUBLE) != 0:
        to(b, TYLONGDOUBLE)
    elif (b.ty.tags and TYLONGDOUBLE) != 0:
        to(a, TYLONGDOUBLE)
    elif (a.ty.tags and TYDOUBLE) != 0:
        to(b, TYDOUBLE)
    elif (b.ty.tags and TYDOUBLE) != 0:
        to(a, TYDOUBLE)
    elif (a.ty.tags and TYFLOAT) != 0:
        to(b, TYFLOAT)
    elif (b.ty.tags and TYFLOAT) != 0:
        to(a, TYFLOAT)
    else:
        integer_promotions(a)
        integer_promotions(b)
        if a.ty.tags == b.ty.tags:
            return
        let
          sizeofa = getsizeof(a.ty)
          sizeofb = getsizeof(b.ty)
          isaunsigned = bool(a.ty.tags and unsigned)
          isbunsigned = bool(b.ty.tags and unsigned)
        if (isaunsigned and isbunsigned) or (isaunsigned == false and isbunsigned == false):
            if sizeofa > sizeofb:
                to(b, a.ty.tags)
            else:
                to(a, b.ty.tags)
        else:
            if isaunsigned and (sizeofa > sizeofb):
                to(b, a.ty.tags)
            elif isbunsigned and (sizeofb > sizeofa):
                to(a, b.ty.tags)
            elif isaunsigned==false and sizeofa > sizeofb:
                to(b, a.ty.tags)
            elif isbunsigned==false and sizeofb > sizeofa:
                to(a, b.ty.tags)
            else:
                if isaunsigned:
                    to(b, a.ty.tags)
                else:
                    to(a, b.ty.tags)


proc compatible*(e: var CType, expected: CType): bool =
    true

proc varargs_conv*(e: var Expr) =
    discard

proc checkInteger*(a: CType, scalar=false): bool =
    if a.spec != TYPRIM:
        if scalar and a.spec == TYPOINTER:
            return true
        return false
    if (a.tags and TYLONGDOUBLE) != 0:
        return false
    if (a.tags and TYDOUBLE) != 0:
        return false
    if (a.tags and TYFLOAT) != 0:
        return false
    if (a.tags and TYCOMPLEX) != 0:
        return false
    return true

proc checkInteger*(a, b: Expr) =
    let ok = checkInteger(a.ty) and checkInteger(b.ty)
    if ok == false:
        type_error("integer expected")

proc checkScalar*(a, b: Expr) =
    let ok = checkInteger(a.ty, scalar=true) and checkInteger(b.ty, scalar=true)
    if ok == false:
        type_error("scalar expected")

proc checkArithmetic*(a: CType): bool =
    if a.spec != TYPRIM:
        return false
    return true

proc checkArithmetic*(a, b: Expr) =
    let ok = checkArithmetic(a.ty) and checkArithmetic(b.ty)
    if ok == false:
        type_error("arithmetic type expected")

proc checkSpec*(a, b: var Expr) =
    if a.ty.spec != b.ty.spec:
        type_error("operands type mismatch: " & $a & ", " & $b)
    else:
        checkScalar(a, b)
        conv(a, b)

