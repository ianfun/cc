## A recursive descent parser for C language
## see ISO C grammar for details
## the main export function is expression, constant_expression, translation_unit, declaration

import token, lexer, eval
from std/sequtils import count

proc type_name*(): (CType, bool)

proc expression*(): Expr

proc constant_expression*(): Expr

proc primary_expression*(): Expr

proc postfix_expression*(): Expr

proc unary_expression*(): Expr

proc cast_expression*(): Expr

proc multiplicative_expression*(): Expr

proc shift_expression*(): Expr

proc relational_expression*(): Expr

proc equality_expression*(): Expr

proc AND_expression*(): Expr

proc exclusive_OR_expression*(): Expr

proc inclusive_OR_expression*(): Expr

proc logical_AND_expression*(): Expr

proc logical_OR_expression*(): Expr

proc conditional_expression*(start: Expr): Expr

proc conditional_expression*(): Expr

proc assignment_expression*(): Expr

proc parse_initializer_list*(): Expr

proc parse_parameter_type_list*(): (bool, seq[(string, CType)])

proc declaration*(): Stmt

# parse many type qualifiers, add to type
# used in decorator
proc parse_type_qualifier_list*(ty: var CType)

# merge many token to a type, for example: `long long int const`
proc merge_types*(ts: seq[Token]): Ctype

# declaration_specfier is used in function and variable declaration/definition
proc parse_declaration_specifiers*(): CType

# specfier_qualifier_list is used in struct declaration
proc parse_specifier_qualifier_list*(): CType

type 
    DeclaratorFlags = enum 
      Direct, Abstract, Function

proc parse_direct_declarator*(base: CType; flags=Direct): Stmt

# take a base type, return the final type and name
# for example: static   int     *foo
#                base-type    decorator
proc parse_declarator*(base: CType; flags=Direct): Stmt

# abstract decorator has no name
# for example: static int        (*)(int, char)
#               base-type      abstact-decorator
template parse_abstract_decorator*(base, f): untyped = parse_declarator(base, flags=f)

# type-name: specifier-qualifier-list optional<abstract_decorator>
# proc parse_type_name*(): CType

# parse a declaration: variable, struct, union, function...
# proc parse_declaration*(): Stmt

# parse a struct or union, return it
# for example: struct Foo
#              struct { ... }
#              struct Foo { ... }
proc parse_struct_union*(t: Token): CType

# parse a enum, return it
# for example: enum State
#              enum { ... }
#              enum State { ... }
proc parse_enum*(): CType

proc parse_static_assert*(): Stmt

proc translation_unit*(): seq[Stmt]

proc statament*(): Stmt

proc compound_statement*(): Stmt

proc binop*(a: Expr, op: BinOP, b: Expr): Expr = Expr(k: EBin, lhs: a, rhs: b, bop: op)

proc unary*(e: Expr, op: UnaryOP): Expr = Expr(k: EUnary, uop: op, uoperand: e)

proc postfix*(e: Expr, op: PostfixOP): Expr = Expr(k: EPostFix, pop: op, poperand: e)

proc consume*() =
    getToken()


# primitive types
const type_specifier_set = {
    Kchar, Kint, Kshort, Ksigned, 
    Kunsigned, 
    Klong, Kdouble, Kfloat,
    K_Atomic,
    K_Complex, Kvoid, K_Bool
} # + K_Alignas

# function specfiers
const function_specifier_set = {
    Kinline, K_Noreturn
}

# storage specfiers
const storage_class_specifier_set = {
    Ktypedef, Kextern, Kstatic, 
    K_Thread_local, Kauto, Kregister
}

# type qualifiers
const type_qualifier_set = {
     Kvolatile, Krestrict, Kconst
} # + atomic

# declaration specfiers
const declaration_specifier_set = 
    type_specifier_set +
    storage_class_specifier_set +
    type_qualifier_set +
    function_specifier_set

proc addTag(ty: var CType, t: Token): bool =
    let t = (
        case t:
        of Kinline: TYINLINE
        of K_Noreturn: TYNORETURN
        of K_Alignas: TYALIGNAS
        of Kextern: TYEXTERN
        of Kstatic: TYSTATIC
        of K_Thread_local: TYTHREAD_LOCAL
        of Kauto: TYAUTO
        of Kregister: TYREGISTER
        of Krestrict: TYRESTRICT
        of Kvolatile: TYVOLATILE
        of Ktypedef: TYTYPEDEF
        of Kconst: TYCONST
        of K_Atomic: TYATOMIC
        else: TYINVALID
    )
    if t == TYINVALID:
        return false
    ty.tags = ty.tags or t
    return true

proc default_storage(ts: var CType) =
    if (ts.tags and (TYTYPEDEF or TYTHREAD_LOCAL or TYAUTO or TYREGISTER or TYSTATIC or TYEXTERN)) == 0:
        ts.tags = ts.tags or TYAUTO # default to auto storage-class-specifier

proc merge_types*(ts: seq[Token]): CType =
    if ts.len == 0:
        return nil
    result = CType(tags: TYINVALID, spec: TYPRIM)
    var b: seq[Token]
    for t in ts:
        if addTag(result, t) == false:
            b.add(t)
    default_storage(result)
    if b.len == 0: # no type
        warning("deault type to `int`")
        result.tags = result.tags or TYINT
        return result
    if b.len == 1: # one type
        result.tags = result.tags or ( 
          case b[0]:
          of Kchar:
            TYCHAR
          of Kint:
            TYINT
          of Kvoid:
            TYVOID
          of Klong:
            TYLONG
          of Ksigned:
            TYINT
          of Kunsigned:
            TYUINT
          of Kshort:
            TYSHORT
          of Kdouble:
            TYDOUBLE
          of Kfloat:
            TYFLOAT
          of K_Bool:
            TYBOOL
          else:
            assert(false)
            TYINVALID
        )
        return result
    var signed = b.count(Ksigned)
    var unsigned = b.count(Kunsigned)
    let su = signed + unsigned
    if signed>1 or unsigned>1:
      type_error("too many `signed`/`unsigned`")
      return nil
    if su == 2:
     type_error("cannot both `signed` and `unsigned`")
     return nil
    let l = b.count(Klong)
    let s = b.count(Kshort)
    let f = b.count(Kfloat)
    let d = b.count(Kdouble)
    let i = b.count(Kint)
    let c = b.count(Kchar)
    let v = b.count(Kvoid)
    if c > 1:
        type_error("too many `char`s")
    if i > 1:
      type_error("too many `int`s")
      return nil
    if f > 0:
      type_error("`float` cannot combine with other types")
      return nil
    if v > 0:
      type_error("`void` cannot combine with other types")
      return nil
    if l >= 3:
      type_error("too many `long`s, max is `long long`")
      return nil
    if s >= 2:
      if s == 2:
        warning("duplicate 'short' declaration specifier")
      else:
        type_error("too many `short`s, max is `short`")
        return nil
    if d > 1:
       type_error("too many `double`s")
       return nil
    if bool(d):
      if d != 1:
        type_error("too many `long` for `double`")
        return nil
      if len(b) != (1 + l):
        type_error("extra `double` declaration specifier")
        return nil
      result.tags = result.tags or (if d==1: TYLONGDOUBLE else: TYDOUBLE)
      return result
    if bool(s): # short+int
      if (s + i + su) != len(b):
        type_error("extra `short` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUSHORT else: TYSHORT)
      return result
    if bool(l): # long+int
      if (l + i + su) != len(b):
        type_error("extra `long` declaration specifier")
        return nil
      case l:
      of 1:
        result.tags = result.tags or (if bool(unsigned): TYULONG else: TYLONG)
      of 2:
        result.tags = result.tags or (if bool(unsigned): TYULONGLONG else: TYLONGLONG)
      else:
        assert(false)
      return result
    if bool(c):
      if (c + su) != len(b):
        type_error("extra `char` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUCHAR else: TYCHAR)
      return result
    type_error("cannot combine types: " & $b)
    return nil

proc parse_type_qualifier_list(ty: var CType) =
    while true:
        case p.tok:
        of Kconst:
            ty.tags = ty.tags or TYCONST
            consume()
        of Krestrict:
            ty.tags = ty.tags or TYRESTRICT
            consume()
        of Kvolatile:
            ty.tags = ty.tags or TYVOLATILE
            consume()
        of K_Atomic:
            ty.tags = ty.tags or TYATOMIC
            consume()
        else:
            break

proc more(s: var seq[Token]) = 
    while p.tok in declaration_specifier_set:
        s.add(p.tok)
        consume()

proc read_enum_sepcs*(c: var CType, sepc: Token) = 
    discard

proc read_struct_union_sepcs*(c: var CType, sepc: Token) = 
    discard

proc handle_typedef(s: var seq[Token], ty: CType): CType =
    consume()
    while p.tok in declaration_specifier_set:
        s.add(p.tok)
        consume()
    if len(s) > 0:
        if Ktypedef in s:
            return ty
        warning("typedef types cannot combine with type-specifier and type-qualifiers")
    result = deepCopy(ty)
    result.tags = ty.tags and (not TYTYPEDEF)
    return result

proc parse_specifier_qualifier_list*(): CType =
    ## TODO: _Alignas
    var s: seq[Token]
    while p.tok in (type_specifier_set + type_qualifier_set + {Kstruct, Kenum, Kunion}):
        if p.tok == Kenum:
            result = parse_enum()
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif p.tok == Kunion or p.tok == Kstruct:
            result = parse_struct_union(p.tok)
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        elif p.tok == K_Atomic:
            consume()
            if p.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    parse_error("expect type-name")
                    return nil
                if p.tok != TRbracket:
                    parse_error("expect ')'")
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        else:
            s.add(p.tok)
            consume()
    if p.tok == TIdentifier:
        let ty = gettypedef(p.val.sval)[0]
        more(s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            return handle_typedef(s, ty)
    if s.len > 0:
        return merge_types(s)
    return nil

proc parse_declarator*(base: CType; flags=Direct): Stmt =
    var ty = base
    while p.tok == TMul:
        consume()
        ty = CType(tags: TYINVALID, spec: TYPOINTER, p: ty)
        parse_type_qualifier_list(ty)
    return parse_direct_declarator(ty, flags)

proc parse_initializer_list*(): Expr =
    if p.tok != TLcurlyBracket:
        return assignment_expression()
    result = Expr(k: EInitializer_list)
    consume()
    while true:
        if p.tok == TRcurlyBracket:
            consume()
            break
        if p.tok == TLcurlyBracket:
            let e = parse_initializer_list()
            if e == nil:
                return nil
            result.inits.add(e)
        else:
            let e = assignment_expression()
            if e == nil:
                error("expect expression")
                return nil
            result.inits.add(e)
        if p.tok == TComma:
            consume()

proc parse_direct_declarator_end*(base: CType, name: string): Stmt

proc parse_direct_declarator*(base: CType; flags=Direct): Stmt =
    case p.tok:
    of TIdentifier:
        if flags == Abstract:
            return nil
        var name = p.val.sval
        consume()
        return parse_direct_declarator_end(base, name)
    of TLbracket:
        consume()
        let st = parse_declarator(base, flags)
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        if st.k == SFunction:
            return parse_direct_declarator_end(st.functy, st.funcname)
        else:
            return parse_direct_declarator_end(st.var1type, st.var1name)
    else:
        if flags != Direct:
            return parse_direct_declarator_end(base, "")
        return nil

proc parse_direct_declarator_end*(base: CType, name: string): Stmt =
    case p.tok:
    of TLSquareBrackets: # int arr[5], int arr[*], int arr[static 5], int arr[static const 5], int arr[const static 5], int arr2[static const restrict 5]
        consume() # eat ]
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: -1, arrtype: base)
        if p.tok == TMul: # int arr[*]
          consume()
          if p.tok != TRSquareBrackets:
            error("expect ']'")
            note("the syntax is:\n\tint arr[*]")
            return nil
          return Stmt(k: SVarDecl1, var1name: name, var1type: ty)
        if p.tok == Kstatic:
           consume()
           ty.tags = ty.tags or TYSTATIC
           parse_type_qualifier_list(ty)
        else:
            parse_type_qualifier_list(ty)
            if p.tok == Kstatic:
                ty.tags = ty.tags or TYSTATIC
                consume()
        let e = assignment_expression()
        ty.arrsize = eval_const_expression(e)
        if p.tok != TRSquareBrackets:
          error("expect ']'")
          return nil
        consume() # eat ]
        return parse_direct_declarator_end(ty, name)
    of TLbracket:
        consume()
        var ty = CType(tags: TYINVALID, spec: TYFUNCTION, ret: base)
        if p.tok != TRbracket:
            let res = parse_parameter_type_list()
            if res[0]:
                ty.params = res[1]
            else:
                return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        if p.tok == TLcurlyBracket: # function definition
            let body = compound_statement()
            if body == nil:
                parse_error("expect function body")
                return nil
            return Stmt(funcname: name, k: SFunction, functy: ty, funcbody: body)
        return parse_direct_declarator_end(ty, name)
    else:
        return Stmt(k: SVarDecl1, var1name: name, var1type: base)

proc parse_struct_declarator(base: CType): (string, CType) =
    if p.tok == TColon:
        consume()
        let e = constant_expression()
        if e == nil:
            parse_error("expect expression")
            note("in bit field declaration")
            return ("", nil)
        let bitsize = eval_const_expression(e)
        return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: base, bitsize: bitsize))
    else:
        let d = parse_declarator(base, Direct)
        if d == nil:
            return ("", nil)
        if p.tok == TColon:
            consume()
            let e = constant_expression()
            if e == nil:
                parse_error("expect expression")
                note("in bit field declaration")
                return
            let bitsize = eval_const_expression(e)
            return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: if d.k == SFunction: d.functy else: d.var1type, bitsize: bitsize))
        if d.k == SFunction:
            return (d.funcname, d.functy)
        return (d.var1name, d.var1type)

proc parse_struct_union*(t: Token): CType =
    consume() # eat struct/union
    var name = ""
    if p.tok == TIdentifier:
        name = p.val.sval
        consume()
        if p.tok != TLcurlyBracket: # struct Foo
           return if t==Kstruct: getstructdef(name) else: getuniondef(name)
    elif p.tok != TLcurlyBracket:
        error("expect '{' for start anonymous struct/union")
        return nil
    if t == Kstruct:
        result = CType(tags: TYINVALID, spec: TYSTRUCT, sname: name)
    else:
        result = CType(tags: TYINVALID, spec: TYUNION, sname: name)
    consume()
    if p.tok != TRcurlyBracket:
        while true:
            if p.tok == K_Static_assert:
                let s = parse_static_assert()
                if s == nil:
                    return nil
                if p.tok == TRcurlyBracket:
                    break
                continue
            var base = parse_specifier_qualifier_list()
            if base == nil:
                parse_error("expect specifier-qualifier-list")
                return nil

            if p.tok == TSemicolon:
                warning("struct/union member declaration doest not declare anything")
                consume()
                continue
            else:
                while true:
                    let e = parse_struct_declarator(base)
                    if e[1] == nil:
                        error("expect struct-declarator")
                        return nil
                    result.selems.add(e)
                    if p.tok == TComma:
                        consume()
                    else:
                        if p.tok != TSemicolon:
                            parse_error("expect ';', got " & showToken())
                            return nil
                        consume()
                        break
                if p.tok == TRcurlyBracket:
                    consume()
                    break
    else:
        consume()
    if len(result.sname) > 0:
        if t == Kstruct:
            putstructdef(result)
        else:
            putenumdef(result)
    return result

let enum_type = CType(tags: TYEXPR or TYINT, spec: TYPRIM)

proc parse_enum*(): CType =
    consume() # eat enum
    var name = ""
    if p.tok == TIdentifier:
        name = p.val.sval
        consume()
        if p.tok != TLcurlyBracket: # struct Foo
          return getenumdef(name)
    elif p.tok != TLcurlyBracket:
        error("expect '{' for start anonymous enum")
        return nil
    result = CType(tags: TYINVALID, spec: TYENUM, ename: name)
    consume()
    var c: int = 0
    while true:
        if p.tok != TIdentifier:
            break # enum {A, } is ok !
        var s = p.val.sval # copy
        consume()
        if p.tok == TAssign:
            consume()
            let e = constant_expression()
            c = eval_const_expression(e)
        result.eelems.add((s, c))
        putsymtype(s, enum_type)
        inc c
        if p.tok == TComma:
            consume()
        else:
            break
    if p.tok != TRcurlyBracket:
        error("expect '}'")
    consume()
    if len(result.ename) > 0:
        putenumdef(result)
    return result

proc parse_parameter_type_list*(): (bool, seq[(string, CType)]) =
    result = (true, default(typeof(result[1])))
    while true:
        var base = parse_declaration_specifiers()
        if base == nil:
            return (false, default(typeof(result[1])))
        let res = parse_abstract_decorator(base, Function)
        if res == nil:
            return (false, default(typeof(result[1])))
        if res.k == SFunction:
            result[1].add((res.funcname, res.functy))
        else:
            result[1].add((res.var1name, res.var1type))
        if p.tok == TRbracket:
            break
        if p.tok == TComma:
            consume()
        if p.tok == PPEllipsis:
            result[1].add(("", nil))
            consume()
            break
        elif p.tok == TRbracket:
            break

let default_int = CType(tags: TYINVALID or TYINT, spec: TYPRIM)

proc istype(a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        return true
    if a == TIdentifier:
      let ty = gettypedef(p.val.sval)[0]
      return ty != nil and (ty.tags and TYTYPEDEF) != 0
    return false

proc parse_declaration_specifiers*(): CType =
    ## TODO: _Alignas
    var s: seq[Token]
    while p.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        if p.tok == Kenum:
            result = parse_enum()
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif p.tok == Kunion or p.tok == Kstruct:
            result = parse_struct_union(p.tok)
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        elif p.tok == K_Atomic:
            consume()
            if p.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    parse_error("expect type-name")
                    return nil
                if p.tok != TRbracket:
                    parse_error("expect ')'")
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        else:
            s.add(p.tok)
            consume()
    if p.tok == TIdentifier:
        let ty = gettypedef(p.val.sval)[0]
        more(s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            return handle_typedef(s, ty)
    if s.len > 0:
        return merge_types(s)
    warning("type defaults to 'int' in declaration")
    note("expect a type, got " & showToken())
    note("specify one or more storage class specifier or type specifier")
    return default_int

proc parse_static_assert*(): Stmt =
    consume()
    if p.tok != TLbracket:
        parse_error("expect '('")
        return nil
    consume()
    var msg = ""
    let e = constant_expression()
    let ok = eval_const_expression_bool(e)
    if p.tok == TRbracket: # no message
        if ok == false:
            error("static assert failed!")
        consume()
        note("static assert with no message")
    elif p.tok == TComma:
        consume()
        if p.tok != TStringLit:
            parse_error("expect string literal in static assert")
            return nil
        if ok == false:
            error(p.val.sval)
            msg = p.val.sval
        consume()
    else:
        parse_error("expect ',' or ')'")
        return nil
    if p.tok != TSemicolon:
        error("expect ';'")
        return nil
    consume()
    return Stmt(k: SStaticAssertDecl, assertexpr: e, msg: msg)

proc declaration*(): Stmt =
    if p.tok == K_Static_assert:
        return parse_static_assert()
    else:
        if p.tok == TSemicolon:
            consume()
            return Stmt(k: SVarDecl)
        var base = parse_declaration_specifiers()
        if base == nil:
            parse_error("expect declaration-specifiers")
            return nil
        result = Stmt(k: SVarDecl)
        while true:
            if p.tok == TSemicolon:
                consume()
                break
            let st = parse_declarator(base)
            if st == nil:
                parse_error("declarator expected")
                return nil
            if st.k == SFunction:
                putsymtype(st.funcname, st.functy)
                return st
            assert st.k == SVarDecl1
            result.vars.add((st.var1name, st.var1type, nil))
            putsymtype(st.var1name, st.var1type)
            if p.tok == TAssign:
                consume()
                let init = parse_initializer_list()
                if init == nil:
                    return nil
                result.vars[^1][2] = init
            if p.tok == TComma:
                consume()
            elif p.tok == TSemicolon:
                consume()
                break
        return result

proc cast_expression*(): Expr =
    case p.tok:
    of TLbracket:
        consume()
        let n = expression()
        if n == nil: 
            return nil
        if p.tok != TRbracket:
            parse_error("expect `)`")
            return nil
        consume()
        let e = cast_expression()
        return Expr(k: ECast, casttype: n, castval: e)
    else:
        return unary_expression()

proc type_name*(): (CType, bool) =
    let base = parse_declaration_specifiers()
    if base == nil:
        (nil, false)
    elif p.tok == TRbracket:
        (base, false)
    else:
        let st = parse_abstract_decorator(base, Abstract)
        if st == nil:
            (nil, false)
        elif st.k == SFunction:
            (st.functy, true)
        else:
            (st.var1type, false)

proc unary_expression*(): Expr =
    let tok = p.tok
    case tok:
    of TNot, TMul, TBitAnd, TBitNot:
        consume()
        let e = cast_expression()
        if e == nil:
            return nil
        let op = (
            case tok:
            of TNot: OLogicalNot
            of TMul: ODereference
            of TBitAnd: OAddressOf
            of TBitNot: OBitwiseNot
            else: UNop
        )
        return unary(e, op)
    of TAdd, TDash:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        let op = (
            case tok:
            of TAdd: OUnaryPlus
            of TDash: OUnaryMinus
            else: UNop
        )
        return unary(e, op)
    of TAddAdd, TSubSub:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        return unary(e, if tok == TAddAdd: OPrefixIncrement else: OPrefixDecrement)
    of Ksizeof:
        consume()
        if p.tok == TLbracket:
            consume()
            if istype(p.tok):
                let ty = type_name()
                if ty[0] == nil:
                    parse_error("expect type-name or expression")
                    return nil
                if ty[1] == true:
                    type_error("invalid application of 'sizeof' to a function type")
                if p.tok != TRbracket:
                    parse_error("expect ')'")
                    return nil
                consume()
                return Expr(k: ESizeOf, sizeofx: nil, sizeofty: ty[0])
            let e = unary_expression()
            if e == nil:
                parse_error("expect expression")
                return nil
            if p.tok != TRbracket:
                parse_error("expect ')'")
                return nil
            consume()
            return Expr(k: ESizeOf, sizeofx: e, sizeofty: nil)
        else:
            let e = unary_expression()
            if e == nil:
                parse_error("expect expression")
                return nil
            return Expr(k: ESizeOf, sizeofx: e, sizeofty: nil)
    of K_Alignof:
        consume()
        if p.tok != TLbracket:
            parse_error("expect (")
            return nil
        consume()
        if istype(p.tok):
            let ty = type_name()
            if ty[0] == nil:
                parse_error("expect type-name")
                return nil
            if ty[1] == true:
                type_error("invalid application of '_Alignof' to a function type")
            result = Expr(k: EAlignof, sizeofx: nil, sizeofty: ty[0])
        else:
            let e = constant_expression()
            if e == nil:
                parse_error("expect expression")
                return nil
            result = Expr(k: EAlignof, sizeofx: e, sizeofty: nil)
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        return result
    else:
        return postfix_expression()

proc primary_expression*(): Expr =
    case p.tok:
    of TCharLit:
        result = Expr(k: ECharLit, ival: p.val.ival)
        consume()
    of TNumberLit:
        result = Expr(k: EIntLit, ival: p.val.ival)
        consume()
    of TFloatLit:
        result = Expr(k: EFloatLit, fval: p.val.fval)
        consume()
    of TStringLit:
        result = Expr(k: EStringLit, sval: p.val.sval)
        consume()
    of TIdentifier:
        result = Expr(k: EVar, sval: p.val.sval)
        consume()
    of TLbracket:
        consume()
        result = expression()
        if result == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
    of K_Generic:
        consume()
        if p.tok != TLbracket:
            parse_error("expect '('")
            return nil
        consume()
        let test = assignment_expression()
        if test == nil:
            parse_error("expect expression")
            note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
            return nil
        if p.tok != TComma:
            parse_error("expect ,")
            return nil
        consume()
        var selectors: seq[(Expr, Expr)]
        while true:
            var tname: Expr = nil
            if p.tok == Kdefault:
                consume()
            else:
                tname = expression()
                if tname == nil:
                    parse_error("expect expression")
                    note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                    return nil
            if p.tok != TColon:
                parse_error("expect ':'")
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            consume()
            let e = assignment_expression()
            if e == nil:
                parse_error("expect expression")
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            selectors.add((tname, e))
            case p.tok:
            of TComma:
              consume()
              continue
            of TRbracket:
              consume()
              break
            else:
              parse_error("expect ','' or ')'")
              return nil
        return Expr(k: EGeneric, selectexpr: test, selectors: selectors)
    of TEOF:
        return nil
    else:
        parse_error("primary_expression: bad token: " & showToken())
        return nil

proc postfix_expression*(): Expr =
    let e = primary_expression()
    case p.tok:
    of TSubSub, TAddAdd:
        let op = if p.tok == TAddAdd: OPostfixIncrement else: OPostfixDecrement
        consume()
        return postfix(e, op)
    of TArrow, TDot: # member access
        let isarrow = p.tok == TArrow
        consume()
        let i = p.tok
        if i != TIdentifier:
            parse_error("expect identifier")
            note("the syntax is:\n\tfoo.member\n\tfoo.member")
            return nil
        return binop(e, (if isarrow: OMemberAccess else: OPointerMemberAccess), Expr(k: EVar, sval: p.val.sval))
    of TLbracket: # function call
        consume()
        let args = expression()
        if args == nil:
            parse_error("expect expression")
            note("the syntax is:\n\tfunction-name(argument)")
            return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return
        consume()
        return Expr(k: ECall, left: e, right: args)
    of TLSquareBrackets: # array subscript
        consume()
        let args = expression()
        if args == nil:
            parse_error("expect expression")
            note("the syntax is:\n\tarray[expression]")
            return nil
        if p.tok != TRSquareBrackets:
            parse_error("expect ']'")
            return
        consume()
        return Expr(k: ESubscript, left: e, right: args)
    else:
        return e

proc multiplicative_expression*(): Expr =
    result = cast_expression()
    case p.tok:
    of TMul:
        consume()
        let r = cast_expression()
        if r == nil:
            return nil
        result = binop(result, OMultiplication, r)
    of TSlash:
        consume()
        let r = cast_expression()
        if r == nil:
            return nil
        result = binop(result, ODivision, r)
    of TPercent:
        consume()
        let r = cast_expression()
        if r == nil:
            return nil
        result = binop(result, Oremainder, r)
    else:
        return result

proc additive_expression*(): Expr =
    result = multiplicative_expression()
    case p.tok:
    of TAdd:
        consume()
        let r = multiplicative_expression()
        if r == nil:
            return nil
        result = binop(result, OAddition, r)
    of TDash:
        consume()
        let r = multiplicative_expression()
        if r == nil:
            return nil
        result = binop(result, OSubtraction, r)
    else:
        return result

proc shift_expression*(): Expr =
    result = additive_expression()
    case p.tok:
    of Tshl:
        consume()
        let r = additive_expression()
        if r == nil:
            return nil
        result = binop(result, OShl, r)
    of Tshr:
        consume()
        let r = additive_expression()
        if r == nil:
            return nil
        result = binop(result, OShr, r)
    else:
        return result

proc relational_expression*(): Expr =
    result = shift_expression()
    case p.tok:
    of TLt:
        consume()
        let r = shift_expression()
        if r == nil:
            return nil
        result = binop(result, OLt, r)
    of TLe:
        consume()
        let r = shift_expression()
        if r == nil:
            return nil
        result = binop(result, OLe, r)
    of TGt:
        consume()
        let r = shift_expression()
        if r == nil:
            return nil
        result = binop(result, OGt, r)
    of TGe:
        consume()
        let r = shift_expression()
        if r == nil:
            return nil
        result = binop(result, OGe, r)
    else:
        return result


proc equality_expression*(): Expr =
    result = relational_expression()
    case p.tok:
    of TEq:
        consume()
        let r = relational_expression()
        if r == nil:
            return nil
        result = binop(result, OEq, r)
    of TNe:
        consume()
        let r = relational_expression()
        if r == nil:
            return nil
        result = binop(result, ONe, r)
    else:
        return result

proc AND_expression*(): Expr =
    result = equality_expression()
    case p.tok:
    of TBitAnd:
        consume()
        let r = equality_expression()
        if r == nil:
            return nil
        result = binop(result, OBitwiseAnd, r)
    else:
        return result

proc exclusive_OR_expression*(): Expr =
    result = AND_expression()
    case p.tok:
    of TXOr:
        consume()
        let r = AND_expression()
        if r == nil:
            return nil
        result = binop(result, OBitwiseXor, r)
    else:
        return result

proc inclusive_OR_expression*(): Expr =
    result = exclusive_OR_expression()
    case p.tok:
    of TBitOr:
        consume()
        let r = exclusive_OR_expression()
        if r == nil:
            return nil
        result = binop(result, OBitwiseOr, r)
    else:
        return result

proc logical_AND_expression*(): Expr =
    result = inclusive_OR_expression()
    case p.tok:
    of TBitOr:
        consume()
        let r = inclusive_OR_expression()
        if r == nil:
            return nil
        result = binop(result, OLogicalAnd, r)
    else:
        return result

proc logical_OR_expression*(): Expr =
    result = logical_AND_expression()
    case p.tok:
    of TLogicalOr:
        consume()
        let r = logical_AND_expression()
        if r == nil:
            return nil
        result = binop(result, OLogicalOr, r)
    else:
        return result

proc constant_expression*(): Expr = 
    conditional_expression()

proc conditional_expression*(start: Expr): Expr =
    consume() # TQuestionMark
    let lhs = expression()
    if lhs == nil:
        parse_error("expect expression")
        return nil
    if p.tok != TColon:
        return lhs
    consume()
    let rhs = conditional_expression()
    if rhs == nil:
        parse_error("expect expression")
        note("the syntax is:\n\texpr ? a ? b")
        return nil
    return Expr(k: ECondition, cond: start, cleft: lhs, cright: rhs)

proc conditional_expression*(): Expr =
    let e = logical_OR_expression()
    if e == nil:
        return nil
    if p.tok == TQuestionMark:
      return conditional_expression(e)
    return e

proc get_assignment_op(a: Token): BinOP =
    case a:
    of TAssign: OAsign
    of TAsignAdd: OAsignAdd
    of TAsignSub: OAsignSub
    of TAsignMul: OAsignMul
    of TAsignDiv: OAsignDiv
    of TAsignRem: OAsignRem
    of TAsignShl: OAsignShl
    of TAsignShr: OAsignShr
    of TAsignBitAnd: OAsignBitAnd
    of TAsignBitOr: OAsignBitOr
    of TAsignBitXor: OAsignBitXor
    else: BNop

proc assignment_expression*(): Expr =
    result = logical_OR_expression()
    if result == nil:
        return nil
    let tok = p.tok
    if tok == TQuestionMark:
        return conditional_expression(result)
    let op = get_assignment_op(tok)
    if op != BNop:
        consume()
        let e = assignment_expression()
        if e == nil:
            parse_error("expect expression")
            note("the syntax is:\n\texpr1=expr2, expr3=expr4, ...")
            return nil
        result = binop(result, op, e)

proc expression*(): Expr =
    result = assignment_expression()
    if p.tok == TComma:
        let r = assignment_expression()
        if r == nil:
            return nil
        result = binop(result, Comma, r)

proc translation_unit*(): seq[Stmt] =
    result = newSeqOfCap[Stmt](20)
    while p.tok != TEOF:
        let s = declaration()
        if s == nil:
            break
        echo s
        result.add(s)

proc compound_statement*(): Stmt =
    result = Stmt(k: SCompound)
    consume()
    enterBlock()
    while p.tok != TRcurlyBracket:
        var s: Stmt = nil
        if istype(p.tok):
            s = declaration()
        else:
            s = statament()
        if s == nil:
            leaveBlock()
            return nil
        result.stmts.add(s)
    leaveBlock()
    consume()
    return result

proc statament*(): Stmt =
    if p.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon)
    elif p.tok == TLcurlyBracket:
        return compound_statement()
    elif p.tok == Kcase:
        consume()
        let e = constant_expression()
        if e == nil:
            parse_error("expect constant-expression")
            return nil
        if p.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            parse_error("expect statament")
            return nil
        return Stmt(k: Scase, case_expr: e, case_stmt: s)
    elif p.tok == Kdefault:
        consume()
        if p.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            return nil
        return Stmt(k: SDefault, default_stmt: s)
    elif p.tok == Kgoto:
        consume()
        if p.tok != TIdentifier:
            parse_error("expect identifier")
            note("the syntax is:\n\tgoto label;")
            return nil
        var location = p.val.sval
        let l = getLabel(location)[0]
        consume()
        if p.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        if l == -1:
            error("undeclared label " & location)
            return nil
        return Stmt(k: SGoto, location: location)
    elif p.tok == Kcontinue:
        consume()
        if p.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        return Stmt(k: SContinue)
    elif p.tok == Kbreak:
        consume()
        if p.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        return Stmt(k: SBreak)
    elif p.tok == Kreturn:
        consume()
        if p.tok == TSemicolon:
            consume()
            return Stmt(k: SReturn)
        let e = expression()
        if e == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TSemicolon:
            error("expect '}'")
            return nil
        consume()
        return Stmt(k: SReturn, exprbody: e)
    elif p.tok == Kif:
        consume()
        if p.tok != TLbracket:
            parse_error("expect '('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            parse_error("expect statament")
            return nil
        var elsebody: Stmt = nil
        if p.tok == Kelse:
            consume()
            elsebody = statament()
            if elsebody == nil:
                parse_error("expect statament")
                return nil
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody)
    elif p.tok == Kwhile or p.tok == Kswitch:
        let tok = p.tok
        consume()
        if p.tok != TLbracket:
            parse_error("expect '('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            parse_error("expect statament")
            return nil
        if tok == Kwhile:
            return Stmt(k: SWhile, test: e, body: s)
        else:
            return Stmt(k: SSwitch, test: e, body: s)  
    elif p.tok == Kfor:
        consume()
        if p.tok != TLbracket:
            parse_error("expect '('")
            return nil
        consume()
        enterBlock()
        # init-clause may be an expression or a declaration
        var init: Stmt = nil
        if istype(p.tok):
            init = declaration()
            if init == nil:
                parse_error("expect statament")
                return nil
        else:
            let ex = expression()
            if ex == nil:
                parse_error("expect expression")
                return nil
            init = Stmt(k: SExpr, exprbody: ex)
        if p.tok != TSemicolon:
            error("expect ';'")
        consume()
        #  cond-expression 
        let cond = expression()
        if cond == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TSemicolon:
            error("expect ';'")
        consume()
        let forincl = expression()
        if forincl == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            parse_error("expect statament")
            return nil
        leaveBlock()
        return Stmt(k: SFor, forinit: init, forcond: cond, forincl: forincl, forbody: s)
    elif p.tok == Kdo:
        consume()
        let s = statament()
        if s == nil:
            parse_error("expect statament")
            return nil
        if p.tok != Kwhile:
            parse_error("expect 'while'")
            return nil
        consume()
        if p.tok != TLbracket:
            parse_error("expect '('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            parse_error("expect ')'")
            return nil
        consume()
        if p.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        return Stmt(k: SDoWhile, test: e, body: s)
    elif p.tok == TIdentifier:
        var val = p.val.sval
        consume()
        if p.tok == TColon: # # labeled-statement
            consume()
            let s = statament()
            if s == nil:
                parse_error("expect statament")
                note("to add a empty statement, use:\n\tlabel: ;")
                return nil
            putLable(val, 100)
            return Stmt(k: SLabled, label: val, labledstmt: s)
        else: # expression
            putToken()
            p.tok = TIdentifier
            p.val.sval = val
    let e = expression()
    if e == nil:
        parse_error("expect expression")
        return nil
    if p.tok != TSemicolon:
        parse_error("expect ';'")
        return nil
    consume()
    return Stmt(k: SExpr, exprbody: e)

