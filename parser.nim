## A recursive descent parser for C language
##
## see ISO C grammar for details
##
## the main export function is expression, constant_expression, translation_unit, declaration

import token, lexer, eval
from std/sequtils import count

import typesystem

var
  program*: seq[TokenV]
  pc*: int = 0

proc type_name*(): (CType, bool)

proc expression*(): Expr

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

proc initializer_list*(): Expr

proc parameter_type_list*(): (bool, seq[(string, CType)])

proc declaration*(): Stmt

proc type_qualifier_list*(ty: var CType)

proc merge_types*(ts: seq[Token]): Ctype

proc declaration_specifiers*(): CType

proc specifier_qualifier_list*(): CType


type 
    DeclaratorFlags* = enum 
        ## * Direct: declarator with name
        ## * Abstract: declarator without name
        ## * Function: function parameter-type-list
        Direct, Abstract, Function

proc direct_declarator*(base: CType; flags=Direct): Stmt

proc declarator*(base: CType; flags=Direct): Stmt

template abstract_decorator*(base, f): untyped = 
    ## abstract decorator has no name
    ## for example: `static int        (*)(int, char)`
    ##               base-type      abstact-decorator
    declarator(base, flags=f)


proc struct_union*(t: Token): CType

proc penum*(): CType

proc static_assert*(): Stmt

proc translation_unit*()

proc statament*(): Stmt

proc compound_statement*(): Stmt

proc binop*(a: Expr, op: BinOP, b: Expr, ty: CType = nil): Expr = 
    ## construct a binary operator
    Expr(k: EBin, lhs: a, rhs: b, bop: op, ty: ty)


proc unary*(e: Expr, op: UnaryOP, ty: CType=nil): Expr = 
    ## construct a unary operator
    Expr(k: EUnary, uop: op, uoperand: e, ty: ty)


proc postfix*(e: Expr, op: PostfixOP, ty: CType=nil): Expr = 
    ## construct a postfix operator
    Expr(k: EPostFix, pop: op, poperand: e, ty: ty)


proc consume*() =
    ## eat token from lexer and c preprocesser
    getToken()

proc expect(msg: string) =
    ## emit `expect ...` error message
    parse_error("expect " & msg & ", got " & showToken())

const type_specifier_set* = {
    Kchar, Kint, Kshort, Ksigned, 
    Kunsigned, 
    Klong, Kdouble, Kfloat,
    K_Atomic,
    K_Complex, Kvoid, K_Bool
} ## primitive types


const function_specifier_set* = {
    Kinline, K_Noreturn
} ## function specfiers


const storage_class_specifier_set* = {
    Ktypedef, Kextern, Kstatic, 
    K_Thread_local, Kauto, Kregister
} ## storage specfiers


const type_qualifier_set* = {
     Kvolatile, Krestrict, Kconst
} ## type qualifiers


const declaration_specifier_set* = 
    type_specifier_set +
    storage_class_specifier_set +
    type_qualifier_set +
    function_specifier_set ## declaration specfiers

proc addTag(ty: var CType, t: Token): bool =
    let t = (
        case t:
        of Kinline: TYINLINE
        of K_Noreturn: TYNORETURN
        of K_Alignas: TYALIGNAS
        of Kextern: TYEXTERN
        of Kstatic: TYSTATIC
        of K_Thread_local: TYTHREAD_LOCAL
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

proc merge_types*(ts: seq[Token]): CType =
    ## merge many token to a type
    ##
    ## for example:
    ##   `long long int const` => const long long
    if ts.len == 0:
        return nil
    result = CType(tags: TYINVALID, spec: TYPRIM)
    var b: seq[Token]
    for t in ts:
        if addTag(result, t) == false:
            b.add(t)
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

proc type_qualifier_list(ty: var CType) =
    ## parse many type qualifiers, add to type
    while true:
        case p.tok.tok:
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
    while p.tok.tok in declaration_specifier_set:
        s.add(p.tok.tok)
        consume()

proc read_enum_sepcs*(c: var CType, sepc: Token) = 
    discard

proc read_struct_union_sepcs*(c: var CType, sepc: Token) = 
    discard

proc handle_typedef(s: var seq[Token], ty: CType): CType =
    consume()
    while p.tok.tok in declaration_specifier_set:
        s.add(p.tok.tok)
        consume()
    if len(s) > 0:
        if Ktypedef in s:
            return ty
        warning("typedef types cannot combine with type-specifier and type-qualifiers")
    result = deepCopy(ty)
    result.tags = ty.tags and (not TYTYPEDEF)
    return result

proc specifier_qualifier_list*(): CType =
    ## specfier_qualifier_list is used in struct declaration
    var s: seq[Token]
    while p.tok.tok in (type_specifier_set + type_qualifier_set + {Kstruct, Kenum, Kunion}):
        if p.tok.tok == Kenum:
            result = penum()
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif p.tok.tok == Kunion or p.tok.tok == Kstruct:
            result = struct_union(p.tok.tok)
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        elif p.tok.tok == K_Atomic:
            consume()
            if p.tok.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name")
                    return nil
                if p.tok.tok != TRbracket:
                    expect("')'")
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        else:
            s.add(p.tok.tok)
            consume()
    if p.tok.tok == TIdentifier:
        let ty = gettypedef(p.tok.s)[0]
        more(s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            return handle_typedef(s, ty)
    if s.len > 0:
        return merge_types(s)
    return nil

proc declarator*(base: CType; flags=Direct): Stmt =
    ## take a base type, return the final type and name
    ## for example: `static   int     *foo`
    ##                base-type    decorator
    var ty = base
    while p.tok.tok == TMul:
        consume()
        ty = CType(tags: TYINVALID, spec: TYPOINTER, p: ty)
        type_qualifier_list(ty)
    return direct_declarator(ty, flags)

proc initializer_list*(): Expr =
    if p.tok.tok != TLcurlyBracket:
        return assignment_expression()
    result = Expr(k: EArray)
    consume()
    while true:
        if p.tok.tok == TRcurlyBracket:
            consume()
            break
        if p.tok.tok == TLcurlyBracket:
            let e = initializer_list()
            if e == nil:
                return nil
            result.arr.add(e)
        else:
            let e = assignment_expression()
            if e == nil:
                error("expect expression")
                return nil
            result.arr.add(e)
        if p.tok.tok == TComma:
            consume()

proc direct_declarator_end*(base: CType, name: string): Stmt

proc direct_declarator*(base: CType; flags=Direct): Stmt =
    case p.tok.tok:
    of TIdentifier:
        if flags == Abstract:
            return nil
        var name = p.tok.s
        consume()
        return direct_declarator_end(base, name)
    of TLbracket:
        consume()
        let st = declarator(base, flags)
        if st == nil:
            expect("declarator")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        if st.k == SFunction:
            return direct_declarator_end(st.functy, st.funcname)
        else:
            return direct_declarator_end(st.var1type, st.var1name)
    else:
        if flags != Direct:
            return direct_declarator_end(base, "")
        return nil

proc direct_declarator_end*(base: CType, name: string): Stmt =
    case p.tok.tok:
    of TLSquareBrackets: # int arr[5], int arr[*], int arr[static 5], int arr[static const 5], int arr[const static 5], int arr2[static const restrict 5]
        consume() # eat ]
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: -1, arrtype: base)
        if p.tok.tok == TMul: # int arr[*]
          consume()
          if p.tok.tok != TRSquareBrackets:
            error("expect ']'")
            note("the syntax is:\n\tint arr[*]")
            return nil
          return Stmt(k: SVarDecl1, var1name: name, var1type: ty)
        if p.tok.tok == Kstatic:
           consume()
           ty.tags = ty.tags or TYSTATIC
           type_qualifier_list(ty)
        else:
            type_qualifier_list(ty)
            if p.tok.tok == Kstatic:
                ty.tags = ty.tags or TYSTATIC
                consume()
        if p.tok.tok != TRSquareBrackets:
            let e = assignment_expression()
            if e == nil:
                expect("expression")
                return nil
            ty.arrsize = eval_const_expression(e)
            if p.tok.tok != TRSquareBrackets:
               error("expect ']'")
               return nil
        consume() # eat ]
        return direct_declarator_end(ty, name)
    of TLbracket:
        consume()
        var ty = CType(tags: TYINVALID, spec: TYFUNCTION, ret: base)
        if p.tok.tok != TRbracket:
            let res = parameter_type_list()
            if res[0]:
                ty.params = res[1]
            else:
                return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        if p.tok.tok == TLcurlyBracket: # function definition
            let body = compound_statement()
            if body == nil:
                expect("function body")
                return nil
            return Stmt(funcname: name, k: SFunction, functy: ty, funcbody: body)
        return direct_declarator_end(ty, name)
    else:
        return Stmt(k: SVarDecl1, var1name: name, var1type: base)

proc struct_declarator(base: CType): (string, CType) =
    if p.tok.tok == TColon:
        consume()
        let e = constant_expression()
        if e == nil:
            expect("expression")
            note("in bit field declaration")
            return ("", nil)
        let bitsize = eval_const_expression(e)
        return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: base, bitsize: bitsize))
    else:
        let d = declarator(base, Direct)
        if d == nil:
            return ("", nil)
        if p.tok.tok == TColon:
            consume()
            let e = constant_expression()
            if e == nil:
                expect("expression")
                note("in bit field declaration")
                return
            let bitsize = eval_const_expression(e)
            return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: if d.k == SFunction: d.functy else: d.var1type, bitsize: bitsize))
        if d.k == SFunction:
            return (d.funcname, d.functy)
        return (d.var1name, d.var1type)

proc struct_union*(t: Token): CType =
    ## parse a struct or union, return it
    ## for example:  `struct Foo`
    ##
    ##               `struct { ... }`
    ##
    ##               `struct Foo { ... }`
    consume() # eat struct/union
    var name = ""
    if p.tok.tok == TIdentifier:
        name = p.tok.s
        consume()
        if p.tok.tok != TLcurlyBracket: # struct Foo
           return if t==Kstruct: getstructdef(name) else: getuniondef(name)
    elif p.tok.tok != TLcurlyBracket:
        error("expect '{' for start anonymous struct/union")
        return nil
    if t == Kstruct:
        result = CType(tags: TYINVALID, spec: TYSTRUCT, sname: name)
    else:
        result = CType(tags: TYINVALID, spec: TYUNION, sname: name)
    consume()
    if p.tok.tok != TRcurlyBracket:
        while true:
            if p.tok.tok == K_Static_assert:
                let s = static_assert()
                if s == nil:
                    return nil
                if p.tok.tok == TRcurlyBracket:
                    break
                continue
            var base = specifier_qualifier_list()
            if base == nil:
                expect("specifier-qualifier-list")
                return nil

            if p.tok.tok == TSemicolon:
                warning("struct/union member declaration doest not declare anything")
                consume()
                continue
            else:
                while true:
                    let e = struct_declarator(base)
                    if e[1] == nil:
                        error("expect struct-declarator")
                        return nil
                    result.selems.add(e)
                    if p.tok.tok == TComma:
                        consume()
                    else:
                        if p.tok.tok != TSemicolon:
                            expect("';'")
                            return nil
                        consume()
                        break
                if p.tok.tok == TRcurlyBracket:
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

proc penum*(): CType =
    ## parse a enum, return it
    ## for example: 
    ## 
    ##      `enum State`
    ##
    ##      `enum { ... }`
    ##
    ##      `enum State { ... }`
    consume() # eat enum
    var name = ""
    if p.tok.tok == TIdentifier:
        name = p.tok.s
        consume()
        if p.tok.tok != TLcurlyBracket: # struct Foo
          return getenumdef(name)
    elif p.tok.tok != TLcurlyBracket:
        error("expect '{' for start anonymous enum")
        return nil
    result = CType(tags: TYINVALID, spec: TYENUM, ename: name)
    consume()
    var c: intmax_t = 0
    while true:
        if p.tok.tok != TIdentifier:
            break # enum {A, } is ok !
        var s = p.tok.s # copy
        consume()
        if p.tok.tok == TAssign:
            consume()
            let e = constant_expression()
            c = eval_const_expression(e)
        result.eelems.add((s, c))
        putsymtype(s, enum_type)
        inc c
        if p.tok.tok == TComma:
            consume()
        else:
            break
    if p.tok.tok != TRcurlyBracket:
        error("expect '}'")
    consume()
    if len(result.ename) > 0:
        putenumdef(result)
    return result

proc parameter_type_list*(): (bool, seq[(string, CType)]) =
    result = (true, default(typeof(result[1])))
    while true:
        var base = declaration_specifiers()
        if base == nil:
            return (false, default(typeof(result[1])))
        let res = abstract_decorator(base, Function)
        if res == nil:
            return (false, default(typeof(result[1])))
        if res.k == SFunction:
            result[1].add((res.funcname, res.functy))
        else:
            result[1].add((res.var1name, res.var1type))
        if p.tok.tok == TRbracket:
            break
        if p.tok.tok == TComma:
            consume()
        if p.tok.tok == TEllipsis:
            result[1].add(("", nil))
            consume()
            break
        elif p.tok.tok == TRbracket:
            break

proc istype(a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        return true
    if a == TIdentifier:
      let ty = gettypedef(p.tok.s)[0]
      return ty != nil and (ty.tags and TYTYPEDEF) != 0
    return false

proc declaration_specifiers*(): CType =
    ## declaration_specfier is used in function and variable declaration/definition
    var s: seq[Token]
    while p.tok.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        if p.tok.tok == Kenum:
            result = penum()
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif p.tok.tok == Kunion or p.tok.tok == Kstruct:
            result = struct_union(p.tok.tok)
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        elif p.tok.tok == K_Atomic:
            consume()
            if p.tok.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name")
                    return nil
                if p.tok.tok != TRbracket:
                    expect("')'")
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        else:
            s.add(p.tok.tok)
            consume()
    if p.tok.tok == TIdentifier:
        let ty = gettypedef(p.tok.s)[0]
        more(s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            return handle_typedef(s, ty)
    if s.len > 0:
        return merge_types(s)
    warning("type defaults to 'int' in declaration")
    return CType(tags: TYINVALID or TYINT, spec: TYPRIM)

proc static_assert*(): Stmt =
    consume()
    if p.tok.tok != TLbracket:
        expect("'('")
        return nil
    consume()
    var msg = ""
    let e = constant_expression()
    let ok = eval_const_expression_bool(e)
    if p.tok.tok == TRbracket: # no message
        if ok == false:
            error("static assert failed!")
        consume()
        note("static assert with no message")
    elif p.tok.tok == TComma:
        consume()
        if p.tok.tok != TStringLit:
            expect("string literal in static assert")
            return nil
        if ok == false:
            error(p.tok.str)
            msg = p.tok.str
        consume()
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
    else:
        expect("',' or ')'")
        return nil
    if p.tok.tok != TSemicolon:
        expect("';'")
        return nil
    consume()
    return Stmt(k: SStaticAssertDecl, assertexpr: e, msg: msg)

proc declaration*(): Stmt =
    ## parse variable declaration or function definition
    ## 
    ## this is different from ISO C grammar
    if p.tok.tok == K_Static_assert:
        return static_assert()
    else:
        if p.tok.tok == TSemicolon:
            consume()
            return Stmt(k: SVarDecl)
        var base = declaration_specifiers()
        if base == nil:
            expect("declaration-specifiers")
            return nil
        result = Stmt(k: SVarDecl)
        while true:
            if p.tok.tok == TSemicolon:
                consume()
                break
            let st = declarator(base)
            if st == nil:
                expect("declarator")
                return nil
            if st.k == SFunction:
                putsymtype(st.funcname, st.functy)
                return st
            assert st.k == SVarDecl1
            if (st.var1type.tags and TYINLINE) != 0:
                warning("inline declaration is in block scope has no effect")
            if st.var1type.spec == TYFUNCTION:
                if (st.var1type.tags or (TYREGISTER)) != 0:
                    warning("'register' in function has no effect")
                if (st.var1type.tags or (TYTHREAD_LOCAL)) != 0:
                    warning("'_Thread_local' in function has no effect")
            result.vars.add((st.var1name, st.var1type, nil))
            putsymtype(st.var1name, st.var1type)
            if p.tok.tok == TAssign:
                consume()
                let init = initializer_list()
                if init == nil:
                    expect("initializer-list")
                    return nil
                result.vars[^1][2] = init
            if p.tok.tok == TComma:
                consume()
            elif p.tok.tok == TSemicolon:
                consume()
                break
        return result

proc cast_expression*(): Expr =
    case p.tok.tok:
    of TLbracket:
        consume()
        if istype(p.tok.tok):
            let (n, isf) = type_name()
            discard isf
            if n == nil:
                return nil
            if p.tok.tok != TRbracket:
                expect("`)`")
                return nil
            consume()
            let e = cast_expression()
            return Expr(k: ECast, ty: n, castval: e)
        putToken()
        p.tok = TokenV(tok: TLbracket, tags: TVNormal)
    else:
        discard
    return unary_expression()

proc type_name*(): (CType, bool) =
    ## parse a type-name
    ##
    ## for example:
    ##
    ##    sizeof(type-name)
    let base = declaration_specifiers()
    if base == nil:
        (nil, false)
    elif p.tok.tok == TRbracket:
        (base, false)
    else:
        let st = abstract_decorator(base, Abstract)
        if st == nil:
            (nil, false)
        elif st.k == SFunction:
            (st.functy, true)
        else:
            (st.var1type, false)

proc unary_expression*(): Expr =
    let tok = p.tok.tok
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
        result = unary(e, op, e.ty)
        integer_promotions(result)
        return result
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
        result = unary(e, op, e.ty)
        integer_promotions(result)
        return result
    of TAddAdd, TSubSub:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        return unary(e, if tok == TAddAdd: OPrefixIncrement else: OPrefixDecrement, e.ty)
    of Ksizeof:
        consume()
        if p.tok.tok == TLbracket:
            consume()
            if istype(p.tok.tok):
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name or expression")
                    return nil
                if ty[1] == true:
                    type_error("invalid application of 'sizeof' to a function type")
                if p.tok.tok != TRbracket:
                    expect("')'")
                    return nil
                consume()
                return Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getsizeof(ty[0])))
            let e = unary_expression()
            if e == nil:
                expect("expression")
                return nil
            if p.tok.tok != TRbracket:
                expect("')'")
                return nil
            consume()
            return Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getsizeof(e)))
        else:
            let e = unary_expression()
            if e == nil:
                expect("expression")
                return nil
            return Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getsizeof(e)))
    of K_Alignof:
        consume()
        if p.tok.tok != TLbracket:
            expect("(")
            return nil
        consume()
        if istype(p.tok.tok):
            let ty = type_name()
            if ty[0] == nil:
                expect("type-name")
                return nil
            if ty[1] == true:
                type_error("invalid application of '_Alignof' to a function type")
            result = Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getAlignof(ty[0])))
        else:
            let e = constant_expression()
            if e == nil:
                expect("expression")
                return nil
            result = Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getAlignof(e)))
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        return result
    else:
        return postfix_expression()

proc primary_expression*(): Expr =
    ## primary expressions:
    ##     * constant
    ##     * `(` expression `)`
    ##     * identfier
    case p.tok.tok:
    of TCharLit:
        var tags = 0'u32
        case p.tok.itag:
        of Iint:
            tags = TYCHAR
        of Ilong:
            tags = TYUINT16
        of Iulong:
            tags = TYUINT32
        of Ilonglong:
            when defined(windows):
                tags = TYUINT32
            else:
                tags = TYUINT32
        else:
            assert false
        result = Expr(k: ECharLit, ival: p.tok.i, ty: CType(tags: tags, spec: TYPRIM))
        consume()
    of TNumberLit:
        var tags = TYINT
        case p.tok.itag:
        of Iint:
            tags = TYINT
        of Ilong:
            tags = TYLONG
        of Iulong:
            tags = TYULONG
        of Ilonglong:
            tags = TYLONGLONG
        of Iulonglong:
            tags = TYULONGLONG
        of Iuint:
            tags = TYUINT
        result = Expr(k: EIntLit, ival: p.tok.i, ty: CType(tags: tags, spec: TYPRIM))
        consume()
    of TFloatLit:
        result = Expr(k: EFloatLit, fval: p.tok.f, ty: CType(tags: if p.tok.ftag == Ffloat: TYFLOAT else: TYDOUBLE, spec: TYPRIM))
        consume()
    of TStringLit:
        var s: string
        var enc = p.tok.enc
        while true:
            s.add(p.tok.str)
            consume()
            if p.tok.tok != TStringLit:
                break
            if p.tok.enc != enc:
                type_error("unsupported non-standard concatenation of string literals")
                note("concatenation UTF-" & $enc & " and UTF-" & $p.tok.enc)
                return
        case enc:
        of 8:
            var a: seq[Expr]
            let ty = CType(tags: TYCHAR, spec: TYPRIM)
            for i in s:
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(k: EArray, ty: CType(tags: TYINVALID, spec: TYPOINTER, p: ty) , arr: a)
        of 16:
            var a: seq[Expr]
            let ty = CType(tags: TYUSHORT, spec: TYPRIM)
            for i in writeUTF8toUTF16(s):
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(k: EArray, ty: CType(tags: TYINVALID, spec: TYPOINTER, p: ty), arr: a)
        of 32:
            var a: seq[Expr]
            let ty = CType(tags: TYUINT, spec: TYPRIM)
            for i in writeUTF8toUTF32(s):
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(k: EArray, ty: CType(tags: TYINVALID, spec: TYPOINTER, p: ty), arr: a)
        else:
            assert false
    of TPPNumber:
        var f: float
        var n: int
        let ok = read_pp_number(p.tok.s, f, n)
        case ok:
        of 0:
            result = nil
        of 1:
            result = Expr(ty: CType(tags: TYINT, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 2, 3:
            result = Expr(ty: CType(tags: if ok == 2: TYDOUBLE else: TYFLOAT, spec: TYPRIM), k: EFloatLit, fval: f)
            consume()
        of 4:
            result = Expr(ty: CType(tags: TYLONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 5:
            result = Expr(ty: CType(tags: TYULONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume() 
        of 6:
            result = Expr(ty: CType(tags: TYLONGLONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 7:
            result = Expr(ty: CType(tags: TYULONGLONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 8:
            result = Expr(ty: CType(tags: TYUINT, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        else:
            assert false
    of TIdentifier:
        let ty = getsymtype(p.tok.s)
        if ty == nil:
            type_error("symbol not found: " & p.tok.s)
            return nil
        result = Expr(k: EVar, sval: p.tok.s, ty: ty)
        consume()
    of CPPident:
        result = Expr(k: ECppVar, sval: p.tok.s, ty: CType(tags: TYINT, spec: TYPRIM))
        consume()
    of TLbracket:
        consume()
        result = expression()
        if result == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
    of K_Generic:
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let test = assignment_expression()
        if test == nil:
            expect("expression")
            note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
            return nil
        if p.tok.tok != TComma:
            expect("','")
            return nil
        consume()
        let testty = test.ty
        var defaults: Expr
        while true:
            var tname: CType = nil
            if p.tok.tok == Kdefault:
                if defaults != nil:
                    type_error("more then one default case in Generic expression")
                    return nil
                consume()
            else:
                tname = type_name()[0]
                if tname == nil:
                    expect("type-name")
                    note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                    return nil
            if p.tok.tok != TColon:
                expect("':'")
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            consume()
            let e = assignment_expression()
            if e == nil:
                expect("expression")
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            if tname == nil:
                defaults = e
            elif tname.tags == testty.tags and tname.spec == testty.spec:
                if result != nil:
                    type_error("more then one compatible types in Generic expression")
                    return nil
                result = e
            case p.tok.tok:
            of TComma:
              consume()
              continue
            of TRbracket:
              consume()
              break
            else:
              expect("','' or ')'")
              return nil
        if result != nil:
            return result
        if defaults != nil:
            return defaults
        type_error("Generic expression not compatible with any generic association type")
        note("no match of type " & $testty)
    of TEOF, TNul:
        return nil
    else:
        parse_error("unexpected token in expression: " & showToken())
        return nil

proc postfix_expression*(): Expr =
    let e = primary_expression()
    case p.tok.tok:
    of TSubSub, TAddAdd:
        let op = if p.tok.tok == TAddAdd: OPostfixIncrement else: OPostfixDecrement
        consume()
        return postfix(e, op, e.ty)
    of TArrow, TDot: # member access
        let isarrow = p.tok.tok == TArrow
        consume()
        if p.tok.tok != TIdentifier:
            expect("identifier")
            note("the syntax is:\n\tfoo.member\n\tfoo->member")
            return nil
        if not (e.ty.spec == TYSTRUCT or e.ty.spec == TYUNION):
            type_error("member access is not struct or union")
            note("in the expression " & $e)
            return nil
        if isarrow and e.ty.spec != TYPOINTER:
            type_error("pointer member access('->') must be used in a pointer")
            note("maybe you mean: '.'")
            return nil
        elif isarrow == false and e.ty.spec == TYPOINTER:
            type_error("member access('.') cannot used in a pointer")
            note("maybe you mean: '->'")
        for i in 0..<len(e.ty.selems):
            if p.tok.s == e.ty.selems[i][0]:
                return binop(e, (if isarrow: OMemberAccess else: OPointerMemberAccess), Expr(k: EVar, sval: p.tok.s))
        type_error("struct/union " & $e.ty.sname & " has no member " & p.tok.s)
        return nil
    of TLbracket: # function call
        consume()
        if e.ty.spec != TYFUNCTION:
            type_error("expression " & $e & " is of type " & $e.ty & " and is not callable")
            return nil
        var args: seq[Expr]
        if p.tok.tok == TRbracket:
            consume()
        else:
            while true:
                let a = assignment_expression()
                if a == nil:
                    expect("expression")
                    note("the syntax is:\n\tfunction-name(argument)")
                    return nil
                args.add(a)
                if p.tok.tok == TComma:
                    consume()
                elif p.tok.tok == TRbracket:
                    consume()
                    break
        if len(e.ty.params) > 0 and e.ty.params[^1][1] == nil: # varargs
            if len(args) < (len(e.ty.params) - 1):
                type_error("too few arguments to variable argument function")
                note("at lease " & $(len(e.ty.params) - 0) & " arguments needed")
                return nil
        elif len(e.ty.params) != len(args):
            type_error("expect " & $(len(e.ty.params)) & " parameters, " & $len(args) & " provided")
            note("in the expression " & $e)
            return nil
        var i = 0
        var ivarargs = false
        while true:
            if i == len(e.ty.params):
                break
            if e.ty.params[i][1] == nil:
                ivarargs = true
                break
            if not compatible(args[i].ty, expected=e.ty.params[i][1]):
                type_error("incompatible type for argument " & $i & " for calling function " & $e)
                note("expected " & $e.ty.params[i] & " but argument is of type " & $args[i].ty)
                return nil
        if ivarargs:
            for j in i ..< len(args):
                varargs_conv(args[i])
        return Expr(k: ECall, callfunc: e, callargs: args, ty: e.ty.ret)
    of TLSquareBrackets: # array subscript
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            note("the syntax is:\n\tarray[expression]")
            return nil
        if p.tok.tok != TRSquareBrackets:
            expect("']'")
            return
        if e.ty.spec == TYPOINTER:
            discard
        else:
            type_error("expression " & $e & " is of type " & $e.ty & ", and is not subscriptable")
            return nil
        consume()
        return Expr(k: ESubscript, left: e, right: e, ty: e.ty.p)
    else:
        return e

proc multiplicative_expression*(): Expr =
    result = cast_expression()
    while true:
        case p.tok.tok:
        of TMul:
            consume()
            var r = cast_expression()
            if r == nil:
                return nil
            conv(result, r)
            result = binop(result, OMultiplication, r, r.ty)
        of TSlash:
            consume()
            var r = cast_expression()
            if r == nil:
                return nil
            conv(result, r)
            result = binop(result, ODivision, r, r.ty)
        of TPercent:
            consume()
            var r = cast_expression()
            if r == nil:
                return nil
            conv(result, r)
            result = binop(result, Oremainder, r, r.ty)
        else:
            return result

proc additive_expression*(): Expr =
    result = multiplicative_expression()
    while true:
        case p.tok.tok:
        of TAdd:
            consume()
            var r = multiplicative_expression()
            if r == nil:
                return nil
            conv(result, r)
            result = binop(result, OAddition, r, r.ty)
        of TDash:
            consume()
            var r = multiplicative_expression()
            if r == nil:
                return nil
            conv(result, r)
            result = binop(result, OSubtraction, r, r.ty)
        else:
            return result

proc shift_expression*(): Expr =
    result = additive_expression()
    while true:
        case p.tok.tok:
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
    while true:
        case p.tok.tok:
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
    while true:
        case p.tok.tok:
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
    while true:
        case p.tok.tok:
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
    while true:
        case p.tok.tok:
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
    while true:
        case p.tok.tok:
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
    while true:
        case p.tok.tok:
        of TLogicalAnd:
            consume()
            let r = inclusive_OR_expression()
            if r == nil:
                return nil
            result = binop(result, OLogicalAnd, r)
        else:
            return result

proc logical_OR_expression*(): Expr =
    result = logical_AND_expression()
    while true:
        case p.tok.tok:
        of TLogicalOr:
            consume()
            let r = logical_AND_expression()
            if r == nil:
                return nil
            result = binop(result, OLogicalOr, r)
        else:
            return result

proc expression*(): Expr =
    ## parse a expression
    result = assignment_expression()
    while true:
        if p.tok.tok == TComma:
            let r = assignment_expression()
            if r == nil:
                return nil
            result = binop(result, Comma, r)
        else:
            return result

proc conditional_expression*(start: Expr): Expr =
    consume()
    var lhs = logical_OR_expression()
    if lhs == nil:
        expect("expression")
        return nil
    if p.tok.tok != TColon:
        return lhs
    consume()
    var rhs = conditional_expression()
    if rhs == nil:
        expect("expression")
        note("the syntax is:\n\texpr ? a ? b")
        return nil
    if not compatible(lhs.ty, rhs.ty):
        type_error("incompatible type for conditional-expression")
        note("the left is " & $lhs.ty)
        note("the right is " & $rhs.ty)
    conv(lhs, rhs)
    return Expr(k: ECondition, cond: start, cleft: lhs, cright: rhs)

proc conditional_expression*(): Expr =
    let e = logical_OR_expression()
    if e == nil:
        return nil
    if p.tok.tok == TQuestionMark:
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
    let tok = p.tok.tok
    if tok == TQuestionMark:
        return conditional_expression(result)
    let op = get_assignment_op(tok)
    if op != BNop:
        consume()
        let e = assignment_expression()
        if e == nil:
            expect("expression")
            note("the syntax is:\n\texpr1=expr2, expr3=expr4, ...")
            return nil
        result = binop(result, op, e)

proc translation_unit*() =
    ## parse a file, the entry point of program
    while p.tok.tok != TEOF:
        let s = declaration()
        if s == nil:
            break
        echo s

proc compound_statement*(): Stmt =
    result = Stmt(k: SCompound)
    consume()
    enterBlock()
    while p.tok.tok != TRcurlyBracket:
        var s: Stmt = nil
        if istype(p.tok.tok):
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
    ## parse a statement
    if p.tok.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon)
    elif p.tok.tok == TLcurlyBracket:
        return compound_statement()
    elif p.tok.tok == Kcase:
        consume()
        let e = constant_expression()
        if e == nil:
            expect("constant-expression")
            return nil
        if p.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        return Stmt(k: Scase, case_expr: e, case_stmt: s)
    elif p.tok.tok == Kdefault:
        consume()
        if p.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            return nil
        return Stmt(k: SDefault, default_stmt: s)
    elif p.tok.tok == Kgoto:
        consume()
        if p.tok.tok != TIdentifier:
            expect("identifier")
            note("the syntax is:\n\tgoto label;")
            return nil
        var location = p.tok.s
        let l = getLabel(location)[0]
        consume()
        if p.tok.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        if l == -1:
            error("undeclared label " & location)
            return nil
        return Stmt(k: SGoto, location: location)
    elif p.tok.tok == Kcontinue:
        consume()
        if p.tok.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        return Stmt(k: SContinue)
    elif p.tok.tok == Kbreak:
        consume()
        if p.tok.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        return Stmt(k: SBreak)
    elif p.tok.tok == Kreturn:
        consume()
        if p.tok.tok == TSemicolon:
            consume()
            return Stmt(k: SReturn)
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TSemicolon:
            error("expect '}'")
            return nil
        consume()
        return Stmt(k: SReturn, exprbody: e)
    elif p.tok.tok == Kif:
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        var elsebody: Stmt = nil
        if p.tok.tok == Kelse:
            consume()
            elsebody = statament()
            if elsebody == nil:
                expect("statament")
                return nil
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody)
    elif p.tok.tok == Kwhile or p.tok.tok == Kswitch:
        let tok = p.tok.tok
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        if tok == Kwhile:
            return Stmt(k: SWhile, test: e, body: s)
        else:
            return Stmt(k: SSwitch, test: e, body: s)  
    elif p.tok.tok == Kfor:
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        enterBlock()
        # init-clause may be an expression or a declaration
        var init: Stmt = nil
        if istype(p.tok.tok):
            init = declaration()
            if init == nil:
                expect("declaration")
                return nil
        else:
            if p.tok.tok != TSemicolon:
                let ex = expression()
                if ex == nil:
                    expect("expression")
                    return nil
                init = Stmt(k: SExpr, exprbody: ex)
                if p.tok.tok != TSemicolon:
                    expect("';'")
                    return nil
            consume()
        #  cond-expression 
        var cond: Expr = nil
        if p.tok.tok != TSemicolon:
            cond = expression()
            if cond == nil:
                expect("expression")
                return nil
            if p.tok.tok != TSemicolon:
                expect("';'")
                return nil
        consume()
        var forincl: Expr = nil
        if p.tok.tok != TRbracket:
            forincl = expression()
            if forincl == nil:
                expect("expression")
                return nil
            if p.tok.tok != TRbracket:
                expect("')'")
                return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        leaveBlock()
        return Stmt(k: SFor, forinit: init, forcond: cond, forincl: forincl, forbody: s)
    elif p.tok.tok == Kdo:
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        if p.tok.tok != Kwhile:
            expect("'while'")
            return nil
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        if p.tok.tok != TSemicolon:
            error("expect ';'")
            return nil
        consume()
        return Stmt(k: SDoWhile, test: e, body: s)
    elif p.tok.tok == TIdentifier:
        var val = p.tok.s
        consume()
        if p.tok.tok == TColon: # # labeled-statement
            consume()
            let s = statament()
            if s == nil:
                expect("statament")
                note("to add a empty statement, use:\n\tlabel: ;")
                return nil
            putLable(val, 100)
            return Stmt(k: SLabled, label: val, labledstmt: s)
        else: # expression
            putToken()
            p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: val)
    let e = expression()
    if e == nil:
        expect("expression")
        return nil
    if p.tok.tok != TSemicolon:
        expect("';'")
        return nil
    consume()
    return Stmt(k: SExpr, exprbody: e)
