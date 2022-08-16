## A recursive descent parser for C language

import token, lexer, eval
from std/sequtils import count
# ==================  declarations ==================

# parse expression

proc expression*(p: var Parser): Expr

proc constant_expression*(p: var Parser): Expr

proc primary_expression*(p: var Parser): Expr

proc postfix_expression*(p: var Parser): Expr

proc unary_expression*(p: var Parser): Expr

proc cast_expression*(p: var Parser): Expr

proc multiplicative_expression*(p: var Parser): Expr

proc shift_expression*(p: var Parser): Expr

proc relational_expression*(p: var Parser): Expr

proc equality_expression*(p: var Parser): Expr

proc AND_expression*(p: var Parser): Expr

proc exclusive_OR_expression*(p: var Parser): Expr

proc inclusive_OR_expression*(p: var Parser): Expr

proc logical_AND_expression*(p: var Parser): Expr

proc logical_OR_expression*(p: var Parser): Expr

proc conditional_expression*(p: var Parser, start: Expr): Expr

proc conditional_expression*(p: var Parser): Expr

proc assignment_expression*(p: var Parser): Expr

proc parse_initializer_list*(p: var Parser): Expr

proc parse_parameter_type_list*(p: var Parser): (bool, seq[(string, CType)])

# ================ type ====================

proc declaration*(p: var Parser): bool

# parse many type qualifiers, add to type
# used in decorator
proc parse_type_qualifier_list*(p: var Parser, ty: var CType)

# merge many token to a type, for example: `long long int const`
proc merge_types*(p: var Parser, ts: seq[Token]): Ctype

# declaration_specfier is used in function and variable declaration/definition
proc parse_declaration_specifiers*(p: var Parser): CType

# specfier_qualifier_list is used in struct declaration
proc parse_specifier_qualifier_list*(p: var Parser): CType

type 
    DeclaratorFlags = enum 
      Direct, Abstract, Function

proc parse_direct_declarator*(p: var Parser, base: CType; flags=Direct): (string, CType)

# take a base type, return the final type and name
# for example: static   int     *foo
#                base-type    decorator
proc parse_declarator*(p: var Parser, base: CType; flags=Direct): (string, CType)

# abstract decorator has no name
# for example: static int        (*)(int, char)
#               base-type      abstact-decorator
template parse_abstract_decorator*(p, base, f): untyped = parse_declarator(p, base, flags=f)

# type-name: specifier-qualifier-list optional<abstract_decorator>
# proc parse_type_name*(p: var Parser): CType

# parse a declaration: variable, struct, union, function...
# proc parse_declaration*(p: var Parser): Stmt

# parse a struct or union, return it
# for example: struct Foo
#              struct { ... }
#              struct Foo { ... }
proc parse_struct_union*(p: var Parser, t: Token): CType

# parse a enum, return it
# for example: enum State
#              enum { ... }
#              enum State { ... }
proc parse_enum*(p: var Parser): CType

proc parse_static_assert*(p: var Parser): bool

# ==================  statement =================

proc translation_unit*(p: var Parser)

proc statament*(p: var Parser): Stmt

proc compound_statement*(p: var Parser): Stmt

# ==================  utilities ==================

proc binop*(a: Expr, op: BinOP, b: Expr): Expr = Expr(k: EBin, lhs: a, rhs: b, bop: op)

proc unary*(e: Expr, op: UnaryOP): Expr = Expr(k: EUnary, uop: op, uoperand: e)

proc postfix*(e: Expr, op: PostfixOP): Expr = Expr(k: EPostFix, pop: op, poperand: e)

proc consume*(p: var Parser) =
    nextTok(p)

# ==================  definitions ==================

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
        of K_Atomic: TYATOMIC
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
        else: TYINVALID
    )
    if t == TYINVALID:
        return false
    ty.tags = ty.tags or t
    return true

proc default_storage(ts: var CType) =
    if (ts.tags and (TYTYPEDEF or TYTHREAD_LOCAL or TYAUTO or TYREGISTER or TYSTATIC or TYEXTERN)) == 0:
        ts.tags = ts.tags or TYAUTO # default to auto storage-class-specifier

proc merge_types*(p: var Parser, ts: seq[Token]): CType =
    if ts.len == 0:
        return nil
    result = CType(tags: TYINVALID, spec: TYPRIM)
    var b: seq[Token]
    for t in ts:
        if addTag(result, t) == false:
            b.add(t)
    default_storage(result)
    if b.len == 0: # no type
        p.warning("deault type to `int`")
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
            echo "bad type ", b[0]
            assert(false)
            TYINVALID
        )
        return result
    var signed = b.count(Ksigned)
    var unsigned = b.count(Kunsigned)
    let su = signed + unsigned
    if signed>1 or unsigned>1:
      p.type_error("too many `signed`/`unsigned`")
      return nil
    if su == 2:
     p.type_error("cannot both `signed` and `unsigned`")
     return nil
    let l = b.count(Klong)
    let s = b.count(Kshort)
    let f = b.count(Kfloat)
    let d = b.count(Kdouble)
    let i = b.count(Kint)
    let c = b.count(Kchar)
    let v = b.count(Kvoid)
    if c > 1:
        p.type_error("too many `char`s")
    if i > 1:
      p.type_error("too many `int`s")
      return nil
    if f > 0:
      p.type_error("`float` cannot combine with other types")
      return nil
    if v > 0:
      p.type_error("`void` cannot combine with other types")
      return nil
    if l >= 3:
      p.type_error("too many `long`s, max is `long long`")
      return nil
    if s >= 2:
      if s == 2:
        p.warning("duplicate 'short' declaration specifier")
      else:
        p.type_error("too many `short`s, max is `short`")
        return nil
    if d > 1:
       p.type_error("too many `double`s")
       return nil
    if bool(d):
      if d != 1:
        p.type_error("too many `long` for `double`")
        return nil
      if len(b) != (1 + l):
        p.type_error("extra `double` declaration specifier")
        return nil
      result.tags = result.tags or (if d==1: TYLONGDOUBLE else: TYDOUBLE)
      return result
    if bool(s): # short+int
      if (s + i + su) != len(b):
        p.type_error("extra `short` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUSHORT else: TYSHORT)
      return result
    if bool(l): # long+int
      if (l + i + su) != len(b):
        p.type_error("extra `long` declaration specifier")
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
        p.type_error("extra `char` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUCHAR else: TYCHAR)
      return result
    p.type_error("cannot combine types: " & $b)
    return nil

# const/restrict/atomic/volatile
proc parse_type_qualifier_list(p: var Parser, ty: var CType) =
    # TODO: _Atomic
    while true:
        case p.tok:
        of Kconst:
            ty.tags = ty.tags or TYCONST
            consume(p)
        of Krestrict:
            ty.tags = ty.tags or TYRESTRICT
            consume(p)
        of Kvolatile:
            ty.tags = ty.tags or TYVOLATILE
            consume(p)
        else:
            break

proc parse_specifier_qualifier_list*(p: var Parser): CType =
    # alignment-specifie
    var s: seq[Token]
    while p.tok in type_specifier_set + type_qualifier_set:
        s.add(p.tok)
        consume(p)
    return merge_types(p, s)

proc parse_declarator*(p: var Parser, base: CType; flags=Direct): (string, CType) =
    var ty = base
    while p.tok == TMul:
        consume(p)
        ty = CType(tags: TYINVALID, spec: TYPOINTER, p: ty)
        parse_type_qualifier_list(p, ty)
    return parse_direct_declarator(p, ty, flags)

proc parse_initializer_list*(p: var Parser): Expr =
    if p.tok != TLcurlyBracket:
        return assignment_expression(p)
    result = Expr(k: EInitializer_list)
    consume(p)
    while true:
        if p.tok == TRcurlyBracket:
            consume(p)
            break
        if p.tok == TLcurlyBracket:
            let e = parse_initializer_list(p)
            if e == nil:
                return nil
            result.inits.add(e)
        else:
            let e = assignment_expression(p)
            if e == nil:
                p.error("expect expression")
                return nil
            result.inits.add(e)
        if p.tok == TComma:
            consume(p)

proc parse_direct_declarator_end*(p: var Parser, base: CType, name: string): (string, CType)

proc parse_direct_declarator*(p: var Parser, base: CType; flags=Direct): (string, CType) =
    case p.tok:
    of TIdentifier:
        if flags == Abstract:
            return ("", nil)
        var name = p.val.sval
        consume(p)
        return parse_direct_declarator_end(p, base, name)
    of TLbracket:
        consume(p)
        let (name, ty) = parse_declarator(p, base, flags)
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return ("", nil)
        consume(p)
        return parse_direct_declarator_end(p, ty, name)
    else:
        if flags != Direct:
            return parse_direct_declarator_end(p, base, "")
        return ("", nil)

proc parse_direct_declarator_end*(p: var Parser, base: CType, name: string): (string, CType) =
    case p.tok:
    of TLSquareBrackets: # int arr[5], int arr[*], int arr[static 5], int arr[static const 5], int arr[const static 5], int arr2[static const restrict 5]
        consume(p) # eat ]
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: -1, arrtype: base)
        if p.tok == TMul: # int arr[*]
          consume(p)
          if p.tok != TRSquareBrackets:
            p.error("expect ']'")
            p.note("the syntax is:\n\tint arr[*]")
            return ("", nil)
          return (name, ty)
        if p.tok == Kstatic:
           consume(p)
           ty.tags = ty.tags or TYSTATIC
           parse_type_qualifier_list(p, ty)
        else:
            parse_type_qualifier_list(p, ty)
            if p.tok == Kstatic:
                ty.tags = ty.tags or TYSTATIC
                consume(p)
        let e = assignment_expression(p)
        ty.arrsize = eval_const_expression(e)
        if p.tok != TRSquareBrackets:
          p.error("expect ']'")
          return ("", nil)
        consume(p) # eat ]
        return parse_direct_declarator_end(p, ty, name)
    of TLbracket:
        consume(p)
        var ty = CType(tags: TYINVALID, spec: TYFUNCTION, ret: base)
        if p.tok != TRbracket:
            let res = parse_parameter_type_list(p)
            if res[0]:
                ty.params = res[1]
            else:
                return ("", nil)
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return ("", nil)
        consume(p)
        if p.tok == TLcurlyBracket: # function definition
            let body = compound_statement(p)
            if body == nil:
                p.parse_error("expect function body")
                return ("", nil)
            echo "body => ", $body
            return ("!", ty) # magic '!', a identfier cannot has '!' char
        return parse_direct_declarator_end(p, ty, name)
    else:
        return (name, base)

proc parse_struct_declarator(p: var Parser, base: CType): (string, CType) =
    if p.tok == TColon:
        consume(p)
        let e = constant_expression(p)
        if e == nil:
            p.parse_error("expect expression")
            p.note("in bit field declaration")
            return ("", nil)
        let bitsize = eval_const_expression(e)
        return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: base, bitsize: bitsize))
    else:
        let d = parse_declarator(p, base)
        if d[1] == nil:
            return ("", nil)
        if p.tok == TColon:
            consume(p)
            let e = constant_expression(p)
            if e == nil:
                p.parse_error("expect expression")
                p.note("in bit field declaration")
                return
            let bitsize = eval_const_expression(e)
            return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: d[1], bitsize: bitsize))
        return d

proc parse_struct_union*(p: var Parser, t: Token): CType =
    consume(p) # eat struct/union
    var name = ""
    if p.tok == TIdentifier:
        name = p.val.sval
        consume(p)
        if p.tok != TLcurlyBracket: # struct Foo
           return if t==Kstruct: getstructdef(p, name) else: getuniondef(p, name)
    elif p.tok != TLcurlyBracket:
        p.error("expect '{' for start anonymous struct/union")
        return nil
    if t == Kstruct:
        result = CType(tags: TYINVALID, spec: TYSTRUCT, sname: name)
    else:
        result = CType(tags: TYINVALID, spec: TYUNION, sname: name)
    consume(p)
    if p.tok != TRcurlyBracket:
        while true:
            if p.tok == K_Static_assert:
                if parse_static_assert(p) == false:
                    return nil
                if p.tok == TRcurlyBracket:
                    break
                continue
            var base = parse_specifier_qualifier_list(p)
            if base == nil:
                p.parse_error("expect specifier-qualifier-list")
                return nil

            if p.tok == TSemicolon:
                p.warning("struct/union member declaration doest not declare anything")
                consume(p)
                continue
            else:
                while true:
                    let e = parse_struct_declarator(p, base)
                    if e[1] == nil:
                        p.error("expect struct-declarator")
                        return nil
                    result.selems.add(e)
                    if p.tok == TComma:
                        consume(p)
                    else:
                        if p.tok != TSemicolon:
                            p.parse_error("expect ';', got " & showToken(p))
                            return nil
                        consume(p)
                        break
                if p.tok == TRcurlyBracket:
                    consume(p)
                    break
    else:
        consume(p)
    if len(result.sname) > 0:
        if t == Kstruct:
            putstructdef(p, result)
        else:
            putenumdef(p, result)
    return result

let enum_type = CType(tags: TYEXPR or TYINT, spec: TYPRIM)

proc parse_enum*(p: var Parser): CType =
    consume(p) # eat enum
    var name = ""
    if p.tok == TIdentifier:
        name = p.val.sval
        consume(p)
        if p.tok != TLcurlyBracket: # struct Foo
          return getenumdef(p, name)
    elif p.tok != TLcurlyBracket:
        p.error("expect '{' for start anonymous enum")
        return nil
    result = CType(tags: TYINVALID, spec: TYENUM, ename: name)
    consume(p)
    var c: int = 0
    while true:
        if p.tok != TIdentifier:
            break # enum {A, } is ok !
        var s = p.val.sval # copy
        consume(p)
        if p.tok == TAssign:
            consume(p)
            let e = constant_expression(p)
            c = eval_const_expression(e)
        result.eelems.add((s, c))
        putsymtype(p, s, enum_type)
        inc c
        if p.tok == TComma:
            consume(p)
        else:
            break
    if p.tok != TRcurlyBracket:
        p.error("expect '}'")
    consume(p)
    if len(result.ename) > 0:
        putenumdef(p, result)
    return result

proc parse_parameter_type_list*(p: var Parser): (bool, seq[(string, CType)]) =
    result = (true, default(typeof(result[1])))
    while true:
        var base = parse_declaration_specifiers(p)
        if base == nil:
            return (false, default(typeof(result[1])))
        let res = parse_abstract_decorator(p, base, Function)
        if res[1] == nil:
            return (false, default(typeof(result[1])))
        result[1].add(res)
        if p.tok == TRbracket:
            break
        if p.tok == TComma:
            consume(p)
        if p.tok == PPEllipsis:
            result[1].add(("", nil))
            consume(p)
            break
        elif p.tok == TRbracket:
            break

proc read_enum_sepcs*(c: var CType, sepc: Token) = 
    discard

proc read_struct_union_sepcs*(c: var CType, sepc: Token) = 
    discard

proc more(p: var Parser, s: var seq[Token]) = 
    while p.tok in declaration_specifier_set:
        s.add(p.tok)
        consume(p)

let default_int = CType(tags: TYINVALID or TYINT, spec: TYPRIM)

proc istype(p: var Parser, a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        return true
    if a == TIdentifier:
      let ty = gettypedef(p, p.val.sval)
      return ty != nil and (ty.tags and TYTYPEDEF) != 0
    return false

proc parse_declaration_specifiers*(p: var Parser): CType =
    ## TODO: _Alignas
    var s: seq[Token]
    while p.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        if p.tok == Kenum:
            result = parse_enum(p)
            more(p, s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif p.tok == Kunion or p.tok == Kstruct:
            result = parse_struct_union(p, p.tok)
            more(p, s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        s.add(p.tok)
        consume(p)
    if p.tok == TIdentifier:
        let ty = gettypedef(p, p.val.sval)
        more(p, s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            consume(p)
            while p.tok in declaration_specifier_set:
                s.add(p.tok)
                consume(p)
            if len(s) > 0:
                if s[0] == Ktypedef:
                    return ty
                p.warning("typedef types cannot combine with type-specifier and type-qualifiers")
            result = deepCopy(ty)
            result.tags = ty.tags and (not TYTYPEDEF)
            return result
    if s.len > 0:
        return merge_types(p, s)
    p.warning("type defaults to 'int' in declaration")
    echo showToken(p)
    p.note("specify one or more storage class specifier or type specifier")
    return default_int

proc parse_static_assert*(p: var Parser): bool =
    consume(p)
    if p.tok != TLbracket:
        p.parse_error("expect '('")
        return false
    consume(p)
    let e = constant_expression(p)
    let ok = eval_const_expression_bool(e)
    if p.tok == TRbracket: # no message
        if ok == false:
            p.error("static assert failed!")
        consume(p)
        p.note("static assert with no message")
    elif p.tok == TComma:
        consume(p)
        if p.tok != TStringLit:
            p.parse_error("expect string literal in static assert")
            return false
        if ok == false:
            p.error(p.val.sval)
        consume(p)
    else:
        p.parse_error("expect ',' or ')'")
        return false
    if p.tok != TSemicolon:
        p.error("expect ';'")
        return false
    consume(p)
    return true

proc declaration*(p: var Parser): bool =
    if p.tok == K_Static_assert:
        return parse_static_assert(p)
    else:
        var isstruct = p.tok in  {Kstruct, Kunion, Kenum}
        var base = parse_declaration_specifiers(p)
        if base == nil:
            return false
        while true:
            if p.tok == TSemicolon:
                consume(p)
                if isstruct==false:
                    p.warning("declaration does not declare anything")
                break
            let (name, ty) = parse_declarator(p, base)
            if ty == nil:
                p.parse_error("declarator expected")
                return false
            if name == "!":
                return true
            echo name, " => ", ty
            putsymtype(p, name, ty)
            if p.tok == TAssign:
                consume(p)
                let init = parse_initializer_list(p)
                if init == nil:
                    return false
                echo " init => ",init
            if p.tok == TComma:
                consume(p)
            elif p.tok == TSemicolon:
                consume(p)
                break
        return true

proc cast_expression*(p: var Parser): Expr =
    case p.tok:
    of TLbracket:
        consume(p)
        let n = expression(p)
        if n == nil: 
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect `)`")
            return nil
        consume(p)
        let e = cast_expression(p)
        return Expr(k: ECast, casttype: n, castval: e)
    else:
        return unary_expression(p)

proc unary_expression*(p: var Parser): Expr =
    let tok = p.tok
    case tok:
    of TNot, TMul, TBitAnd, TBitNot:
        consume(p)
        let e = cast_expression(p)
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
        consume(p)
        let e = unary_expression(p)
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
        consume(p)
        let e = unary_expression(p)
        if e == nil:
            return nil
        return unary(e, if tok == TAddAdd: OPrefixIncrement else: OPrefixDecrement)
    of Ksizeof:
        consume(p)
        if p.tok == TLbracket:
            consume(p)
            let e = unary_expression(p)
            if e == nil:
                return nil
            if p.tok != TRbracket:
                p.parse_error("expect )")
                return nil
            consume(p)
            return Expr(k: ESizeOf, sizeofx: e)
        else:
            let e = unary_expression(p)
            if e == nil:
                return nil
            return Expr(k: ESizeOf, sizeofx: e)
    of K_Alignof:
        consume(p)
        if p.tok != TLbracket:
            p.parse_error("expect (")
            p.note("the syntax is:\n\t_Alignof(type-name)")
            return nil
        consume(p)
        let e = expression(p)
        if p.tok != TRbracket:
            p.parse_error("expect )")
            p.note("the syntax is:\n\t_Alignof(type-name)")
            return nil
        consume(p)
        return Expr(k: EAlignof, sizeofx: e)
    else:
        return postfix_expression(p)

proc primary_expression*(p: var Parser): Expr =
    case p.tok:
    of TCharLit:
        result = Expr(k: ECharLit, ival: p.val.ival)
        consume(p)
    of TNumberLit:
        result = Expr(k: EIntLit, ival: p.val.ival)
        consume(p)
    of TFloatLit:
        result = Expr(k: EFloatLit, fval: p.val.fval)
        consume(p)
    of TStringLit:
        result = Expr(k: EStringLit, sval: p.val.sval)
        consume(p)
    of TIdentifier:
        result = Expr(k: EVar, sval: p.val.sval)
        consume(p)
    of TLbracket:
        consume(p)
        result = expression(p)
        if result == nil:
            p.parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return nil
        consume(p)
    of K_Generic:
        consume(p)
        if p.tok != TLbracket:
            p.parse_error("expect '('")
            return nil
        consume(p)
        let test = assignment_expression(p)
        if test == nil:
            p.parse_error("expect expression")
            p.note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
            return nil
        if p.tok != TComma:
            p.parse_error("expect ,")
            return nil
        consume(p)
        var selectors: seq[(Expr, Expr)]
        while true:
            var tname: Expr = nil
            if p.tok == Kdefault:
                consume(p)
            else:
                tname = expression(p)
                if tname == nil:
                    p.parse_error("expect expression")
                    p.note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                    return nil
            if p.tok != TColon:
                p.parse_error("expect ':'")
                p.note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            consume(p)
            let e = assignment_expression(p)
            if e == nil:
                p.parse_error("expect expression")
                p.note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            selectors.add((tname, e))
            case p.tok:
            of TComma:
              consume(p)
              continue
            of TRbracket:
              consume(p)
              break
            else:
              p.parse_error("expect ','' or ')'")
              return nil
        return Expr(k: EGeneric, selectexpr: test, selectors: selectors)
    of TEOF:
        return nil
    else:
        p.parse_error("primary_expression: bad token: " & showToken(p))
        return nil

proc postfix_expression*(p: var Parser): Expr =
    let e = primary_expression(p)
    case p.tok:
    of TSubSub, TAddAdd:
        let op = if p.tok == TAddAdd: OPostfixIncrement else: OPostfixDecrement
        consume(p)
        return postfix(e, op)
    of TArrow, TDot: # member access
        let isarrow = p.tok == TArrow
        consume(p)
        let i = p.tok
        if i != TIdentifier:
            p.parse_error("expect identifier")
            p.note("the syntax is:\n\tfoo.member\n\tfoo.member")
            return nil
        return binop(e, (if isarrow: OMemberAccess else: OPointerMemberAccess), Expr(k: EVar, sval: p.val.sval))
    of TLbracket: # function call
        consume(p)
        let args = expression(p)
        if args == nil:
            p.parse_error("expect expression")
            p.note("the syntax is:\n\tfunction-name(argument)")
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return
        consume(p)
        return Expr(k: ECall, left: e, right: args)
    of TLSquareBrackets: # array subscript
        consume(p)
        let args = expression(p)
        if args == nil:
            p.parse_error("expect expression")
            p.note("the syntax is:\n\tarray[expression]")
            return nil
        if p.tok != TRSquareBrackets:
            p.parse_error("expect ']'")
            return
        consume(p)
        return Expr(k: ESubscript, left: e, right: args)
    else:
        return e

proc multiplicative_expression*(p: var Parser): Expr =
    result = cast_expression(p)
    case p.tok:
    of TMul:
        consume(p)
        let r = cast_expression(p)
        if r == nil:
            return nil
        result = binop(result, OMultiplication, r)
    of TSlash:
        consume(p)
        let r = cast_expression(p)
        if r == nil:
            return nil
        result = binop(result, ODivision, r)
    of TPercent:
        consume(p)
        let r = cast_expression(p)
        if r == nil:
            return nil
        result = binop(result, Oremainder, r)
    else:
        return result

proc additive_expression*(p: var Parser): Expr =
    result = multiplicative_expression(p)
    case p.tok:
    of TAdd:
        consume(p)
        let r = multiplicative_expression(p)
        if r == nil:
            return nil
        result = binop(result, OAddition, r)
    of TDash:
        consume(p)
        let r = multiplicative_expression(p)
        if r == nil:
            return nil
        result = binop(result, OSubtraction, r)
    else:
        return result

proc shift_expression*(p: var Parser): Expr =
    result = additive_expression(p)
    case p.tok:
    of Tshl:
        consume(p)
        let r = additive_expression(p)
        if r == nil:
            return nil
        result = binop(result, OShl, r)
    of Tshr:
        consume(p)
        let r = additive_expression(p)
        if r == nil:
            return nil
        result = binop(result, OShr, r)
    else:
        return result

proc relational_expression*(p: var Parser): Expr =
    result = shift_expression(p)
    case p.tok:
    of TLt:
        consume(p)
        let r = shift_expression(p)
        if r == nil:
            return nil
        result = binop(result, OLt, r)
    of TLe:
        consume(p)
        let r = shift_expression(p)
        if r == nil:
            return nil
        result = binop(result, OLe, r)
    of TGt:
        consume(p)
        let r = shift_expression(p)
        if r == nil:
            return nil
        result = binop(result, OGt, r)
    of TGe:
        consume(p)
        let r = shift_expression(p)
        if r == nil:
            return nil
        result = binop(result, OGe, r)
    else:
        return result


proc equality_expression*(p: var Parser): Expr =
    result = relational_expression(p)
    case p.tok:
    of TEq:
        consume(p)
        let r = relational_expression(p)
        if r == nil:
            return nil
        result = binop(result, OEq, r)
    of TNe:
        consume(p)
        let r = relational_expression(p)
        if r == nil:
            return nil
        result = binop(result, ONe, r)
    else:
        return result

proc AND_expression*(p: var Parser): Expr =
    result = equality_expression(p)
    case p.tok:
    of TBitAnd:
        consume(p)
        let r = equality_expression(p)
        if r == nil:
            return nil
        result = binop(result, OBitwiseAnd, r)
    else:
        return result

proc exclusive_OR_expression*(p: var Parser): Expr =
    result = AND_expression(p)
    case p.tok:
    of TXOr:
        consume(p)
        let r = AND_expression(p)
        if r == nil:
            return nil
        result = binop(result, OBitwiseXor, r)
    else:
        return result

proc inclusive_OR_expression*(p: var Parser): Expr =
    result = exclusive_OR_expression(p)
    case p.tok:
    of TBitOr:
        consume(p)
        let r = exclusive_OR_expression(p)
        if r == nil:
            return nil
        result = binop(result, OBitwiseOr, r)
    else:
        return result

proc logical_AND_expression*(p: var Parser): Expr =
    result = inclusive_OR_expression(p)
    case p.tok:
    of TBitOr:
        consume(p)
        let r = inclusive_OR_expression(p)
        if r == nil:
            return nil
        result = binop(result, OLogicalAnd, r)
    else:
        return result

proc logical_OR_expression*(p: var Parser): Expr =
    result = logical_AND_expression(p)
    case p.tok:
    of TLogicalOr:
        consume(p)
        let r = logical_AND_expression(p)
        if r == nil:
            return nil
        result = binop(result, OLogicalOr, r)
    else:
        return result

proc constant_expression*(p: var Parser): Expr = 
    conditional_expression(p)

proc conditional_expression*(p: var Parser, start: Expr): Expr =
    consume(p) # TQuestionMark
    let lhs = expression(p)
    if lhs == nil:
        p.parse_error("expect expression")
        return nil
    if p.tok != TColon:
        return lhs
    consume(p)
    let rhs = conditional_expression(p)
    if rhs == nil:
        p.parse_error("expect expression")
        p.note("the syntax is:\n\texpr ? a ? b")
        return nil
    return Expr(k: ECondition, cond: start, cleft: lhs, cright: rhs)

proc conditional_expression*(p: var Parser): Expr =
    let e = logical_OR_expression(p)
    if e == nil:
        return nil
    if p.tok == TQuestionMark:
      return conditional_expression(p, e)
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

proc assignment_expression*(p: var Parser): Expr =
    result = logical_OR_expression(p)
    if result == nil:
        return nil
    let tok = p.tok
    if tok == TQuestionMark:
        return conditional_expression(p, result)
    let op = get_assignment_op(tok)
    if op != BNop:
        consume(p)
        let e = assignment_expression(p)
        if e == nil:
            p.parse_error("expect expression")
            p.note("the syntax is:\n\texpr1=expr2, expr3=expr4, ...")
            return nil
        result = binop(result, op, e)

proc expression*(p: var Parser): Expr =
    result = assignment_expression(p)
    if p.tok == TComma:
        let r = assignment_expression(p)
        if r == nil:
            return nil
        result = binop(result, Comma, r)

proc translation_unit*(p: var Parser) =
    while p.tok != TEOF:
        let ok = declaration(p)
        if ok == false:
            break

proc compound_statement*(p: var Parser): Stmt =
    result = Stmt(k: SCompound)
    consume(p)
    while p.tok != TRcurlyBracket:
        var s: Stmt = nil
        if p.istype(p.tok):
            if declaration(p) == false:
                return nil
            s = Stmt(k: SVarDecl)
        else:
            s = statament(p)
            if s == nil:
                return nil
        result.stmts.add(s)
    consume(p)
    return result

proc statament*(p: var Parser): Stmt =
    if p.tok == TSemicolon:
        consume(p)
        return Stmt(k: SSemicolon)
    elif p.tok == TLcurlyBracket:
        return compound_statement(p)
    elif p.tok == Kcase:
        consume(p)
        let e = constant_expression(p)
        if e == nil:
            p.parse_error("expect constant-expression")
            return nil
        if p.tok != TColon:
            p.parse_error("':' expected")
            return nil
        consume(p)
        let s = statament(p)
        if s == nil:
            p.parse_error("expect statament")
            return nil
        return Stmt(k: Scase, case_expr: e, case_stmt: s)
    elif p.tok == Kdefault:
        consume(p)
        if p.tok != TColon:
            p.parse_error("':' expected")
            return nil
        consume(p)
        let s = statament(p)
        if s == nil:
            return nil
        return Stmt(k: SDefault, default_stmt: s)
    elif p.tok == Kgoto:
        consume(p)
        if p.tok != TIdentifier:
            p.parse_error("expect identifier")
            p.note("the syntax is:\n\tgoto label;")
            return nil
        let l = getLabel(p, p.val.sval)
        consume(p)
        if p.tok != TSemicolon:
            p.error("expect ';'")
            return nil
        consume(p)
        if l == -1:
            p.error("undeclared label " & p.val.sval)
            return nil
        return Stmt(k: SGoto, location: l)
    elif p.tok == Kcontinue:
        consume(p)
        if p.tok != TSemicolon:
            p.error("expect ';'")
            return nil
        consume(p)
        return Stmt(k: SContinue)
    elif p.tok == Kbreak:
        consume(p)
        if p.tok != TSemicolon:
            p.error("expect ';'")
            return nil
        consume(p)
        return Stmt(k: SBreak)
    elif p.tok == Kreturn:
        consume(p)
        if p.tok == TSemicolon:
            consume(p)
            return Stmt(k: SReturn)
        let e = expression(p)
        if e == nil:
            p.parse_error("expect expression")
            return nil
        return Stmt(k: SReturn, exprbody: e)
    elif p.tok == Kif:
        consume(p)
        if p.tok != TLbracket:
            p.parse_error("expect '('")
            return nil
        consume(p)
        let e = expression(p)
        if e == nil:
            p.parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return nil
        consume(p)
        let s = statament(p)
        if s == nil:
            p.parse_error("expect statament")
            return nil
        var elsebody: Stmt = nil
        if p.tok == Kelse:
            consume(p)
            elsebody = statament(p)
            if elsebody == nil:
                p.parse_error("expect statament")
                return nil
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody)
    elif p.tok == Kwhile or p.tok == Kswitch:
        let tok = p.tok
        consume(p)
        if p.tok != TLbracket:
            p.parse_error("expect '('")
            return nil
        consume(p)
        let e = expression(p)
        if e == nil:
            p.parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return nil
        consume(p)
        let s = statament(p)
        if s == nil:
            p.parse_error("expect statament")
            return nil
        if tok == Kwhile:
            return Stmt(k: SWhile, test: e, body: s)
        else:
            return Stmt(k: SSwitch, test: e, body: s)  
    elif p.tok == Kfor:
        consume(p)
        if p.tok != TLbracket:
            p.parse_error("expect '('")
            return nil
        consume(p)
        enterBlock(p)
        # init-clause may be an expression or a declaration
        var init: Expr = nil
        if p.istype(p.tok):
            if declaration(p) == false:
                p.parse_error("expect declaration")
                return nil
        else:
            init = expression(p)
            if init == nil:
                p.parse_error("expect expression")
                return nil
        if p.tok != TSemicolon:
            p.error("expect ';'")
        consume(p)
        #  cond-expression 
        let cond = expression(p)
        if cond == nil:
            p.parse_error("expect expression")
            return nil
        if p.tok != TSemicolon:
            p.error("expect ';'")
        consume(p)
        let forincl = expression(p)
        if forincl == nil:
            p.parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return nil
        consume(p)
        let s = statament(p)
        if s == nil:
            p.parse_error("expect statament")
            return nil
        leaveBlock(p)
        return Stmt(k: SFor, forinit: init, forcond: cond, forincl: forincl, forbody: s)
    elif p.tok == Kdo:
        consume(p)
        let s = statament(p)
        if s == nil:
            p.parse_error("expect statament")
            return nil
        if p.tok != Kwhile:
            p.parse_error("expect 'while'")
            return nil
        consume(p)
        if p.tok != TLbracket:
            p.parse_error("expect '('")
            return nil
        consume(p)
        let e = expression(p)
        if e == nil:
            p.parse_error("expect expression")
            return nil
        if p.tok != TRbracket:
            p.parse_error("expect ')'")
            return nil
        consume(p)
        if p.tok != TSemicolon:
            p.error("expect ';'")
            return nil
        consume(p)
        return Stmt(k: SDoWhile, test: e, body: s)
    elif p.tok == TIdentifier:
        var val = p.val.sval
        consume(p)
        if p.tok == TColon: # # labeled-statement
            consume(p)
            let s = statament(p)
            if s == nil:
                p.parse_error("expect statament")
                p.note("to add a empty statement, use:\n\tlabel: ;")
                return nil
            let label = p.col
            putLable(p, val, label)
            return Stmt(k: SLabled, label: label, labledstmt: s)
        else: # expression
            p.putToken(PPToken(s: val, tok: TIdentifier))
    let e = expression(p)
    if e == nil:
        p.parse_error("expect expression")
        return nil
    if p.tok != TSemicolon:
        p.parse_error("expect ';'")
        return nil
    consume(p)
    return Stmt(k: SExpr, exprbody: e)

