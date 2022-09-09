## ast.nim - C's abstract syntax tree

import types, operators
import std/[strutils]

export types, operators

type
    StmtKind* = enum
      SSemicolon, SCompound, SGoto, SContinue, SBreak, SReturn, SExpr, SLabled, SIf, 
      SDoWhile, SWhile, SFor, SSwitch, SDeclOnly, SAsm
      SVarDecl, SDefault, SCase, SFunction, SVarDecl1
    StmtList* = seq[Stmt] # compound_stmt, { body }
    Stmt* = ref object
      case k*: StmtKind
      of SFunction:
        funcname*: string
        functy*: CType
        funcbody*: Stmt
      of SAsm:
        asms*: string
      of SCompound:
        stmts*: StmtList
      of SDefault:
        default_stmt*: Stmt
      of Scase:
        case_expr*: Expr
        case_stmt*: Stmt
      of SBreak, SContinue, SSemicolon:
        discard
      of SExpr, SReturn:
        exprbody*: Expr
      of SGoto:
        location*: string
      of SLabled:
        label*: string
        labledstmt*: Stmt
      of SIf:
        iftest*: Expr
        ifbody*: Stmt
        elsebody*: Stmt
      of SDoWhile, SWhile, SSwitch:
        test*: Expr
        body*: Stmt
      of SFor:
        forinit*: Stmt
        forcond*, forincl*: Expr
        forbody*: Stmt
      of SVarDecl1:
        var1name*: string
        var1type*: CType
      of SVarDecl:
        vars*: seq[(string, CType, Expr)]
        # (name, type, init<opt>, align<opt>)
      of SDeclOnly:
        decl*: CType

    ExprKind* = enum
      EBin, EUnary, EPostFix, EIntLit, EFloatLit, EVoid,
      EVar, ECondition, ECast, ECall, ESubscript, EDefault,
      EArray, EStruct, EBackend, EString, EUndef
      EPointerMemberAccess, EMemberAccess, ArrToAddress
    Expr* = ref object
      ty*: CType
      case k*: ExprKind
      of EVoid, ArrToAddress:
        voidexpr*: Expr 
      of EBin:
        lhs*, rhs*: Expr
        bop*: BinOP
      of EUnary:
        uop*: UnaryOP
        uoperand*: Expr
      of EPostFix:
        pop*: PostfixOP
        poperand*: Expr
      of EIntLit:
        ival*: int
      of EFloatLit:
        fval*: float
      of EString:
        str*: string
      of EVar:
        sval*: string
      of EArray, EStruct:
        arr*: seq[Expr]
      of ECondition:
        cond*: Expr
        cleft*, cright*: Expr
      of ECast:
        castop*: CastOp
        castval*: Expr
      of ECall:
        callfunc*: Expr
        callargs*: seq[Expr]
      of ESubscript:
        left*, right*: Expr
      of EPointerMemberAccess, EMemberAccess:
        obj*: Expr
        idx*: int
      of EBackend:
        p*: pointer
      of EDefault, EUndef:
        discard

proc `$`*(a: Stmt, level=0): string

proc `$`*(e: Expr): string

proc joinShow6*(a: seq[(string, CType, Expr)]): string =
    result = ""
    for i in 0..<len(a):
        result.add(a[i][0])
        result.add(": ")
        result.add($a[i][1])
        if a[i][2] != nil:
          result.add('=')
          result.add($a[i][2])
        if i < (len(a)-1):
          result.add(", ")

proc joinShow5*(a: seq[Stmt], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for i in a:
        result.add(padding & '\t')
        result.add(`$`(i, level + 1))
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc joinShow*[T](a: seq[T], c: string = " "): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i])
        result.add(c)
    result.add($a[^1])


proc `$`*(a: Stmt, level=0): string =
  if a == nil:
    return "<nil>"
  case a.k:
  of SAsm:
    "__asm__(" & a.asms & ')'
  of SFunction:
    "Function " & a.funcname & " :\n" & $a.functy & `$`(a.funcbody, level + 1)
  of SVarDecl1:
    "<SVarDecl1>"
  of SVarDecl:
    joinShow6(a.vars)
  of SCompound:
    joinShow5(a.stmts, level+1)
  of SDefault:
    "default: " & $a.default_stmt
  of Scase:
    "case " & $a.case_expr & ':' & $a.case_stmt
  of SBreak:
    "break;"
  of SContinue:
    "continue;"
  of SSemicolon:
    ";"
  of SExpr:
    $a.exprbody & ';'
  of SReturn:
    if a.exprbody == nil:
      "return;"
    else:
      "return " & $a.exprbody & ';'
  of SGoto:
    "goto " & a.location & ';'
  of SLabled:
    a.label & ':' & $a.labledstmt
  of SIf:
    "if (" & $a.iftest & ") " & `$`(a.ifbody, level + 1) & (if a.elsebody==nil: "" else: "else " & `$`(a.elsebody, level + 1))
  of SWhile:
    "while (" & $a.test & ") " & `$`(a.body, level + 1)
  of SSwitch:
    "switch (" & $a.test & ") " & `$`(a.body, level + 1)
  of SDoWhile:
    "do " & `$`(a.body, level + 1) & " while (" & $a.test & ");"
  of SFor:
    "for (" & (if a.forinit==nil: ";" else: $a.forinit) & 
    (if a.forcond==nil: "" else: $a.forcond) & ';' &
    (if a.forincl==nil: "" else: $a.forincl) & ')' &
    `$`(a.forbody, level+1)
  of SDeclOnly:
    $(a.decl)

proc `$`*(e: Expr): string =
  if e == nil:
    return "<nil>"
  case e.k:
  of EUndef:
    "<undefined-value>"
  of EVoid:
    "(void)" & $e.voidexpr
  of ArrToAddress:
    "&(" & $e.voidexpr & ")"
  of EDefault:
    "<zero>"
  of EStruct:
    "<struct-constant>"
  of EMemberAccess:
    $e.obj & '.' & $e.idx
  of EPointerMemberAccess:
    $e.obj & "->" & $e.idx
  of EString:
    repr(e.str)
  of EBin:
    '(' & $e.lhs & ' ' & $e.bop & ' ' & $e.rhs & ')'
  of EPostFix:
    $e.poperand & $e.pop
  of EUnary:
    $e.uop & $e.uoperand
  of EIntLit:
    $e.ival
  of EFloatLit:
    $e.fval
  of EVar:
    e.sval
  of ECondition:
    $e.cond & '?' & $e.cleft & ':' & $e.cright
  of ECast:
    '(' & $e.ty & ')' & $e.castval
  of ESubscript:
    $e.left & '[' & $e.right & ']'
  of ECall:
    $e.callfunc & '(' & $joinShow(e.callargs, ", ") & ')'
  of EArray:
    "{" & $e.arr & "}"
  of EBackend:
    "<backend>"

proc isConstant*(e: Expr): bool =
    ## check whether a expression is a constant-expression
    case e.k:
    of EVoid:
        false
    of ArrToAddress:
        true
    of EBin:
        isConstant(e.lhs) and isConstant(e.rhs)
    of EUnary:
        isConstant(e.uoperand)
    of EPostFix:
        false
    of EIntLit, EFloatLit, EString, EVar, EArray, EStruct:
        true
    of ECondition:
        isConstant(e.cond) and isConstant(e.cleft) and isConstant(e.cright)
    of ECast:
        true
    of ECall:
        false
    of ESubscript:
        false
    of EMemberAccess:
        isConstant(e.obj)
    of EPointerMemberAccess:
        false
    of EBackend:
        false
    of EDefault, EUndef:
        true
