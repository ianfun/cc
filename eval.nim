import token

proc `&&`(a, b: intmax_t): intmax_t {.importc: "myopand", nodecl, header: "myop.h".}

proc `||`(a, b: intmax_t): intmax_t {.importc: "myopor", nodecl, header: "myop.h".}

proc `!`(a: intmax_t): intmax_t {.importc: "myopnot", nodecl, header: "myop.h".}

proc evali*(e: Expr): intmax_t =
  case e.k:
  of EBin:
      case e.bop:
      of OAddition:
        evali(e.lhs) + evali(e.rhs)
      of OSubtraction:
        evali(e.lhs) - evali(e.rhs)
      of OMultiplication:
        evali(e.lhs) * evali(e.rhs)
      of ODivision:
        evali(e.lhs) div evali(e.rhs)
      of Oremainder:
        evali(e.lhs) mod evali(e.rhs)
      of Oshl:
        evali(e.lhs) shl evali(e.rhs)
      of OShr:
        evali(e.lhs) shr evali(e.rhs)
      of OGe:
        if evali(e.lhs) >= evali(e.rhs): 1 else: 0
      of OGt:
        if evali(e.lhs) > evali(e.rhs): 1 else: 0
      of OEq:
        if evali(e.lhs) == evali(e.rhs): 1 else: 0
      of ONe:
        if evali(e.lhs) != evali(e.rhs): 1 else: 0
      of OLe:
        if evali(e.lhs) <= evali(e.rhs): 1 else: 0
      of OLt:
        if evali(e.lhs) < evali(e.rhs): 1 else: 0
      of OBitwiseAnd:
        evali(e.lhs) and evali(e.rhs)
      of OBitwiseXor:
        evali(e.lhs) xor evali(e.rhs)
      of OBitwiseOr:
        evali(e.lhs) or evali(e.rhs)
      of OLogicalAnd:
        evali(e.lhs) && evali(e.rhs)
      of OLogicalOr:
        evali(e.lhs) || evali(e.rhs)
      else:
        echo "bad binary op!"
        intmax_t(0)
  of EPostFix:
    echo "bad postfix!"
    0
  of EUnary:
    case e.uop:
    of OUnaryPlus:
      evali(e.uoperand)
    of OUnaryMinus:
      -evali(e.uoperand)
    of OLogicalNot:
      ! evali(e.uoperand)
    of OBitwiseNot:
      not evali(e.uoperand)
    else:
      echo "bad uop"
      0
  of ECharLit, EIntLit:
    e.ival
  of EFloatLit:
    echo "bad float"
    0
  of ECondition:
    if evali(e.cond) != 0: evali(e.cleft) else: evali(e.cright)
  of ECast:
    echo "bad cast"
    0
  of ESubscript:
    echo "bad subscript"
    0
  of ECall:
    echo "bad call"
    0
  of ECppVar:
    0
  of EVar:
    echo "bad var"
    0
  of EArray:
    echo "bad array"
    0

proc eval_const_expression*(e: Expr): intmax_t =
  evali(e)

proc eval_const_expression_bool*(e: Expr): bool =
  eval_const_expression(e) != 0

proc constant_expression*(): Expr

import parser

proc constant_expression*(): Expr = 
    ## parse a constant-expression, which can eval at compile time
    conditional_expression()

