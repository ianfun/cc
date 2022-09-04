import core
from parser import constant_expression, p

proc my_UNEG(a: uintmax_t): intmax_t {.importc: "myopneg", nodecl, header: "myop.h".}

proc my_SNEG(a: intmax_t): intmax_t {.importc: "myopneg", nodecl, header: "myop.h".}

proc `&&`(a, b: intmax_t): intmax_t {.importc: "myopand", nodecl, header: "myop.h".}

proc `||`(a, b: intmax_t): intmax_t {.importc: "myopor", nodecl, header: "myop.h".}

proc `!`(a: intmax_t): intmax_t {.importc: "myopnot", nodecl, header: "myop.h".}

proc eval_error*(msg: string) =
    if p.eval_error == false:
      stderr.writeLine("\e[34m" & p.filename & ": " & $p.line & '.' & $p.col & ": parse error: " & msg & "\e[0m")
      p.eval_error = true

proc evali*(e: Expr): intmax_t

proc setEval*() =
  app.eval_const_expression = evali


proc evali*(e: Expr): intmax_t =
  ## run the constant expression
  ##
  ## if error, setting eval_error
  case e.k:
  of EBin:
      case e.bop:
      of SAdd, UAdd:
        evali(e.lhs) + evali(e.rhs)
      of SSub, USub:
        evali(e.lhs) - evali(e.rhs)
      of SMul, UMul:
        evali(e.lhs) * evali(e.rhs)
      of UDiv:
        cast[intmax_t](cast[uintmax_t](evali(e.lhs)) div cast[uintmax_t](evali(e.rhs)))
      of SDiv:
        evali(e.lhs) div evali(e.rhs)
      of URem:
        cast[intmax_t](cast[uintmax_t](evali(e.lhs)) mod cast[uintmax_t](evali(e.rhs)))
      of Shr:
        cast[intmax_t](cast[uintmax_t](evali(e.lhs)) shr cast[uintmax_t](evali(e.rhs)))
      of AShr:
        # https://stackoverflow.com/questions/53746160/how-to-implement-arithmetic-right-shift-in-c
        # https://stackoverflow.com/questions/7622/are-the-shift-operators-arithmetic-or-logical-in-c
        evali(e.lhs) shr evali(e.rhs)
      of EQ:
        if evali(e.lhs) == evali(e.rhs): 1 else: 0
      of NE:
        if evali(e.lhs) != evali(e.rhs): 1 else: 0
      of SGE:
        if evali(e.lhs) >= evali(e.rhs): 1 else: 0
      of SGT:
        if evali(e.lhs) > evali(e.rhs): 1 else: 0
      of SLE:
        if evali(e.lhs) <= evali(e.rhs): 1 else: 0
      of SLT:
        if evali(e.lhs) < evali(e.rhs): 1 else: 0
      of UGE:
        if cast[uintmax_t](evali(e.lhs)) >= cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of UGT:
        if cast[uintmax_t](evali(e.lhs)) > cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of ULE:
        if cast[uintmax_t](evali(e.lhs)) <= cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of ULT:
        if cast[uintmax_t](evali(e.lhs)) < cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of And:
        evali(e.lhs) and evali(e.rhs)
      of Xor:
        evali(e.lhs) xor evali(e.rhs)
      of Or:
        evali(e.lhs) or evali(e.rhs)
      of LogicalAnd:
        evali(e.lhs) && evali(e.rhs)
      of LogicalOr:
        evali(e.lhs) || evali(e.rhs)
      else:
        eval_error("cannot eval constant-expression: " & $e)
        intmax_t(0)
  of EUnary:
    case e.uop:
    of Pos:
      evali(e.uoperand)
    of UNeg:
      cast[intmax_t](my_UNEG(cast[uintmax_t](evali(e.uoperand))))
    of SNeg:
      my_SNEG(evali(e.uoperand))
    of LogicalNot:
      ! evali(e.uoperand)
    of Not:
      not evali(e.uoperand)
    else:
      eval_error("cannot eval constant-expression: bad unary operator: " & $e)
      intmax_t(0)
  of EIntLit:
    e.ival
  of EFloatLit:
    eval_error("floating constant in constant-expression")
    intmax_t(0)
  of ECondition:
    if evali(e.cond) != 0: evali(e.cleft) else: evali(e.cright)
  else:
    eval_error("cannot eval constant-expression: " & $e)
    intmax_t(0)
