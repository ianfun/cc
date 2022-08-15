import token

proc eval_const_expression*(e: Expr): int =
  1

proc eval_const_expression_bool*(e: Expr): bool =
  true

#[
proc eval*(e: Expr): CValue {.raises: [EvalError].} =
  case e.k:
  of EBin:
    case e.bop:
    of OAddition:
      
    of OOSubtraction:

  of EPostFix:
    $e.operand & $e.uop
  of EUnary:
    $e.uop & $e.operand
  of ECharLit:
    show(char(e.ival))
  of EIntLit:
    $e.ival
  of EFloatLit:
    $e.fval
  of EStringLit, EVar:
    e.sval
  of ECondition:
    $e.cond & '?' & $e.cleft & ':' & $e.cright
  of EAlignof:
    "_Alignof(" & $e.sizeofx & ')'
  of ESizeOf:
    "sizeof(" & $e.sizeofx & ')'
  of ECast:
    $e.casttype & '(' & $e.castval & ')'
  of ESubscript:
    $e.left & '[' & $e.right & ']'
  of ECall:
    $e.left & '(' & $e.right & ')'
  of EGeneric:
    "_Generic(" & $e.selectexpr & (var s: string;for (tp, e) in e.selectors: s.add($tp & ':' & $e & ',');s) & ')'

]#
