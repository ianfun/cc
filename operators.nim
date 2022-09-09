type
    PostfixOP* = enum
      PostfixIncrement="++", PostfixDecrement="--"
    UnaryOP* = enum
      Pos, # it like nop, but it will do integer promotion
      UNeg, SNeg, FNeg,
      Not, AddressOf,
      PrefixIncrement, PrefixDecrement, Dereference, LogicalNot
    BinOP* = enum
      Nop=0,
      # Arithmetic operators
      UAdd, SAdd, FAdd, 
      USub, SSub, FSub,
      UMul, SMul, FMul, # no SMul!
      UDiv, SDiv, FDiv, 
      URem, SRem, FRem,
      Shr, AShr, Shl, # or sar in GNU Assembly

      And, Xor, Or,

      LogicalAnd, LogicalOr, 
      Assign,
      SAddP,
      PtrDiff,
      Comma,

      # compare operators
      EQ, NE, 
      UGT, UGE, ULT, ULE, 
      SGT, SGE, SLT, SLE,
      # ordered float compare
      FEQ, FNE, 
      FGT, FGE, FLT, FLE
    CastOp* = enum
      Trunc,
      ZExt,
      SExt,
      FPToUI, 
      FPToSI, 
      UIToFP, 
      SIToFP, 
      FPTrunc,
      FPExt,
      PtrToInt, 
      IntToPtr, 
      BitCast

proc `$`*(o: UnaryOP): string =
  case o:
  of Pos:
    "+"
  of UNeg, SNeg, FNeg:
    "-"
  of Not:
    "!"
  of AddressOf:
    "&&"
  of PrefixIncrement:
    "++"
  of PrefixDecrement:
    "--"
  of Dereference:
    "*"
  of LogicalNot:
    "~"
proc `$`*(o: BinOP): string =
  case o:
  of Nop: "<nop>"
  of UAdd, SAdd, FAdd, SAddP: "+"
  of USub, SSub, FSub, PtrDiff: "-"
  of UMul, SMul, FMul: "*"
  of UDiv, SDiv, FDiv: "/"
  of URem, SRem, FRem: "%"
  of Shr, AShr: ">>"
  of Shl: "<<"
  of And: "&"
  of Xor: "^"
  of Or: "|"
  of LogicalAnd: "&&"
  of LogicalOr: "||"
  of Assign: "="
  of Comma: ","
  of EQ, FEQ: "=="
  of NE, FNE: "!="
  of UGT, SGT, FGT: ">"
  of UGE, SGE, FGE: ">="
  of ULT, SLT, FLT: "<"
  of ULE, SLE, FLE: "<="
