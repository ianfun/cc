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
      SAddP, # num add/sub pointer 
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
