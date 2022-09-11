type
  DisasmContextRef* = pointer
  OpInfoCallback* = proc (disInfo: pointer; pc: uint64T; offset: uint64T;
                       opSize: uint64T; instSize: uint64T; tagType: cint;
                       tagBuf: pointer): cint

type
  OpInfoSymbol1* {.bycopy.} = object
    present*: uint64T 
    name*: cstring    
    value*: uint64T   

  OpInfo1* {.bycopy.} = object
    addSymbol*: OpInfoSymbol1
    subtractSymbol*: OpInfoSymbol1
    value*: uint64T
    variantKind*: uint64T


const
  DisassemblerVariantKindNone* = 0


const
  DisassemblerVariantKindARM_HI16* = 1
  DisassemblerVariantKindARM_LO16* = 2


const
  DisassemblerVariantKindARM64PAGE* = 1
  DisassemblerVariantKindARM64PAGEOFF* = 2
  DisassemblerVariantKindARM64GOTPAGE* = 3
  DisassemblerVariantKindARM64GOTPAGEOFF* = 4
  DisassemblerVariantKindARM64TLVP* = 5
  DisassemblerVariantKindARM64TLVOFF* = 6


type
  SymbolLookupCallback* = proc (disInfo: pointer; referenceValue: uint64T;
                             referenceType: ptr uint64T; referencePC: uint64T;
                             referenceName: cstringArray): cstring


const
  DisassemblerReferenceTypeInOutNone* = 0
  DisassemblerReferenceTypeInBranch* = 1
  DisassemblerReferenceTypeInPCrelLoad* = 2
  DisassemblerReferenceTypeInARM64ADRP* = 0x100000001'i64
  DisassemblerReferenceTypeInARM64ADDXri* = 0x100000002'i64
  DisassemblerReferenceTypeInARM64LDRXui* = 0x100000003'i64
  DisassemblerReferenceTypeInARM64LDRXl* = 0x100000004'i64
  DisassemblerReferenceTypeInARM64ADR* = 0x100000005'i64
  DisassemblerReferenceTypeOutSymbolStub* = 1
  DisassemblerReferenceTypeOutLitPoolSymAddr* = 2
  DisassemblerReferenceTypeOutLitPoolCstrAddr* = 3
  DisassemblerReferenceTypeOutObjcCFStringRef* = 4
  DisassemblerReferenceTypeOutObjcMessage* = 5
  DisassemblerReferenceTypeOutObjcMessageRef* = 6
  DisassemblerReferenceTypeOutObjcSelectorRef* = 7
  DisassemblerReferenceTypeOutObjcClassRef* = 8
  DisassemblerReferenceTypeDeMangledName* = 9
