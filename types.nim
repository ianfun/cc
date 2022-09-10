## types.nim - definition of C's Types

import config
import std/[macrocache, strutils]

type
    CTypeSpec* = enum
      TYPRIM,
      TYPOINTER,
      TYSTRUCT,
      TYUNION,
      TYENUM,
      TYBITFIELD,
      TYARRAY,
      TYFUNCTION,
      TYINCOMPLETE
    CType* = ref object
      tags*: uint32
      align*: uint32 # zero if use default (not specified)
      case spec*: CTypeSpec
      of TYPRIM:
        discard
      of TYPOINTER:
        p*: CType
      of TYSTRUCT, TYUNION:
        sname*: string
        selems*: seq[(string, CType)]
        packed*: bool
      of TYENUM:
        ename*: string
        eelems*: seq[(string, intmax_t)]
      of TYBITFIELD:
        bittype*: CType
        bitsize*: intmax_t
      of TYFUNCTION:
        fname*: string
        ret*: CType
        params*: seq[(string, CType)]
      of TYARRAY:
        arrsize*: intmax_t
        arrtype*: CType
        hassize*: bool
      of TYINCOMPLETE:
        tag*: CTypeSpec
        name*: string


const tyCounter = CacheCounter"tyCounter"

proc make_ty(): uint32 =
  result = 1'u32 shl tyCounter.value
  tyCounter.inc()

const ## optional type tags
  TYINVALID* = 0'u32
  # TYAUTO* = make_ty()
  TYCONST* = make_ty()
  TYRESTRICT* = make_ty()
  TYVOLATILE* = make_ty()
  TYATOMIC* = make_ty()
  TYINLINE* = make_ty()
  TYSTATIC* = make_ty()
  TYNORETURN* = make_ty()
  TYALIGNAS* = make_ty()
  TYEXTERN* = make_ty()
  TYREGISTER* = make_ty()
  TYTHREAD_LOCAL* = make_ty()
  TYTYPEDEF* = make_ty()
  TYLVALUE* = make_ty()

const ## basic types
  TYVOID* = make_ty()
  TYBOOL* = make_ty()
  TYCOMPLEX* = make_ty()
  TYINT8* = make_ty()
  TYINT16* = make_ty()
  TYINT32* = make_ty()
  TYINT64* = make_ty()
  TYUINT8* = make_ty()
  TYUINT16* = make_ty()
  TYUINT32* = make_ty()
  TYUINT64* = make_ty()
  TYFLOAT* = make_ty()
  TYDOUBLE* = make_ty()

const ## type alias
  TYCHAR* = TYINT8
  TYSHORT* = TYINT16
  TYWCHAR* = TYSHORT ## wchar type: may be 2 or 4 bytes ...
  TYINT* = TYINT32
  TYLONG* = TYINT64 ## 32 bit in MSVC, 64 bit in GCC/JVM
  TYLONGLONG* = TYINT64
  TYUCHAR* = TYUINT8
  TYUSHORT* = TYUINT16
  TYUINT* = TYUINT32
  TYULONG* = TYUINT64 ## 32 bit in MSVC, 64 bit in GCC/JVM
  TYULONGLONG* = TYUINT64
  TYLONGDOUBLE* = TYDOUBLE
  TYSIZE_T* = when sizeof(pointer) == 8: TYUINT64 else: TYUINT32

const 
  prim* = TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
    TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or 
    TYFLOAT or TYDOUBLE or
    TYBOOL
  signed*   = TYINT8  or TYINT16  or TYINT32  or TYINT64
  unsigned* = TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64

proc `$`*(a: CType, level=0): string

proc addTag*(a: uint32, dst: var string) =
# if bool(a and TYAUTO): dst.add("auto ")
  if bool(a and TYCONST): dst.add("const ")
  if bool(a and TYRESTRICT): dst.add("restrict ")
  if bool(a and TYVOLATILE): dst.add("volatile ")
  if bool(a and TYATOMIC): dst.add("_Atomic ")
  if bool(a and TYINLINE): dst.add("inline ")
  if bool(a and TYSTATIC): dst.add("static ")
  if bool(a and TYNORETURN): dst.add("_Noreturn ")
  if bool(a and TYALIGNAS): dst.add("_Alignas ")
  if bool(a and TYEXTERN): dst.add("extern ")
  if bool(a and TYREGISTER): dst.add("register ")
  if bool(a and TYTHREAD_LOCAL): dst.add("_Thread_local ")
  if bool(a and TYTYPEDEF): dst.add("typedef ")

proc joinShow2*[A, B](a: seq[(A, B)]): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i][1])
        result.add(' ')
        result.add($a[i][0])
        result.add(", ")
    result.add($a[^1][1] & ' ' & $a[^1][0])

proc joinShow4*(a: seq[(string, intmax_t)], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for (s, val) in a:
        result.add(padding & '\t')
        result.add(s)
        result.add('=')
        result.add($val)
        result.add(',')
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc joinShow3*(a: seq[(string, CType)], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for (s, ty) in a:
        result.add(padding & '\t')
        result.add(`$`(ty, level=level + 1))
        result.add(' ')
        result.add(s)
        result.add(';')
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc `$`*(a: CType, level=0): string =
  if a == nil:
    return "<nil>"
  result = ""
  case a.spec:
  of TYINCOMPLETE:
    let s = if a.tag == TYSTRUCT:  "struct" else: (if a.tag == TYUNION: "union" else: "enum")
    return s & ' ' & a.name & " <incomplete-type>"
  of TYPRIM: # format style: const int
      addTag(a.tags, result)
      let s = (
        if bool(a.tags and TYVOID): "void"
        elif bool(a.tags and TYBOOL): "_Bool"
        elif bool(a.tags and TYCOMPLEX): "_Complex"
        elif bool(a.tags and TYINT8): "char"
        elif bool(a.tags and TYINT16): "short"
        elif bool(a.tags and TYINT32): "int"
        elif bool(a.tags and TYINT64): "long long"
        elif bool(a.tags and TYUINT8): "unsigned char"
        elif bool(a.tags and TYUINT16): "unsigned short"
        elif bool(a.tags and TYUINT32): "unsigned int"
        elif bool(a.tags and TYUINT64): "unsigned long long"
        elif bool(a.tags and TYFLOAT): "float"
        elif bool(a.tags and TYDOUBLE): "double"
        else: ""
      )
      if s.len > 0:
        result.add(s)
  of TYENUM:
    result.add("enum " & a.ename & ' ' & joinShow4(a.eelems, level + 1))
  of TYFUNCTION:
    result.add($a.ret & a.fname & " (" & joinShow2(a.params) & ')')
  of TYSTRUCT:
    result.add("struct " & a.sname & ' ' & joinShow3(a.selems, level + 1))
  of TYUNION:
    result.add("union " & a.sname & ' ' & joinShow3(a.selems, level + 1))
  of TYBITFIELD:
    result.add($a.bittype & " : " &  $a.bitsize)
  of TYPOINTER: # format style: (int *const)
    result.add('(')
    result.add($a.p & '*')
    addTag(a.tags, result)
    result.add(')')
  of TYARRAY:
    result.add($a.arrtype & " [" & (if a.arrsize < 0: "*" else: $a.arrsize) & "]")

proc make_primitive(tag: uint32): CType =
  CType(tags: tag, spec: TYPRIM)

let
  b = make_primitive(TYBOOL)
  v = make_primitive(TYVOID)
  i8 = make_primitive(TYINT8)
  u8 = make_primitive(TYUINT8)
  i16 = make_primitive(TYINT16)
  u16 = make_primitive(TYINT16)
  i32 = make_primitive(TYINT32)
  u32 = make_primitive(TYUINT32)
  i64 = make_primitive(TYINT64)
  u64 = make_primitive(TYUINT64)
  ff = make_primitive(TYFLOAT)
  fd = make_primitive(TYDOUBLE)

template get*(a: uint32): CType =
  case a:
  of TYVOID: v
  of TYBOOL: b
  of TYINT8: i8
  of TYUINT8: u8
  of TYINT16: i16
  of TYUINT16: u16
  of TYINT32: i32
  of TYUINT32: u32
  of TYINT64: i64
  of TYUINT64: u64
  of TYFLOAT: ff
  of TYDOUBLE: fd
  else:
    unreachable()
    nil

proc getCharType*(): CType = get(TYCHAR)

proc getIntType*(): CType = get(TYINT)

proc getBoolType*(): CType = get(TYBOOL)

proc getInt8Type*(): CType = get(TYINT8)

proc getUInt8Type*(): CType = get(TYUINT8)

proc getInt16Type*(): CType = get(TYINT16)

proc getUInt16Type*(): CType = get(TYUINT16)

proc getInt32Type*(): CType = get(TYINT32)

proc getUInt32Type*(): CType = get(TYUINT32)

proc getInt64Type*(): CType = get(TYINT64)

proc getUInt64Type*(): CType = get(TYUINT64)

proc getLongType*(): CType = get(TYLONG)

proc getULongType*(): CType = get(TYULONG)

proc getLongLongType*(): CType = get(TYLONGLONG)

proc getULongLongType*(): CType = get(TYULONGLONG)

proc getSizetType*(): CType = get(TYSIZE_T)

proc getUIntType*(): CType = get(TYUINT)

proc getShortType*(): CType = get(TYSHORT)

proc getUShortType*(): CType = get(TYUSHORT)

proc getFloatType*(): CType = get(TYFLOAT)

proc getDoubleType*(): CType = get(TYDOUBLE)

proc getVoidType*(): CType = get(TYVOID)

proc getPointerType*(base: CType): CType = CType(tags: TYINVALID, spec: TYPOINTER, p: base)
