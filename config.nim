import token

type
  intmax_t* = int64
  uintmax_t* = uint64
  Codepoint* = uint32

const 
  INFO_USED* = 2

proc builtin_unreachable() {.importc: "__builtin_unreachable", nodecl.}

proc unreachable*() =
  # https://learn.microsoft.com/en-us/cpp/intrinsics/assume?view=msvc-170
  # https://clang.llvm.org/docs/LanguageExtensions.html#builtin-assume
  # https://stackoverflow.com/questions/63493968/reproducing-clangs-builtin-assume-for-gcc
  when defined(debug):
    assert false, "INTERNAL ERROR: unreachable executed!"
  else:
    builtin_unreachable()

proc cc_exit*(c: auto) =
  quit c

iterator getDefines*(): (string, seq[TokenV]) =
  proc str(s: string): TokenV =
    TokenV(tok: TStringLit, tags: TVSVal, s: s)
  proc num(s: string): TokenV =
    TokenV(tok: TPPNumber, tags: TVSVal, s: s)
  proc space(): TokenV = 
    TokenV(tok: TSpace, tags: TVNormal)
  proc empty(): seq[TokenV] =
    discard
  yield ("__STDC__", @[num("1")])
  yield ("__STDC_VERSION__", @[num("201710L")])
  yield ("__STDC_HOSTED__", @[num("1")])
#  yield ("__STDC_NO_THREADS__", @[num("1")])
#  yield ("__STDC_NO_ATOMICS__", @[num("1")])
  yield ("__STDC_UTF_16__", @[num("1")])
  yield ("__STDC_UTF_32__", @[num("1")])
  yield ("__SIZE_TYPE__", @[str("size_t")])
  yield ("__INT8_TYPE__", @[str("__int8")])
  yield ("__INT16_TYPE__", @[str("__int16")])
  yield ("__INT32_TYPE__", @[str("__int32")])
  yield ("__INT64_TYPE__", @[str("__int64")])
  yield ("__INT_FAST8_TYPE__", @[str("__int8")])
  yield ("__INT_FAST16_TYPE__", @[str("__int16")])
  yield ("__INT_FAST32_TYPE__", @[str("__int32")])
  yield ("__INT_FAST64_TYPE__", @[str("__int64")])
  yield ("__UINT_FAST8_TYPE__", @[str("unsigned"), space(), str("__int8")])
  yield ("__UINT_FAST16_TYPE__", @[str("unsigned"), space(), str("__int16")])
  yield ("__UINT_FAST32_TYPE__", @[str("unsigned"), space(), str("__int32")])
  yield ("__UINT_FAST64_TYPE__", @[str("unsigned"), space(), str("__int64")])
  yield ("__INTPTR_TYPE__", @[str("long"), space(), str("long"), space(), str("int")])
  yield ("__UINTPTR_TYPE__", @[str("unsigned"), space(),str("long"), space(), str("long"), space(), str("int")])
  yield ("__CHAR_BIT__", @[num("8")])
  case hostOS:
  of "windows":
    yield ("_WIN32", empty())
    yield ("WIN32", empty())
    if sizeof(cstring) == 8:
      yield ("WIN64", empty())
      yield ("_WIN64", empty())
  of "macosx":
    yield ("__APPLE__", empty())
  of "linux":
    yield ("__linux__", empty())
    yield ("__linux", empty())
  of "netbsd":
    yield ("__NetBSD__", empty())
  of "freebsd":
    yield ("__FreeBSD__", empty())

proc unsafe_utf8_codepoint*(s: cstring): (Codepoint, int) =
    if 0xf0 == (0xf8 and s[0].Codepoint): # 4 byte
      (
        ((0x07 and s[0].Codepoint) shl 18) or 
        ((0x3f and s[1].Codepoint) shl 12) or
        ((0x3f and s[2].Codepoint) shl 6) or
         (0x3f and s[3].Codepoint),
      4)
    elif 0xe0 == (0xf0 and s[0].Codepoint): # 3 byte
      ( 
        ((0x0f and s[0].Codepoint) shl 12) or 
        ((0x3f and s[1].Codepoint) shl 6) or 
         (0x3f and s[2].Codepoint),
      3)
    elif 0xc0 == (0xe0 and s[0].Codepoint): # 2 byte
      (
        ((0x1f and s[0].Codepoint) shl 6) or 
         (0x3f and s[1].Codepoint),
      2)
    else: # 1 byte
      (s[0].Codepoint, 1)

proc writeUTF8toUTF32*(s: string): seq[Codepoint] =
  # TODO: reserve 3 more bytes to prevent bad utf8 terminate access overflow
  var i = 0
  while true:
    if i >= len(s):
      break
    let (codepoint, length) = unsafe_utf8_codepoint(cast[cstring](cast[int](cstring(s)) + i))
    result.add(codepoint)
    i += length

proc writeUTF8toUTF16*(s: string): seq[uint16] =
  let u32 = writeUTF8toUTF32(s)
  result = newSeqOfCap[uint16](len(u32) div 2)
  for codepoint in u32:
    var c = codepoint
    if c <= 0xFFFF:
      result.add(uint16(c))
    else:
      c -= 0x10000
      result.add(uint16(0xD800 + (c shr 10)))
      result.add(uint16(0xDC00 + (c and 0x3FF)))

proc show*(c: char): string =
  let n = int(c)
  if n >= 33 and n <= 126:
    result = "'"
    result.add(c)
    result.add('\'')
  else:
    result = "<"
    result.addInt(n)
    result.add('>')
