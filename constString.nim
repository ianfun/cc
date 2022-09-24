## A const, immutable string, but has length
##
## it can be used as UTF-8 encoded string, or byte-string
##
## it is **StringRef** in LLVM


type
    ConstString* = object
      len*: int
      str*: cstring

proc constStr*(s: cstring | string): ConstString = ConstString(len: len(s), str: s)

proc `$`*(self: ConstString): string = $self.str

proc `&`*(s: string, c: ConstString): string =
    s & $c.str

proc `[]`*(self: ConstString, idx: auto): char =
    assert idx < self.len, "index too large for const string!"
    self.str[idx]

proc hash*(self: ConstString): uint64 =
    result = 5381
    for i in 0 ..< self.len:
        result = ((result shl 5) + result) + uint64(self.str[i])

proc len*(self: ConstString): int = self.len

proc `==`*(a: ConstString, b: string): bool =
    if len(a) != len(b):
        return false
    for i in 0 ..< len(a):
        if a.str[i] != b[i]:
            return false
    return true

proc `==`*(a: string, b: ConstString): bool =
    if len(a) != len(b):
        return false
    for i in 0 ..< len(a):
        if a[i] != b.str[i]:
            return false
    return true

proc `==`*(a, b: ConstString): bool =
    if a.len != b.len:
        return false
    for i in 0 ..< a.len:
        if a.str[i] != b.str[i]:
            return false
    return true
