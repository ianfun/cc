import ast

type
  Location* = object
    line*: int
    col*: int
  Info* = ref object
    tag*: uint32
    loc*: Location
    ty*: CType

proc `$`*(loc: Location): string =
  $loc.line & ':' & $loc.col
