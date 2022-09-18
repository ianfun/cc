let EOF2* {.importc: "EOF", header: "stdio.h", nodecl.}: cint

proc add_history*(s: cstring) {.importc: "add_history", header: "readline/history.h".}
