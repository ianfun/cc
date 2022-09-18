let EOF* {.importc: "EOF", header: "stdio.h", nodecl.}: cint

proc free*(p: pointer) {.importc: "free", nodecl, header: "stdlib.h".}

proc readline*(prompt: cstring): cstring {.importc: "readline", header: "readline/readline.h".}
