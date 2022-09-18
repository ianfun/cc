proc free*(p: pointer) {.importc: "free".}

proc readline*(prompt: cstring): cstring {.importc: "readline".}
