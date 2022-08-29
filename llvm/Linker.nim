proc linkModules2*(dest: ModuleRef; src: ModuleRef): Bool {.
    importc: "LLVMLinkModules2".}
