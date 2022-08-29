type
  RemarkStringRef* = ptr remarkOpaqueString

proc remarkStringGetData*(string: RemarkStringRef): cstring {.
    importc: "LLVMRemarkStringGetData".}

proc remarkStringGetLen*(string: RemarkStringRef): uint32T {.
    importc: "LLVMRemarkStringGetLen".}


type
  RemarkDebugLocRef* = ptr remarkOpaqueDebugLoc


proc remarkDebugLocGetSourceFilePath*(dl: RemarkDebugLocRef): RemarkStringRef {.
    importc: "LLVMRemarkDebugLocGetSourceFilePath".}

proc remarkDebugLocGetSourceLine*(dl: RemarkDebugLocRef): uint32T {.
    importc: "LLVMRemarkDebugLocGetSourceLine".}


proc remarkDebugLocGetSourceColumn*(dl: RemarkDebugLocRef): uint32T {.
    importc: "LLVMRemarkDebugLocGetSourceColumn".}


type
  RemarkArgRef* = ptr remarkOpaqueArg

proc remarkArgGetKey*(arg: RemarkArgRef): RemarkStringRef {.
    importc: "LLVMRemarkArgGetKey".}


proc remarkArgGetValue*(arg: RemarkArgRef): RemarkStringRef {.
    importc: "LLVMRemarkArgGetValue".}


proc remarkArgGetDebugLoc*(arg: RemarkArgRef): RemarkDebugLocRef {.
    importc: "LLVMRemarkArgGetDebugLoc".}


type
  RemarkEntryRef* = ptr remarkOpaqueEntry


proc remarkEntryDispose*(remark: RemarkEntryRef) {.
    importc: "LLVMRemarkEntryDispose".}

proc remarkEntryGetType*(remark: RemarkEntryRef): RemarkType {.
    importc: "LLVMRemarkEntryGetType".}

proc remarkEntryGetPassName*(remark: RemarkEntryRef): RemarkStringRef {.
    importc: "LLVMRemarkEntryGetPassName".}

proc remarkEntryGetRemarkName*(remark: RemarkEntryRef): RemarkStringRef {.
    importc: "LLVMRemarkEntryGetRemarkName".}


proc remarkEntryGetFunctionName*(remark: RemarkEntryRef): RemarkStringRef {.
    importc: "LLVMRemarkEntryGetFunctionName".}


proc remarkEntryGetDebugLoc*(remark: RemarkEntryRef): RemarkDebugLocRef {.
    importc: "LLVMRemarkEntryGetDebugLoc".}

proc remarkEntryGetHotness*(remark: RemarkEntryRef): uint64T {.
    importc: "LLVMRemarkEntryGetHotness".}

proc remarkEntryGetNumArgs*(remark: RemarkEntryRef): uint32T {.
    importc: "LLVMRemarkEntryGetNumArgs".}

proc remarkEntryGetFirstArg*(remark: RemarkEntryRef): RemarkArgRef {.
    importc: "LLVMRemarkEntryGetFirstArg".}


proc remarkEntryGetNextArg*(it: RemarkArgRef; remark: RemarkEntryRef): RemarkArgRef {.
    importc: "LLVMRemarkEntryGetNextArg".}
type
  RemarkParserRef* = ptr remarkOpaqueParser

proc remarkParserCreateYAML*(buf: pointer; size: uint64T): RemarkParserRef {.
    importc: "LLVMRemarkParserCreateYAML".}

proc remarkParserCreateBitstream*(buf: pointer; size: uint64T): RemarkParserRef {.
    importc: "LLVMRemarkParserCreateBitstream".}


proc remarkParserGetNext*(parser: RemarkParserRef): RemarkEntryRef {.
    importc: "LLVMRemarkParserGetNext".}

proc remarkParserHasError*(parser: RemarkParserRef): Bool {.
    importc: "LLVMRemarkParserHasError".}

proc remarkParserGetErrorMessage*(parser: RemarkParserRef): cstring {.
    importc: "LLVMRemarkParserGetErrorMessage".}

proc remarkParserDispose*(parser: RemarkParserRef) {.
    importc: "LLVMRemarkParserDispose".}


proc remarkVersion*(): uint32T {.importc: "LLVMRemarkVersion".}
