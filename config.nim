## config.nim - default configs for cc

const
  cc_version {.strdefine.} = gorge("git describe --tags --abbrev=0")
  DWARF_VERSION {.intdefine.} = 5
  cc_version_full {.strdefine.} = "cc version " & cc_version & "(https://github.com/ianfun/cc.git)"
  CC_LONG64 {.booldefine.} = not defined(windows) ## true if long is 64 bit
  CC_WCHAR32 {.booldefine.} = not defined(windows) ## true if wchar is 32 bit
  CC_NO_RAEADLINE {.booldefine.} = false
  USE_SetConsoleTextAttribute {.booldefine.} = defined(windows)
  STREAM_BUFFER_SIZE {.intdefine.} = 8192 ## 8 KB
  STDIN_PROMPT {.strdefine.} = ">>> "
