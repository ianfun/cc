import std/winlean

type CONSOLE_SCREEN_BUFFER_INFO* {.importc, nodecl, header: "windows.h".} = object
    wAttributes*: DWORD

let 
  FOREGROUND_BLUE *{.importc, nodecl.}: DWORD
  FOREGROUND_GREEN* {.importc, nodecl.}: DWORD
  FOREGROUND_RED* {.importc, nodecl.}: DWORD
  FOREGROUND_INTENSITY* {.importc, nodecl.}: DWORD
  BACKGROUND_BLUE* {.importc, nodecl.}: DWORD
  BACKGROUND_GREEN* {.importc, nodecl.}: DWORD
  BACKGROUND_RED* {.importc, nodecl.}: DWORD
  BACKGROUND_INTENSITY* {.importc, nodecl.}: DWORD
  COMMON_LVB_LEADING_BYTE* {.importc, nodecl.}: DWORD
  COMMON_LVB_TRAILING_BYTE* {.importc, nodecl.}: DWORD
  COMMON_LVB_GRID_HORIZONTAL* {.importc, nodecl.}: DWORD
  COMMON_LVB_GRID_LVERTICAL* {.importc, nodecl.}: DWORD
  COMMON_LVB_GRID_RVERTICAL* {.importc, nodecl.}: DWORD
  COMMON_LVB_REVERSE_VIDEO* {.importc, nodecl.}: DWORD
  COMMON_LVB_UNDERSCORE* {.importc, nodecl.}: DWORD
  STD_ERROR_HANDLE* {.importc, nodecl.}: DWORD

proc GetStdHandle*(nStdHandle: DWORD): HANDLE {.importc, nodecl, header: "windows.h".}

proc SetConsoleTextAttribute*(hConsoleOutput: HANDLE, dwAttributes: DWORD): WINBOOL {.importc, nodecl, header: "windows.h".}

proc GetConsoleScreenBufferInfo*(hConsoleOutput: HANDLE, lpConsoleScreenBufferInfo: ptr CONSOLE_SCREEN_BUFFER_INFO): WINBOOL {.importc, nodecl, header: "windows.h".}

export HANDLE
