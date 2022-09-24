## Message Writter - write frontend messages

import stream, constString

type
  MessageType* = enum
    MRaw, MNote, MVerbose, MError, MTypeError, MEvalError, MParseError, Mperror
  MessageWriter* = ref object of RootObj
  ConsoleColoredWriter* = ref object of MessageWriter
    fd*: Fd
  JSONWriter* = ref object of MessageWriter

method write*(m: MessageWriter, s: cstring, t: MessageType) {.base.} =
  discard

method write*(m: MessageWriter, s: string, t: MessageType) {.base.} =
  discard

method write*(m: MessageWriter, s: ConstString, t: MessageType) {.base.} =
  discard

proc newConsoleColoredWritter*(fd: Fd = fstderr): ConsoleColoredWriter = 
  ConsoleColoredWriter(fd: fd)

proc newJSONWritter*(): JSONWriter = 
  JSONWriter()

