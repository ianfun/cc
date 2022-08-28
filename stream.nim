type
    Fd {.importc: "struct FILE*", nodecl, header: "stdio.h".} = pointer
    StreamType = enum
      FileStream, StringStream, StdinStream
    Stream* = ref object
      lastc: cint
      case k: StreamType
      of FileStream:
        fd: Fd
      of StringStream, StdinStream:
        s: string
        i: int

proc fgetc(stream: Fd): cint {.importc, header: "stdio.h", nodecl.}

proc fclose(stream: Fd): cint {.importc, header: "stdio.h", nodecl.}

proc fopen(filename, mode: cstring): Fd {.importc, header: "stdio.h", nodecl.}

let EOF {.importc: "EOF", nodecl.} : cint

proc newStringStream*(s: string): Stream =
    result = Stream(k: StringStream, s: s, i: 0, lastc: 256)

proc newFileStream*(path: string): Stream =
    let fd = fopen(path, "rb")
    if fd == nil:
        return nil
    result = Stream(k: FileStream, fd: fd, lastc: 256)

proc newFileStream*(fd: File): Stream =
    result = Stream(k: FileStream, fd: fd, lastc: 256)

proc newStdinStream*(): Stream =
    result = Stream(k: StdinStream, lastc: 256)

proc putc*(s: Stream, c: cint) =
    s.lastc = c

proc readChar*(s: Stream): char =
    if s.lastc <= 0xFF:
        result = char(s.lastc)
        s.lastc = 256
        return
    case s.k:
    of FileStream:
        let c = fgetc(s.fd)
        if c == EOF:
            return '\0'
        else:
            return char(c and 0xFF)
    of StringStream:
        if s.i >= len(s.s):
            return '\0'
        result = s.s[s.i]
        inc s.i
    of StdinStream:
        if s.i >= len(s.s):
            stderr.write(">>> ")
            try:
                s.s = stdin.readLine() & '\n'
            except IOError:
                return '\0'
            s.i = 0
        result = s.s[s.i]
        inc s.i


proc close*(s: Stream) =
    case s.k:
    of FileStream:
        discard fclose(s.fd)
    of StringStream, StdinStream:
        discard
