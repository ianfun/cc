type
    Fd* {.importc: "struct FILE*", nodecl, header: "stdio.h".} = pointer
    StreamType* = enum
      FileStream, StringStream, StdinStream
    Stream* = ref object
      lastc*: cint
      case k: StreamType
      of FileStream:
        fd*: Fd
      of StringStream, StdinStream:
        s*: string
        i*: int
    fpos_t* {.importc: "fpos_t", nodecl, header: "stdio.h".} = object

proc fputs*(str: cstring, stream: Fd): cint {.importc, header: "stdio.h", nodecl.}

proc fputc*(c: cint, stream: Fd): cint {.importc, header: "stdio.h", nodecl.}

proc fgetc*(stream: Fd): cint {.importc, header: "stdio.h", nodecl.}

proc fclose*(stream: Fd): cint {.importc, header: "stdio.h", nodecl.}

proc fopen*(filename, mode: cstring): Fd {.importc, header: "stdio.h", nodecl.}

proc fgetpos*(stream: Fd, pos: ptr fpos_t): cint {.importc, header: "stdio.h", nodecl.}

proc fsetpos*(stream: Fd, pos: ptr fpos_t): cint {.importc, header: "stdio.h", nodecl.}

proc fseek*(stream: Fd, offset: clong, origin: cint): cint {.importc, header: "stdio.h", nodecl.}

proc ftell*(stream: Fd): clong {.importc, header: "stdio.h", nodecl.}

proc `<<`*(stream: Fd, msg: string) =
    discard fputs(msg, stream)

proc `<<`*(stream: Fd, msg: cstring) =
    discard fputs(msg, stream)

proc `<<`*(stream: Fd, c: char) =
    discard fputc(c.cint, stream)

proc `<<`*(stream: Fd, c: SomeInteger) =
    discard fputc(c.cint, stream)

proc `<<`*(stream: Fd, b: bool) =
    stream << (if b: cstring("true") else: cstring("false"))

proc `<<<`*(stream: Fd, msg: string) =
    stream << msg
    stream << '\n'

proc `<<<`*(stream: Fd, msg: cstring) =
    stream << msg
    stream << '\n'

let 
  EOF* {.importc: "EOF", nodecl.} : cint
  SEEK_CUR* {.importc: "SEEK_CUR", nodecl.}: cint
  SEEK_END* {.importc: "SEEK_END", nodecl.}: cint
  SEEK_SET* {.importc: "SEEK_SET", nodecl.}: cint
  cstderr* {.importc: "stderr", nodecl.}: Fd

proc printSourceLine*(s: Stream, line: int) {.raises: [].} =
    case s.k:
    of StdinStream, StringStream:
        discard
    of FileStream:
        var old: fpos_t
        if fgetpos(s.fd, addr old) == 0 and fseek(s.fd, -2, SEEK_CUR) == 0:
            var t = fgetc(s.fd)
            if (t == '\n'.cint or t == '\r'.cint) and fseek(s.fd, -2, SEEK_CUR) == 0:
                while true:
                    let c = fgetc(s.fd)
                    if c == EOF or c == '\n'.cint or c == '\r'.cint:
                        break
                    if fseek(s.fd, -2, SEEK_CUR) != 0:
                        break
            var off = 0
            while true:
                let c = fgetc(s.fd)
                if c == EOF or c == '\n'.cint or c == '\r'.cint:
                    break
                if fseek(s.fd, -2, SEEK_CUR) != 0:
                    discard fseek(s.fd, -1, SEEK_CUR)
                    break
                inc off
            var f = $line
            for i in 0..<(6-len(f)):
                cstderr << ' '
            cstderr << f
            cstderr << " | "
            while true:
                let c = fgetc(s.fd)
                if c == EOF or c == '\n'.cint or c == '\r'.cint:
                    break
                stderr << c
            cstderr << "\n       | "
            for i in 0..<off:
                stderr << '~'
            stderr << "\e[32m^\e[0m\n"
            discard fsetpos(s.fd, addr old)

proc newStringStream*(s: string): Stream {.raises: [].} =
    result = Stream(k: StringStream, s: s, i: 0, lastc: 256)

proc newFileStream*(path: string): Stream {.raises: [].} =
    let fd = fopen(path, "rb")
    if fd == nil:
        return nil
    result = Stream(k: FileStream, fd: fd, lastc: 256)

proc newFileStream*(fd: File): Stream {.raises: [].} =
    result = Stream(k: FileStream, fd: fd, lastc: 256)

proc newStdinStream*(): Stream {.raises: [].} =
    result = Stream(k: StdinStream, lastc: 256)

proc putc*(s: Stream, c: cint) {.raises: [].} =
    s.lastc = c

proc readChar*(s: Stream): char {.raises: [].} =
    if s.lastc == EOF:
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
            cstderr << ">>> "
            try:
                s.s = stdin.readLine() & '\n'
            except IOError:
                return '\0'
            s.i = 0
        result = s.s[s.i]
        inc s.i

proc close*(s: Stream) {.raises: [].} =
    case s.k:
    of FileStream:
        discard fclose(s.fd)
    of StringStream, StdinStream:
        discard
