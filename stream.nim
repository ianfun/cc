## File IOs
##
## keep in mind
##  0: stdin
##  1: stdout
##  2: stderr
##
## BUFSIZ is 8192 in linux, 512 in windows!
##
## cc use GNU readline linary to read line from stdin.
##
## define `CC_NO_RAEADLINE` to use `stdin.readline` insteand of GNU readline

when not defined(CC_NO_RAEADLINE):
    import readLine/[readLine, history]

const
  STREAM_BUFFER_SIZE* = 8192 ## 8 KB
  STDIN_PROMPT: cstring = ">>> "

type
    Fd* = cint
    cssize_t*{.importc: "ssize_t", nodecl, header: "stdio.h".} = clonglong
    off_t *{.importc: "off_t ", nodecl, header: "unistd.h".} = clonglong
    BinaryBuffer = pointer | cstring | ptr

proc read(fd: Fd, buf: BinaryBuffer, count: csize_t): cssize_t {.importc: "read", header: "unistd.h".}

proc write(fd: Fd, buf: BinaryBuffer, count: csize_t): cssize_t {.importc: "write", header: "unistd.h".}

proc open(pathname: cstring, flags: cint): cint {.importc: "open", nodecl, header: "fcntl.h".}

proc close(fd: cint): cint {.importc: "close", nodecl, header: "unistd.h".}

proc lseek(fd: Fd, offset: off_t , whence: int): off_t {.importc: "lseek", nodecl, header: "unistd.h".}

let O_RDONLY* {.importc: "O_RDONLY", nodecl, header: "unistd.h".}: cint

const
  fstdin*: cint = 0
  fstdout*: cint = 1
  fstderr*: cint = 2

type
    StreamType* = enum
      FileStream, StringStream, StdinStream
    Stream* = ref object
      lastc*: cint
      case k: StreamType
      of FileStream:
        buf*: pointer
        pos*: uint
        p*: uint
        fd*: Fd
      of StringStream, StdinStream:
        s*: string
        i*: int
    fpos_t* {.importc: "fpos_t", nodecl, header: "stdio.h".} = object

proc `<<`*(stream: Fd, msg: string) =
    discard write(stream, cstring(msg), msg.len.csize_t)

proc `<<`*(stream: Fd, msg: cstring) =
    discard write(stream, msg, msg.len.csize_t)

proc `<<`*(stream: Fd, c: char) =
    var a = c
    discard write(stream, addr a, 1)

proc `<<`*(stream: Fd, c: SomeInteger) =
    var a = cast[char](c)
    discard write(stream, addr a, 1)

proc `<<`*(stream: Fd, b: bool) =
    stream << (if b: "true" else: "false")

proc `<<<`*(stream: Fd, msg: auto) =
    stream << msg
    stream << '\n'

let 
  EOF* {.importc: "EOF", nodecl.} : cint
  SEEK_CUR* {.importc: "SEEK_CUR", nodecl.}: cint
  SEEK_END* {.importc: "SEEK_END", nodecl.}: cint
  SEEK_SET* {.importc: "SEEK_SET", nodecl.}: cint

proc printSourceLine*(s: Stream, line: int) {.raises: [].} =
    case s.k:
    of StdinStream, StringStream:
        discard
    of FileStream:
        var old = lseek(s.fd, 0, SEEK_CUR)
        if old > 0 and lseek(s.fd, -2, SEEK_CUR) == 0:
            var c: char
            let t = read(s.fd, addr c, 1)
            if t < 0:
                return
            if (c == '\n' or c == '\r') and lseek(s.fd, -2, SEEK_CUR) == 0:
                while true:
                    var c: char
                    let ok = read(s.fd, addr c, 1)
                    if ok < 0 or c == '\n' or c == '\r':
                        break
                    if lseek(s.fd, -2, SEEK_CUR) < 0:
                        break
            var off = 0
            while true:
                var c: char
                let t = read(s.fd, addr c, 1)
                if t < 0 or c == '\n' or c == '\r':
                    break
                if lseek(s.fd, -2, SEEK_CUR) != 0:
                    discard lseek(s.fd, -1, SEEK_CUR)
                    break
                inc off
            var f = $line
            for i in 0..<(6-len(f)):
                fstderr << ' '
            fstderr << f
            fstderr << " | "
            while true:
                var c: char
                let t = read(s.fd, addr c, 1)
                if t < 0 or c == '\n' or c == '\r':
                    break
                fstderr << c
            fstderr << "\n       | "
            for i in 0..<off:
                fstderr << '~'
            fstderr << "\e[32m^\e[0m\n"
            discard lseek(s.fd, old, SEEK_SET)

proc newStringStream*(s: string): Stream {.raises: [].} =
    result = Stream(k: StringStream, s: s, i: 0, lastc: 256)

proc newFileStream*(path: string): Stream {.raises: [].} =
    let fd = open(path, O_RDONLY)
    if fd < 0:
        return nil
    result = Stream(k: FileStream, fd: fd, lastc: 256, buf: alloc(STREAM_BUFFER_SIZE))

proc newStdinStream*(): Stream {.raises: [].} =
    result = Stream(k: StdinStream, lastc: 256)

proc putc*(s: Stream, c: cint) {.raises: [].} =
    s.lastc = c

proc readChar*(s: Stream): char {.raises: [].} =
    ## try to read from stream, if any error happens, return '\0'(EOF)
    if s.lastc == EOF:
        result = char(s.lastc)
        s.lastc = 256
        return
    case s.k:
    of FileStream:
        if s.pos == 0:
            let status = read(s.fd, s.buf, STREAM_BUFFER_SIZE)
            if status <= 0:
                return '\0'
            s.pos = uint(status)
            s.p = 0
        let str = cast[cstring](s.buf)
        let c = str[s.p]
        inc s.p
        dec s.pos
        return c
    of StringStream:
        if s.i >= len(s.s):
            return '\0'
        result = s.s[s.i]
        inc s.i
    of StdinStream:
        if s.i >= len(s.s):
            when defined(CC_NO_RAEADLINE):
                fstdout << STDIN_PROMPT
                try:
                    s.s = stdin.readLine()
                except EOFError, IOError:
                    return '\0'
            else:
                var line = readLine(STDIN_PROMPT)
                if line == nil:
                    return '\0'
                s.s = $line
                add_history(line)
                free(line)
                s.s.add('\n')
            s.i = 0
        result = s.s[s.i]
        inc s.i

proc close*(s: Stream) {.raises: [].} =
    case s.k:
    of FileStream:
        if s.buf != nil:
            dealloc(s.buf)
        discard close(s.fd)
    of StringStream, StdinStream:
        discard
