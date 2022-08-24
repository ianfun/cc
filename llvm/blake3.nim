## ===-- llvm-c/blake3.h - BLAKE3 C Interface ----------------------*- C -*-===*\
## |*                                                                            *|
## |* Released into the public domain with CC0 1.0                               *|
## |* See 'llvm/lib/Support/BLAKE3/LICENSE' for info.                            *|
## |* SPDX-License-Identifier: CC0-1.0                                           *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to LLVM's BLAKE3 implementation.      *|
## |* Original BLAKE3 C API: https://github.com/BLAKE3-team/BLAKE3/tree/1.3.1/c  *|
## |*                                                                            *|
## |* Symbols are prefixed with 'llvm' to avoid a potential conflict with        *|
## |* another BLAKE3 version within the same program.                            *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

const
  _BLAKE3_VERSION_STRING* = "1.3.1"
  _BLAKE3_KEY_LEN* = 32
  _BLAKE3_OUT_LEN* = 32
  _BLAKE3_BLOCK_LEN* = 64
  _BLAKE3_CHUNK_LEN* = 1024
  _BLAKE3_MAX_DEPTH* = 54

##  This struct is a private implementation detail. It has to be here because
##  it's part of llvm_blake3_hasher below.

type
  LlvmBlake3ChunkState* {.bycopy.} = object
    cv*: array[8, uint32T]
    chunkCounter*: uint64T
    buf*: array[blake3Block_Len, uint8T]
    bufLen*: uint8T
    blocksCompressed*: uint8T
    flags*: uint8T

  LlvmBlake3Hasher* {.bycopy.} = object
    key*: array[8, uint32T]
    chunk*: LlvmBlake3ChunkState
    cvStackLen*: uint8T ##  The stack size is MAX_DEPTH + 1 because we do lazy merging. For example,
                      ##  with 7 chunks, we have 3 entries in the stack. Adding an 8th chunk
                      ##  requires a 4th entry, rather than merging everything down to 1, because we
                      ##  don't know whether more input is coming. This is different from how the
                      ##  reference implementation does things.
    cvStack*: array[(blake3Max_Depth + 1) * blake3Out_Len, uint8T]


proc llvmBlake3Version*(): cstring {.importc: "llvm_blake3_version".}
proc llvmBlake3HasherInit*(self: ptr LlvmBlake3Hasher) {.
    importc: "llvm_blake3_hasher_init".}
proc llvmBlake3HasherInitKeyed*(self: ptr LlvmBlake3Hasher;
                               key: array[blake3Key_Len, uint8T]) {.
    importc: "llvm_blake3_hasher_init_keyed".}
proc llvmBlake3HasherInitDeriveKey*(self: ptr LlvmBlake3Hasher; context: cstring) {.
    importc: "llvm_blake3_hasher_init_derive_key".}
proc llvmBlake3HasherInitDeriveKeyRaw*(self: ptr LlvmBlake3Hasher; context: pointer;
                                      contextLen: csize_t) {.
    importc: "llvm_blake3_hasher_init_derive_key_raw".}
proc llvmBlake3HasherUpdate*(self: ptr LlvmBlake3Hasher; input: pointer;
                            inputLen: csize_t) {.
    importc: "llvm_blake3_hasher_update".}
proc llvmBlake3HasherFinalize*(self: ptr LlvmBlake3Hasher; `out`: ptr uint8T;
                              outLen: csize_t) {.
    importc: "llvm_blake3_hasher_finalize".}
proc llvmBlake3HasherFinalizeSeek*(self: ptr LlvmBlake3Hasher; seek: uint64T;
                                  `out`: ptr uint8T; outLen: csize_t) {.
    importc: "llvm_blake3_hasher_finalize_seek".}
proc llvmBlake3HasherReset*(self: ptr LlvmBlake3Hasher) {.
    importc: "llvm_blake3_hasher_reset".}