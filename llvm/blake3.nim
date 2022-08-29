const
  _BLAKE3_VERSION_STRING* = "1.3.1"
  _BLAKE3_KEY_LEN* = 32
  _BLAKE3_OUT_LEN* = 32
  _BLAKE3_BLOCK_LEN* = 64
  _BLAKE3_CHUNK_LEN* = 1024
  _BLAKE3_MAX_DEPTH* = 54


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
    cvStackLen*: uint8T 
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