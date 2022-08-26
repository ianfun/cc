type
    VerboseLevel* = enum
      WError, WWarning, WNote, WVerbose

type CC* = object
    optLevel*: cuint ## 0 = -O0, 1 = -O1, 2 = -O2, 3 = -O3
    sizeLevel*: cuint ## 0 = none, 1 = -Os, 2 = -Oz
    inlineThreshold*: cuint
    output*: string
    verboseLevel*: VerboseLevel

var app* = CC(optLevel: 0.cuint, sizeLevel: 0.cuint, inlineThreshold: 0, verboseLevel: WNote)
