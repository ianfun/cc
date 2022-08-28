## this module provides `pragma` function to handle CPP's `_Pragma` and `#pragma`
##
## https://docs.microsoft.com/en-us/cpp/preprocessor/pragma-directives-and-the-pragma-keyword
##
## https://gcc.gnu.org/onlinedocs/gcc/Pragmas.html
##
## https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html

import core

proc pragma*(s: seq[TokenV]) =
    echo "pragma: ", len(s)
    discard

