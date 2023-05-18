if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn region lineComment start="//" end="\n"
syntax region blockComment start="/\*" end="\*/"
hi link lineComment Comment
hi link blockComment Comment

syn keyword bitsyKeyword mod enum type shape struct pub end of match if else wire reg incoming outgoing gate field let port init builtin
hi link bitsyKeyword Keyword
syn keyword bitsyConstant true false X io
hi link bitsyConstant Number

syn match bitsyDecNumber /\<[1-9][0-9]*\>/
syn match bitsyBinNumber /\<0b[01_]\+\>/
syn match bitsyOctNumber /\<0[0-7_]*\>/
syn match bitsyHexNumber /\<0x[0-9a-fA-F_]\+\>/
syn match bitsyDecFloatNumber /\<\([1-9][0-9_]*\|0\)\.[0-9]\+\([eE][+-]\?[0-9_]\+\)\?\>/
syn match bitsyDecFloatNumber /\<\([1-9][0-9_]*\|0\)[eE][+-]\?[0-9_]\+\>/
syn match bitsyHexFloatNumber /\<0x[0-9a-fA-F_]\+\.[0-9a-fA-F_]\+\([pP][+-]\?[0-9a-fA-F_]\+\)\?\>/
syn match bitsyHexFloatNumber /\<0x[0-9a-fA-F_]\+[pP][+-]\?[0-9a-fA-F_]\+\>/
hi link bitsyDecNumber bitsyNumber
hi link bitsyBinNumber bitsyNumber
hi link bitsyOctNumber bitsyNumber
hi link bitsyHexNumber bitsyNumber
hi link bitsyNumber Number
hi link bitsyDecFloatNumber bitsyFloatNumber
hi link bitsyHexFloatNumber bitsyFloatNumber
hi link bitsyFloatNumber Float

syn match bitsyIdentifier "\<[a-z_][A-Za-z0-9_]*"
syn match bitsyUpperIdentifier "\<[A-Z][A-Za-z0-9_]*"
hi link bitsyIdentifier Function
hi link bitsyUpperIdentifier Type

syn match bitsyAt "@"
syn match bitsyColon ":"
syn match bitsyRevArrow "<="
syn match bitsyEq "="
syn match bitsyEqOp "=="
syn match bitsyNeqOp "!="
syn match bitsyArrow "=>"
syn match bitsyAdd "+"
syn match bitsyMul "*"
hi link bitsyAt Keyword
hi link bitsyColon Keyword
hi link bitsyArrow Keyword
hi link bitsyRevArrow Keyword
hi link bitsyEq Keyword
hi link bitsyEqOp Keyword
hi link bitsyNeqOp Keyword
hi link bitsyAdd Keyword
hi link bitsyMul Keyword

syn match bitsyCtor "@\<[a-zA-Z_][A-Za-z0-9_]*"
syn match bitsyOtherwise "otherwise"
hi link bitsyCtor Constant
hi link bitsyOtherwise Constant
