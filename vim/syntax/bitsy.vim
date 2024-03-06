if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn region lineComment start="//" end="\n"
syntax region blockComment start="/\*" end="\*/"
hi link lineComment Comment
hi link blockComment Comment

syn keyword bitsyKeyword mod enum type shape struct pub end of match if else wire reg incoming outgoing gate field let port init builtin fn top ext reset node when alt import from tb clock dom on dependson with test while always task loop drv mon it assert cover is set poke peek
hi link bitsyKeyword Keyword
syn keyword bitsyConstant true false XXX io
hi link bitsyConstant Number

syn match bitsyNumber /\<[0-9][0-9]*\>/
hi link bitsyNumber Number

syn match bitsyWord /\<[0-9][_0-9]*w[0-9][0-9]*\>/
hi link bitsyWord Number
syn match bitsyWordInfer /\<[0-9][_0-9]*\>/
hi link bitsyWordInfer Number

syn match bitsyWordHex /\<0x[0-9a-fA-F][_0-9a-fA-F]*w[0-9][0-9]*\>/
hi link bitsyWordHex Number
syn match bitsyWordInferHex /\<0x[0-9a-fA-F][_0-9a-fA-F]*\>/
hi link bitsyWordInferHex Number

syn match bitsyWordBin /\<0b[0-1][_0-1]*w[0-9][0-9]*\>/
hi link bitsyWordBin Number
syn match bitsyWordInferBin /\<0b[0-1][_0-1]*\>/
hi link bitsyWordInferBin Number

syn match bitsyIdentifier "\<[a-z_][A-Za-z0-9_]*\>"
syn match bitsyUpperIdentifier "\<[A-Z][A-Za-z0-9_]*\>"
hi link bitsyIdentifier Function
hi link bitsyUpperIdentifier Type

syn match bitsyAt "@"
syn match bitsyColon ":"
syn match bitsyRevFatArrow "<="
syn match bitsyRevFatDollarArrow "$="
syn match bitsyRevFatArrowBang "<=!"
syn match bitsyRevColonArrow ":="
syn match bitsyRevColonArrowBang ":=!"
syn match lt "<"
syn match gt ">"
syn match bitsyArrow "->"
syn match bitsyEq "="
syn match bitsyEqOp "=="
syn match bitsyNeqOp "!="
syn match bitsyAnd "&&"
syn match bitsyOr "||"
syn match bitsyFatArrow "=>"
syn match bitsyAdd "+"
syn match bitsyAddCarry "+%"
syn match bitsyMul "*"
syn match bitsyHole "?"
hi link bitsyAt Keyword
hi link bitsyColon Keyword
hi link lt Keyword
hi link gt Keyword
hi link bitsyArrow Keyword
hi link bitsyRevFatDollarArrow Keyword
hi link bitsyRevFatArrow Keyword
hi link bitsyRevFatArrowBang Keyword
hi link bitsyRevColonArrow Keyword
hi link bitsyRevColonArrowBang Keyword
hi link bitsyFatArrow Keyword
hi link bitsyEq Keyword
hi link bitsyEqOp Keyword
hi link bitsyNeqOp Keyword
hi link bitsyAnd Keyword
hi link bitsyAdd Keyword
hi link bitsyAddCarry Keyword
hi link bitsyOr Keyword
hi link bitsyMul Keyword
hi link bitsyHole Comment

syn match bitsyCtor "@\<[a-zA-Z_][A-Za-z0-9_]*"
syn match bitsyOtherwise "otherwise"
hi link bitsyCtor Constant
hi link bitsyOtherwise Constant

syn match bitsyX "X"
hi link bitsyX Number
