if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn region lineComment start="//" end="\n"
syntax region blockComment start="/\*" end="\*/"
hi link lineComment Comment
hi link blockComment Comment

syn keyword nettleKeyword mod enum type shape struct pub end of match if else wire reg incoming outgoing gate field let port init builtin fn top ext reset node
hi link nettleKeyword Keyword
syn keyword nettleConstant true false X io
hi link nettleConstant Number

syn match nettleNumber /\<[0-9][0-9]*\>/
hi link nettleNumber Number

syn match nettleWord /\<[0-9][0-9]*w[0-9][0-9]*\>/
hi link nettleWord Number

syn match nettleIdentifier "\<[a-z_][A-Za-z0-9_]*\>"
syn match nettleUpperIdentifier "\<[A-Z][A-Za-z0-9_]*\>"
hi link nettleIdentifier Function
hi link nettleUpperIdentifier Type

syn match nettleAt "@"
syn match nettleColon ":"
syn match nettleRevFatArrow "<="
syn match lt "<"
syn match gt ">"
syn match nettleArrow "->"
syn match nettleEq "="
syn match nettleEqOp "=="
syn match nettleNeqOp "!="
syn match nettleFatArrow "=>"
syn match nettleAdd "+"
syn match nettleMul "*"
syn match nettleHole "?"
hi link nettleAt Keyword
hi link nettleColon Keyword
hi link lt Keyword
hi link gt Keyword
hi link nettleArrow Keyword
hi link nettleRevFatArrow Keyword
hi link nettleFatArrow Keyword
hi link nettleEq Keyword
hi link nettleEqOp Keyword
hi link nettleNeqOp Keyword
hi link nettleAdd Keyword
hi link nettleMul Keyword
hi link nettleHole Comment

syn match nettleCtor "@\<[a-zA-Z_][A-Za-z0-9_]*"
syn match nettleOtherwise "otherwise"
hi link nettleCtor Constant
hi link nettleOtherwise Constant
