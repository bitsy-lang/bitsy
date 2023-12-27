from sphinx.domains import Domain
from docutils.parsers.rst import roles

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *
from docutils import nodes


class BitsyLexer(RegexLexer):
    name = 'Bitsy'
    aliases = ['bitsy']
    filenames = ['*.bitsy']

    KEYWORDS = [
        'top', 'reg', 'node', 'mod', 'ext', 'if', 'reset',
        'incoming', 'outgoing', 'enum', 'of', 'type',
        'fn', 'pub', 'otherwise', 'match', 'alt', 'struct',
    ]

    BUILTINS = ['Bit', 'Word', 'Vec', 'tuple', 'Nat', 'Shape', 'io']
    CONSTANTS = ['false', 'true']

    tokens = {
        'root': [
            (r'=>', Punctuation),
            (r'<=', Punctuation),
            (r':=', Punctuation),
            (r'!', Punctuation),
            (r'=', Punctuation),
            (r'==', Punctuation),
            (r'::', Punctuation),
            (r'\.', Punctuation),
            (r'[-+()<>{}\[\],;]', Punctuation),
            (r'\$', Punctuation),
            (r'[0-9][0-9_]*w[0-9][0-9]*', Number.Integer),
            (r'[0-9]b[0-1_]*w[0-9][0-9]*', Number.Integer),
            (r'[0-9]x[0-9a-fA-F_]*w[0-9][0-9]*', Number.Integer),
            (r'[0-9]+', Number.Integer),
            (words(KEYWORDS, suffix=r'\b'), Keyword),
            (r'@\b([a-zA-Z_][a-zA-Z_0-9]*)\b', Name.Decorator),
            (r'\s+', Text),
            (r'#.*$', Comment.Single),
            (r'\b([A-Z_][a-zA-Z_0-9]*)\b', Name.Class),
            (r'\b[A-Z_]\b', Name.Constant),
            (r'\?[a-zA-Z_][_a-zA-Z_0-9]*', Name.Constant),
            (words(CONSTANTS, suffix=r'\b'), Name.Constant),
            (words(BUILTINS, suffix=r'\b'), Name.Builtin),
            (r'\b([a-zA-Z_][_a-zA-Z_0-9]*)\b', Name.Variable),
            (r'//.*$', Comment.Single),  # Line comments starting with //
            (r'/\*', Comment.Multiline, 'block_comment'),  # Block comment /* ... */
        ],
        'block_comment': [
            (r'.*?\*/', Comment.Multiline, '#pop'),  # End of the block comment
            (r'.*$', Comment.Multiline),
        ],
    }


class BitsyDomain(Domain):
    name = 'bitsy'
    label = 'Bitsy Language'

    directives = {}

    roles = {}


def setup(app):
    app.add_domain(BitsyDomain)

