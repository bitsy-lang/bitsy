from sphinx.domains import Domain
from docutils.parsers.rst import roles

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *
from docutils import nodes


class NettleLexer(RegexLexer):
    name = 'Nettle'
    aliases = ['nettle']
    filenames = ['*.nettle']

    KEYWORDS = [
        'top', 'reg', 'node', 'mod', 'ext', 'if', 'reset',
        'incoming', 'outgoing',
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
            (r'\.', Punctuation),
            (r'[-+()<>{}\[\],;]', Punctuation),
            (r'\$', Punctuation),
            (r'[0-9][0-9]*w[0-9][0-9]*', Number.Integer),
            (r'[0-9]+', Number.Integer),
            (words(KEYWORDS, suffix=r'\b'), Keyword),
            (r'@\b([a-zA-Z][a-zA-Z0-9]*)\b', Name.Decorator),
            (r'\s+', Text),
            (r'#.*$', Comment.Single),
            (r'\b([A-Z][a-zA-Z0-9]*)\b', Name.Class),
            (r'\b[A-Z]\b', Name.Constant),
            (words(CONSTANTS, suffix=r'\b'), Name.Constant),
            (words(BUILTINS, suffix=r'\b'), Name.Builtin),
            (r'\b([_a-zA-Z][_a-zA-Z0-9]*)\b', Name.Variable),
            (r'//.*$', Comment.Single),  # Line comments starting with //
            (r'/\*', Comment.Multiline, 'block_comment'),  # Block comment /* ... */
        ],
        'block_comment': [
            (r'.*?\*/', Comment.Multiline, '#pop'),  # End of the block comment
            (r'.*$', Comment.Multiline),
        ],
    }


class NettleDomain(Domain):
    name = 'nettle'
    label = 'Nettle Language'

    directives = {}

    roles = {}


def setup(app):
    app.add_domain(NettleDomain)

