from sphinx.domains import Domain
from docutils.parsers.rst import roles

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *
from docutils import nodes


class BitsyLexer(RegexLexer):
    name = 'Bitsy'
    aliases = ['bitsy']
    filenames = ['*.bitsy']

    KEYWORDS = ['pub', 'enum', 'of', 'end', 'shape', 'fn', 'let', 'if', 'else', 'match', 'port', 'incoming', 'outgoing', 'wire', 'reg', 'port', 'struct', 'field', 'otherwise', 'mod']
    BUILTINS = ['Bit', 'Word', 'Vec', 'tuple', 'Nat', 'Shape', 'io']

    tokens = {
        'root': [
            (r'=>', Punctuation),
            (r'<=', Punctuation),
            (r'=', Punctuation),
            (r'==', Punctuation),
            (r'\.', Punctuation),
            (r'[-+()<>{}\[\],;]', Punctuation),
            (r'\$', Punctuation),
            (r'[1-9][0-9]*', Number.Integer),
            (words(KEYWORDS, suffix=r'\b'), Keyword),
            (r'@\b([a-zA-Z][a-zA-Z0-9]*)\b', Name.Decorator),
            (r'\s+', Text),
            (r'#.*$', Comment.Single),
#            (r'<[^>]+>', Comment.Special),
            (r'\b([A-Z][a-zA-Z0-9]*)\b', Name.Class),
            (r'\b[A-Z]\b', Name.Constant),
            (words(BUILTINS, suffix=r'\b'), Name.Builtin),
            (r'\b([a-zA-Z][a-zA-Z0-9]*)\b', Name.Variable),
        ]
    }



class BitsyDomain(Domain):
    name = 'bitsy'
    label = 'Bitsy Language'

    directives = {}

    roles = {}


def setup(app):
    app.add_domain(BitsyDomain)
