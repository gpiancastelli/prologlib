'''
Module for parsing Prolog programs.
'''

from collections import namedtuple

# character sets
_GRAPHIC_CHARS = ('#', '$', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^', '~')
_QUOTE_CHARS = ("'", '"', '`')
_WHITESPACE_CHARS = (' ', '\r', '\n', '\t', '\f')

# token types
_TYPEMASK = 0x00FF
_ATTRMASK = 0xFF00
_LPAR = 0x0001
_RPAR = 0x0002
_LBRA = 0x0003
_RBRA = 0x0004
_LCBR = 0x0005
_RCBR = 0x0006
_PIPE = 0x0007
_INTEGER = 0x0008
_FLOAT = 0x0009
_ATOM = 0x000A
_VARIABLE = 0x000B
_SQ_SEQUENCE = 0x000C
_DQ_SEQUENCE = 0x000D
_END = 0x000F
_FUNCTOR = 0x0100
_OPERATOR = 0x0200
EOF = 0x1000

Token = namedtuple('Token', ['value', 'type'])
Operator = namedtuple('Operator', ['name', 'priority', 'specifier', 'token'])


class InvalidTermException(Exception):
    pass


class PrologTokenizer:
    def __init__(self, reader):
        try:
            self._reader = self._open(reader)
        except TypeError:
            message = 'A file or string object is needed, found {} instead'
            raise TypeError(message.format(type(reader)))
        self.line = 1
        # used to let the lexer unread characters
        self._unread_characters = []
        # used to let the parser unread tokens
        self._unread_tokens = []

    def _open(self, source):
        """
        Produces a file object from source.
        The source can be either a file object already, or a string.
        """
        if hasattr(source, 'read'):
            return source
        else:
            from io import StringIO
            return StringIO(source)

    def _read(self):
        if self._unread_characters:
            return self._unread_characters.pop(0)
        else:
            return self._reader.read(1)

    def _pushback(self, char):
        self._unread_characters.append(char)

    def read_token(self):
        if self._unread_tokens:
            return self._unread_tokens.pop()
        else:
            return self.read_next_token()

    def unread_token(self, token):
        self._unread_tokens.append(token)

    def _skip_whitespace(self, start):
        c = start
        skip_line_feed = False
        while c in _WHITESPACE_CHARS:
            if c == '\r':
                skip_line_feed = True
                self.line += 1
            elif c == '\n' and not skip_line_feed:
                self.line += 1
            c = self._read()
        self._pushback(c)

    def read_next_token(self):
        c = self._read()
        
        # skip whitespace
        if c in _WHITESPACE_CHARS:
            self._skip_whitespace(c)
            c = self._read()
        ## skip_line_feed = False
        ## while c in _WHITESPACE_CHARS:
        ##     if c == '\r':
        ##         skip_line_feed = True
        ##         self.line += 1
        ##     elif c == '\n' and not skip_line_feed:
        ##         self.line += 1
        ##     c = self._read()

        if not c:
            return Token('', EOF)

        synchars = {'(': Token('(', _LPAR),
                    ')': Token(')', _RPAR),
                    '{': Token('{', _LCBR),
                    '}': Token('}', _RCBR),
                    '[': Token('[', _LBRA),
                    ']': Token(']', _RBRA),
                    '|': Token('|', _PIPE),
                    '!': Token('!', _ATOM),
                    ',': Token(',', _OPERATOR),
                    ';': Token(';', _ATOM | _OPERATOR)
                   }
        if c in synchars:
            return synchars[c]

        # An END token (i.e. '.') must be followed by a layout character.
        # See ISO Standard 6.4.8 endnote.
        if c == '.':
            layout = self._read()
            if layout == '':
                return Token('.', _END)
            elif layout in _WHITESPACE_CHARS:
                # self._skip_whitespace(layout)
                return Token('.', _END)
            elif layout == '%':
                self._pushback(layout)
                return Token('.', _END)
            else:
                self._pushback(layout)

        if 'a' <= c <= 'z':
            return self._read_atom(c)
        elif 'A' <= c <= 'Z' or c == '_':
            return self._read_variable(c)
        elif '0' <= c <= '9':
            return self._read_number(c)
        elif c in _QUOTE_CHARS:
            return self._read_quoted_atom(c)
        # skip single-line comments
        elif c == '%':
            c = self._read()
            while c != '\r' and c != '\n' and c != '':
                c = self._read()
            self._pushback(c)
            return self.read_next_token()
        # skip multi-line comments
        elif c == '/':
            c2 = self._read()
            if c2 == '*':
                c, c2 = c2, self._read()
                while c != '*' or c2 != '/':
                    if c == '\n' or c == '\r':
                        self.line += 1
                    c, c2 = c2, self._read()
                return self.read_next_token()
            else:
                # if '/' doesn't start a multi-line comment, try to read a
                # Prolog symbol, since '/' is included in _GRAPHIC_CHARS
                self._pushback(c2)
                return self._read_symbol(c)
        elif c in _GRAPHIC_CHARS + ('\\',):
            return self._read_symbol(c)
        else:
            message = 'Invalid token character {} at line {}'
            raise InvalidTermException(message.format(c, self.line))

    def _read_atom(self, start):
        buffer = [start]
        c = self._read()
        while 'a' <= c <= 'z' or 'A' <= c <= 'Z' or c == '_' or '0' <= c <= '9':
            buffer.append(c)
            c = self._read()
        self._pushback(c)
        if c == '(':
            return Token(''.join(buffer), _ATOM | _FUNCTOR)
        elif c in _WHITESPACE_CHARS:
            return Token(''.join(buffer), _ATOM | _OPERATOR)
        else:
            return Token(''.join(buffer), _ATOM)

    def _read_variable(self, start):
        buffer = [start]
        c = self._read()
        while 'a' <= c <= 'z' or 'A' <= c <= 'Z' or c == '_' or '0' <= c <= '9':
            buffer.append(c)
            c = self._read()
        self._pushback(c)
        return Token(''.join(buffer), _VARIABLE)

    def _read_number(self, start):
        BASES = {'b': 2, 'o': 8, 'x': 16}
        integer = []
        if start == '0':
            b = self._read()
            if b in BASES:
                base = BASES[b]
                c = self._read()
                while '0' <= c <= '9' or 'A' <= c <= 'F' or 'a' <= c <= 'f':
                    integer.append(c)
                    c = self._read()
                self._pushback(c)
                try:
                    return Token(str(int(''.join(integer), base)), _INTEGER)
                except ValueError:
                    message = 'Invalid number {} at line {}'
                    number = ''.join([start, b] + integer)
                    raise InvalidTermException(message.format(number, self.line))
            else:
                self._pushback(b)
                integer = [start]
        else:
            integer = [start]

        c = self._read()
        while '0' <= c <= '9':
            integer.append(c)
            c = self._read()

        # ordinary integers
        if c not in ('.', "'"):
            self._pushback(c)
            try:
                return Token(str(int(''.join(integer))), _INTEGER)
            except ValueError:
                message = 'Invalid number {} at line {}'
                raise InvalidTermException(message.format(integer, self.line))
        # character code constant
        if c == "'" and start == '0':
            char = self._read()
            code = self._character_code(char)
            if code > 0:
                return Token(str(code), _INTEGER)
            else:
                message = "Invalid character code constant 0'{} at line {}"
                raise InvalidTermException(message.format(char, self.line))
        # the integer part must be followed by a period
        if c != '.':
            message = 'Invalid number {} at line {}'
            raise InvalidTermException(message.format(''.join(integer) + c, self.line))

        decimal = []
        d = self._read()
        while '0' <= d <= '9':
            decimal.append(d)
            d = self._read()
        if not decimal:
            self._pushback(c)
            self._pushback(d) # double pushback
            try:
                return Token(str(int(''.join(integer))), _INTEGER)
            except ValueError:
                message = 'Invalid number {} at line {}'
                raise InvalidTermException(message.format(''.join(integer), self.line))

        if d == 'E' or d == 'e':
            sign = self._read()
            if sign == '+' or sign == '-':
                exponent = []
                e = self._read()
                while '0' <= e <= '9':
                    exponent.append(e)
                    e = self._read()
                    self._pushback(e)
                    number = ''.join(integer + ['.'] + decimal + [d, sign] + exponent)
                    try:
                        return Token(str(float(number)), _FLOAT)
                    except ValueError:
                        message = 'Invalid number {} at line {}'
                        raise InvalidTermException(message.format(number, self.line))
        self._pushback(d)
        number = ''.join(integer + ['.'] + decimal)
        try:
            return Token(str(float(number)), _FLOAT)
        except ValueError:
            message = 'Invalid number {} at line {}'
            raise InvalidTermException(message.format(number, self.line))

    def _character_code(self, char):
        """
        Return the code for a character sequence read after a 0' sequence.

        Currently, only single-character sequences are supported, excluding ISO
        Prolog 'solo characters' (see 6.5.3). Meta/Control/Octal/Hexadecimal
        escape sequences are yet to be supported as well (see 6.4.2.1). Please
        also note that limitations on the range of admissible characters (as
        per the ISO specification) are not yet enforced.
        """
        return ord(char)

    def _read_quoted_atom(self, quote_char):
        """
        Read a single/double quoted atom.

        The double_quotes flag is handled in the parser.
        """
        quote = []
        # run through entire quote adding characters to quote buffer
        while True:
            c = self._read()
            # handle escape sequences
            if c == '\\':
                c2 = self._read()
                if c2 == '\n':
                    continue
                if c2 == '\r':
                    c3 = self._read()
                    if c3 == '\n':
                        continue
                    self._pushback(c3)
                    continue
                self._pushback(c2)
            # handle quote character
            if c == quote_char:
                c2 = self._read()
                if c2 == quote_char:
                    quote.append(quote_char)
                    continue
                else:
                    self._pushback(c2)
                    break
            if c in ('\r', '\n'):
                message = 'Line break not allowed in quoted atom at line {}'
                raise InvalidTermException(message.format(self.line))
            if c == '':
                message = 'Open quote left unclosed in quoted atom at line {}'
                raise InvalidTermException(message.format(self.line))
            quote.append(c)

        quote = ''.join(quote)
        quote_type = _SQ_SEQUENCE if quote_char == "'" else _DQ_SEQUENCE if quote_char == '"' else _SQ_SEQUENCE
        if quote_type == _SQ_SEQUENCE:
            import re
            atom = re.compile("(!|[a-z][a-zA-Z_0-9]*)")
            if atom.match(quote):
                quote_type = _ATOM
            # look ahead to identify the atom type
            c2 = self._read()
            self._pushback(c2)
            if c2 == '(':
                return Token(quote, quote_type | _FUNCTOR)
        return Token(quote, quote_type)

    def _read_symbol(self, start):
        symbol = [start]
        c = self._read()
        while c in _GRAPHIC_CHARS + ('\\',):
            symbol.append(c)
            c = self._read()
        self._pushback(c)
        if c == '(':
            return Token(''.join(symbol), _ATOM | _FUNCTOR)
        else:
            return Token(''.join(symbol), _ATOM | _OPERATOR)


class PrologParser:
    def __init__(self, reader):
        self._lexer = PrologTokenizer(reader)
        self._ot = OperatorTable()

    def read_term(self):
        token = self._lexer.read_token()
        if token.type == EOF:
            return None
        self._lexer.unread_token(token)
        n = self._ot.MAX_PRIORITY
        term = self._next_term(n)
        if term:
            token = self._lexer.read_token()
            if token.type != _END:
                message = 'End token expected for term: ' + str(term)
                raise InvalidTermException(message)
            return term
        return None
        
    def _next_term(self, n):
        lterm = self._parse_prefix_term(n)
        if not lterm:
            lterm = self._parse_term(n)
        if lterm is not None:
            t = term = self._parse_infix_or_postfix_term(n, lterm)
            while t:
                t = self._parse_infix_or_postfix_term(n, term)
                if t:
                    term = t
            return term if term else lterm
        return None
    
    def _parse_prefix_term(self, n):
        operator = self._op(n, ('fy', 'fx'))
        if not operator:
            return None
        # special case for negative numbers
        if operator.name == '-' and operator.priority == 200 and operator.specifier == 'fy':
            number = self._lexer.read_token()
            if number.type == _INTEGER:
                return Atomic(-int(number.value))
            if number.type == _FLOAT:
                return Atomic(-float(number.value))
            self._lexer.unread_token(number)
        if operator.priority <= n and operator.specifier == 'fy':
            term = self._next_term(n)
            if not term:
                self._lexer.unread_token(operator.token)
                return None
            return Compound(operator.name, term, priority=operator.priority)
        if operator.priority <= n and operator.specifier == 'fx':
            term = self._next_term(self._ot.next_priority(operator.priority))
            if not term:
                self._lexer.unread_token(operator.token)
                return None
            return Compound(operator.name, term, priority=operator.priority)
        raise InvalidTermException("Invalid prefix term")

    def _parse_infix_or_postfix_term(self, n, lterm):
        p = lterm.priority
        operator = self._op(n, ('yf', 'xf', 'xfy', 'xfx', 'yfx'))
        if not operator:
            return None
        if operator.priority >= p and operator.specifier == 'yf':
            return Compound(operator.name, lterm, priority=operator.priority)
        if operator.priority > p and operator.specifier == 'xf':
            return Compound(operator.name, lterm, priority=operator.priority)
        if operator.priority > p and operator.specifier == 'xfy':
            term = self._next_term(operator.priority)
            if not term:
                message = 'Term expected after: ' + str(lterm) + operator.name
                raise InvalidTermException(message)
            return Compound(operator.name, lterm, term, priority=operator.priority)
        if operator.priority > p and operator.specifier == 'xfx':
            term = self._next_term(self._ot.next_priority(operator.priority))
            if not term:
                message = 'Term expected after: ' + str(lterm) + operator.name
                raise InvalidTermException(message)
            return Compound(operator.name, lterm, term, priority=operator.priority)
        if operator.priority >= p and operator.specifier == 'yfx':
            term = self._next_term(self._ot.next_priority(operator.priority))
            if not term:
                message = 'Term expected after: ' + str(lterm) + operator.name
                raise InvalidTermException(message)
            return Compound(operator.name, lterm, term, priority=operator.priority)
        message = 'Invalid infix or postfix term: ' + str(lterm)
        raise InvalidTermException(message)

    def _parse_term(self, n):
        token = self._lexer.read_token()
        if token.type == _INTEGER:
            return Atomic(int(token.value), 0)
        if token.type == _FLOAT:
            return Atomic(float(token.value), 0)
        if token.type == _VARIABLE:
            return Variable(token.value)
        if token.type & _TYPEMASK == _DQ_SEQUENCE:
            from . import core
            double_quotes = core._FLAGS['double_quotes'].value
            if double_quotes == 'codes':
                t = tuple(Atomic(ord(c), 0) for c in token.value)
                return Compound('.', *t )
            elif double_quotes == 'chars':
                t = tuple(Atomic(c, 0) for c in token.value)
                return Compound('.', *t )
            else: # double_quotes == 'atom':
                return Atomic(token.value, 0)
        if token.type & _TYPEMASK in (_ATOM, _SQ_SEQUENCE):
            if token.type & _ATTRMASK != _FUNCTOR:
                priority = self._ot.MAX_PRIORITY + 1 if self._ot.isoperator(token.value) else 0
                return Atomic(token.value, priority)
            functor = token.value
            lpar = self._lexer.read_token()
            if lpar.type != _LPAR:
                raise InvalidTermException("Invalid compound term")
            args = self._arg_list()
            rpar = self._lexer.read_token()
            if rpar.type != _RPAR:
                message = 'Invalid compound term with functor {0} at line {1}'
                raise InvalidTermException(message.format(functor, self._lexer.line))
            return Compound(functor, *args)
        if token.type == _LPAR:
            term = self._next_term(self._ot.MAX_PRIORITY)
            rpar = self._lexer.read_token()
            if rpar.type != _RPAR:
                raise InvalidTermException("Invalid parenthesized term: " + str(term))
            term.priority = 0
            return term
        if token.type == _LBRA:
            rbra = self._lexer.read_token()
            if rbra.type == _RBRA:
                return List.EMPTY_LIST
            self._lexer.unread_token(rbra)
            term, tail = self._items()
            rbra = self._lexer.read_token()
            if rbra.type != _RBRA:
                raise InvalidTermException("Invalid list")
            return List.from_list(term, tail)
        if token.type == _LCBR:
            rcbr = self._lexer.read_token()
            if rcbr.type == _RCBR:
                return Atomic('{}', 0)
            self._lexer.unread_token(rcbr)
            term = self._next_term(self._ot.MAX_PRIORITY)
            rcbr = self._lexer.read_token()
            if rcbr.type != _RCBR:
                raise InvalidTermException("Invalid curly bracketed term: " + str(term))
            return Compound('{}', term)
        self._lexer.unread_token(token)
        return None

    def _arg_list(self):
        args = [self._arg()]
        token = self._lexer.read_token()
        while token.value == ',':
            args.append(self._arg())
            token = self._lexer.read_token()
        self._lexer.unread_token(token)
        return args

    def _arg(self):
        token = self._lexer.read_token()
        if token.type == _ATOM and self._ot.isoperator(token.value):
            return Atomic(token.value, 0)
        self._lexer.unread_token(token)
        term = self._next_term(999)
        if term is None:
            message = 'Term expected near token: ' + str(token)
            raise InvalidTermException(message)
        return term

    def _items(self):
        items = [self._arg()]
        tail = None
        token = self._lexer.read_token()
        while token.value == ',':
            items.append(self._arg())
            token = self._lexer.read_token()
        if token.type == _PIPE:
            tail = self._arg()
        else:
            self._lexer.unread_token(token)
        return items, tail

    def _op(self, max_priority, allowed_specifiers):
        token = self._lexer.read_token()
        ops = self._ot.operator(token.value)
        if not ops:
            self._lexer.unread_token(token)
            return None
        allowed = lambda op: op[1] <= max_priority and op[2] in allowed_specifiers
        ops = [op for op in ops if allowed(op)]
        if not ops:
            self._lexer.unread_token(token)
            return None
        op = ops[0]
        return Operator(op[0], op[1], op[2], token)


class OperatorTable:
    MAX_PRIORITY = 1200
    MIN_PRIORITY = 0

    def __init__(self):
        self._table = {':-' : [(1200, 'xfx'), (1200, 'fx')],
                       '-->' : [(1200, 'xfx')],
                       '?-' : [(1200, 'fx')],
                       ';' : [(1100, 'xfy')],
                       '->' : [(1050, 'xfy')],
                       ',' : [(1000, 'xfy')],
                       '\\+' : [(900, 'fy')],
                       '=' : [(700, 'xfx')],
                       '\\=' : [(700, 'xfx')],
                       '==' : [(700, 'xfx')],
                       '\\==' : [(700, 'xfx')],
                       '@<' : [(700, 'xfx')],
                       '@=<' : [(700, 'xfx')],
                       '@>' : [(700, 'xfx')],
                       '@>=' : [(700, 'xfx')],
                       '=..' : [(700, 'xfx')],
                       'is' : [(700, 'xfx')],
                       '=:=' : [(700, 'xfx')],
                       '=\\=' : [(700, 'xfx')],
                       '<' : [(700, 'xfx')],
                       '=<' : [(700, 'xfx')],
                       '>' : [(700, 'xfx')],
                       '>=' : [(700, 'xfx')],
                       '+' : [(500, 'yfx')],
                       '-' : [(500, 'yfx'), (200, 'fy')],
                       '/\\' : [(500, 'yfx')],
                       '\\/' : [(500, 'yfx')],
                       '*' : [(400, 'yfx')],
                       '/' : [(400, 'yfx')],
                       '//' : [(400, 'yfx')],
                       'rem' : [(400, 'yfx')],
                       'mod' : [(400, 'yfx')],
                       '<<' : [(400, 'yfx')],
                       '>>' : [(400, 'yfx')],
                       '**' : [(200, 'xfx')],
                       '^' : [(200, 'xfy')],
                       '\\' : [(200, 'fy')]
                       }

    def operator(self, symbol):
        ops = self._table.get(symbol)
        if not ops:
            return None
        return [(symbol,) + op for op in ops]

    def add(self, symbol, priority, specifier):
        defined = self._table.get(symbol)
        if defined:
            defined.append((priority, specifier))
            self._table[symbol] = defined
        else:
            self._table[symbol] = [(priority, specifier)]

    def next_priority(self, priority):
        return priority - 1 # TODO Just a stub

    def isoperator(self, symbol):
        return self._table.get(symbol) is not None


### Perhaps the classes representing Prolog terms should be
### placed in their own module?

class Term:
    def __le__(self, other):
        return self.__lt__(other) or self.__eq__(other)
    def __gt__(self, other):
        return not self.__lt__(other)
    def __ge__(self, other):
        return self.__gt__(other) or self.__eq__(other)
    def __ne__(self, other):
        return not self.__eq__(other)
    def predicate_indicator(self):
        return '{}/{}'.format(Atomic(self.name)._unquote(), self.arity)
    def isdirective(self):
        return False


class Atomic(Term):    
    def __init__(self, value, priority=0):
        self.value = value
        self.name = None if self._isnumber() else value
        self.arity = 0
        self.priority = priority

    # TODO Maybe temporary?
    def head(self):
        return self

    # TODO Maybe temporary?
    def body(self):
        return Atomic.TRUE

    def apply(self, mgu):
        pass

    def isquoted(self):
        if not self.name: # numbers and empty atoms
            return False
        return self.value[0] == self.value[-1] == "'"

    def rename(self, index=None):
        return Atomic(self.value, self.priority)

    def toquotedform(self):
        if self.name is None:
            return str(self.value)
        name = self.name[1:-1] if self.isquoted() else self.name
        import re
        letter_digit_token = re.compile('\A[a-z][a-zA-Z0-9_]*\Z')
        graphic_token = re.compile('\A[' + ''.join(_GRAPHIC_CHARS) + '\\\\' + ']+\Z')
        semicolon_token = re.compile('\A;\Z')
        cut_token = re.compile('\A!\Z')
        empty_list_token = re.compile('\A\[\]\Z')
        if (letter_digit_token.match(name) or
            graphic_token.match(name) or
            semicolon_token.match(name) or
            cut_token.match(name) or
            empty_list_token.match(name)):
            return name
        else:
            return "'" + name + "'"

    def _copy_term(self):
        return self.rename()

    def _free_variables(self):
        return []

    def _isnumber(self):
        return isinstance(self.value, (int, float))

    def _touiform(self):
        '''Returns a representation suitable for the TopLevel UI'''
        return self.toquotedform()

    def _unquote(self):
        return self.value[1:-1] if self.isquoted() else self.value

    def __lt__(self, other):
        if isinstance(other, Variable):
            if other.isfree():
                return False
            else:
                return self.__lt__(other.binding())
        if isinstance(self.value, float):
            if isinstance(other.value, float):
                return self.value < other.value
            else:
                return True
        if isinstance(self.value, int):
            if isinstance(other.value, float):
                return False
            elif isinstance(other.value, int):
                return self.value < other.value
            else:
                return True
        if isinstance(self.value, str):
            if isinstance(other.value, (float, int)):
                return False
            else:
                return self.value < other.value

    def __eq__(self, other):
        if not isinstance(other, Atomic):
            return False
        if self._isnumber():
            if isinstance(self.value, int) and isinstance(other.value, float):
                return False
            if isinstance(self.value, float) and isinstance(other.value, int):
                return False
        return self._unquote() == other._unquote() # and self.priority == other.priority

    def __hash__(self):
        return hash(self.value)

    def __repr__(self):
        return "''" if self.value == '' else str(self.value)

Atomic.TRUE = Atomic('true')
Atomic.FAIL = Atomic('fail')


class Variable(Term):
    ANONYMOUS_COUNTER = 0
    
    def __init__(self, name, priority=0):
        self.value = None
        if name == '_':
            self.name = name + str(Variable.ANONYMOUS_COUNTER)
            Variable.ANONYMOUS_COUNTER += 1
        else:
            self.name = name
        self.priority = priority
        self.arity = -1 # TODO Uhm...

    def predicate_indicator(self):
        term = self.binding()
        if self == term:
            return super().predicate_indicator()
        else:
            return term.predicate_indicator()

    def binding(self):
        if self.value is not None:
            if isinstance(self.value, Variable):
                return self.value.binding()
            else:
                return self.value
        return self

    def isfree(self):
        binding = self.binding()
        #return self == binding or binding.value is None
        return self == binding or (hasattr(binding, 'value') and binding.value is None)

    def isanonymous(self):
        return self.name.startswith('_') or self.name.find('#') > 0

    def apply(self, mgu):
        binding = self.value
        names = [self.name]
        while binding is not None:
            if isinstance(binding, Variable) and binding.value is None:
                names.append(binding.name)
                for name in names:
                    value = mgu[name]
                    if value is not None and value != binding:
                        binding.value = value
                        return
                return
            elif isinstance(binding, Variable) and binding.value is not None:
                names.append(binding.name)
                binding = binding.value
            else:
                binding.apply(mgu)
                return
        value = mgu[self.name]
        if value is not None:
            self.value = value

    ## def apply(self, mgu):
    ##     if self.value is None:
    ##         value = mgu[self.name]
    ##         if value is not None:
    ##             self.value = value
    ##     else:
    ##         binding = self.value
    ##         if isinstance(binding, Variable) and binding.value is None:
    ##             value = mgu[self.name]
    ##             if value is None:
    ##                 value = mgu[binding.name]
    ##             if value is not None and value != binding:
    ##                 binding.value = value
    ##         else:
    ##             binding.apply(mgu)
    
    def rename(self, index=None):
        '''Rename a variable, returning an instance of itself with
        a different name, appending a numeric index, prefixed by "#".
        Assumes that "#" characters cannot be used in a variable name.'''
        name = self.name
        sep = self.name.rfind('#') + 1
        if sep:
            if index is None:
                n = int(self.name[sep:])
                name = self.name[:sep] + str(n + 1)
            else:
                name = self.name[:sep] + str(index)
        else:
            if index is None:
                name += '#0'
            else:
                name += '#' + str(index)
        v = Variable(name, self.priority)
        v.value = self.value if self.value is None else self.value.rename()
        return v

    def toquotedform(self):
        return self.value.toquotedform() if self.value is not None else self.name

    def _copy_term(self):
        '''Similar to rename, but used by copy_term/2.'''
        name = self.name
        cpsep = self.name.find('#C') + 1
        rensep = self.name.rfind('#') + 1
        if cpsep:
            n = int(self.name[cpsep+1:rensep-1])
            name = self.name[:cpsep] + str(n + 1) + self.name[rensep-1:]
        else:
            if rensep:
                name = self.name[:rensep] + 'C0#' + self.name[rensep:]
            else:
                name += '#C0#0'
        v = Variable(name, self.priority)
        v.value = self.value if self.value is None else self.value._copy_term()
        return v

    def _free_variables(self):
        return [self] if self.isfree() else []

    def _isrenamed(self):
        return self.name.find('#') > 0

    def _touiform(self):
        '''Returns a representation suitable for the TopLevel UI'''
        name = self.name
        if self.isanonymous():
            name = '_'
        return self.value._touiform() if self.value is not None else name

    def __lt__(self, other):
        if not self.isfree():
            return self.binding().__lt__(other)
        if not isinstance(other, Variable) or not other.isfree():
            return True
        return self.name < other.name
    
    def __eq__(self, other):
#        if self.value:
#            return self.value.__eq__(other)
        if not isinstance(other, Variable):
            return False
        # skipping priority because in Variable should always be 0
        return self.name == other.name and self.value == other.value

    def __hash__(self):
        return self.name.__hash__()

    def __repr__(self):
        return str(self.value) if self.value is not None else self.name
#        binding = self.binding()
#        return self.name if self == binding else str(binding)


class Compound(Term):
    def __init__(self, name, *value, priority=0):
        self.name = name
        self.value = (name,) + value
        self.arity = len(value)
        self.priority = priority

    def head(self):
        if self.name == ':-' and self.arity == 2:
            return self.value[1]
        else:
            return self

    def body(self):
        if self.name == ':-' and self.arity == 2:
            return self.value[2]
        else:
            return Atomic.TRUE

    def isdirective(self):
        return self.name == ':-' and self.arity == 1

    def apply(self, mgu):
        for arg in self.value[1:]:
            arg.apply(mgu)

    def rename(self, index=None):
        args = tuple(arg.rename(index) for arg in self.value[1:])
        return Compound(self.name, *args, priority=self.priority)

    def toquotedform(self):
        args = ','.join([arg.toquotedform() for arg in self.value[1:]])
        return Atomic(self.name).toquotedform() + '(' + args + ')'

    def _copy_term(self):
        args = tuple(arg._copy_term() for arg in self.value[1:])
        return Compound(self.name, *args, priority=self.priority)

    def _free_variables(self):
        fv = []
        for arg in self.value[1:]:
            fv += arg._free_variables()
        return fv

    def _touiform(self):
        '''Returns a representation suitable for the TopLevel UI'''
        args = ','.join([arg._touiform() for arg in self.value[1:]])
        return Atomic(self.name)._touiform() + '(' + args + ')'

    def __lt__(self, other):
        if not isinstance(other, Compound):
            return False
        if self.arity < other.arity:
            return True
        if self.arity == other.arity and self.name < other.name:
            return True
        if self.arity == other.arity and self.name == other.name:
            for arg, otherarg in zip(self.value[1:], other.value[1:]):
                if arg == otherarg:
                    continue
                elif arg < otherarg:
                    return True
                else:
                    return False
        return False

    def __eq__(self, other):
        if isinstance(other, List):
            if self.predicate_indicator() != './2':
                return False
            if self.value[1] != other.head:
                return False
            return self.value[2] == other.tail
        if not isinstance(other, Compound):
            return False
        if (self.name != other.name
            or self.arity != other.arity
            or self.priority != other.priority): # TODO Does this make sense?
            return False
        result = True
        for arg, otherarg in zip(self.value[1:], other.value[1:]):
            result = result and arg == otherarg
        return result

    def __hash__(self):
        h = hash(self.name)
        for arg in self.value[1:]:
            h ^= hash(arg)
        return h

    def __repr__(self):
        args = ','.join([str(arg) for arg in self.value[1:]])
        return self.name + '(' +  args + ')'


class List(Term):
    '''Represents Prolog lists, partial lists, and everything that
    can be built using square brackets, e.g. [foo|bar].'''
    
    EMPTY_LIST = Atomic('[]', 0)
    EMPTY_LIST.as_list = lambda: []

    def __init__(self, head, tail=EMPTY_LIST, priority=0):
        self.name = '.'
        self.arity = 2
        self.head = head
        self.tail = tail
        self.priority = 0

    @staticmethod
    def from_list(terms, tail=None):
        '''Builds a Prolog list starting from terms in a Python list.'''
        if not terms and tail is None:
            return List.EMPTY_LIST
        lst = List.EMPTY_LIST if tail is None else tail
        for term in reversed(terms):
            lst = List(term, lst)
        return lst

    def apply(self, mgu):
        self.head.apply(mgu)
        self.tail.apply(mgu)

    def as_list(self):
        '''Returns this Prolog list as a Python list. Prolog partial
        lists are represented in the same way as if the "partial" tail
        were the last element of the list.'''
        items = [self.head]
        tail = self.tail
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                if isinstance(tail, Variable):
                    if not tail.isfree():
                        tail = tail.binding()
                        continue
                items.append(tail)
                break
            else:
                items.append(tail.head)
            tail = tail.tail
        return items

    def is_list(self):
        '''Only real lists are detected. Partial lists and
        other square-bracketed terms return False.'''
        tail = self.tail
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                if isinstance(tail, Variable):
                    if not tail.isfree():
                        tail = tail.binding()
                        continue
                return False
            tail = tail.tail
        return True

    def rename(self, index=None):
        items = [self.head.rename(index)]
        tail = self.tail
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                items.append(tail.rename(index))
                break
            else:
                items.append(tail.head.rename(index))
            tail = tail.tail
        lst = items.pop() if tail != List.EMPTY_LIST else List.EMPTY_LIST
        for item in reversed(items):
            lst = List(item, lst)
        return lst

    def toquotedform(self):
        items = [self.head]
        tail = self.tail
        closing = ']'
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                if isinstance(tail, Variable):
                    if not tail.isfree():
                        tail = tail.binding()
                        continue
                closing = '|' + tail.toquotedform() + ']'
                break
            items.append(tail.head)
            tail = tail.tail
        return '[' + ','.join([it.toquotedform() for it in items]) + closing

    def _copy_term(self):
        items = [self.head._copy_term()]
        tail = self.tail
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                items.append(tail._copy_term())
                break
            else:
                items.append(tail.head._copy_term())
            tail = tail.tail
        lst = items.pop() if tail != List.EMPTY_LIST else List.EMPTY_LIST
        for item in reversed(items):
            lst = List(item, lst)
        return lst

    def _free_variables(self):
        return self.head._free_variables() + self.tail._free_variables()

    def _touiform(self):
        '''Returns a representation suitable for the TopLevel UI'''
        items = [self.head]
        tail = self.tail
        closing = ']'
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                if isinstance(tail, Variable):
                    if not tail.isfree():
                        tail = tail.binding()
                        continue
                closing = '|' + tail._touiform() + ']'
                break
            items.append(tail.head)
            tail = tail.tail
        return '[' + ','.join([it._touiform() for it in items]) + closing

    def __eq__(self, other):
        if isinstance(other, Compound):
            if other.predicate_indicator() != './2':
                return False
            if self.head != other.value[1]:
                return False
            return self.tail == other.value[2]
        if not isinstance(other, List):
            return False
        if self.head != other.head:
            return False
        return self.tail == other.tail

    def __hash__(self):
        h = hash('.')
        for e in self.as_list():
            h ^= hash(e)
        return h

    def __len__(self):
        if self.tail == List.EMPTY_LIST:
            return 1
        # partial list
        if isinstance(self.tail, Variable):
            if self.tail.isfree():
                return 1
            else:
                return 1 + len(self.tail.binding())
        return 1 + len(self.tail)

    def __repr__(self):
        '''Represents this Prolog list as a string. Partial lists are
        represented with the pipe before the "partial" tail. (This is
        the reason why this method cannot exploit self.as_list().)'''
        items = [self.head]
        tail = self.tail
        closing = ']'
        while tail != List.EMPTY_LIST:
            if not isinstance(tail, List):
                if isinstance(tail, Variable):
                    if not tail.isfree():
                        tail = tail.binding()
                        continue
                closing = '|' + str(tail) + ']'
                break
            items.append(tail.head)
            tail = tail.tail
        return '[' + ','.join([str(it) for it in items]) + closing

### Utility functions

def isvariable(term):
    return isinstance(term, Variable) and term.isfree()

def isatom(term):
    return term.arity == 0 and not term._isnumber()

def isnumber(term):
    return term.arity == 0 and term._isnumber()

def islist(term):
    if term == List.EMPTY_LIST:
        return True
    if isinstance(term, List):
        return term.is_list()
    if isinstance(term, Compound):
        return term.name == '.' and term.arity == 2 and islist(term.value[2])
    return False

def ispartiallist(term):
    if isvariable(term):
        return True
    if isinstance(term, List):
        return ispartiallist(term.tail)
    if isinstance(term, Compound):
        return term.name == '.' and term.arity == 2 and ispartiallist(term.value[2])
    return False

def iscallable(term):
    return isatom(term) or isinstance(term, Compound)
