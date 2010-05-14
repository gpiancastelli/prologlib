import unittest

from prologlib.parser import PrologTokenizer, EOF, _FUNCTOR, _ATTRMASK, _END, _INTEGER, _ATOM, _VARIABLE, _TYPEMASK, InvalidTermException

class PrologTokenizerTest(unittest.TestCase):
    def test_comments(self):
        s = '''
        %   Single line comment

        /*  Multi line comment
        */
        '''.lstrip()
        lexer = PrologTokenizer(s)
        token = lexer.read_token()
        self.assertEquals(EOF, token.type)
    def test_quoted_functor(self):
        s = "bin_bu(('$bin_cut'(X,Cont):-true(Cont))):-cutp(X)."
        lexer = PrologTokenizer(s)
        # first functor
        token = lexer.read_token()
        self.assertEquals(_FUNCTOR, token.type & _ATTRMASK)
        # skip the next two parentheses tokens
        lexer.read_token()
        lexer.read_token()
        # second functor as a quoted atom
        token = lexer.read_token()
        self.assertEquals(_FUNCTOR, token.type & _ATTRMASK)
    def test_period_after_integer(self):
        s = 'p :- X is 3.'
        lexer = PrologTokenizer(s)
        # skip five tokens to get to the period
        for i in range(5):
            lexer.read_token()
        period = lexer.read_token()
        self.assertEquals(_END, period.type)
    def test_character_code_constant(self):
        s = "q(0'e)."
        lexer = PrologTokenizer(s)
        # skip two tokens to get to the constant
        lexer.read_token()
        lexer.read_token()
        constant = lexer.read_token()
        self.assertEquals(_INTEGER, constant.type)
        self.assertEquals(101, int(constant.value))
    def test_operators_as_functors(self):
        s = 'p :- =(X, Y).'
        lexer = PrologTokenizer(s)
        # skip two tokens to get to the functor
        lexer.read_token()
        lexer.read_token()
        token = lexer.read_token()
        self.assertEquals(_FUNCTOR, token.type & _ATTRMASK)
    def test_graphic_symbols_as_atoms(self):
        s = 'f(a,g(b,?,d,e)).'
        lexer = PrologTokenizer(s)
        # skip eight tokens to get to the graphic symbol
        for i in range(8):
            lexer.read_token()
        token = lexer.read_token()
        self.assertEquals('?', token.value)
        self.assertEquals(_ATOM, token.type & _TYPEMASK)
    def test_semicolon_as_atom(self):
        s = 'f(a((;)), b, c).'
        lexer = PrologTokenizer(s)
        # skip five tokens to get to the semicolon
        for i in range(5):
            lexer.read_token()
        token = lexer.read_token()
        self.assertEquals(';', token.value)
        self.assertEquals(_ATOM, token.type & _TYPEMASK)
    def test_integer_representations(self):
        numbers = ('0b101101', '0o77351', '0xDECAF')
        results = (45, 32489, 912559)
        for number, result in zip(numbers, results):
            lexer = PrologTokenizer(number)
            token = lexer.read_token()
            self.assertEquals(result, int(token.value))
        invalid = ('0b101201', '0o78351', '0xG')
        for number in invalid:
            lexer = PrologTokenizer(number)
            self.assertRaises(InvalidTermException, lexer.read_token)
    def test_character_code(self):
        lexer = PrologTokenizer("0'[")
        token = lexer.read_token()
        self.assertEquals(91, int(token.value))
        self.assertEquals(_INTEGER, token.type)
    def test_variable_names(self):
         z = 'Z'
         lexer = PrologTokenizer(z)
         token = lexer.read_token()
         self.assertEquals('Z', token.value)
         self.assertEquals(_VARIABLE, token.type)


from prologlib.parser import PrologParser, List, Compound, Variable, Atomic

class PrologParserTest(unittest.TestCase):
    def test_operators_within_terms(self):
        s = 'p :- X is 1, q.'
        parser = PrologParser(s)
        p = Atomic('p')
        t = Compound('is', Variable('X'), Atomic(1), priority=700)
        q = Atomic('q')
        term = Compound(':-', p, Compound(',', t, q, priority=1000), priority=1200)
        self.assertEquals(term, parser.read_term())
    def test_yfx_associativity(self):
        s = 'p :- X is 6 - 3 - 3.'
        parser = PrologParser(s)
        three = Compound('-', Atomic(6), Atomic(3), priority=500)
        zero = Compound('-', three, Atomic(3), priority=500)
        t = Compound('is', Variable('X'), zero, priority=700)
        term = Compound(':-', Atomic('p'), t, priority=1200)
        self.assertEquals(term, parser.read_term())
    def test_xfy_associativity(self):
        s = 'p :- a, b, c.'
        parser = PrologParser(s)
        bc = Compound(',', Atomic('b'), Atomic('c'), priority=1000)
        abc = Compound(',', Atomic('a'), bc, priority=1000)
        term = Compound(':-', Atomic('p'), abc, priority=1200)
        self.assertEquals(term, parser.read_term())
    def test_zero_priority_parenthesized_terms(self):
        s = 'p :- a, b, ( f(X,Y,Z) ; g(f(A)), h ), !, end.'
        parser = PrologParser(s)
        term = parser.read_term()
        body = term.value[2]
        pt = body.value[2].value[2].value[1]
        self.assertEquals(';', pt.name)
        self.assertEquals(0, pt.priority)
    def test_atom(self):
        s = 'hello.'
        parser = PrologParser(s)
        self.assertEquals(Atomic('hello'), parser.read_term())
    def test_empty_text(self):
        s = ''
        parser = PrologParser(s)
        self.assertEquals(None, parser.read_term())
    def test_prefix_operator(self):
        s = 'n(+100).'
        parser = PrologParser(s)
        # + is not defined as a prefix operator
        self.assertRaises(InvalidTermException, parser.read_term)
        s = 'n(-100).'
        parser = PrologParser(s)
        result = Compound('n', Atomic(-100))
        # - is defined as a prefix operator
        self.assertEquals(result, parser.read_term())
    def test_prefix_operator_as_atom(self):
        s = 'f(-, a).'
        parser = PrologParser(s)
        result = Compound('f', Atomic('-', 1201), Atomic('a'))
        self.assertEquals(result, parser.read_term())
    def test_infix_operator(self):
        s = 'abs(3-11).'
        parser = PrologParser(s)
        result = Compound('abs', Compound('-', Atomic(3), Atomic(11), priority=500))
        self.assertEquals(result, parser.read_term())
    def test_list_with_tail(self):
        s = '[a|[b,c]].'
        parser = PrologParser(s)
        result = List.from_list([Atomic('a'), Atomic('b'), Atomic('c')])
        self.assertEquals(result, parser.read_term())
    def test_partial_list(self):
        s = '[p|Y].'
        parser = PrologParser(s)
        result = List(Atomic('p'), Variable('Y'), 0)
        self.assertEquals(result, parser.read_term())
    def test_braces(self):
        s = '{a,b,[3,{4,c},5],{a,b}}.'
        parser = PrologParser(s)
        inner_curly = Compound('{}', Compound(',', Atomic(4), Atomic('c'), priority=1000))
        inner_list = List.from_list([Atomic(3), inner_curly, Atomic(5)])
        ab = Compound(',', Atomic('a'), Atomic('b'), priority=1000)
        result = Compound('{}',
                     Compound(',', Atomic('a'),
                         Compound(',', Atomic('b'),
                             Compound(',', inner_list,
                                 Compound('{}', ab), priority=1000), priority=1000), priority=1000))
        self.assertEquals(result, parser.read_term())
    def test_bracketed_operator_as_term(self):
##         TODO Fix this bug!
        s = 'u (b1) b2 (b3).'
        parser = PrologParser(s)
        parser._ot.add('u', 200, 'fx')
        parser._ot.add('b1', 400, 'yfx')
        parser._ot.add('b2', 500, 'yfx')
        parser._ot.add('b3', 300, 'yfx')
        result = Compound('b2', Compound('u', Atomic('b1'), priority=200), Atomic('b3'), priority=500)
        self.assertEquals(result, parser.read_term())
        s = '(u) b1 (b2) b3 a.'
        parser = PrologParser(s)
        parser._ot.add('u', 200, 'fx')
        parser._ot.add('b1', 400, 'yfx')
        parser._ot.add('b2', 500, 'yfx')
        parser._ot.add('b3', 300, 'yfx')
        result = Compound('b1', Atomic('u'), Compound('b3', Atomic('b2'), Atomic('a'), priority=300), priority=400)
        self.assertEquals(result, parser.read_term())
    def test_curlied_multiple_terms(self):
        s = '{a, b, c}.'
        parser = PrologParser(s)
        bc = Compound(',', Atomic('b'), Atomic('c'), priority=1000)
        abc = Compound(',', Atomic('a'), bc, priority=1000)
        term = Compound('{}', abc)
        self.assertEquals(term, parser.read_term())
    def test_malformed_curlied_term(self):
        malformed = ('{1, 2, , 4}.', '{1 @ 2 @ 4}.', '{1, 2.', '{1, 2,}.')
        for term in malformed:
            parser = PrologParser(term)
            self.assertRaises(InvalidTermException, parser.read_term)
    def test_list(self):
        l = '[foo, a, b].'
        parser = PrologParser(l)
        term = parser.read_term()
        self.assertEquals('.', term.name)
        self.assertEquals(2, term.arity)


from prologlib.core import Substitution

class VariableTest(unittest.TestCase):
     def test_apply_mgu_to_first_variable_in_chain(self):
        v = Variable('P')
        v.value = Variable('Q')
        v.value.value = Variable('R')
        mgu = Substitution()
        mgu['P'] = Atomic(3)
        v.apply(mgu)
        self.assertEquals(Atomic(3), v.binding())
     def test_apply_mgu_to_middle_variable_in_chain(self):
        v = Variable('P')
        v.value = Variable('Q')
        v.value.value = Variable('R')
        v.value.value.value = Variable('S')
        mgu = Substitution()
        mgu['Q'] = Atomic(3)
        v.apply(mgu)
        self.assertEquals(Atomic(3), v.binding())
     def test_apply_mgu_to_last_variable_in_chain(self):
        v = Variable('P')
        v.value = Variable('Q')
        v.value.value = Variable('R')
        mgu = Substitution()
        mgu['R'] = Atomic(3)
        v.apply(mgu)
        self.assertEquals(Atomic(3), v.binding())

if __name__ == '__main__':
    unittest.main()
