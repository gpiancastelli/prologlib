import unittest
from io import StringIO

from prologlib.builtin import io as prologio

from prologlib.core import Engine, PrologError
from prologlib.parser import Atomic, Variable, Compound, List

# Monkeypatching unittest.TestCase.assertRaises to make it return the
# exception object it catches, in order to make some tests on it.
def assertRaises(self, excClass, callableObj, *args, **kwargs):
    try:
        callableObj(*args, **kwargs)
    except excClass as e:
        return e
    else:
        excName = str(getattr(excClass, '__name__', excClass))
        objName = str(getattr(callableObj, '__name__', callableObj))
        raise self.failureException("%s not raised by %s" % (excName, objName))
unittest.TestCase.assertRaises = assertRaises

###
### Stream selection and control (ISO 8.11)
### ISO lacks tests for: close/1-2, flush_output/0-1,
###                      at_end_of_stream/0-1, set_stream_position/2
###

class OpenTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_binary_file(self):
        goal = "open('tests/resources/data', read, D, [type(binary)])."
        self.assertTrue(self.engine.solve(goal))
        st = self.engine.currsubst()['D']
        self.assertEquals('$stream', st.name)
        self.assertTrue(isinstance(st.value[1].value, int))
        stream = prologio.stream_terms[str(st)]
        self.assertEquals('read', stream.mode)
        self.assertEquals('binary', stream.type)
        self.assertFalse(stream.stream.closed)
        # TODO actually close the stream
        del prologio.stream_terms[str(st)]
    def test_file_alias(self):
        goal = "open('tests/resources/editor', write, D, [alias(editor)])."
        self.assertTrue(self.engine.solve(goal))
        st = self.engine.currsubst()['D']
        self.assertEquals('$stream', st.name)
        self.assertTrue(isinstance(st.value[1].value, int))
        stream = prologio.stream_terms[str(st)]
        self.assertEquals('write', stream.mode)
        self.assertEquals('text', stream.type)
        self.assertEquals('editor', stream.alias)
        self.assertEquals(prologio.stream_aliases[stream.alias], stream)
        self.assertFalse(stream.stream.closed)
        stream.stream.close()
        del prologio.stream_terms[str(st)]
        del prologio.stream_aliases[stream.alias]
    def test_text_file(self):
        goal = "open('tests/resources/data.txt', read, D, [])."
        self.assertTrue(self.engine.solve(goal))
        st = self.engine.currsubst()['D']
        self.assertEquals('$stream', st.name)
        self.assertTrue(isinstance(st.value[1].value, int))
        stream = prologio.stream_terms[str(st)]
        self.assertEquals('read', stream.mode)
        self.assertEquals('text', stream.type)
        self.assertFalse(stream.stream.closed)
        stream.stream.close()
        del prologio.stream_terms[str(st)]

class StreamPropertyTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_file_name_property(self):
        goal = 'stream_property(S, file_name(F)).'
        self.assertTrue(self.engine.solve(goal))
        s = Compound('$stream', Atomic(1))
        self.assertEquals(s, self.engine.currsubst()['S'])
        self.assertEquals(Atomic('<stdout>'), self.engine.currsubst()['F'])
        self.assertTrue(self.engine.solve_next())
        s = Compound('$stream', Atomic(0))
        self.assertEquals(s, self.engine.currsubst()['S'])
        self.assertEquals(Atomic('<stdin>'), self.engine.currsubst()['F'])
        self.assertFalse(self.engine.solve_next())
    def test_output_property(self):
        goal = 'stream_property(S, output).'
        self.assertTrue(self.engine.solve(goal))
        s = Compound('$stream', Atomic(1))
        self.assertEquals(s, self.engine.currsubst()['S'])
        self.assertFalse(self.engine.solve_next())

###
### Character input/output (ISO 8.12)
###

class GetCharTest(unittest.TestCase):
    '''This class comprises tests for get_char/2 and get_char/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_current_input_stream_char(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        self.assertTrue(self.engine.solve('get_char(Char).'))
        self.assertEquals(Atomic('q'), self.engine.currsubst()['Char'])
        self.assertEquals('werty', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_stream_char(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("get_char('$stream'(999), Char)."))
        self.assertEquals(Atomic('q'), self.engine.currsubst()['Char'])
        self.assertEquals('werty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_quote(self):
        s = open('tests/resources/quoted_qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("get_char('$stream'(999), Char)."))
        self.assertEquals(Atomic("'"), self.engine.currsubst()['Char'])
        self.assertEquals("qwerty'", s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_wrong_char(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertFalse(self.engine.solve("get_char('$stream'(999), p)."))
        self.assertEquals('werty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_end_of_stream(self):
        s = open('tests/resources/editor')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("get_char('$stream'(999), Char)."))
        self.assertEquals(Atomic('end_of_file'), self.engine.currsubst()['Char'])
        # The stream-position past-end-of-stream is not supported
        del prologio.stream_terms[str(stream.stream_term)]
    def test_output_stream(self):
        goal = 'get_char(user_output, X).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        args = (Atomic('input'), Atomic('stream'), Atomic('user_output'))
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class GetCodeTest(unittest.TestCase):
    '''This class comprises tests for get_code/2 and get_code/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_current_input_stream_code(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        self.assertTrue(self.engine.solve('get_code(Code).'))
        self.assertEquals(Atomic(113), self.engine.currsubst()['Code'])
        self.assertEquals('werty', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_stream_code(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("get_code('$stream'(999), Code)."))
        self.assertEquals(Atomic(113), self.engine.currsubst()['Code'])
        self.assertEquals('werty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_quote(self):
        s = open('tests/resources/quoted_qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("get_code('$stream'(999), Code)."))
        self.assertEquals(Atomic(39), self.engine.currsubst()['Code'])
        self.assertEquals("qwerty'", s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_wrong_code(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertFalse(self.engine.solve("get_code('$stream'(999), 0'p)."))
        self.assertEquals('werty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_end_of_stream(self):
        s = open('tests/resources/editor')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("get_code('$stream'(999), Code)."))
        self.assertEquals(Atomic(-1), self.engine.currsubst()['Code'])
        # The stream-position past-end-of-stream is not supported
        del prologio.stream_terms[str(stream.stream_term)]
    def test_output_stream(self):
        goal = 'get_code(user_output, X).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        args = (Atomic('input'), Atomic('stream'), Atomic('user_output'))
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class PeekCharTest(unittest.TestCase):
    '''This class comprises tests for peek_char/2 and peek_char/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_current_input_stream_char(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        self.assertTrue(self.engine.solve('peek_char(Char).'))
        self.assertEquals(Atomic('q'), self.engine.currsubst()['Char'])
        self.assertEquals('qwerty', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_stream_char(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("peek_char('$stream'(999), Char)."))
        self.assertEquals(Atomic('q'), self.engine.currsubst()['Char'])
        self.assertEquals('qwerty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_quote(self):
        s = open('tests/resources/quoted_qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("peek_char('$stream'(999), Char)."))
        self.assertEquals(Atomic("'"), self.engine.currsubst()['Char'])
        self.assertEquals("'qwerty'", s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_wrong_char(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertFalse(self.engine.solve("peek_char('$stream'(999), p)."))
        self.assertEquals('qwerty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_end_of_stream(self):
        s = open('tests/resources/editor')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("peek_char('$stream'(999), Char)."))
        self.assertEquals(Atomic('end_of_file'), self.engine.currsubst()['Char'])
        del prologio.stream_terms[str(stream.stream_term)]
    # TODO Misses a test on past-end-of-stream and eof_action/1
    def test_output_stream(self):
        goal = 'peek_char(user_output, X).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        args = (Atomic('input'), Atomic('stream'), Atomic('user_output'))
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class PeekCodeTest(unittest.TestCase):
    '''This class comprises tests for peek_code/2 and peek_code/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_current_input_stream_code(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        self.assertTrue(self.engine.solve('peek_code(Code).'))
        self.assertEquals(Atomic(113), self.engine.currsubst()['Code'])
        self.assertEquals('qwerty', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_stream_code(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("peek_code('$stream'(999), Code)."))
        self.assertEquals(Atomic(113), self.engine.currsubst()['Code'])
        self.assertEquals('qwerty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_quote(self):
        s = open('tests/resources/quoted_qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("peek_code('$stream'(999), Code)."))
        self.assertEquals(Atomic(39), self.engine.currsubst()['Code'])
        self.assertEquals("'qwerty'", s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_stream_wrong_code(self):
        s = open('tests/resources/qwerty')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertFalse(self.engine.solve("peek_code('$stream'(999), 0'p)."))
        self.assertEquals('qwerty', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_end_of_stream(self):
        s = open('tests/resources/editor')
        stream = prologio.Stream(s)
        stream.stream_term = Compound('$stream', Atomic(999))
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("peek_code('$stream'(999), Code)."))
        self.assertEquals(Atomic(-1), self.engine.currsubst()['Code'])
        del prologio.stream_terms[str(stream.stream_term)]
    def test_output_stream(self):
        goal = 'peek_code(user_output, X).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        args = (Atomic('input'), Atomic('stream'), Atomic('user_output'))
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class PutCharAndNLTest(unittest.TestCase):
    '''This class comprises tests for put_char/2, put_char/1, nl/0 and nl/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_current_output_stream_char(self):
        s = StringIO('qwer')
        s.seek(4)
        s.name = 'qwer'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        old_current_output_stream = prologio.current_output_stream
        prologio.current_output_stream = stream
        self.assertTrue(self.engine.solve('put_char(t).'))
        s.seek(0)
        self.assertEquals('qwert', s.read())
        prologio.current_output_stream = old_current_output_stream
    def test_stream_char(self):
        s = StringIO('qwer')
        s.seek(4)
        s.name = 'qwer'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("put_char('$stream'(999), 'A')."))
        s.seek(0)
        self.assertEquals('qwerA', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_current_output_stream_newline_and_char(self):
        s = StringIO('qwer')
        s.seek(4)
        s.name = 'qwer'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        old_current_output_stream = prologio.current_output_stream
        prologio.current_output_stream = stream
        self.assertTrue(self.engine.solve('nl, put_char(a).'))
        s.seek(0)
        self.assertEquals('qwer\na', s.read())
        prologio.current_output_stream = old_current_output_stream
    def test_stream_newline_and_char(self):
        s = StringIO('qwer')
        s.seek(4)
        s.name = 'qwer'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        goal = "nl('$stream'(999)), put_char('$stream'(999), a)."
        self.assertTrue(self.engine.solve(goal))
        s.seek(0)
        self.assertEquals('qwer\na', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_char_as_variable(self):
        goal = 'put_char(my_file, C).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_char_as_string(self):
        goal = "put_char(my_file, 'ty')."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('character'), Atomic('ty'))
        self.assertEquals(error, caught.error_term())
    def test_stream_as_variable(self):
        goal = 'nl(Str).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_input_stream(self):
        goal = 'nl(user_input).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        args = (Atomic('output'), Atomic('stream'), Atomic('user_input'))
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class PutCodeTest(unittest.TestCase):
    '''This class comprises tests for put_code/2 and put_code/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_current_output_stream_code(self):
        s = StringIO('qwer')
        s.seek(4)
        s.name = 'qwer'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        old_current_output_stream = prologio.current_output_stream
        prologio.current_output_stream = stream
        self.assertTrue(self.engine.solve("put_code(0't)."))
        s.seek(0)
        self.assertEquals('qwert', s.read())
        prologio.current_output_stream = old_current_output_stream
    def test_stream_code(self):
        s = StringIO('qwer')
        s.seek(4)
        s.name = 'qwer'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("put_code('$stream'(999), 0't)."))
        s.seek(0)
        self.assertEquals('qwert', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_code_as_variable(self):
        goal = 'put_code(my_file, C).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_code_as_string(self):
        goal = "put_code(my_file, 'ty')."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('integer'), Atomic('ty'))
        self.assertEquals(error, caught.error_term())

###
### Term input/output (ISO 8.14)
###

class ReadTermTest(unittest.TestCase):
    '''This class comprises tests for read/2 and read/1.'''
    def setUp(self):
        self.engine = Engine()
    def test_term_from_current_input_stream(self):
        s = StringIO('term1. term2.')
        s.name = 'terms'
        s.fileno = lambda: 999
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        self.assertTrue(self.engine.solve('read(T).'))
        self.assertEquals(Atomic('term1'), self.engine.currsubst()['T'])
        self.assertEquals('term2.', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_term_from_stream(self):
        s = StringIO('term1. term2.')
        s.name = 'terms'
        s.fileno = lambda: 999
        stream = prologio.Stream(s)
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("read('$stream'(999), term1)."))
        self.assertEquals('term2.', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    # TODO Misses a test on read_term/3 with read options
    def test_wrong_term(self):
        s = StringIO('3.1. term2.')
        s.name = 'terms'
        s.fileno = lambda: 999
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        self.assertFalse(self.engine.solve('read(4.1).'))
        self.assertEquals('term2.', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_term_with_undefined_operator(self):
        s = StringIO('foo 123. term2.')
        s.name = 'terms'
        s.fileno = lambda: 999
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        goal = "read(T)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        self.assertEquals('syntax_error', caught.error_term().value[0])
        self.assertEquals('term2.', s.read())
        prologio.current_input_stream = old_current_input_stream
    def test_term_without_end_token(self):
        s = StringIO('3.1')
        s.name = 'terms'
        s.fileno = lambda: 999
        stream = prologio.Stream(s)
        old_current_input_stream = prologio.current_input_stream
        prologio.current_input_stream = stream
        goal = "read(T)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        self.assertEquals('syntax_error', caught.error_term().value[0])
        self.assertEquals('', s.read())
        prologio.current_input_stream = old_current_input_stream

class WriteTermTest(unittest.TestCase):
    '''This class comprises tests for write/1-2 and writeq/1-2.'''
    def setUp(self):
        self.engine = Engine()
    def test_list(self):
        s = StringIO()
        s.name = 'list'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("write('$stream'(999), [1,2,3])."))
        s.seek(0)
        self.assertEquals('[1,2,3]', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    # TODO Missing tests for write_canonical
    def test_quoted_atom(self):
        s = StringIO()
        s.name = 'quoted_atom'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("write('$stream'(999), '1<2')."))
        s.seek(0)
        self.assertEquals('1<2', s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    def test_quoted_atom_with_quotes(self):
        s = StringIO()
        s.name = 'quoted_atom'
        s.fileno = lambda: 999
        stream = prologio.Stream(s, mode='w')
        prologio.stream_terms[str(stream.stream_term)] = stream
        # FIXME Single quotes are not preserved in atoms
        self.assertTrue(self.engine.solve("writeq('$stream'(999), '1<2')."))
        s.seek(0)
        self.assertEquals("'1<2'", s.read())
        del prologio.stream_terms[str(stream.stream_term)]
    # TODO Missing tests for '$VAR'(N) and related write-option numbervars(Bool)
