from ..parser import Atomic, Compound, List, Variable, isvariable, islist, isatom, isnumber
from ..core import BuiltIn

class IOBuiltIn(BuiltIn):
    """A specialized version of a Prolog built-in for I/O predicates."""

    def load_stream(self, soa):
        if isvariable(soa):
            self.throw_instantiation_error()
        if isstream(soa):
            if str(soa) not in stream_terms:
                self.throw_existence_error('stream', soa)
            stream = stream_terms[str(soa)]
            if stream.stream.closed:
                self.throw_existence_error('stream', soa)
        elif str(soa) in stream_aliases:
            stream = stream_aliases[str(soa)]
        else:
            self.throw_domain_error('stream_or_alias', soa)
        return stream


class Stream:
    """
    A stream provides a logical view of a source/sink.

    A source/sink is a fundamental notion. A program can output results to a
    sink or input Prolog data from a source. A source/sink may be a file, the
    user's terminal, or other implementation defined possibility permitted by
    the processor.

    Each source/sink is associated with a finite or potentially infinite
    sequence of bytes or characters. A source/sink always has a beginning, but
    has an end only if it is finite.
    """

    OPTIONS_ALLOWED = ('type', 'reposition', 'alias', 'eof_action')
    MODES_ALLOWED = ('read', 'write', 'append')

    def __init__(self, stream, alias='', mode='r', type='text'):
        self.stream = stream
        self.name = Compound('file_name', Atomic(stream.name))
        self.stream_term = Compound('$stream', Atomic(stream.fileno()))
        self.alias = alias
        self.mode = mode
        self.type = type
        # TODO Missing eof_action(A)

    def isinput(self):
        return self.mode == 'r'

    def isoutput(self):
        return self.mode in ('w', 'a')

    def alias_term(self):
        return Compound('alias', Atomic(self.alias))

    def mode_term(self):
        return Compound('mode', Atomic(self.mode))

    def reposition_term(self):
        b = 'true' if self.stream.seekable() else 'false'
        return Compound('reposition', Atomic(b))

    def type_term(self):
        return Compound('type', Atomic(self.type))

    def properties(self):
        '''Returns a list containing all the properties of this stream. This
        method is used by stream_property/2, which is the only means to inspect
        the properties. Direct access to a single property is required only for
        the special case of end_of_stream.'''
        p = []
        p.append(self.name)
        p.append(self.mode_term())
        if self.isinput():
            p.append(Atomic('input'))
        if self.isoutput():
            p.append(Atomic('output'))
        if self.alias:
            p.append(self.alias_term())
        if self.stream.seekable():
            p.append(Compound('position', Atomic(self.stream.tell())))
        if self.at_end_of_stream():
            p.append(Compound('end_of_stream', Atomic('at')))
        else:
            # We do not support past-end-of-stream position
            p.append(Compound('end_of_stream', Atomic('not')))
        p.append(self.reposition_term())
        # TODO Missing eof_action(A)
        p.append(self.type_term())
        return p

    def at_end_of_stream(self):
        '''Python streams do not allow the user to know if they are in a EOF
        position unless you are reading data from the stream; in this case, EOF
        corresponds to an input equal to an empty string. So, to support this
        Prolog stream-property, we adopt the following strategy: store the
        current position in the stream; read one character; reposition the
        stream to its previous position (some characters take more than one
        byte, so we just cannot do a +1 addition). If what we did read is the
        empty string, we indeed are at the end of stream.'''
        if not self.stream.seekable():
            return True
        position = self.stream.tell()
        eof = self.stream.read(1)
        self.stream.seek(position)
        return eof == ''

    def peek(self):
        # in accordance with at_end_of_stream, if a stream cannot be
        # repositioned, peek responds as if the stream is at end_of_stream
        if not self.stream.seekable():
            return ''
        position = self.stream.tell()
        char = self.stream.read(1)
        self.stream.seek(position)
        return char

    def read_term(self):
        from ..parser import PrologParser
        parser = PrologParser(self.stream)
        term = parser.read_term()
        return term if term else Atomic('')

    def write(self, s):
        self.stream.write(s)


import errno
import sys

# The standard input/output streams cannot be closed.
STANDARD_INPUT_STREAM = Stream(sys.stdin, 'user_input', 'r')
STANDARD_OUTPUT_STREAM = Stream(sys.stdout, 'user_output', 'w')

current_input_stream = STANDARD_INPUT_STREAM
current_output_stream = STANDARD_OUTPUT_STREAM

# Any stream may be associated with a stream alias (represented as an atom)
# which may be used to refer to that stream. The association is created when
# a stream is opened and automatically ends when the stream is closed. A
# particular alias shall refer to at most one stream at any time.
stream_aliases = {STANDARD_INPUT_STREAM.alias : STANDARD_INPUT_STREAM,
                  STANDARD_OUTPUT_STREAM.alias : STANDARD_OUTPUT_STREAM}

stream_terms = {str(STANDARD_INPUT_STREAM.stream_term) : STANDARD_INPUT_STREAM,
                str(STANDARD_OUTPUT_STREAM.stream_term) : STANDARD_OUTPUT_STREAM}

###
### Stream selection and control (ISO 8.11)
###

class CurrentInput_1(IOBuiltIn):
    '''current_input(?stream)

    current_input(Stream) is true iff the stream-term Stream identifies the
    current input stream.'''

    def execute(self, stream):
        if not isvariable(stream) and not isstream(stream):
            self.throw_domain_error('stream', stream)

        return self.unify(stream, current_input_stream.stream_term)


class CurrentOutput_1(IOBuiltIn):
    '''current_output(?stream)

    current_output(Stream) is true iff the stream-term Stream identifies the
    current output stream.'''

    def execute(self, stream):
        if not isvariable(stream) and not isstream(stream):
            self.throw_domain_error('stream', stream)

        return self.unify(stream, current_output_stream.stream_term)


class SetInput_1(IOBuiltIn):
    '''set_input(@stream_or_alias)

    set_input(S_or_a) is true. It also has the side effect of setting the
    current input stream to be the stream associated with stream-term or
    alias S_or_a.'''

    def execute(self, soa):
        stream = self.load_stream(soa)
        if stream.isoutput():
            self.throw_permission_error('input', 'stream', soa)

        current_input_stream = stream
        return True


class SetOutput_1(IOBuiltIn):
    '''set_output(@stream_or_alias)

    set_output(S_or_a) is true. It also has the side effect of setting the
    current output stream to be the stream associated with stream-term or
    alias S_or_a.'''

    def execute(self, soa):
        stream = self.load_stream(soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)

        current_output_stream = stream
        return True


class Open_4(IOBuiltIn):
    '''open(@source_sink, @io_mode, -stream, @stream_options)

    open(Source_sink, Mode, Stream, Options) is true. It has the side effect
    of opening the source/sink Source_sink for input or output as indicated
    by input/output mode Mode and the list of stream-options Options.'''

    def execute(self, ss, mode, stream, options):
        args = {}
        if isvariable(ss) or isvariable(mode):
            self.throw_instantiation_error()
        if not isvariable(ss) and isstream(ss) and str(ss) not in stream_terms:
            self.throw_domain_error('source_sink', ss)
        if not islist(options):
            self.throw_type_error('list', options)
        for option in options.as_list():
            if isvariable(option):
                self.throw_instantiation_error()
            if option.name not in Stream.OPTIONS_ALLOWED:
                self.throw_domain_error('stream_option', option)
            args[option.name] = option.value[1].name
        if not isvariable(mode) and not isatom(mode):
            self.throw_type_error('atom', mode)
        if not isvariable(stream):
            self.throw_type_error('variable', stream)
        if isatom(mode) and mode.name not in Stream.MODES_ALLOWED:
            self.throw_domain_error('io_mode', mode)
        if 'alias' in args and args['alias'] in stream_aliases:
            alias = Compound('alias', Atomic(args['alias']))
            self.throw_permission_error('open', 'source_sink', alias)
        
        args['mode'] = mode.name
        m = dict(zip(Stream.MODES_ALLOWED, ('r', 'w', 'a')))[mode.name]
        if args.get('type') == 'binary':
            m += 'b'
        try:
            s = open(ss.name, m)
        except IOError as e:
            if e.errno == errno.EINVAL:
                self.throw_permission_error('open', 'source_sink', ss)
            elif e.errno == errno.ENOENT:
                self.throw_existence_error('source_sink', ss)
            else:
                # TODO Quite incomplete as an error specification...
                #      Could we use PrologSystemError for other I/O error
                #      conditions not explicitly handled by the specification?
                raise e
        if 'reposition' in args and args['reposition'] and not s.seekable():
            reposition = Compound('reposition', Atomic.TRUE)
            self.throw_permission_error('open', 'source_sink', reposition)

        st = Stream(s, **args)
        if st.alias:
            stream_aliases[st.alias] = st
        stream_terms[str(st.stream_term)] = st
        self.unify(stream, st.stream_term)
        return True


class Open_3(IOBuiltIn):
    '''open(@source_sink, @io_mode, -stream)

    The built-in predicate open/3 provides similar funcionality to open/4
    except that a goal open(Source_sink, Mode, Stream) opens the source/sink
    Source_sink with an empty list of stream-options.'''

    def execute(self, ss, mode, stream):
        o = Open_4(self.kb)
        result = o.execute(ss, mode, stream, List.EMPTY_LIST)
        self.substitution.update(o.substitution)
        return result


class Close_2(IOBuiltIn):
    '''close(@stream_or_alias, @close_options)

    close(S_or_a, Options) is true. It has the side effect of closing the
    stream associated with stream-term or alias S_or_a if it is open. The
    behaviour of this built-in predicate may be modified by specifying a
    list of close-options in the Options parameter.'''

    def execute(self, soa, options):
        # TODO close options (and error condition 8.11.6.3-e) are not supported
        if not islist(options):
            self.throw_type_error('list', options)
        for option in options.as_list():
            if isvariable(option):
                self.throw_instantiation_error()
        stream = self.load_stream(soa)
        # Don't know if this is necessary... there is this same control in
        # load_stream, and I'd argue that aliased stream are always open...
        if stream.stream.closed:
            self.throw_existence_error('stream', soa)

        # We suppose that Python automatically flushes an output stream when it
        # gets closed (see step 8.11.6.1-b in close/2's procedural definition)
        if stream == STANDARD_INPUT_STREAM or stream == STANDARD_OUTPUT_STREAM:
            return True
        global current_input_stream
        if stream == current_input_stream:
            current_input_stream = STANDARD_INPUT_STREAM
        global current_output_stream
        if stream == current_output_stream:
            current_output_stream = STANDARD_OUTPUT_STREAM
        stream.stream.close()
        if str(soa) in stream_aliases:
            del stream_aliases[str(soa)]
        return True


class Close_1(IOBuiltIn):
    '''close(@stream_or_alias)

    The built-in predicate close/1 provides similar functionality to close/2
    except that a goal close(S_or_a) closes, with an empty list of
    close-options, the stream associated with stream-term or alias S_or_a if
    it is open.'''

    def execute(self, soa):
        c = Close_2(self.kb)
        return c.execute(soa, List.EMPTY_LIST)


class FlushOutput_1(IOBuiltIn):
    '''flush_output(@stream_or_alias)

    flush_output(S_or_a) is true. It has the side effect of sending any output
    which is currently buffered by the processor for the stream associated with
    stream-term or alias S_or_a to that stream.'''

    def execute(self, soa):
        stream = self.load_stream(soa)
        if stream.stream.closed:
            self.throw_existence_error('stream', soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)

        stream.stream.flush()
        return True


class FlushOutput_0(IOBuiltIn):
    '''flush_output

    The built-in predicate flush_output/0 provides similar functionality to
    flush_output/1 except that a goal flush_output flushes the current output
    stream.'''

    def execute(self):
        current_output_stream.stream.flush()
        return True


class StreamProperty_2(IOBuiltIn):
    '''stream_property(?stream, ?stream_property)

    stream_property(Stream, Property) is true iff the stream associated with
    the stream-term Stream has stream property Property.'''

    def execute(self, stream, property):
        if not isvariable(stream) and not isstream(stream):
            self.throw_domain_error('stream', stream)
        # TODO check that property is a variable or a stream proprety (8.11.8.3-c)
        if not isvariable(stream) and str(stream) not in stream_terms:
            self.throw_existence_error('stream', stream)

        if isvariable(stream):
            streams = [v for v in stream_terms.values()]
        else:
            streams = [stream_terms[str(stream)]]
        self.data = []
        for s in streams:
            from .. import core
            self.data += [(s, p) for p in s.properties() if core.unify(property, p) is not None]
        return self.pick_data(stream, property)

    def reexecute(self, stream, property):
        self.reset_substitution()
        return self.pick_data(stream, property)

    def pick_data(self, stream, property):
        if not self.data:
            return False
        s, p = self.data.pop(0)
        return self.unify(stream, s.stream_term) and self.unify(property, p)


class AtEndOfStream_0(IOBuiltIn):
    '''at_end_of_stream

    The built-in predicate at_end_of_stream/0 examines the single
    stream-property end_of_stream/1. A goal at_end_of_stream is true iff the
    current input stream has a stream position end-of-stream or
    past-end-of-stream.'''

    def execute(self):
        return current_input_stream.at_end_of_stream()


class AtEndOfStream_1(IOBuiltIn):
    '''at_end_of_stream(@stream_or_alias)

    The built-in predicate at_end_of_stream/1 examines the single
    stream-property end_of_stream/1. A goal at_end_of_stream(S_or_a) is true
    iff the stream associated with stream-term of alias S_or_a has a stream
    position end-of-stream or past-end-of-stream.'''

    def execute(self, soa):
        if isvariable(soa):
            self.throw_instantiation_error()

        stream_term = soa
        if str(soa) in stream_aliases:
            stream_term = stream_aliases[str(soa)].stream_term
        aeos = StreamProperty_2(self.kb)
        eos = Compound('end_of_stream', Variable('E'))
        aeos.execute(stream_term, eos)
        return aeos.substitution['E'] == Atomic('at')


class SetStreamPosition_2(IOBuiltIn):
    '''set_stream_position(@stream_or_alias, @stream_position)

    set_stream_position(S_or_a, Position) is true. It has the side effect of
    setting the stream position of the stream associated with stream-term or
    alias S_or_a to Position. Normally, Position will previously have been
    returned as a position/1 stream property of the stream.'''

    def execute(self, soa, position):
        if isvariable(position):
            self.throw_instantiation_error()
        if not self._isposition(position):
            self.throw_domain_error('stream_position', position)
        stream = self.load_stream(soa)
        if not stream.stream.seekable():
            self.throw_permission_error('reposition', 'stream', soa)

        p = position.value[1].value
        stream.stream.seek(p)
        return True

    def _isposition(self, p):
        if not isinstance(p, Compound):
            return False
        if p.name != 'position' or p.arity != 1:
            return False
        if not isinstance(p.value[1].value, int):
            return False
        return True
            
###
### Character input/output (ISO 8.12)
###

class GetChar_2(IOBuiltIn):
    '''get_char(@stream_or_alias, ?in_character)

    get_char(S_or_a, Char) is true iff Char unifies with the next character
    to be input from the target stream. When reading the end of a stream,
    Char will be unified with the atom end_of_file.'''

    def execute(self, soa, char):
        # The stream-position past-end-of-stream is not supported,
        # so error condition 8.12.1.3-h is not checked for.
        if not isvariable(char) and not isatom(char):
            self.throw_type_error('in_character', char)
        if isatom(char) and len(str(char)) != 1:
            self.throw_type_error('in_character', char)
        stream = self.load_stream(soa)
        if stream.isoutput():
            self.throw_permission_error('input', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = stream.stream.read(1)
        if c == '':
            # eof_action(A) is not supported
            return self.unify(char, Atomic('end_of_file'))
        else:
            return self.unify(char, Atomic(c))


class GetChar_1(IOBuiltIn):
    '''get_char(?in_character)

    The built-in predicate get_char/1 provides similar functionality to
    get_char/2. Goal get_char(Char) unfies Char with a one-char atom whose
    name is the next character to be input from the current input stream.'''

    def execute(self, char):
        if not isvariable(char) and not isatom(char):
            self.throw_type_error('in_character', char)
        if isatom(char) and len(str(char)) != 1:
            self.throw_type_error('in_character', char)
        if current_input_stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = current_input_stream.stream.read(1)
        if c == '':
            # eof_action(A) is not supported
            return self.unify(char, Atomic('end_of_file'))
        else:
            return self.unify(char, Atomic(c))


class GetCode_2(IOBuiltIn):
    '''get_code(@stream_or_alias, ?in_character_code)

    The built-in predicate get_code/1 provides similar functionality to
    get_char/2. Goal get_code(S_or_a, Code) unify Code with the character
    code of the next character to be input from the target stream.'''

    def execute(self, soa, code):
        # The stream-position past-end-of-stream is not supported,
        # so error condition 8.12.1.3-h is not checked for.
        if not isvariable(code) and not isnumber(code):
            self.throw_type_error('integer', code)
        if isnumber(code) and not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        stream = self.load_stream(soa)
        if stream.isoutput():
            self.throw_permission_error('input', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = stream.stream.read(1)
        if c == '':
            # eof_action(A) is not supported
            return self.unify(code, Atomic(-1))
        else:
            return self.unify(code, Atomic(ord(c)))


class GetCode_1(IOBuiltIn):
    '''get_code(?in_character_code)

    The built-in predicate get_code/1 provide similar functionality to
    get_code/2. Goal get_code(Code) unfies Code with the character code
    of the next character to be input from the current input stream.'''

    def execute(self, code):
        if not isvariable(code) and not isnumber(code):
            self.throw_type_error('integer', code)
        if isnumber(code) and not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        if current_input_stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = current_input_stream.stream.read(1)
        if c == '':
            # eof_action(A) is not supported
            return self.unify(code, Atomic(-1))
        else:
            return self.unify(code, Atomic(ord(c)))


class PeekChar_2(IOBuiltIn):
    '''peek_char(@stream_or_alias, ?in_character)

    peek_char(S_or_a, Char) is true iff Char unifies with the next character
    to be input from the target stream. The goal peek_char(S_or_a, Char) leaves
    unaltered the stream position of the target stream.'''

    def execute(self, soa, char):
        # The stream-position past-end-of-stream is not supported,
        # so error condition 8.12.2.3-h is not checked for.
        if not isvariable(char) and not isatom(char):
            self.throw_type_error('in_character', char)
        if isatom(char) and len(str(char)) != 1:
            self.throw_type_error('in_character', char)
        stream = self.load_stream(soa)
        if stream.isoutput():
            self.throw_permission_error('input', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = stream.peek()
        if c == '':
            # eof_action(A) is not supported
            return self.unify(char, Atomic('end_of_file'))
        else:
            return self.unify(char, Atomic(c))


class PeekChar_1(IOBuiltIn):
    '''peek_char(?in_character)

    The built-in predicate peek_char/1 provides similar functionality to
    peek_char/2. Goal peek_char(Char) unifies Char with a one-char atom
    whose name is the next character to be input.'''

    def execute(self, char):
        if not isvariable(char) and not isatom(char):
            self.throw_type_error('in_character', char)
        if isatom(char) and len(str(char)) != 1:
            self.throw_type_error('in_character', char)
        if current_input_stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = current_input_stream.peek()
        if c == '':
            # eof_action(A) is not supported
            return self.unify(char, Atomic('end_of_file'))
        else:
            return self.unify(char, Atomic(c))


class PeekCode_2(IOBuiltIn):
    '''peek_code(@stream_or_alias, ?in_character_code)

    The built-in predicate peek_code/2 provides similar functionality to
    peek_char/2. Goal peek_code(S_or_a, Code) unify Code with the character
    code of the next character to be input from the target stream.'''

    def execute(self, soa, code):
        # The stream-position past-end-of-stream is not supported,
        # so error condition 8.12.1.3-h is not checked for.
        if not isvariable(code) and not isnumber(code):
            self.throw_type_error('integer', code)
        if isnumber(code) and not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        stream = self.load_stream(soa)
        if stream.isoutput():
            self.throw_permission_error('input', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = stream.peek()
        if c == '':
            # eof_action(A) is not supported
            return self.unify(code, Atomic(-1))
        else:
            return self.unify(code, Atomic(ord(c)))


class PeekCode_1(IOBuiltIn):
    '''peek_code(?in_character_code)

    The built-in predicate peek_code/1 provides similar functionality to
    peek_code/2. Goal peek_code(Code) unfies Code with the character code
    of the next character to be input from the current input stream.'''

    def execute(self, code):
        if not isvariable(code) and not isnumber(code):
            self.throw_type_error('integer', code)
        if isnumber(code) and not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        if current_input_stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        c = current_input_stream.peek()
        if c == '':
            # eof_action(A) is not supported
            return self.unify(code, Atomic(-1))
        else:
            return self.unify(code, Atomic(ord(c)))


class PutChar_2(IOBuiltIn):
    '''put_char(@stream_or_alias, +character)

    put_char(S_or_a, Char) is true. It has the side effect of outputting the
    character C which is the name of the one-char atom Char to the target
    stream.'''

    def execute(self, soa, char):
        if isvariable(char):
            self.throw_instantiation_error()
        if not isatom(char) or len(str(char)) != 1:
            self.throw_type_error('character', char)
        stream = self.load_stream(soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        stream.write(str(char))
        return True


class PutChar_1(IOBuiltIn):
    '''put_char(+character)

    The built-in predicate put_char/1 provides similar functionality to
    put_char/2. A goal put_char(Char) outputs the character which is the
    name of Char to the current output stream.'''

    def execute(self, char):
        if isvariable(char):
            self.throw_instantiation_error()
        if not isatom(char) or len(str(char)) != 1:
            self.throw_type_error('character', char)
        if current_output_stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        current_output_stream.write(str(char))
        return True


class PutCode_2(IOBuiltIn):
    '''put_code(@stream_or_alias, +character_code)

    The built-in predicate put_code/2 provides similar functionality to
    put_char/2. A goal put_code(S_or_a, Code) outputs the character whose
    character code is Code to the target stream.'''

    def execute(self, soa, code):
        if isvariable(code):
            self.throw_instantiation_error()
        if not isnumber(code) or not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        stream = self.load_stream(soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        try:
            c = chr(code.value)
        except (ValueError, OverflowError, UnicodeDecodeError):
            self.throw_representation_error(code)
        stream.write(c)
        return True


class PutCode_1(IOBuiltIn):
    '''put_code(+character_code)

    The built-in predicate put_code/1 provides similar functionality to
    put_code/2. A goal put_code(Code) outputs the character whose character
    code is Code to the current output stream.'''

    def execute(self, code):
        if isvariable(code):
            self.throw_instantiation_error()
        if not isnumber(code) or not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        if current_output_stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        try:
            c = chr(code.value)
        except (ValueError, OverflowError, UnicodeDecodeError):
            self.throw_representation_error(code)
        current_output_stream.write(c)
        return True


class NL_1(IOBuiltIn):
    '''nl(@stream_or_alias)

    nl(S_or_a) is true. It has the side effect of outputting the newline
    character to the target stream.'''

    def execute(self, soa):
        stream = self.load_stream(soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        stream.write('\n')
        return True


class NL_0(IOBuiltIn):
    '''nl

    The built-in predicate nl/0 provides similar functionality to nl/1. A goal
    nl outputs the newline character to the current output stream.'''

    def execute(self):
        if current_output_stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        current_output_stream.write('\n')
        return True

###
### TODO Byte input/output (ISO 8.13)
###

###
### Term input/output (ISO 8.14)
###

class Read_2(IOBuiltIn):
    '''read(@stream_or_alias, ?term)

    read(S_or_a, Term) is true iff Term unifies with T, where T. is a read-term
    which has been constructed by inputting and parsing characters from the
    target stream.'''

    def execute(self, soa, term):
        # The stream-position past-end-of-stream is not supported,
        # so error condition 8.14.1.3-i is not checked for. Flags
        # max_arity, max_integer and min_integer are not supported,
        # so error condition 8.14.1.3-j is not checked for.
        stream = self.load_stream(soa)
        if stream.isoutput():
            self.throw_permission_error('input', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('input', 'binary_stream', soa)

        from ..parser import InvalidTermException
        try:
            t = stream.read_term()
        except InvalidTermException as e:
            self.throw_syntax_error(Atomic(str(e)))
        return self.unify(term, t)


class Read_1(IOBuiltIn):
    '''read(?term)

    The built-in predicate read/1 provides similar functionality to read/2,
    but goal read(Term) input terms from the currenti input stream.'''

    def execute(self, term):
        from ..parser import InvalidTermException
        try:
            t = current_input_stream.read_term()
        except InvalidTermException as e:
            self.throw_syntax_error(Atomic(str(e)))
        return self.unify(term, t)


class Write_2(IOBuiltIn):
    '''write(@stream_or_alias, @term)

    write(S_or_a, Term) is true. It has the side effect of outputting Term
    to the target stream in a form which is defined by the following list of
    write-options: [quoted(false), ignore_ops(false), numbervars(true)].'''

    def execute(self, soa, term):
        stream = self.load_stream(soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        stream.write(str(term))
        return True


class Write_1(IOBuiltIn):
    '''write(@term)

    write(Term) is true. It has the side effect of outputting Term to the
    current output stream in a form which is defined by the following list of
    write-options: [quoted(false), ignore_ops(false), numbervars(true)].'''
    
    def execute(self, term):
        current_output_stream.write(str(term))
        return True


class Writeq_2(IOBuiltIn):
    '''writeq(@stream_or_alias, @term)

    writeq(S_or_a, Term) is true. It has the side effect of outputting Term
    to the target stream in a form which is defined by the following list of
    write-options: [quoted(true), ignore_ops(false), numbervars(true)].'''

    def execute(self, soa, term):
        stream = self.load_stream(soa)
        if stream.isinput():
            self.throw_permission_error('output', 'stream', soa)
        if stream.type == 'byte':
            self.throw_permission_error('output', 'binary_stream', soa)

        stream.write(term.toquotedform())
        return True


class Writeq_1(IOBuiltIn):
    '''writeq(@term)

    writeq(Term) is true. It has the side effect of outputting Term to the
    current output stream in a form which is defined by the following list of
    write-options: [quoted(true), ignore_ops(false), numbervars(true)].'''

    def execute(self, term):
        current_output_stream.write(term.toquotedform())
        return True


# Utility functions

def isstream(stream):
    """Check that the term passed as an argument is a stream-term.

    A stream-term is a compound term of the form '$stream'(N), where N is an
    integer that uniquely identify the stream."""

    return ((stream.name == "$stream") and
            stream.arity == 1 and
            isinstance(stream.value[1].value, int))

PREDICATES = {
    # Stream selection and control (ISO 8.11)
    'current_input/1' : CurrentInput_1,
    'current_output/1' : CurrentOutput_1,
    'set_input/1' : SetInput_1,
    'set_output/1' : SetOutput_1,
    'open/4' : Open_4,
    'open/3' : Open_3,
    'close/2' : Close_2,
    'close/1' : Close_1,
    'flush_output/1' : FlushOutput_1,
    'flush_output/0' : FlushOutput_0,
    'stream_property/2' : StreamProperty_2,
    'at_end_of_stream/0' : AtEndOfStream_0,
    'at_end_of_stream/1' : AtEndOfStream_1,
    'set_stream_position/2' : SetStreamPosition_2,
    # Character input/output (ISO 8.12)
    'get_char/2' : GetChar_2,
    'get_char/1' : GetChar_1,
    'get_code/2' : GetCode_2,
    'get_code/1' : GetCode_1,
    'peek_char/2' : PeekChar_2,
    'peek_char/1' : PeekChar_1,
    'peek_code/2' : PeekCode_2,
    'peek_code/1' : PeekCode_1,
    'put_char/2' : PutChar_2,
    'put_char/1' : PutChar_1,
    'put_code/2' : PutCode_2,
    'put_code/1' : PutCode_1,
    'nl/1' : NL_1,
    'nl/0' : NL_0,
    # TODO Byte input/output (ISO 8.13)
    # Term input/output (ISO 8.14)
    'read/2' : Read_2,
    'read/1' : Read_1,
    'write/2' : Write_2,
    'write/1' : Write_1,
    'writeq/2' : Writeq_2,
    'writeq/1' : Writeq_1
}
