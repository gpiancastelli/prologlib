import errno
import sys

from .core import Engine, PrologError
from .parser import PrologParser, InvalidTermException

__version__ = 'UNRELEASED'
__date__ = '2010-05-14'

header = """
prologlib {} ({})
Type 'help.' for help, 'halt.' to exit.
""".lstrip().format(__version__, __date__)

HELP_TEXT = """
Welcome to prologlib, an ISO Prolog processor written in Python 3.

Type a goal at the '?-' prompt, press Return to issue it to the Prolog
processor.

If the goal fails, you will get 'no.' as an answer.

If the goal succeeds, you will get a valid substitution in resposnse (or a
blank line if the substitution is empty), followed by a '?' prompt that you can
use to ask for further solutions to the goal. Type ';' (a semicolon) followed
by Return if you want another solution, just press Return if you are satisfied
with what you already got. A solution is followed by 'yes.' as an answer.

Ask help for any ISO builtin predicate by using help/1, e.g. help(findall/3).
Please note that if the predicate functor is also an operator, you have to
enclose it in parentheses, e.g. help((is)/2).

To exit the interactive toplevel, type 'halt.' and press Return.
""".strip()


class Console:

    def __init__(self, stdin=sys.stdin, stdout=sys.stdout):
        self.stdin = stdin
        self.stdout = stdout
        self.engine = Engine()

    def solveloop(self):
        self.write(header)
        stop = None
        while not stop:
            self.stdout.write('?- ')
            self.stdout.flush()
            line = self.stdin.readline().strip()
            if not line:
                continue
            try:
                goal = self.read_term(line)
                if hasattr(self, 'do_' + goal.name):
                    cmd = getattr(self, 'do_' + goal.name)
                    stop = cmd(goal)
                    if not stop:
                        self.write('yes.')
                else:
                    # TODO Resolve the line/goal ambiguity
                    self.solve(line) # yeah, should be goal
            except InvalidTermException as ite:
                self.write('SyntaxError: {}'.format(ite))
            except IOError as ioe:
                if ioe.errno == errno.ENOENT:
                    message = str(ioe)
                    name = message[message.rfind(':')+2:]
                    self.write('Error: the file {} cannot be found'.format(name))
                else:
                    raise

    def read_term(self, line):
        parser = PrologParser(line)
        return parser.read_term()

    def solve(self, goal):
        try:
            result = self.engine.solve(goal)
            if result:
                self.write('')
                subst = self.engine.currsubst()
                for variable in sorted(subst):
                    if not variable.startswith('_'):
                        self.write(variable + ' = ' + subst[variable]._touiform())
                #if self.engine.haschoicepoint():
                if len(self.engine._s) > 1:
                    self.solvenextloop()
                else:
                    self.write('yes.')
            else:
                self.write('no.')
        except PrologError as e:
            self.write('Error: {0}'.format(e.error_term()))

    def solvenextloop(self):
        #while self.engine.haschoicepoint()
        while len(self.engine._s) > 1:
            self.stdout.write(' ? ')
            self.stdout.flush()
            line = self.stdin.readline().strip()
            if not line:
                self.write('yes.')
                break
            elif line == ';':
                result = self.engine.solve_next()
                if result:
                    self.write('')
                    subst = self.engine.currsubst()
                    for variable in sorted(subst):
                        if not variable.startswith('_'):
                            self.write(variable + ' = ' + subst[variable]._touiform())
                else:
                    self.write('no.')
            else:
                self.write('Type ; and press Return to ask for another solution,')
                self.write('or just press Return to accept the current solution.')

    def write(self, message):
        message = '{0}\n'.format(message)
        self.stdout.write(message)

    def do_clear(self, goal):
        self.engine._clear()

    def do_consult(self, goal):
        f = goal.value[1].value
        with open(f) as theory:
            self.engine._consult(theory)
            
    def do_listing(self, goal):
        for procedure in self.engine._kb:
            self.write(str(procedure))

    def do_help(self, goal):
        if goal.arity == 0:
            self.write(HELP_TEXT)
        else:
            self.help(goal.value[1])

    def help(self, indicator):
        from .builtin import search_builtin
        term = mock_term(*indicator.value[1:])
        procedure = search_builtin(term)
        if procedure:
            self.write(procedure.__doc__)
        else:
            pi = '{}/{}'.format(*indicator.value[1:])
            self.write('No built-in predicate known with indicator: ' + pi)


def mock_term(name, arity):
    '''Create a fake term to use as a key for searching in the set of
    available builtin predicates, so as to retrieve the documentation'''
    from .parser import Compound, Variable
    t = tuple(Variable('_') for i in range(arity.value))
    term = Compound(name.name, *t)
    return term
