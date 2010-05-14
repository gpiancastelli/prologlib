'''
Stuff dealing with Prolog knowledge bases.
'''

class Database:
    '''The database is the set of user-defined procedures
    which currently exist during execution. Each procedure
    is identified by a unique predicate indicator.'''
    
    def __init__(self):
        self._db = {}

    def search(self, activator):
        '''Search the database for a procedure p whose predicate
        indicator corresponds to the functor and arity of activator.'''
        pi = activator.predicate_indicator()
        return self._db.get(pi)

    def assert_clause(self, term, append=True):
        '''Used by assertz/1 (with append=True) and asserta/1 (with append=False).'''
        from .core import convert_to_goal, PrologPermissionError
        from .parser import Compound
        head = term.head()
        body = convert_to_goal(term.body())
        if not body:
            from .core import PrologTypeError
            raise PrologTypeError('callable', term.body())
        clause = Compound(':-', head, body)
        pi = head.predicate_indicator()
        # TODO Check that pi is 'correct'
        procedure = self._db.get(pi)
        if procedure:
            if not procedure._dynamic:
                from .core import PrologPermissionError
                from .parser import Compound, Atomic
                pi = Compound('/', Atomic(head.name), Atomic(head.arity))
                raise PrologPermissionError('modify', 'static_procedure', pi)
            if append:
                procedure.assertz(clause)
            else:
                procedure.asserta(clause)
        else:
            self._db[pi] = Procedure(pi, clause, dynamic=True, public=True)

    def retract(self, clause):
        head = clause.head()
        pi = head.predicate_indicator()
        procedure = self._db.get(pi)
        if procedure:
            if not procedure._dynamic:
                from .core import PrologPermissionError
                from .parser import Compound, Atomic
                pi = Compound('/', Atomic(head.name), Atomic(head.arity))
                raise PrologPermissionError('modify', 'static_procedure', pi)
            procedure.retract(clause)

    def abolish(self, pi):
        indicator = '{0}/{1}'.format(*pi.value[1:])
        procedure = self._db.get(indicator)
        if procedure:
            if not procedure._dynamic:
                from .core import PrologPermissionError
                raise PrologPermissionError('modify', 'static_procedure', pi)
            del self._db[indicator]
                

    def append(self, term):
        '''Used to consult a user-defined knowledge base; it skips
        any kind of error detection w.r.t. assert_clause'''
        from .core import convert_to_goal
        from .parser import Compound
        clause = Compound(':-', term.head(), convert_to_goal(term.body()))
        pi = term.head().predicate_indicator()
        # TODO Check that pi is 'correct'
        procedure = self._db.get(pi)
        if procedure:
            procedure.assertz(clause)
        else:
            self._db[pi] = Procedure(pi, clause)

    def insert(self, pi, dynamic=True, public=True):
        self._db[pi] = Procedure(pi, dynamic=dynamic, public=public)

    def __iter__(self):
        return self._db.__iter__()

    def __eq__(self, other):
        if not hasattr(other, '_db'):
            return False
        return self._db.__eq__(other._db)

    def __repr__(self):
        return self._db.__repr__()


class Procedure:
    '''A user-defined procedure is a sequence of (zero or
    more) clauses prepared for execution.

    Each procedure is either dynamic or static. A clause of a
    dynamic procedure can be altered, a clause of a static
    procedure cannot be altered. By default a user-defined
    procedure shall be static, but a directive with indicator
    dynamic/1 in Prolog text overrides the default, and
    asserting a clause of a non-existent procedure shall
    create a dynamic procedure.

    Each procedure is either public or private. A clause of a
    public procedure can be inspected, a clause of a private
    procedure cannot be inspected. A dynamic procedure shall
    be public, and a static user-defined procedure shall be
    private by default.'''
    
    def __init__(self, pi, term=None, dynamic=False, public=False):
        self._pi = pi
        self._clauses = [term] if term else []
        self._dynamic = dynamic
        self._public = public

    def asserta(self, clause):
        self._clauses.insert(0, clause)

    def assertz(self, clause):
        self._clauses.append(clause)

    def retract(self, clause):
        if clause in self._clauses:
            self._clauses.remove(clause)

    def clauses(self):
        return self._clauses

    def __eq__(self, other):
        if not hasattr(other, '_clauses'):
            return False
        return self._clauses.__eq__(other._clauses)

    def __repr__(self):
        dynamic = 'd' if self._dynamic else '_'
        public = 'p' if self._public else '_'
        return '({0} {1} {2})'.format(self._pi, len(self._clauses), dynamic + public)
