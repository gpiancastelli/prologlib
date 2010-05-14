from collections import namedtuple

from .kb import Database
from .parser import PrologParser, Atomic, Variable, Compound, List

def search_control_construct(procedure):
    d = {'true/0' : True_0,
         'fail/0' : Fail_0,
         'call/1' : Call_1,
         '!/0' : Cut_0,
         ',/2' : Conjunction_2,
         ';/2' : Disjunction_2,
         '->/2' : IfThen_2,
         'catch/3' : Catch_3,
         'throw/1' : Throw_1
         }
    return d.get(procedure.predicate_indicator())

def search_builtin_predicate(procedure):
    from .builtin import search_builtin
    return search_builtin(procedure)

# A flag is an atom which is associated with a value that is either
# implementation defined or defined by the user. Each flag has a permitted
# range of values; any other value raises a Domain Error.
Flag = namedtuple('Flag', ['name', 'value', 'allowed'])

# In this implementation, flags are stored as simple strings. Atoms are created
# on necessity by the built-in predicates dealing with flags as Prolog terms.
_FLAGS = {
    # When the 'debug' flag is 'off', procedures have the meaning defined
    # by the ISO/IEC 13211 standard; when the value is 'on', the effect of
    # executing any goal shall be implementation defined.
    # TODO This flag is used only for testing purposes
    'debug' : Flag('debug', 'off', ('on', 'off')),
    # The 'unknown' flag defines the effect of attempting to execute a
    # procedure which does not exist in the complete database.
    'unknown' : Flag('unknown', 'error', ('error', 'fail', 'warning')),
    # The 'double_quotes' flag determines the abstract syntax of a double
    # quoted list token appearing in a Prolog text or in a term input by
    # read_term/3. When value is 'chars', a double quoted list is input as a
    # list of one-char atoms; when value is 'codes', a double quoted list is
    # input as a list of character codes; when value is 'atom', a double
    # quoted list is input as an atom.
    'double_quotes' : Flag('double_quotes', 'atom', ('chars', 'codes', 'atom'))
    }


class PrologError(Exception):
    '''A Prolog system error may happen at any stage of execution.
    The conditions in which there shall be a system error, and the
    action taken by the processor after a system error are
    implementation dependent. A system error may happen for
    example (a) in interactions with the operating system (e.g. a
    disk crash or interrupt) or (b) when a goal throw(B) has been
    executed and there is no active goal catch/3.'''
    
    def __init__(self, error):
        self.error = error

    def error_term(self):
        '''Returns a Prolog term that supplies information
        about the original error.'''
        return self.error.value[1]


class PrologInstantiationError(PrologError):
    '''An Instantiation Error is raised when an argument or one of its
    components is a variable, and an instantiated argument or component
    is required.'''

    def __init__(self):
        self.error = Atomic('instantiation_error')


class PrologTypeError(PrologError):
    '''A Type Error is raised when the type of an argument or one of its
    components is incorrect, but not a variable.'''

    def __init__(self, type, culprit):
        self.error = Compound('type_error', Atomic(type), culprit)


class PrologDomainError(PrologError):
    '''A Domain Error is raised when the type of an argument is correct but the
    value is outside the domain for which the procedure is defined.'''

    def __init__(self, domain, culprit):
        self.error = Compound('domain_error', Atomic(domain), culprit)


class PrologExistenceError(PrologError):
    '''An Existence Error is raised when the object on which an operation is to
    be performed does not exist.'''

    def __init__(self, type, culprit):
        self.error = Compound('existence_error', Atomic(type), culprit)


class PrologPermissionError(PrologError):
    '''A Permission Error is raised when it is not permitted to perform a
    specific operation.'''

    def __init__(self, operation, permission_type, culprit):
        args = (Atomic(operation), Atomic(permission_type), culprit)
        self.error = Compound('permission_error', *args)


class PrologRepresentationError(PrologError):
    '''A Representation Error is raised when an implementation defined limit has
    been breached.'''

    def __init__(self, flag):
        self.error = Compound('representation_error', flag)


class PrologEvaluationError(PrologError):
    '''An Evaluation Error is raised when the operands of an evaluable functor
    are such that the operation has an exceptional value.'''

    def __init__(self, type):
        self.error = Compound('evaluation_error', Atomic(type))


class PrologSyntaxError(PrologError):
    '''A Syntax Error is raised when a sequence of characters which are being
    input as a read-term does not conform to the syntax.'''

    def __init__(self, term):
        self.error = Compound('syntax_error', term)
        

# These are other existing Prolog errors that I have not used yet.
class PrologResourceError(PrologError): pass
class PrologSystemError(PrologError): pass


class Substitution(dict):
    '''A substitution represents a number of bindings between
    Prolog variables and terms by using a dictionary of couples
    having a variable name as key and a Prolog term as value.
    Prolog terms may contain variable names that have not yet
    been resolved, i.e. that appear in that very same substitution
    as key of some other term.'''

    def __getitem__(self, k):
        term = t = super().get(k)
        while t is not None and isvariable(t):
            t = super().get(t.name)
            if t is not None:
                term = t
        if term is None:
            for key, value in self.items():
                if k == value.name:
                    term = Variable(key)
        #return term
        return deref(term)

    def __setitem__(self, k, v):
        t = super().get(k)
        if t is None:
            super().__setitem__(k, v)
        else:
            term = t
            # We just suppose that clients calling __setitem__
            # have already performed unification checks on the
            # set of terms that have to be stored in the object.
            # In particular, Substitution DOES NOT perform any
            # kind of unification, but preserves its current
            # state if an attempt to bind a variable already
            # bound to a non-variable term is done.
            while t is not None and isvariable(t):
                t = super().get(t.name)
                if t:
                    term = t
            if term != v:
                if isvariable(term) and isvariable(v):
                    if term.name > v.name:
                        super().__setitem__(term.name, v)
                    else:
                        super().__setitem__(v.name, term)
                elif isvariable(term):
                    super().__setitem__(term.name, v)
                elif isvariable(v):
                    super().__setitem__(v.name, term)

    # Substitution.update accepts just one parameter
    def update(self, subst):
        for k, v in subst.items():
            self.__setitem__(k, v)
        # Simplify bindings between elements in the substitutions
        for term in self.values():
            for variable in term._free_variables():
                if variable.name in self:
                    variable.value = self.__getitem__(variable.name)

    def flatten(self):
        for k in self:
            term = self._flatten(self.__getitem__(k))
            if term != self.get(k):
                super().__setitem__(k, term)

    def _flatten(self, term):
        result = term
        if isinstance(term, Compound):
            args = []
            for arg in term.value[1:]:
                if isvariable(arg):
                    t = self.get(arg.name)
                    if t:
                        args.append(t)
                    else:
                        args.append(arg)
                else:
                    args.append(self._flatten(arg))
            if isinstance(term, List):
                result = List.from_list(args)
            else:
                result = Compound(term.name, *args)
        return result

    def reduce(self):
        '''Removes any binding involving renamed variables.
        To obtain a meaningful result/state, it is supposed
        to be called by clients after Substitution.flatten'''
        for k in tuple(self.keys()):
            v = self[k]
            if (isrenamed(k) or
                (isvariable(v) and isrenamed(v.name))):
                del self[k]
##             else:
##                 self._reduce(k, v)

##     def _reduce(self, key, term):
##         if isinstance(term, Compound):
##             for arg in term.value[1:]:
##                 if isvariable(arg) and isrenamed(arg.name):
##                     del self[key]
##                 else:
##                     self._reduce(key, arg)            


# FIXME The follwing functions are copied from builtin

def isvariable(term):
    return isinstance(term, Variable) and term.isfree()

def isrenamed(variable):
    return variable.find('#') > 0

def isatom(term):
    return term.arity == 0 and not term._isnumber()


class ExecutionState:
    def __init__(self, s_index=1, decsglstk=[], subst=Substitution(), bi='nil'):
        self.s_index = s_index
        self.decsglstk = decsglstk
        self.subst = subst
        self.bi = bi # also 'ctrl', 'bip', up(CL)
    def __repr__(self):
        es = []
        es.append('(' + str(self.s_index))
        es.append('[' + ' '.join(str(decsgl) for decsgl in self.decsglstk) + ']')
        es.append(str(self.subst))
        es.append(str(self.bi) + ')')
        return ', '.join(es)


class DecoratedSubgoal:
    def __init__(self, activator=None, cutparent=0):
        self._activator = activator
        # cutparent is a pointer to a deeper ExecutionState
        # that indicates where control is resumed should a
        # cut be re-executed
        self.cutparent = cutparent
    @property
    def activator(self):
        return self._activator
    @activator.getter
    def activator(self):
        if isinstance(self._activator, Variable):
            return self._activator.binding()
        else:
            return self._activator
    @activator.setter
    def activator(self, a):
        self._activator = a
    def __repr__(self):
        return '%s CP=%d' % (self.activator, self.cutparent)


class ExecutionStack(list):
    def append(self, state):
        state.s_index = len(self) + 1
        super().append(state)


class Engine:
    def __init__(self):
        # execution stack
        self._s = ExecutionStack()
        # user-defined procedures database
        self._kb = Database()

    def currstate(self):
        '''The current state is the top of the execution stack.'''
        return self._s[-1]

    def currdecsgl(self):
        return self._s[-1].decsglstk[-1]

    def curract(self):
        return self.currdecsgl().activator

    def currsubst(self):
        s = self.currstate().subst
        s.flatten()
        s.reduce()
        return s

    def solve(self, goal):
        # reset the execution stack
        self._s = ExecutionStack()

        parser = PrologParser(goal)
        goal = parser.read_term()
        decorated_goal = DecoratedSubgoal(goal)
        initial_state = ExecutionState(1, [decorated_goal], Substitution())
        self._s.append(initial_state)

        state, param = self.select, None
        while len(self._s) > 0 and len(self.currstate().decsglstk) > 0:
            state, param = state() if param is None else state(param)
        
        if len(self._s) == 0:
            return False
        elif len(self.currstate().decsglstk) == 0:
            return True

    def solve_next(self):
        '''Re-executing a goal (ISO 7.7.6)'''
        if not self._s:
            return False

        self._s.pop()
        state, param = self.backtrack, None
        while len(self._s) > 0 and len(self.currstate().decsglstk) > 0:
            state, param = state() if param is None else state(param)

        if len(self._s) == 0:
            return False
        elif len(self.currstate().decsglstk) == 0:
            return True

    def select(self):
        '''Selecting a clause for execution (ISO 7.7.7)'''
        #activator = convert_to_goal(self.curract())
        activator = self.curract()

        p = search_control_construct(activator)
        if p:
            self.currstate().bi = 'ctrl'
            return self.execute_ctrl, p

        p = search_builtin_predicate(activator)
        if p:
            self.currstate().bi = 'bip'
            return self.execute_bip, p
        
        p = self._kb.search(activator)
        if p is None:
            unknown = _FLAGS['unknown'].value
            if unknown  == 'error':
                pi = Compound('/', Atomic(activator.name), Atomic(activator.arity))
                e = PrologExistenceError('procedure', pi)
                return self.throw, e.error
            elif unknown == 'warning':
                pi = activator.predicate_indicator()
                message = 'Warning: the procedure {0} is undefined\n'.format(pi)
                self._write(message)
                self.currdecsgl().activator = Atomic.FAIL
                return self.select, None
            elif unknown == 'fail':
                self.currdecsgl().activator = Atomic.FAIL
                return self.select, None
        else:
            clauses = p.clauses()
            self.currstate().bi = ('up', clauses)
            return self.execute_up, clauses

    def execute_ctrl(self, ctrl):
        return ctrl().execute(self)

    def reexecute_ctrl(self, ctrl):
        return ctrl().reexecute(self)
        
    def execute_bip(self, bip):
        '''Executing a built-in predicate (ISO 7.7.12)'''
        predicate = bip(self._kb)
        self.curract().bip = predicate
        ccs = copy(self.currstate())
        self._s.append(ccs)
        activator = self.curract()
        try:
            raw_args = [] if isatom(activator) else activator.value[1:]
            args = (deref(arg) for arg in raw_args)
            result = predicate.execute(*args)
        except PrologError as e:
            return self.throw, e.error
        ccg = ccs.decsglstk[-1]
        ccg.activator = Atomic.TRUE if result else Atomic.FAIL
        if predicate.substitution:
            self.currstate().subst.update(predicate.substitution)
            for subgoal in self.currstate().decsglstk[:-1]:
                subgoal.activator.apply(predicate.substitution)
        return self.select, None

    def execute_up(self, clauses):
        '''Executing a user-defined procedure (ISO 7.7.10 and 7.7.11)'''
        if not clauses:
            self._s.pop()
            return self.backtrack, None
        c = renamed_copy(clauses[0], self.currstate().s_index)
        mgu = unify(c.head(), self.curract())
        if mgu is not None:
            c.body().apply(mgu)
            ccs = copy(self.currstate())
            # get a copy of the current goal in currstate
            ccg = ccs.decsglstk[-1]
            ccg.activator.apply(mgu)
            ccg.activator = c.body() # modified by applying MGU
            ccs.bi = 'nil'
            ccs.subst.update(mgu)
            ccs.decsglstk[-1].cutparent = self.currstate().s_index - 1
            self._s.append(ccs)
            for subgoal in self.currstate().decsglstk[:-1]:
                subgoal.activator.apply(mgu)
            return self.select, None
        else:
            self.currstate().bi = ('up', clauses[1:])
            return self.execute_up, clauses[1:]

    def backtrack(self):
        '''Backtracking (ISO 7.7.8)'''
        if len(self._s) > 0:
            bi = self.currstate().bi
            if isinstance(bi, tuple):
                clauses = bi[1]
                self.currstate().bi = ('up', clauses[1:])
                return self.execute_up, clauses[1:]
            elif bi == 'bip':
                cs = self.currstate()
                if hasattr(self.curract().bip, 'reexecute'):
                    ccs = copy(cs)
                    self._s.append(ccs)
                    predicate = self.curract().bip
                    ccg = ccs.decsglstk[-1]
                    raw_args = [] if isatom(ccg.activator) else ccg.activator.value[1:]
                    args = (deref(arg) for arg in raw_args)
                    if predicate.reexecute(*args):
                        if predicate.substitution:
                            self.currstate().subst.update(predicate.substitution)
                            for subgoal in self.currstate().decsglstk[:-1]:
                                subgoal.activator.apply(predicate.substitution)
                        cg = cs.decsglstk[-1]
                        cg.activator.bip = ccg.activator.bip
                        ccg.activator = Atomic.TRUE
                    else:
                        self._s.pop()
                        ccs = self.currstate()
                        ccg = ccs.decsglstk[-1]
                        ccg.activator = Atomic.FAIL
                else:
                    cg = cs.decsglstk[-1]
                    cg.activator = Atomic.FAIL
                return self.select, None
            elif bi == 'ctrl':
                p = search_control_construct(self.curract())
                return self.reexecute_ctrl, p
            elif bi == 'nil':
                return self.select, None

    def throw(self, error):
        error = Compound('error', error, Atomic(0))
        self.currstate().decsglstk[-1].activator = Compound('throw', error)
        self.currstate().bi = 'nil'
        return self.select, None

    def _consult(self, theory):
        parser = PrologParser(theory)
        term = parser.read_term()
        while term:
            if term.isdirective():
                d = term.value[1]
                directive = search_directives(d)
                if directive is None:
                    message = 'Warning: the directive {} is unknown\n'
                    self._write(message.format(d.predicate_indicator()))
                else:
                    directive(self, *d.value[1:])
            else:
                self._kb.append(term)
            term = parser.read_term()

    def _clear(self):
        self._kb = Database()

    def _write(self, message):
        from .builtin import io
        io.STANDARD_OUTPUT_STREAM.write(message)


def search_directives(procedure):
    d = {'dynamic/1' : dynamic_1}
    return d.get(procedure.predicate_indicator())

def dynamic_1(engine, *indicators):
    '''A directive dynamic(PI) where PI is a predicate indicator,
    a predicate indicator sequence, or a predicate indicator list
    specifies that each user-defined procedure indicated by PI is
    dynamic.

    No procedure indicated by PI shall be a control construct or
    a built-in predicate.

    The first directive dynamic(PI) that specified a user-defined
    procedure P to be dynamic shall precede all clauses for P.
    Further, if P is defined to be a dynamic procedure in one
    Prolog text, then a directive dynamic(PI) indicating P shall
    occur in every Prolog text which contain clauses for P.'''
    for indicator in indicators:
        t = tuple(Variable('_') for i in range(indicator.value[2].value))
        witness = Compound(indicator.value[1].name, *t)
        if not search_control_construct(witness) and not search_builtin_predicate(witness):
            if not engine._kb.search(witness):
                pi = '{}/{}'.format(indicator.value[1], indicator.value[2])
                engine._kb.insert(pi)

def unify(c1, c2):
    '''Unifies two clauses, producing a Most General Unifier, yet
    avoiding to apply the MGU to each clause. The MGU is returned
    as a dictionary of Variable names and Term couples.'''
    from copy import deepcopy
    c1 = deepcopy(c1)
    c2 = deepcopy(c2)
    # flat variables
    if isinstance(c1, Variable): # TODO is this deref()?
        c1 = c1.binding()
    if isinstance(c2, Variable): # TODO is this deref()?
        c2 = c2.binding()
    # atomic case
    if isinstance(c1, Atomic):
        if isinstance(c2, Variable):
            mgu = Substitution()
            mgu[c2.name] = c1
            return mgu
        elif isinstance(c2, Atomic):
            return Substitution() if c1 == c2 else None
        else:
            return None
    # variable case
    if isinstance(c1, Variable):
        mgu = Substitution()
        if isinstance(c2, Variable):
            if c1 == c2:
                return mgu
            if isrenamed(c2.name):
                mgu[c2.name] = c1
                return mgu
            if c2 > c1:
                mgu[c2.name] = c1
            else:
                mgu[c1.name] = c2
            return mgu
        mgu[c1.name] = c2
        return mgu
    # compound case
    if isinstance(c1, Compound):
        if isinstance(c2, Variable):
            mgu = Substitution()
            mgu[c2.name] = c1
            return mgu
        elif isinstance(c2, Compound):
            if c1.predicate_indicator() == c2.predicate_indicator():
                mgu = Substitution()
                for a1, a2 in zip(c1.value[1:], c2.value[1:]):
                    arg_mgu = unify(a1, a2)
                    if arg_mgu is not None:
                        for k, v in arg_mgu.items():
                            if k in mgu:
                                value_mgu = unify(v, mgu[k])
                                if value_mgu is not None:
                                    mgu.update(value_mgu)
                                else:
                                    return None
                            else:
                                mgu.update(arg_mgu)
                    else:
                        return None
                return mgu
            else:
                return None
        elif isinstance(c2, List):
            if c1.predicate_indicator() == c2.predicate_indicator():
                mgu = Substitution()
                head_mgu = unify(c1.value[1], c2.head)
                if head_mgu is not None:
                    for k, v in head_mgu.items():
                        if k in mgu:
                            value_mgu = unify(v, mgu[k])
                            if value_mgu is not None:
                                mgu.update(value_mgu)
                            else:
                                return None
                        else:
                            mgu.update(head_mgu)
                else:
                    return None
                tail_mgu = unify(c1.value[2], c2.tail)
                if tail_mgu is not None:
                    for k, v in tail_mgu.items():
                        if k in mgu:
                            value_mgu = unify(v, mgu[k])
                            if value_mgu is not None:
                                mgu.update(value_mgu)
                            else:
                                return None
                        else:
                            mgu.update(tail_mgu)
                else:
                    return None
                return mgu
            else:
                return None
        else:
            return None
    # list case
    if isinstance(c1, List):
        if isinstance(c2, Variable):
            mgu = Substitution()
            mgu[c2.name] = c1
            return mgu
        elif isinstance(c2, List):
            mgu = Substitution()
            head_mgu = unify(c1.head, c2.head)
            if head_mgu is not None:
                for k, v in head_mgu.items():
                    if k in mgu:
                        value_mgu = unify(v, mgu[k])
                        if value_mgu is not None:
                            mgu.update(value_mgu)
                        else:
                            return None
                    else:
                        mgu.update(head_mgu)
            else:
                return None
            tail_mgu = unify(c1.tail, c2.tail)
            if tail_mgu is not None:
                for k, v in tail_mgu.items():
                    if k in mgu:
                        value_mgu = unify(v, mgu[k])
                        if value_mgu is not None:                            
                            mgu.update(value_mgu)
                        else:
                            return None
                    else:
                        mgu.update(tail_mgu)
            else:
                return None
            return mgu
        elif isinstance(c2, Compound):
            if c1.predicate_indicator() == c2.predicate_indicator():
                mgu = Substitution()
                head_mgu = unify(c1.head, c2.value[1])
                if head_mgu is not None:
                    for k, v in head_mgu.items():
                        if k in mgu:
                            value_mgu = unify(v, mgu[k])
                            if value_mgu is not None:
                                mgu.update(value_mgu)
                            else:
                                return None
                        else:
                            mgu.update(head_mgu)
                else:
                    return None
                tail_mgu = unify(c1.tail, c2.value[2])
                if tail_mgu is not None:
                    for k, v in tail_mgu.items():
                        if k in mgu:
                            value_mgu = unify(v, mgu[k])
                            if value_mgu is not None:
                                mgu.update(value_mgu)
                            else:
                                return None
                        else:
                            mgu.update(tail_mgu)
                else:
                    return None
                return mgu
            else:
                return None
        else:
            return None

def renamed_copy(clause, index=None):
    return clause.rename(index)

def copy(state):
    from copy import deepcopy
    return deepcopy(state)

def deref(term):
    if isinstance(term, Variable):
        return term.binding()
    return term

###
### TODO Place control constructs stuff in its own module?
### Use module.__dict__ to access a dictionary of name:class couples
### or getattr(__import__(module), name) to get the class object
###

class Fail_0:
    '''fail/0
    fail is false.'''
    
    def execute(self, engine):
        engine._s.pop()
        return engine.backtrack, None


class True_0:
    '''true/0
    true is true.'''
    
    def execute(self, engine):
        engine.currstate().decsglstk.pop()
        engine.currstate().bi = 'nil'
        return engine.select, None


class Call_1:
    '''call/1
    call(G) is true iff G represents a goal which is true.
    When G contains ! as a subgoal, the effect of ! does
    not extend outside G.'''
    
    def execute(self, engine):
        ccs = copy(engine.currstate())
        ccs.bi = 'nil'
        currdecsgl = ccs.decsglstk.pop()
        g = currdecsgl.activator.value[1]
        if isinstance(g, Variable) and g.isfree():
            error = Atomic('instantiation_error')
            return engine.throw, error
        if isinstance(g, Atomic) and g._isnumber():
            error = Compound('type_error', g)
            return engine.throw, error
        goal = convert_to_goal(g)
        if not goal:
            error = Compound('type_error', Atomic('callable'), g)
            return engine.throw, error
        n = engine.currstate().s_index - 1
        ccs.decsglstk.append(DecoratedSubgoal(goal, n))
        engine._s.append(ccs)
        return engine.select, None
    
    def reexecute(self, engine):
        engine._s.pop()
        return engine.backtrack, None


class Cut_0:
    '''!/0
    ! is true.'''
    
    def execute(self, engine):
        ccs = copy(engine.currstate())
        ccs.bi = 'nil'
        ccs.decsglstk[-1].activator = Atomic.TRUE
        engine._s.append(ccs)
        return engine.select, None
    
    def reexecute(self, engine):
        # make a copy of cutparent of currstate
        cut = engine.currstate().decsglstk[-1].cutparent
        engine._s.pop()
        while len(engine._s) > 0 and cut != engine.currstate().s_index:
            engine._s.pop()
        return engine.backtrack, None


class Conjunction_2:
    """','/2
    ','(First, Second) is true iff First is true and Second is true."""
    
    def execute(self, engine):
        ccs = copy(engine.currstate())
        conjunction = ccs.decsglstk[-1].activator
        second = conjunction.value[2]
        ccs.decsglstk[-1].activator = second
        first = conjunction.value[1]
        ccs.decsglstk.append(DecoratedSubgoal(first, ccs.decsglstk[-1].cutparent))
        ccs.bi = 'nil'
        engine._s.append(ccs)
        return engine.select, None
    
    def reexecute(self, engine):
        engine._s.pop()
        return engine.backtrack, None


class Disjunction_2:
    """';'/2
    ';'(Either, Or) is true iff Either is true or Or is true.
    ';'('->'(If, Then), Else) is true iff (1a) If is true, and
    (1b) Then is true for the first solution of If, or (2) If
    is false and Else is true."""
    
    def execute(self, engine):
        disjunction = engine.currdecsgl().activator
        if disjunction.value[1].predicate_indicator() == '->/2':
            return self.execute_ifthenelse(engine)
        currstate = engine.currstate()
        ccs_either = copy(currstate)
        ccs_or = copy(currstate)
        ccs_either.bi = 'nil'
        ccs_or.bi = 'nil'
        ccs_either.decsglstk[-1].activator = disjunction.value[1]
        ccs_or.decsglstk[-1].activator = disjunction.value[2]
        engine._s.append(ccs_or)
        engine._s.append(ccs_either)
        return engine.select, None
    
    def reexecute(self, engine):
        disjunction = engine.currdecsgl().activator
        if disjunction.value[1].predicate_indicator() == '->/2':
            return self.reexecute_ifthenelse(engine)
        engine._s.pop()
        return engine.backtrack, None
    
    def execute_ifthenelse(self, engine):
        ccs = copy(engine.currstate())
        ccs.bi = 'nil'
        currdecsgl = ccs.decsglstk.pop()
        n = engine.currstate().s_index
        if_then = currdecsgl.activator.value[1]
        if_goal = DecoratedSubgoal(if_then.value[1], n)
        then_goal = DecoratedSubgoal(if_then.value[2], currdecsgl.cutparent)
        ccs.decsglstk.append(then_goal)
        ccs.decsglstk.append(DecoratedSubgoal(Atomic('!'), n - 1))
        ccs.decsglstk.append(if_goal)
        engine._s.append(ccs)
        return engine.select, None
    
    def reexecute_ifthenelse(self, engine):
        ccs = copy(engine.currstate())
        ccs.bi = 'nil'
        currdecsgl = ccs.decsglstk.pop()
        else_goal = DecoratedSubgoal(currdecsgl.activator.value[2], currdecsgl.cutparent)
        ccs.decsglstk.append(else_goal)
        n = engine.currstate().s_index - 1
        ccs.decsglstk.append(DecoratedSubgoal(Atomic('!'), n))
        engine._s.append(ccs)
        return engine.select, None


class IfThen_2:
    """'->/2
    '->/2'(If, Then) is true iff (1) If is true, and (2) Then
    is true for the first solution of If."""
    
    def execute(self, engine):
        ccs = copy(engine.currstate())
        ccs.bi = 'nil'
        currdecsgl = ccs.decsglstk.pop()
        n = engine.currstate().s_index - 1
        if_goal = DecoratedSubgoal(currdecsgl.activator.value[1], currdecsgl.cutparent)
        then_goal = DecoratedSubgoal(currdecsgl.activator.value[2], currdecsgl.cutparent)
        ccs.decsglstk.append(then_goal)
        ccs.decsglstk.append(DecoratedSubgoal(Atomic('!'), n))
        ccs.decsglstk.append(if_goal)
        engine._s.append(ccs)
        return engine.select, None
    
    def reexecute(self, engine):
        engine._s.pop()
        return engine.backtrack, None


class Catch_3:
    '''catch/3
    catch(G, C, R) is true iff (1) call(G) is true, or (2) the
    call of G is interrupted by a call of throw/1 whose argument
    unifies with C, and call(R) is true.'''
    
    def execute(self, engine):
        ccs = copy(engine.currstate())
        catch = ccs.decsglstk[-1].activator
        ccs.decsglstk[-1].activator = Compound('call', catch.value[1])
        ccs.bi = 'nil'
        engine._s.append(ccs)
        return engine.select, None
    
    def reexecute(self, engine):
        engine._s.pop()
        return engine.backtrack, None


class Throw_1:
    '''throw/1
    throw(B) is a control construct that is neither true nor
    false. It exists only for its procedural effect of causing
    the normal flow of control to be transferred back to an
    existing call of catch/3.'''
    
    def execute(self, engine):
        ca = renamed_copy(engine.curract(), engine.currstate().s_index)
        ball = ca.value[1]
        if isinstance(ball, Variable) and ball.isfree():
            error = Atomic('instantiation_error')
            return engine.throw, error
        cp = engine.currdecsgl().cutparent
        # jump to the predicate which threw the ball
        while engine.currstate().s_index != cp + 1:
            engine._s.pop()
            if not engine._s:
                raise PrologError(ball)
        cp = engine.currdecsgl().cutparent
        # jump in the proximity of the proper catch, in order to avoid
        # stumbling upon another catch related to a different predicate
        while engine.currstate().s_index != cp + 1:
            engine._s.pop()
            if not engine._s:
                raise PrologError(ball)
        mgu = None
        while mgu is None or engine.currdecsgl().cutparent >= cp:
            if engine.curract().predicate_indicator() == 'catch/3':
                mgu = unify(ball, engine.curract().value[2])
                if mgu is not None:
                    break
            cp = engine.currdecsgl().cutparent
            engine._s.pop()
            if not engine._s:
                raise PrologError(ball)
        # update the current substitution
        engine.currstate().subst.update(mgu)
        # propagate the substitution through the whole subgoal stack
        for subgoal in engine.currstate().decsglstk:
            subgoal.activator.apply(mgu)
        recovery = engine.curract().value[3]
        engine.currstate().decsglstk[-1].activator = Compound('call', recovery)
        engine.currstate().bi = 'nil'
        return engine.select, None


class BuiltIn:
    '''A built-in predicate is a procedure which is provided automatically
    by a standard-conforming Prolog processor.'''

    def __init__(self, kb):
        self.substitution = Substitution()
        self.kb = kb
    
    def unify(self, x, y):
        mgu = unify(x, y)
        if mgu is not None:
            if mgu:
                self.substitution.update(mgu)
                x.apply(mgu)
                y.apply(mgu)
            return True
        return False

    def reset_substitution(self):
        '''Resets the substitution stored by this built-in.
        This is needed for reexecutable built-in predicates.'''
        self.substitution = Substitution()

    def throw_instantiation_error(self):
        raise PrologInstantiationError()

    def throw_type_error(self, type, culprit):
        raise PrologTypeError(type, culprit)

    def throw_domain_error(self, domain, culprit):
        raise PrologDomainError(domain, culprit)

    def throw_existence_error(self, type, culprit):
        raise PrologExistenceError(type, culprit)

    def throw_permission_error(self, operation, permission_type, culprit):
        raise PrologPermissionError(operation, permission_type, culprit)

    def throw_representation_error(self, flag):
        raise PrologRepresentationError(flag)

    def throw_syntax_error(self, term):
        raise PrologSyntaxError(term)


def convert_to_goal(term):
    '''Convert a term to a goal before executing it by means
    of call/1, or a body of a clause to a goal before inserting
    the clause in the database. (ISO 7.6.2)'''
    if isinstance(term, Variable):
        #return Compound('call', term.binding())
        if term.isfree():
            return Compound('call', term)
        else:
            term = term.binding()
    if isinstance(term, Atomic) and term._isnumber():
        return None
    if isinstance(term, Compound):
        pi = term.predicate_indicator()
        if pi == ',/2' or pi == ';/2' or pi == '->/2':
            args = [convert_to_goal(a) for a in term.value[1:]]
            if not all(args):
                return None
            return Compound(term.name, *args, priority=term.priority)
    return term

######

class Caller(Engine):
    '''Helper class to wrap a Prolog engine and make it execute call/1 goals in
    the form of Prolog terms instead of strings. Needed because of a bug in
    PrologParser that prevents some goals to be reconstructed from the string
    representation of the Compound term that represents the goal.'''

    def solve(self, goal):
        goal = Compound('call', goal)
        decorated_goal = DecoratedSubgoal(goal)
        initial_state = ExecutionState(1, [decorated_goal], Substitution())
        self._s.append(initial_state)

        state, param = self.select, None
        while len(self._s) > 0 and len(self.currstate().decsglstk) > 0:
            state, param = state() if param is None else state(param)
        
        if len(self._s) == 0:
            return False
        elif len(self.currstate().decsglstk) == 0:
            return True
