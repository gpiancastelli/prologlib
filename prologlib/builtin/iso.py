from ..parser import Atomic, Variable, Compound, List
from ..parser import isvariable, isatom, isnumber, islist, ispartiallist, iscallable
from ..core import BuiltIn

###
### Term unification (ISO 8.2)
###

class Unify_2(BuiltIn):
    """'='(?term, ?term)

    If X and Y are NSTO (Not Subject To Occur-check) then '='(X, Y) is true
    iff X and Y are unifiable."""
    
    def execute(self, x, y):
        # TODO prologlib crashes if you attempt to unify two STO terms by =/2
        # instead of using the proper unify_with_occur_check/2 predicate.
        return self.unify(x, y)


class NotUnifiable_2(BuiltIn):
    """'\\='(@term, @term)

    If X and Y are NSTO (Not Subject To Occur-check) then '\\='(X, Y) is true
    iff X and Y are not unifiable."""

    def execute(self, x, y):
        from .. import core
        return core.unify(x, y) is None

###
### Type testing (ISO 8.3)
###

class Var_1(BuiltIn):
    '''var(@term)
    
    var(X) is true iff X is a member of the set V.'''
    
    def execute(self, x):
        return isvariable(x)


class Atom_1(BuiltIn):
    '''atom(@term)
    
    atom(X) is true iff X is a member of the set A.'''

    def execute(self, x):
        return isatom(x)


class Integer_1(BuiltIn):
    '''integer(@term)
    
    integer(X) is true iff X is a member of the set I.'''

    def execute(self, x):
        return x.arity == 0 and isinstance(x.value, int)


class Float_1(BuiltIn):
    '''float(@term)
    
    float(X) is true iff X is a member of the set F.'''

    def execute(self, x):
        return x.arity == 0 and isinstance(x.value, float)


class Atomic_1(BuiltIn):
    '''atomic(@term)
    
    atomic(X) is true if X is a member of the set A or I
    or F and is false if X is a member of the set V or CT.'''

    def execute(self, x):
        return isinstance(x, Atomic)


class Compound_1(BuiltIn):
    '''compound(@term)
    
    compound(X) is true iff X is a member of the set CT.'''

    def execute(self, x):
        return isinstance(x, (Compound, List))


class Nonvar_1(BuiltIn):
    '''nonvar(@term)
    
    nonvar(X) is true iff X is not a member of the set V.'''

    def execute(self, x):
        return not isvariable(x)


class Number_1(BuiltIn):
    '''number(@term)
    
    number(X) is true if X is a member of the set I or F
    and is false if X is a member of the set V, A, or CT.'''

    def execute(self, x):
        return isnumber(x)

###
### Term comparison (ISO 8.4)
###

class TermLessThanOrEqual_2(BuiltIn):
    """'@=<'(@term, @term)

    Test the ordering of two terms. '@=<'(X, Y) is true iff
    X preceeds Y or X and Y are identical terms."""

    def execute(self, x, y):
        # The Python __eq__ method does not hold Prolog
        # semantics for anonymous variables
        if (isvariable(x) and isvariable(y) and
            x.name == '_' and y.name == '_'):
            return True
        return x <= y


class TermIdentical_2(BuiltIn):
    """'=='(@term, @term)

    Test the ordering of two terms. '=='(X, Y) is true iff
    X and Y are identical terms."""

    def execute(self, x, y):
        # The Python __eq__ method does not hold Prolog
        # semantics for anonymous variables
        if (isvariable(x) and isvariable(y) and
            x.name == '_' and y.name == '_'):
            return False
        return x == y


class TermNotIdentical_2(BuiltIn):
    """'\=='(@term, @term)

    Test the ordering of two terms. '\=='(X, Y) is true iff
    X and Y are not identical terms."""

    def execute(self, x, y):
        # The Python __ne__ method does not hold Prolog
        # semantics for anonymous variables
        if (isvariable(x) and isvariable(y) and
            x.name == '_' and y.name == '_'):
            return True
        return x != y


class TermLessThan_2(BuiltIn):
    """'@<'(@term, @term)

    Test the ordering of two terms. '@<'(X, Y) is true iff
    X preceeds Y."""

    def execute(self, x, y):
        return x < y


class TermGreaterThan_2(BuiltIn):
    """'@>(@term, @term)

    Test the ordering of two terms. '@>'(X, Y) is true iff
    Y preceeds X."""

    def execute(self, x, y):
        return x > y


class TermGreaterThanOrEqual_2(BuiltIn):
    """'@>=(@term, @term)

    Test the ordering of two terms. '@>='(X, Y) is true iff
    Y preceeds X or Y and X are identical terms."""

    def execute(self, x, y):
        # The Python __eq__ method does not hold Prolog
        # semantics for anonymous variables
        if (isvariable(x) and isvariable(y) and
            x.name == '_' and y.name == '_'):
            return False
        return x >= y

###
### Term creation and decomposition (ISO 8.5)
###

class Functor_3(BuiltIn):
    '''functor(-nonvar, +atomic, +integer)
    functor(+nonvar, ?atomic, ?integer)
    
    functor(Term, Name, Arity) is true iff:
      * Term is a compound term with a functor whose identifier
      is Name and arity Arity, or
      * Term is an atomic term equal to Name and Arity is 0.'''

    def execute(self, term, name, arity):
        if isvariable(term) and isvariable(name):
            self.throw_instantiation_error()
        if isvariable(term) and isvariable(arity):
            self.throw_instantiation_error()
        if isvariable(term) and not isinstance(arity.value, int):
            self.throw_type_error('integer', arity)
        # TODO Missing max_arity related error
        if isvariable(term) and arity.value < 0:
            self.throw_domain_error('not_less_than_zero', arity)
        if isvariable(term) and not isinstance(name, Atomic):
            self.throw_type_error('atomic', name)
        if isvariable(term) and not isatom(name) and arity.value > 0:
            self.throw_type_error('atom', name)
        
        if isinstance(term, Atomic):
            return self.unify(term, name) and self.unify(arity, Atomic(0))
        if isinstance(term, (Compound, List)):
            return (self.unify(Atomic(term.name), name) and
                    self.unify(Atomic(term.arity), arity))
        if isinstance(term, Variable):
            if isinstance(name, Atomic) and arity.value == 0:
                return self.unify(term, name)
            if isatom(name) and arity.value > 0:
                t = (Variable('_') for i in range(arity.value))
                c = Compound(name.name, *t)
                return self.unify(term, c)
            return False


class Arg_3(BuiltIn):
    '''arg(+integer, +compound_term, ?term)
    
    arg(N, Term, Arg) is true iff the Nth argument of Term is Arg.'''

    def execute(self, n, term, arg):
        if isvariable(n) or isvariable(term):
            self.throw_instantiation_error()
        if not isinstance(n.value, int):
            self.throw_type_error('integer', n)
        if not isinstance(term, Compound):
            self.throw_type_error('compound', term)
        if n.value < 0:
            self.throw_domain_error('not_less_than_zero', n)

        if n.value >= len(term.value):
            return False
        return self.unify(arg, term.value[n.value])


class Univ_2(BuiltIn):
    """'=..'(+nonvar, ?list)
    '=..'(-nonvar, +list)

    '=..'(Term, List) is true iff:
      * Term is an atomic term and List is the list whose only
      element is Term, or
      * Term is a compound term and List is the list whose head
      is the functor name of Term and whose tail is a list of the
      arguments of Term."""

    def execute(self, term, elements):
        if isvariable(term) and ispartiallist(elements):
            self.throw_instantiation_error()
        if not islist(elements) and not ispartiallist(elements):
            self.throw_type_error('list', elements)
        if isvariable(term) and islist(elements) and isvariable(elements.head):
            self.throw_instantiation_error()
        if islist(elements) and not isatom(elements.head) and len(elements) > 1:
            self.throw_type_error('atom', elements.head)
        if islist(elements) and isinstance(elements.head, Compound) and len(elements) > 1:
            self.throw_type_error('atomic', elements.head)
        if isvariable(term) and elements == List.EMPTY_LIST:
            self.throw_domain_error('non_empty_list', elements)
        # TODO Missing max_arity related error

        if isinstance(term, Atomic):
            l = List(term)
            return self.unify(elements, l)
        if isinstance(term, Compound):
            l = List.from_list([Atomic(term.name)] + list(term.value[1:]))
            return self.unify(elements, l)
        if isinstance(term, Variable):
            # elements is a list
            if elements.name == '.' and elements.arity == 2:
                if len(elements) == 1:
                    t = elements.head
                    return self.unify(term, t)
                elif len(elements) > 1:
                    name = elements.head.name
                    t = Compound(name, *elements.as_list()[1:])
                    return self.unify(term, t)
                else:
                    return False
            else:
                return False


class CopyTerm_2(BuiltIn):
    '''copy_term(?term, ?term)

    copy_term(Term_1, Term_2) is true iff Term_2 unifies with
    a term T which is a renamed copy of Term_1.'''

    def execute(self, t1, t2):
        from .. import core
        #t = core.renamed_copy(t1)
        t = t1._copy_term()
        # Can't directly use BuiltIn.unify because the bindings
        # between the renamed copy of t1 and t2 retain validity
        # only in the context of the copy_term/2 built-in
        mgu = core.unify(t2, t)
        if mgu is not None:
            if mgu:
                t2.apply(mgu)
                # Do not propagate renamed term variables bindings
                # outside the context of the copy_term/2 built-in
                if t2.name in mgu:
                    # Still preserve the binding for t2 just in
                    # case t2 were a renamed variable (e.g. coming
                    # from a clause renaming)
                    temp = mgu[t2.name]
                    mgu.reduce()
                    mgu.update({t2.name : temp})
                else:
                    mgu.reduce()
                self.substitution.update(mgu)
            return True
        return False

###
### Arithmetic evaluation (ISO 8.6)
### Simple arithmetic functors (ISO 9.1)
### Other arithmetic functors (ISO 9.3)
### Bitwise functors (ISO 9.4)
###

class Is_2(BuiltIn):
    """is(?term, @evaluable)

    'is'(Result, Expression) is true iff the value of evaluating
    Expression as an expression is Result."""

    def execute(self, result, expression):
        if isvariable(expression):
            self.throw_instantiation_error()

        c = evaluate_expression(expression)
        return self.unify(result, Atomic(c))

def evaluate_expression(term):
    # TODO No overflow/underflow errors
    # TODO No undefined errors
    if isvariable(term):
        from ..core import PrologInstantiationError
        raise PrologInstantiationError()
    if term.arity == 0 and term._isnumber():
        return term.value
    if isinstance(term, Compound):
        from ..core import deref
        args = (evaluate_expression(deref(a)) for a in term.value[1:])
        pi = term.predicate_indicator()
        functor = search_evaluable_functor(pi)
        if not functor:
            from ..core import PrologTypeError
            raise PrologTypeError('evaluable', Atomic(pi))
        return functor(*args)
    from ..core import PrologTypeError
    raise PrologTypeError('number', term)

def search_evaluable_functor(name):
    import math
    import operator
    d = {'+/2' : operator.add, '*/2' : operator.mul, '-/2' : operator.sub,
         '-/1' : operator.neg, '//2' : divide, '///2' : intdivide,
         'mod/2' : module, 'rem/2' : module, 'floor/1' : math.floor,
         'round/1' : round, 'ceiling/1' : math.ceil, 'truncate/1' : math.trunc,
         'float/1' : float, 'abs/1' : operator.abs, 'sign/1' : sign,
         'float_integer_part/1' : float_integer_part,
         'float_fractional_part/1' : float_fractional_part,
         '**/2' : power, 'sin/1' : math.sin, 'cos/1' : math.cos,
         'atan/1' : math.atan, 'exp/1' : math.exp, 'log/1' : logarithm,
         'sqrt/1' : squareroot,
         '>>/2' : rightshift, '<</2' : leftshift,
         '/\\/2' : bitand, '\\//2' : bitor, '\\/1' : bitnot}
    return d.get(name)

def divide(x, y):
    '''Redefined w.r.t. Python because in ISO Prolog div(x, y)
    with x and y integers is equivalent to intdiv(x, y). Also,
    we need to manage ZeroDivisionError errors on our own.'''
    if not y:
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('zero_divisor')
    if isinstance(x, int) and isinstance(y, int):
        return x // y
    return x / y

def intdivide(x, y):
    '''Redefined w.r.t. Python because in ISO Prolog x // y
    is valid only when x and y are integers. Also, we need to
    manage ZeroDivisionError errors on our own.'''
    if not y:
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('zero_divisor')
    if not isinstance(x, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(x))
    if not isinstance(y, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(y))
    return x // y

def module(x, y):
    '''Redefined w.r.t. Python because in ISO Prolog mod(x, y)
    is valid only when x and y are integers. Also, we need to
    manage ZeroDivisionError errors on our own.'''
    if not y:
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('zero_divisor')
    if not isinstance(x, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(x))
    if not isinstance(y, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(y))
    return x % y

def sign(x):
    '''Redefined w.r.t. Python because in ISO Prolog sign(x)
    must return the same type of number as its input.'''
    if not x:
        return 0 if isinstance(x, int) else 0.0
    from math import copysign
    s = copysign(1, x)
    return int(s) if isinstance(x, int) else s

def float_integer_part(x):
    '''Redefined w.r.t. Python because in ISO Prolog
    float_integer_part(x) is valid only when x is a float.'''
    if not isinstance(x, float):
        from ..core import PrologTypeError
        raise PrologTypeError('float', Atomic(x))
    from math import modf
    f, i = modf(x)
    return i

def float_fractional_part(x):
    '''Redefined w.r.t. Python because in ISO Prolog
    float_fractional_part(x) is valid only when x is a float.'''
    if not isinstance(x, float):
        from ..core import PrologTypeError
        raise PrologTypeError('float', Atomic(x))
    from math import modf
    f, i = modf(x)
    return f

def power(x, y):
    '''Redefined w.r.t. Python because in ISO Prolog x ** y
    with x < 0 is defined only when y is an integer, and
    always returns a float. Also, we need to manage
    ZeroDivisionError errors on our own.'''
    if x < 0 and isinstance(y, float):
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('undefined')
    if not x and y < 0:
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('undefined')
    return float(x ** y)

def logarithm(x):
    '''Redefined w.r.t. Python because we need to manage
    ValueError errors (e.g. for log(0)) on our own.'''
    if not x:
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('undefined')
    from math import log
    return log(x)

def squareroot(x):
    '''Redefined w.r.t. Python because we need to manage
    ValueError errors (e.g. for x < 0) on our own.'''
    if x < 0:
        from ..core import PrologEvaluationError
        raise PrologEvaluationError('undefined')
    from math import sqrt
    return sqrt(x)

def rightshift(n, s):
    '''Redefined w.r.t. Python because we need to manage
    TypeError errors (e.g. n as float) on our own.'''
    if not isinstance(n, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(n))
    if not isinstance(s, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(s))
    return n >> s

def leftshift(n, s):
    '''Redefined w.r.t. Python because we need to manage
    TypeError errors (e.g. n as float) on our own.'''
    if not isinstance(n, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(n))
    if not isinstance(s, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(s))
    return n << s

def bitand(x, y):
    '''Redefined w.r.t. Python because we need to manage
    TypeError errors (e.g. x or y as float) on our own.'''
    if not isinstance(x, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(n))
    if not isinstance(y, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(s))
    return x & y

def bitor(x, y):
    '''Redefined w.r.t. Python because we need to manage
    TypeError errors (e.g. x or y as float) on our own.'''
    if not isinstance(x, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(n))
    if not isinstance(y, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(s))
    return x | y

def bitnot(x):
    '''Redefined w.r.t. Python because we need to manage
    TypeError errors (e.g. x or y as float) on our own.'''
    if not isinstance(x, int):
        from ..core import PrologTypeError
        raise PrologTypeError('integer', Atomic(x))
    return ~x

###
### Arithmetic comparison (ISO 8.7)
###

class ArithmeticEqual_2(BuiltIn):
    """'=:='(@evaluable, @evaluable)

    '=:='(E1, E2) is true iff evaluating E1 and E2 as expressions
    the corresponding arithmetic values are equal."""

    def execute(self, e1, e2):
        if isvariable(e1) or isvariable(e2):
            self.throw_instantiation_error()

        v1 = evaluate_expression(e1)
        v2 = evaluate_expression(e2)
        return v1 == v2


class ArithmeticNotEqual_2(BuiltIn):
    """'=\='(@evaluable, @evaluable)

    '=\='(E1, E2) is true iff evaluating E1 and E2 as expressions
    the corresponding arithmetic values are not equal."""

    def execute(self, e1, e2):
        if isvariable(e1) or isvariable(e2):
            self.throw_instantiation_error()

        v1 = evaluate_expression(e1)
        v2 = evaluate_expression(e2)
        return v1 != v2


class ArithmeticLessThan_2(BuiltIn):
    """'<'(@evaluable, @evaluable)

    '<'(E1, E2) is true iff evaluating E1 and E2 as expressions
    the corresponding arithmetic value of E1 is less than the
    corresponding arithmetic value of E2."""

    def execute(self, e1, e2):
        if isvariable(e1) or isvariable(e2):
            self.throw_instantiation_error()

        v1 = evaluate_expression(e1)
        v2 = evaluate_expression(e2)
        return v1 < v2


class ArithmeticLessThanOrEqual_2(BuiltIn):
    """'=<'(@evaluable, @evaluable)

    '=<'(E1, E2) is true iff evaluating E1 and E2 as expressions
    the corresponding arithmetic value of E1 is less than or
    equal to the corresponding arithmetic value of E2."""

    def execute(self, e1, e2):
        if isvariable(e1) or isvariable(e2):
            self.throw_instantiation_error()

        v1 = evaluate_expression(e1)
        v2 = evaluate_expression(e2)
        return v1 <= v2


class ArithmeticGreaterThan_2(BuiltIn):
    """'>'(@evaluable, @evaluable)

    '>'(E1, E2) is true iff evaluating E1 and E2 as expressions
    the corresponding arithmetic value of E1 is greater than
    the corresponding arithmetic value of E2."""

    def execute(self, e1, e2):
        if isvariable(e1) or isvariable(e2):
            self.throw_instantiation_error()

        v1 = evaluate_expression(e1)
        v2 = evaluate_expression(e2)
        return v1 > v2


class ArithmeticGreaterThanOrEqual_2(BuiltIn):
    """'>='(@evaluable, @evaluable)

    '>='(E1, E2) is true iff evaluating E1 and E2 as expressions
    the corresponding arithmetic value of E1 is greater than or
    equal to the corresponding arithmetic value of E2."""

    def execute(self, e1, e2):
        if isvariable(e1) or isvariable(e2):
            self.throw_instantiation_error()

        v1 = evaluate_expression(e1)
        v2 = evaluate_expression(e2)
        return v1 >= v2

###
### Clause retrival and information (ISO 8.8)
###

class Clause_2(BuiltIn):
    '''clause(+head, ?callable_term)

    clause(Head, Body) is true iff:
      * the predicate of Head is public, and
      * there is a clause in the database which corresponds
      to a term H :- B which unifies with Head :- Body.'''

    def execute(self, head, body):
        if isvariable(head):
            self.throw_instantiation_error()
        if not iscallable(head):
            self.throw_type_error('callable', head)
        if not (isvariable(body) or iscallable(body)):
            self.throw_type_error('callable', body)
        
        self.clauses = []
        procedure = self.kb.search(head)
        if not procedure:
            return False
        if not procedure._public:
            pi = Compound('/', Atomic(head.name), Atomic(head.arity))
            self.throw_permission_error('access', 'private_procedure', pi)
        from .. import core
        for clause in procedure.clauses():
            h, b = convert_clause_to_term(clause.head(), clause.body())
            if (core.unify(h, head) is not None and
                core.unify(b, body) is not None):
                self.clauses.append(Compound('clause', h, b))
        return self.pick_clause(head, body)

    def reexecute(self, head, body):
        self.reset_substitution()
        return self.pick_clause(head, body)

    def pick_clause(self, head, body):
        if not self.clauses:
            return False
        c = self.clauses.pop(0)
        return self.unify(Compound('clause', head, body), c)

def convert_clause_to_term(head, body):
    return (convert_to_term(head), convert_to_term(body))

def convert_to_term(head):
    if head.arity == 0:
        return Atomic(head.name)
    from ..core import renamed_copy
    return renamed_copy(head)

class CurrentPredicate_1(BuiltIn):
    '''current_predicate(?predicate_indicator)

    current_predicate(PI) is true iff PI is a predicate indicator
    for one of the user-defined procedures in the database.'''

    def execute(self, pi):
        if not isvariable(pi) and not (pi.name == '/' and pi.arity == 2):
            self.throw_type_error('predicate_indicator', pi)

        self.indicators = []
        for i in self.kb:
            n, a = i.split('/')
            indicator = Compound('/', Atomic(n), Atomic(int(a)))
            from .. import core
            if core.unify(pi, indicator) is not None:
                self.indicators.append(indicator)
        return self.pick_indicator(pi)

    def reexecute(self, pi):
        self.reset_substitution()
        return self.pick_indicator(pi)

    def pick_indicator(self, pi):
        if not self.indicators:
            return False
        # the order in which predicate indicators are found by
        # current_predicate/1 is implementation dependent
        i = self.indicators.pop()
        return self.unify(pi, i)

###
### Clause creation and destruction (ISO 8.9)
###

class Asserta_1(BuiltIn):
    '''asserta(@clause)

    asserta(Clause) is true. It is used to add Clause to the
    database before all existing clauses of the procedure whose
    predicate is equal to the functor of the head of Clause.'''

    def execute(self, clause):
        head = clause.value[1] if clause.predicate_indicator() == ':-/2' else clause
        if isvariable(head):
            self.throw_instantiation_error()
        if isnumber(head):
            self.throw_type_error('callable', head)
        # errors on the conversion of the clause body to a
        # goal and on access permission to a user-defined
        # procedure are handled directly by the database
        from ..builtin import search_builtin
        if search_builtin(head):
            pi = Compound('/', Atomic(head.name), Atomic(head.arity))
            self.throw_permission_error('modify', 'static_procedure', pi)
        
        self.kb.assert_clause(clause, append=False)
        return True


class Assertz_1(BuiltIn):
    '''assertz(@clause)

    assertz(Clause) is true. It is used to add Clause to the
    database after all existing clauses of the procedure whose
    predicate is equal to the functor of the head of Clause.'''

    def execute(self, clause):
        head = clause.value[1] if clause.predicate_indicator() == ':-/2' else clause
        if isvariable(head):
            self.throw_instantiation_error()
        if isnumber(head):
            self.throw_type_error('callable', head)
        # errors on the conversion of the clause body to a
        # goal and on access permission to a user-defined
        # procedure are handled directly by the database
        from ..builtin import search_builtin
        if search_builtin(head):
            pi = Compound('/', Atomic(head.name), Atomic(head.arity))
            self.throw_permission_error('modify', 'static_procedure', pi)
        
        self.kb.assert_clause(clause, append=True)
        return True


class Retract_1(BuiltIn):
    '''retract(+clause)

    retract(Clause) is true iff the database contains at least
    one dynamic procedure with a clause Clause which unifies
    with Head :- Body. It is used to remove those unifying
    clauses from the database.'''

    def execute(self, clause):
        if clause.predicate_indicator() == ':-/2':
            head = clause.value[1]
            body = clause.value[2]
        else:
            head = clause
            body = Atomic.TRUE
            
        if isvariable(head):
            self.throw_instantiation_error()
        if isnumber(head):
            self.throw_type_error('callable', head)
        # error on access permission to a user-defined
        # procedure is handled directly by the database
        from ..builtin import search_builtin
        if search_builtin(head):
            pi = Compound('/', Atomic(head.name), Atomic(head.arity))
            self.throw_permission_error('modify', 'static_procedure', pi)

        self.clauses_to_unify = []
        self.clauses_to_remove = []
        procedure = self.kb.search(head)
        if not procedure:
            return False
        from .. import core
        for clause in procedure.clauses():
            h, b = convert_clause_to_term(clause.head(), clause.body())
            if (core.unify(h, head) is not None and
                core.unify(b, body) is not None):
                self.clauses_to_unify.append(Compound('clause', h, b))
                self.clauses_to_remove.append(clause)
        return self.pick_clause(head, body)

    def reexecute(self, clause):
        self.reset_substitution()
        if clause.predicate_indicator() == ':-/2':
            head = clause.value[1]
            body = clause.value[2]
        else:
            head = clause
            body = Atomic.TRUE
        return self.pick_clause(head, body)

    def pick_clause(self, head, body):
        if not self.clauses_to_remove:
            return False
        self.kb.retract(self.clauses_to_remove.pop(0))
        c = self.clauses_to_unify.pop(0)
        return self.unify(Compound('clause', head, body), c)


class Abolish_1(BuiltIn):
    '''abolish(@predicate_indicator)

    abolish(Pred) is true. It is used to remove from the database
    the procedure specified by the predicate indicator Pred and
    all its clauses, leaving the database in the same state as if
    the procedure identified by Pred had never existed.'''

    def execute(self, pi):
        if isvariable(pi):
            self.throw_instantiation_error()
        if pi.name == '/' and pi.arity == 2:
            name, arity = pi.value[1:]
            if isvariable(name) or isvariable(arity):
                self.throw_instantiation_error()
            if not isinstance(arity.value, int):
                self.throw_type_error('integer', arity)
            if not isatom(name):
                self.throw_type_error('atom', name)
            # TODO Missing max_arity related errors
            if arity.value < 0:
                self.throw_domain_error('not_less_than_zero', arity)
            # error on access permission to a user-defined
            # procedure is handled directly by the database
            t = tuple(Variable('_') for i in range(arity.value))
            c = Compound(name.name, *t)
            from ..builtin import search_builtin
            if search_builtin(c):
                self.throw_permission_error('modify', 'static_procedure', pi)
        else:
            self.throw_type_error('predicate_indicator', pi)

        self.kb.abolish(pi)
        return True

###
### All solutions (ISO 8.10)
###

class Findall_3(BuiltIn):
    '''findall(?term, +callable_term, ?list)

    findall(Template, Goal, Instances) is true iff Instances
    unifies with the list of values to which a variable X not
    occurring in Template or Goal would be instantiated by
    successive re-executions of "call(Goal), X=Template" after
    systematic replacement of all variables in X by new
    variables.'''

    def execute(self, template, goal, instances):
        if isvariable(goal):
            self.throw_instantiation_error()
        if isnumber(goal):
            self.throw_type_error('callable', goal)
        if (not isvariable(instances) and
            (not islist(instances) and not ispartiallist(instances))):
            self.throw_type_error('list', instances)
        
        from .. import core
        caller = core.Caller()
        caller._kb = self.kb
        values = []
        result = caller.solve(goal)
        while result:
            from copy import deepcopy
            v = ground(deepcopy(template), caller.currsubst())
            #values.append(core.renamed_copy(v))
            values.append(v._copy_term())
            result = caller.solve_next()
        values = List.EMPTY_LIST if not values else List.from_list(values)
        return self.unify(values, instances)

def ground(term, mgu):
    if isinstance(term, Variable):
        if not term.value:
            value = mgu.get(term.name)
            if value:
                return value
        else:
            return ground(term.binding(), mgu)
    if isinstance(term, Compound):
        args = []
        for arg in term.value[1:]:
            args.append(ground(arg, mgu))
        return Compound(term.name, *args)
    return term

class Bagof_3(BuiltIn):
    '''bagof(?term, +callable_term, ?list)

    bagof(Template, Goal, Instances) assembles as a list the
    solutions of Goal for each different instantiation of the
    free variables in it. The elements of each list are in
    order of solution, but the order in which each list is
    found is undefined.'''

    def execute(self, template, goal, instances):
        fvs = free_variable_set(goal, template)
        self.witness = Compound('witness', *fvs) if fvs else Atomic('witness')
        g = iterated_goal_term(goal)
        findall = Findall_3(self.kb)
        findall.execute(Compound('+', self.witness, template), g, Variable('S'))
        s = findall.substitution['S']
        self.s = self._create_solution_list(s)
        if not self.s:
            return False
        return self.pick_bag(template, goal, instances)

    def reexecute(self, template, goal, instances):
        self.reset_substitution()
        if not self.s:
            return False
        return self.pick_bag(template, goal, instances)

    def pick_bag(self, template, goal, instances):
        wt = self.s[0]
        wt_list = [e for e in self.s if isvariant(wt.value[1], e.value[1])]
        t_list = [e.value[2] for e in wt_list]
        s_next = [e for e in self.s if e not in wt_list]

        from .. import core
        for wwtt, t in zip(wt_list, t_list):
            ww = wwtt.value[1]
            #from copy import deepcopy
            #subst = core.unify(ww, deepcopy(self.witness))
            subst = core.unify(ww, self.witness)
            ww.apply(subst)
            t.apply(subst)
            self.substitution.update(subst)

        t_list = List.from_list(t_list)
        self.s = s_next
        return self.unify(t_list, instances)

    def _create_solution_list(self, s):
        return [] if s == List.EMPTY_LIST else s.as_list()


class Setof_3(Bagof_3):
    '''setof(?term, +callable_term, ?list)

    setof/3 assembles as a list the solutions of a goal for each different
    instantiation of the free variables in that goal. Each list is a sorted
    list, but the order in which each list is found is undefined.'''

    def _create_solution_list(self, s):
        solutions = [] if s == List.EMPTY_LIST else s.as_list()
        solutions = list(set(solutions))
        solutions.sort()
        return solutions

###
### Logic and control (ISO 8.15)
###

class Not_1(BuiltIn):
    """not(@callable_term)

    not(Term) is true iff call(Term) is false."""
#    """'\\+'(@callable_term)
#
#    '\\+'(Term) is true iff call(Term) is false."""

    def execute(self, term):
        if isvariable(term):
            self.throw_instantiation_error()
        if isnumber(term):
            self.throw_type_error('callable', term)

        from .. import core
        caller = core.Caller()
        caller._kb = self.kb
        result = caller.solve(term)
        return not result


class Repeat_0(BuiltIn):
    '''repeat

    repeat is true. repeat is re-executable.
    '''

    def execute(self):
        return True

    def reexecute(self):
        return True

###
### Atomic term processing (ISO 8.16)
###

class AtomLength_2(BuiltIn):
    '''atom_length(+atom, ?integer)

    atom_length(Atom, Length) is true iff integer Length
    equals the number of characters of the name of the
    atom Atom.'''

    def execute(self, atom, length):
        if isvariable(atom):
            self.throw_instantiation_error()
        if not isatom(atom):
            self.throw_type_error('atom', atom)
        if (not isvariable(length) and
            not (isnumber(length) and isinstance(length.value, int))):
            self.throw_type_error('integer', length)
        if isnumber(length) and length.value < 0:
            self.throw_domain_error('not_less_than_zero', length)
        
        size = Atomic(len(atom.name))
        return self.unify(length, size)


class AtomConcat_3(BuiltIn):
    '''atom_concat(?atom, ?atom, +atom)\natom_concat(+atom, +atom, -atom)

    atom_concat(Atom_1, Atom_2, Atom_12) is true iff characters
    of the name of the atom Atom_12 are the result of concatenating
    the characters of the name of the atom Atom_2 to the characters
    of the name of the atom Atom_1.'''

    def execute(self, atom1, atom2, atom12):
        if isvariable(atom1) and isvariable(atom12):
            self.throw_instantiation_error()
        if isvariable(atom2) and isvariable(atom12):
            self.throw_instantiation_error()
        if not isvariable(atom1) and not isatom(atom1):
            self.throw_type_error('atom', atom1)
        if not isvariable(atom2) and not isatom(atom2):
            self.throw_type_error('atom', atom2)
        if not isvariable(atom12) and not isatom(atom12):
            self.throw_type_error('atom', atom12)
        
        if isvariable(atom1) and isvariable(atom2):
            s = atom12.name
            self.data = [(s[:i], s[i:], s) for i in range(len(s) + 1)]
        elif isvariable(atom1):
            s = atom12.name
            if not s.endswith(atom2.name):
                return False
            else:
                i = s.index(atom2.name)
                self.data = [(s[:i], s[i:], s)]
        elif isvariable(atom2):
            s = atom12.name
            if not s.startswith(atom1.name):
                return False
            else:
                i = len(atom1.name)
                self.data = [(s[:i], s[i:], s)]
        else:
            n1 = atom1.name
            n2 = atom2.name
            self.data = [(n1, n2, n1 + n2)]
        return self.pick_data(atom1, atom2, atom12)

    def reexecute(self, atom1, atom2, atom12):
        self.reset_substitution()
        if not self.data:
            return False
        return self.pick_data(atom1, atom2, atom12)

    def pick_data(self, atom1, atom2, atom12):
        c = self.data.pop(0)
        return (self.unify(atom1, Atomic(c[0])) and
                self.unify(atom2, Atomic(c[1])) and
                self.unify(atom12, Atomic(c[2])))


class SubAtom_5(BuiltIn):
    '''sub_atom(+atom, ?integer, ?integer, ?integer, ?atom)

    sub_atom(Atom, Before, Length, After, Sub_atom) is true iff atom Atom can
    be broken into three pieces, AtomL, Sub_atom, and AtomR, such that Before
    is the number of characters of the name of AtomL, Length is the number of
    characters of the name of Sub_atom, and After is the number of characters
    of the name of AtomR.'''

    def execute(self, atom, before, length, after, subatom):
        if isvariable(atom):
            self.throw_instantiation_error()
        if not isvariable(atom) and not isatom(atom):
            self.throw_type_error('atom', atom)
        if not isvariable(subatom) and not isatom(subatom):
            self.throw_type_error('atom', subatom)
        if (not isvariable(before) and
            not (isnumber(before) and isinstance(before.value, int))):
            self.throw_type_error('integer', before)
        if (not isvariable(length) and
            not (isnumber(length) and isinstance(length.value, int))):
            self.throw_type_error('integer', length)
        if (not isvariable(after) and
            not (isnumber(after) and isinstance(after.value, int))):
            self.throw_type_error('integer', after)
        if isnumber(before) and before.value < 0:
            self.throw_domain_error('not_less_than_zero', before)
        if isnumber(length) and length.value < 0:
            self.throw_domain_error('not_less_than_zero', length)
        if isnumber(after) and after.value < 0:
            self.throw_domain_error('not_less_than_zero', after)
        
        n = atom.name
        start = before.value if isinstance(before, Atomic) else 0
        end = len(n) - (after.value if isinstance(after, Atomic) else 0)
        self.data = []
        while start <= end:
            for i in range(start, end + 1):
                self.data.append((n[start:i], start))
            start += 1
        if isinstance(before, Atomic):
            self.data = [(d, p) for (d, p) in self.data if n.index(d, p) == before.value]
        if isinstance(length, Atomic):
            self.data = [(d, p) for (d, p) in self.data if len(d) == length.value]
        if isinstance(after, Atomic):
            self.data = [(d, p) for (d, p) in self.data if len(n) - n.index(d, p) - len(d) == after.value]
        if isinstance(subatom, Atomic):
            self.data = [(d, p) for (d, p) in self.data if d == subatom.value]
        if not self.data:
            return False
        return self.pick_data(atom, before, length, after, subatom)

    def reexecute(self, atom, before, length, after, subatom):
        self.reset_substitution()
        if not self.data:
            return False
        return self.pick_data(atom, before, length, after, subatom)

    def pick_data(self, atom, before, length, after, subatom):
        s, p = self.data.pop(0)
        b = atom.name.index(s, p)
        l = len(s)
        a = len(atom.name) - (b + l)
        return (self.unify(before, Atomic(b)) and
                self.unify(length, Atomic(l)) and
                self.unify(after, Atomic(a)) and
                self.unify(subatom, Atomic(s)))


class AtomChars_2(BuiltIn):
    '''atom_chars(+atom, ?character_list)\natom_chars(-atom, +character_list)

    atom_chars(Atom, List) is true iff List is a list whose elements
    are the one-char atoms whose names are the successive characters
    of the name of atom Atom.'''

    def execute(self, atom, charlist):
        if not isvariable(atom) and not isatom(atom):
            self.throw_type_error('atom', atom)
        if isvariable(atom):
            if isvariable(charlist):
                self.throw_instantiation_error()
            if not islist(charlist) and not ispartiallist(charlist):
                self.throw_type_error('list', charlist)
            for element in charlist.as_list():
                if isvariable(element):
                    self.throw_instantiation_error()
                if isatom(element) and len(element.name) != 1:
                    self.throw_type_error('character', element)
        
        if isvariable(atom):
            from ..core import deref
            chars = [deref(c).name for c in charlist.as_list()]
            return self.unify(atom, Atomic(''.join(chars)))
        elif isvariable(charlist) or islist(charlist) or ispartiallist(charlist):
            chars = [Atomic(c) for c in atom.name]
            return self.unify(charlist, List.from_list(chars))
        else:
            chars = [c.name for c in charlist.as_list()]
            return atom.name == ''.join(chars)


class AtomCodes_2(BuiltIn):
    '''atom_codes(+atom, ?character_code_list)\natom_codes(-atom, +character_code_list)

    atom_codes(Atom, List) is true iff List is a list whose elements
    correspond to the successive characters of the name of atom Atom,
    and the value of each element is the character code for the
    corresponding character of the name.'''

    def execute(self, atom, codelist):
        if not isvariable(atom) and not isatom(atom):
            self.throw_type_error('atom', atom)
        if isvariable(atom):
            if ispartiallist(codelist):
                self.throw_instantiation_error()
            if not islist(codelist) and not ispartiallist(codelist):
                self.throw_type_error('list', codelist)
            for element in codelist.as_list():
                if isvariable(element):
                    self.throw_instantiation_error()
                if not isvariable(element):
                    try:
                        chr(element.value)
                    except UnicodeDecodeError:
                        self.throw_representation_error(element)

        if isvariable(atom):
            chars = [chr(code.value) for code in codelist.as_list()]
            return self.unify(atom, Atomic(''.join(chars)))
        elif isvariable(codelist) or ispartiallist(codelist):
            codes = [Atomic(ord(char)) for char in atom.name]
            return self.unify(codelist, List.from_list(codes))
        else:
            chars = [chr(code.value) for code in codelist.as_list()]
            return atom.name == ''.join(chars)


class CharCode_2(BuiltIn):
    '''char_code(+character, ?character_code)\nchar_code(-character, +character_code)

    char_code(Char, Code) is true iff the character code for the one-char atom
    Char is Code.'''

    def execute(self, char, code):
        if isvariable(char) and isvariable(code):
            self.throw_instantiation_error()
        if not isvariable(char) and len(char.name) != 1:
            self.throw_type_error('character', char)
        if not isvariable(code) and not isinstance(code.value, int):
            self.throw_type_error('integer', code)
        if not isvariable(code):
            try:
                chr(code.value)
            except UnicodeDecodeError:
                self.throw_representation_error(code)
        
        if isvariable(char):
            c = chr(code.value)
            return self.unify(char, Atomic(c))
        elif isvariable(code):
            c = ord(char.name)
            return self.unify(code, Atomic(c))
        else:
            return ord(char.name) == code.value


class NumberChars_2(BuiltIn):
    '''number_chars(+number, ?character_list)\nnumber_chars(-number, +character_list)

    number_chars(Number, List) is true iff List is a list whose elements are
    the one-char atoms corresponding to a character sequence of Number which
    could be output.'''

    def execute(self, number, charlist):
        if isvariable(number) and ispartiallist(charlist):
            self.throw_instantiation_error()
        if isvariable(number):
            for element in charlist.as_list():
                if isvariable(element):
                    self.throw_instantiation_error()
        if not isvariable(number) and not isnumber(number):
            self.throw_type_error('number', number)
        if isvariable(number) and not islist(charlist) and not ispartiallist(charlist):
            self.throw_type_error('list', charlist)
        if islist(charlist):
            for element in charlist.as_list():
                if isatom(element) and len(element.name) != 1:
                    self.throw_type_error('character', element)

        if isvariable(number) or islist(charlist):
            from ..parser import PrologParser, InvalidTermException
            s = ''.join([char.name for char in charlist.as_list()])
            try:
                # the parser needs an End Token
                n = PrologParser(s + '.').read_term()
                return self.unify(number, n)
            except InvalidTermException as e:
                self.throw_syntax_error(Atomic(s))
        else:
            chars = list(str(number.value)) # FIXME this should use write_canonical/1
            lst = [Atomic(c) for c in chars]
            return self.unify(charlist, List.from_list(lst))


class NumberCodes_2(BuiltIn):
    '''number_codes(+number, ?character_code_list)\nnumber_codes(-number, ?character_code_list)

    number_codes(Number, List) is true iff List is a list whose elements are
    the character codes corresponding to a character sequence of Number which
    could be output.'''

    def execute(self, number, codelist):
        if isvariable(number) and ispartiallist(codelist):
            self.throw_instantiation_error()
        if isvariable(number):
            for element in codelist.as_list():
                if isvariable(element):
                    self.throw_instantiation_error()
        if not isvariable(number) and not isnumber(number):
            self.throw_type_error('number', number)
        if isvariable(number) and not islist(codelist) and not ispartiallist(codelist):
            self.throw_type_error('list', codelist)
        if islist(codelist):
            for element in codelist.as_list():
                if not isvariable(element):
                    try:
                        chr(element.value)
                    except UnicodeDecodeError:
                        self.throw_representation_error(element)

        if isvariable(number) or islist(codelist):
            from ..parser import PrologParser, InvalidTermException
            s = ''.join([chr(code.value) for code in codelist.as_list()])
            try:
                # the parser needs an End Token
                n = PrologParser(s + '.').read_term()
                return self.unify(number, n)
            except InvalidTermException as e:
                self.throw_syntax_error(Atomic(s))
        else:
            chars = list(str(number.value)) # FIXME this should use write_canonical/1
            lst = [Atomic(ord(c)) for c in chars]
            return self.unify(codelist, List.from_list(lst))
                
###
### Implementation defined hooks (ISO 8.17)
###

class SetPrologFlag_2(BuiltIn):
    '''set_prolog_flag(+flag, @nonvar)

    A goal set_prolog_flag(Flag, Value) enables the value associated with a
    Prolog flag to be altered.'''
    
    def execute(self, flag, value):
        if isvariable(flag) or isvariable(value):
            self.throw_instantiation_error()
        if not isvariable(flag) and not isatom(flag):
            self.throw_type_error('atom', flag)

        from .. import core # for flags
        if flag.name not in core._FLAGS:
            self.throw_domain_error('prolog_flag', flag)
        f = core._FLAGS[flag.name]
        if len(f.allowed) == 1:
            self.throw_permission_error('modify', 'flag', flag)
        if value.name not in f.allowed:
            culprit = Compound('+', flag, value)
            self.throw_domain_error('flag_value', culprit)

        core._FLAGS[flag.name] = f._replace(value=value.name)
        return True


class CurrentPrologFlag_2(BuiltIn):
    '''current_prolog_flag(?flag, ?term)

    current_prolog_flag(Flag, Value) is true iff Flag is a flag supported by
    the processor, and Value is the value currently associated with it.'''

    def execute(self, flag, value):
        from .. import core # for flags
        
        if not isvariable(flag) and not isatom(flag):
            self.throw_type_error('atom', flag)
        if isatom(flag) and not core._FLAGS[flag.name]:
            self.throw_domain_error('prolog_flag', flag)
        
        self.flags = {f for f in core._FLAGS.values() if core.unify(flag, Atomic(f.name)) is not None}
        if not self.flags:
            return False
        return self.pick_flag(flag, value)

    def reexecute(self, flag, value):
        self.reset_substitution()
        if not self.flags:
            return False
        return self.pick_flag(flag, value)
    
    def pick_flag(self, flag, value):
        f = self.flags.pop()
        return self.unify(flag, Atomic(f.name)) and self.unify(value, Atomic(f.value))


class Halt_0(BuiltIn):
    '''halt

    halt neither succeeds nor fails. It has the side effect of exiting from the
    processor and returning to whatever system invoked Prolog.'''

    def execute(self):
        exit(0)


class Halt_1(BuiltIn):
    '''halt(+integer)

    halt(X) neither succeeds nor fails. It has the side effect of exiting from
    the processor and returning to whatever system invoked Prolog, passing the
    value of X as a message.'''

    def execute(self, x):
        if isvariable(x):
            self.throw_instantiation_error()
        if not isvariable(x) and not isnumber(x) and not isinstance(x.value, int):
            self.throw_type_error('integer', x)

        exit(x.value)


# Utility functions

def free_variable_set(t, v):
    '''The free variable set FV of a term T with respect to
    a term V is a set of variables defined as the set
    difference of the variable set of T and BV where BV is
    a set of variables defined as the union of the variable
    set of V and the existential variable set of T.'''
    vst = variable_set(t)
    vsv = variable_set(v)
    est = existential_variable_set(t)
    return vst.difference(vsv.union(est))

# TODO This should be distributed onto the Term hierarchy classes
def variable_set(term):
    '''The variable set Sv of a term T is a set of variables
    defined recursively as follows:
      * if T is an atomic term, then Sv is the empty set
      * else if T is a variable then Sv is {T}
      * else if T is a compound term then Sv is the union of
      the variable sets for each of the arguments of T.'''
    from ..core import deref
    if isinstance(term, Variable):
        if term.isfree():
            return {term}
        else:
            term = deref(term)
    if isinstance(term, Atomic):
        return set()
    s = set()
    if isinstance(term, Compound):
        for arg in term.value[1:]:
            s.update(variable_set(arg))
    else: # a list
        for e in term.as_list():
            s.update(variable_set(e))
    return s

def existential_variable_set(term):
    '''The existential variables set EV of a term T is a set
    of variables defined recursively as follows:
      * if T is a variable or an atomic term, then EV is the
      empty set
      * else if T unifies with ^(V, G) then EV is the union
      of the variable set of V and the existential variables
      set of the term G
      * else EV is the empty set.'''
    s = set()
    if isinstance(term, Atomic) or isvariable(term):
        return s
    if term.name == '^' and term.arity == 2:
        s.update(variable_set(term.value[1]))
        s.update(existential_variable_set(term.value[2]))
        return s
    return s
    
def iterated_goal_term(term):
    '''The iterated goal term G of a term T is a term defined
    recursively as follows:
      * if T unifies with ^(_, Goal) then G is the iterated
      goal term of Goal
      * else G is T.'''
    if term.name == '^' and term.arity == 2:
        return iterated_goal_term(term.value[2])
    return term

def isvariant(t, v):
    '''Two terms are variants if there is a bijection s of
    the variables of the former to the variables of the
    latter such that the latter term results from replacing
    each variable X in the former by Xs.'''
    from ..core import deref
    t = deref(t)
    v = deref(v)
    if isinstance(t, Atomic) and isinstance(v, Atomic):
        return t == v
    if isvariable(t) and isvariable(v):
        return True
    if isinstance(t, Compound) and isinstance(v, Compound):
        if t.name != v.name or t.arity != v.arity:
            return False
        bijection = {}
        for a1, a2 in zip(t.value[1:], v.value[1:]):
            if isvariable(a1) and isvariable(a2) and not a1.name.startswith('_'):
                a = bijection.get(a1)
                if a is not None and a2 != a:
                    return False
                else:
                    bijection[a1] = a2
            else:
                if not isvariant(a1, a2):
                    return False
        return True
    return False

PREDICATES = {
    # Term unification (ISO 8.2)
    '=/2' : Unify_2,
    '\=/2' : NotUnifiable_2,
    # Type testing (ISO 8.3)
    'var/1' : Var_1,
    'atom/1' : Atom_1,
    'integer/1' : Integer_1,
    'float/1' : Float_1,
    'atomic/1' : Atomic_1,
    'compound/1' : Compound_1,
    'nonvar/1' : Nonvar_1,
    'number/1' : Number_1,
    # Term comparison (ISO 8.4)
    '@=</2' : TermLessThanOrEqual_2,
    '==/2' : TermIdentical_2,
    '\==/2' : TermNotIdentical_2,
    '@</2' : TermLessThan_2,
    '@>/2' : TermGreaterThan_2,
    '@>=/2' : TermGreaterThanOrEqual_2,
    # Term creation and decomposition (ISO 8.5)
    'functor/3' : Functor_3,
    'arg/3' : Arg_3,
    '=../2' : Univ_2,
    'copy_term/2' : CopyTerm_2,
    # Arithmetic evaluation (ISO 8.6)
    'is/2' : Is_2,
    # Arithmetic comparison (ISO 8.7)
    '=:=/2' : ArithmeticEqual_2,
    '=\=/2' : ArithmeticNotEqual_2,
    '</2' : ArithmeticLessThan_2,
    '=</2' : ArithmeticLessThanOrEqual_2,
    '>/2' : ArithmeticGreaterThan_2,
    '>=/2' : ArithmeticGreaterThanOrEqual_2,
    # Clause retrival and information (ISO 8.8)
    'clause/2' : Clause_2,
    'current_predicate/1' : CurrentPredicate_1,
    # Clause creation and destruction (ISO 8.9)
    'asserta/1' : Asserta_1,
    'assertz/1' : Assertz_1,
    'retract/1' : Retract_1,
    'abolish/1' : Abolish_1,
    # All solutions (ISO 8.10)
    'findall/3' : Findall_3,
    'bagof/3' : Bagof_3,
    'setof/3' : Setof_3,
    # Logic and control (ISO 8.15)
    # FIXME \+ does not work because of what is probably a parser
    # bug: the operator's "scope" is much wider than the single
    # goal, even when using parentheses!
    '\+/1' : Not_1, 'not/1' : Not_1,
    'repeat/0' : Repeat_0,
    # Atomic term processing (ISO 8.16)
    'atom_length/2' : AtomLength_2,
    'atom_concat/3' : AtomConcat_3,
    'sub_atom/5' : SubAtom_5,
    'atom_chars/2' : AtomChars_2,
    'atom_codes/2' : AtomCodes_2,
    'char_code/2' : CharCode_2,
    'number_chars/2' : NumberChars_2,
    'number_codes/2' : NumberCodes_2,
    # Implementation defined hooks (ISO 8.17)
    'set_prolog_flag/2' : SetPrologFlag_2,
    'current_prolog_flag/2' : CurrentPrologFlag_2,
    'halt/0' : Halt_0,
    'halt/1' : Halt_1
}
