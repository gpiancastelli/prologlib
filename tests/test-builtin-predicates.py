import unittest

from prologlib.builtin import iso

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
### Term unification (ISO 8.2)
###

class UnifyTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_number_unification(self):
        goal = "'='(1, 1)."
        self.assertTrue(self.engine.solve(goal))
    def test_normal_unification(self):
        goal = "'='(X, 1)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
    def test_variable_unification(self):
        goal = "'='(X, Y)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'Y' : Variable('X')}, self.engine.currsubst())
    def test_anonymous_variable_unification(self):
        goal = "'='(_, _)."
        self.assertTrue(self.engine.solve(goal))
    def test_double_unification(self):
        goal = "'='(X, Y), '='(X, abc)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('abc'), self.engine.currsubst()['X'])
        self.assertEquals(Atomic('abc'), self.engine.currsubst()['Y'])
    def test_cross_unification(self):
        goal = "'='(f(X, def), f(def, Y))."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('def'), self.engine.currsubst()['X'])
        self.assertEquals(Atomic('def'), self.engine.currsubst()['Y'])
    def test_different_numbers(self):
        goal = "'='(1, 2)."
        self.assertFalse(self.engine.solve(goal))
    def test_integer_vs_real_numbers(self):
        goal = "'='(1, 1.0)."
        self.assertFalse(self.engine.solve(goal))
    def test_different_functors(self):
        goal = "'='(g(X), f(f(X)))."
        self.assertFalse(self.engine.solve(goal))
    def test_different_arity(self):
        goal = "'='(f(X, 1), f(a(X)))."
        self.assertFalse(self.engine.solve(goal))
    def test_different_arity_over_occur_check(self):
        goal = "'='(f(X, Y, X), f(a(X), a(Y), Y, 2))."
        self.assertFalse(self.engine.solve(goal))
    # TODO Missing four undefined tests that verify the behaviour of
    # =/2 with occur-check problems. Currently, prologlib crashes if
    # you attempt to unify two STO terms by =/2 instead of using the
    # proper unify_with_occur_check/2 predicate.

class NotUnifiableTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_number_unification(self):
        goal = "'\\='(1, 1)."
        self.assertFalse(self.engine.solve(goal))
    def test_normal_unification(self):
        goal = "\=(X, 1)."
        self.assertFalse(self.engine.solve(goal))
    def test_variable_unification(self):
        goal = "'\\='(X, Y)."
        self.assertFalse(self.engine.solve(goal))
    def test_anonymous_variable_unification(self):
        goal = "\=(_, _)."
        self.assertFalse(self.engine.solve(goal))
    def test_cross_unification(self):
        goal = "\=(f(X, def), f(def, Y))."
        self.assertFalse(self.engine.solve(goal))
    def test_different_numbers(self):
        goal = "'\\='(1, 2)."
        self.assertTrue(self.engine.solve(goal))
    def test_integer_vs_real_numbers(self):
        goal = "\=(1, 1.0)."
        self.assertTrue(self.engine.solve(goal))
    def test_different_functors(self):
        goal = "'\\='(g(X), f(f(X)))."
        self.assertTrue(self.engine.solve(goal))
    def test_different_arity(self):
        goal = "\=(f(X, 1), f(a(X)))."
        self.assertTrue(self.engine.solve(goal))
    def test_different_arity_over_occur_check(self):
        goal = "'\\='(f(X, Y, X), f(a(X), a(Y), Y, 2))."
        self.assertTrue(self.engine.solve(goal))
    def test_terms_subject_to_occur_check(self):
        goal = "\=(X, a(X))."
        self.assertFalse(self.engine.solve(goal))
        goal = "'\\='(f(X, 1), f(a(X), 2))."
        self.assertTrue(self.engine.solve(goal))
        goal = "'\\='(f(1, X, 1), f(2, a(X), 2))."
        self.assertTrue(self.engine.solve(goal))
        goal = "\=(f(2, X), f(2, a(X)))."
        self.assertFalse(self.engine.solve(goal))
        goal = "'\\='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2))."
        self.assertTrue(self.engine.solve(goal))

###
### Type testing (ISO 8.3)
###

class VarTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_atom(self):
        self.assertFalse(self.engine.solve('var(foo).'))
    def test_variable(self):
        self.assertTrue(self.engine.solve('var(Foo).'))
    def test_bounded_variable(self):
        self.assertFalse(self.engine.solve('foo = Foo, var(Foo).'))
    def test_anonymous_variable(self):
        self.assertTrue(self.engine.solve('var(_).'))

class AtomTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_atom(self):
        self.assertTrue(self.engine.solve('atom(atom).'))
    def test_single_quoted_atom(self):
        self.assertTrue(self.engine.solve("atom('string')."))
    def test_compound(self):
        self.assertFalse(self.engine.solve('atom(a(b)).'))
    def test_variable(self):
        self.assertFalse(self.engine.solve('atom(Var).'))
    def test_empty_list(self):
        self.assertTrue(self.engine.solve('atom([]).'))
    def test_integer_number(self):
        self.assertFalse(self.engine.solve('atom(6).'))
    def test_float_number(self):
        self.assertFalse(self.engine.solve('atom(3.3).'))

class IntegerTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_positive_integer(self):
        self.assertTrue(self.engine.solve('integer(3).'))
    def test_negative_integer(self):
        self.assertTrue(self.engine.solve('integer(-3).'))
    def test_float(self):
        self.assertFalse(self.engine.solve('integer(3.3).'))
    def test_variable(self):
        self.assertFalse(self.engine.solve('integer(X).'))
    def test_atom(self):
        self.assertFalse(self.engine.solve('integer(atom).'))

class FloatTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_positive_float(self):
        self.assertTrue(self.engine.solve('float(3.3).'))
    def test_negative_float(self):
        self.assertTrue(self.engine.solve('float(-3.3).'))
    def test_integer(self):
        self.assertFalse(self.engine.solve('float(3).'))
    def test_atom(self):
        self.assertFalse(self.engine.solve('float(atom).'))
    def test_variable(self):
        self.assertFalse(self.engine.solve('float(X).'))

class AtomicTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_atom(self):
        self.assertTrue(self.engine.solve('atomic(atom).'))
    def test_compound(self):
        self.assertFalse(self.engine.solve('atomic(a(b)).'))
    def test_variable(self):
        self.assertFalse(self.engine.solve('atomic(Var).'))
    def test_integer(self):
        self.assertTrue(self.engine.solve('atomic(3).'))
    def test_float(self):
        self.assertTrue(self.engine.solve('atomic(3.3).'))

class CompoundTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_positive_float(self):
        self.assertFalse(self.engine.solve('compound(33.3).'))
    def test_negative_float(self):
        self.assertFalse(self.engine.solve('compound(-33.3).'))
    def test_negative_atom(self):
        self.assertTrue(self.engine.solve('compound(-a).'))
    def test_anonymous_variable(self):
        self.assertFalse(self.engine.solve('compound(_).'))
    def test_atom(self):
        self.assertFalse(self.engine.solve('compound(a).'))
    def test_compound(self):
        self.assertTrue(self.engine.solve('compound(a(b)).'))
    def test_empty_list(self):
        self.assertFalse(self.engine.solve('compound([]).'))
    def test_nonempty_list(self):
        self.assertTrue(self.engine.solve('compound([a]).'))

class NonvarTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_float(self):
        self.assertTrue(self.engine.solve('nonvar(33.3).'))
    def test_atom(self):
        self.assertTrue(self.engine.solve('nonvar(foo).'))
    def test_variable(self):
        self.assertFalse(self.engine.solve('nonvar(Foo).'))
    def test_bounded_variable(self):
        self.assertTrue(self.engine.solve('foo = Foo, nonvar(Foo).'))
    def test_anonymous_variable(self):
        self.assertFalse(self.engine.solve('nonvar(_).'))
    def test_compound(self):
        self.assertTrue(self.engine.solve('nonvar(a(b)).'))

class NumberTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_positive_integer(self):
        self.assertTrue(self.engine.solve('number(3).'))
    def test_postive_float(self):
        self.assertTrue(self.engine.solve('number(3.3).'))
    def test_negative_integer(self):
        self.assertTrue(self.engine.solve('number(-3).'))
    def test_atom(self):
        self.assertFalse(self.engine.solve('number(a).'))
    def test_variable(self):
        self.assertFalse(self.engine.solve('number(X).'))

###
### Term comparison (ISO 8.4)
###

class TermComparisonTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_term_less_than_or_equal(self):
        self.assertTrue(self.engine.solve("'@=<'(1.0, 1)."))
        self.assertTrue(self.engine.solve("'@=<'(aardvark, zebra)."))
        self.assertTrue(self.engine.solve("'@=<'(short, short)."))
        self.assertTrue(self.engine.solve("'@=<'(short, shorter)."))
        self.assertTrue(self.engine.solve("'@=<'(X, X)."))
        self.assertTrue(self.engine.solve("'@=<'(X, Y)."))
        self.assertTrue(self.engine.solve("'@=<'(_, _)."))
        self.assertTrue(self.engine.solve("'@=<'(foo(X, a), foo(Y, b))."))
    def test_term_identical(self):
        self.assertTrue(self.engine.solve("'=='(X, X)."))
        self.assertFalse(self.engine.solve("'=='(X, Y)."))
        self.assertFalse(self.engine.solve("'=='(_, _)."))
    def test_list_identical(self):
        '''A non-ISO regression test to avoid a bug when comparing lists and compounds'''
        self.assertTrue(self.engine.solve("'=='([1,2,3], .(1,.(2,.(3,[]))))."))
        self.assertTrue(self.engine.solve("'=='(.(1,.(2,.(3,[]))), [1,2,3])."))
    def test_term_not_identical(self):
        self.assertFalse(self.engine.solve("'\\=='(1, 1)."))
        self.assertTrue(self.engine.solve('\==(_, _).'))
    def test_term_less_than(self):
        self.assertTrue(self.engine.solve("'@<'(1.0, 1)."))
        self.assertFalse(self.engine.solve("'@<'(foo(a, b), north(a))."))
        self.assertTrue(self.engine.solve("'@<'(foo(a, X), foo(b, Y))."))
        self.assertTrue(self.engine.solve("'@<'(foo(X, a), foo(Y, b))."))
    def test_term_greater_than(self):
        self.assertTrue(self.engine.solve("'@>'(foo(b), foo(a))."))
    def test_term_greater_than_or_equal(self):
        self.assertFalse(self.engine.solve("'@>='(short, shorter)."))
        self.assertFalse(self.engine.solve("'@>='(_, _)."))

###
### Term creation and decomposition (ISO 8.5)
###

class FunctorTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_compound_decomposition(self):
        goal = 'functor(foo(a, b, c), foo, 3).'
        self.assertTrue(self.engine.solve(goal))
    def test_compound_unification(self):
        goal = 'functor(foo(a, b, c), X, Y).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('foo'), self.engine.currsubst()['X'])
        self.assertEquals(Atomic(3), self.engine.currsubst()['Y'])
    def test_compound_composition(self):
        goal = 'functor(X, foo, 3).'
        self.assertTrue(self.engine.solve(goal))
        term = self.engine.currsubst()['X'] # should be foo(_,_,_)
        self.assertEquals('foo', term.name)
        for v in term.value[1:]:
            self.assertTrue(isinstance(v, Variable))
            self.assertTrue(v.isfree())
            self.assertTrue(v.isanonymous())
    def test_atom_composition(self):
        goal = 'functor(X, foo, 0).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('foo'), self.engine.currsubst()['X'])
    def test_compound_with_variables(self):
        goal = 'functor(mats(A, B), A, B).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('mats'), self.engine.currsubst()['A'])
        self.assertEquals(Atomic(2), self.engine.currsubst()['B'])
    def test_wrong_arity(self):
        goal = 'functor(foo(a), foo, 2).'
        self.assertFalse(self.engine.solve(goal))
    def test_wrong_name(self):
        goal = 'functor(foo(a), fo, 1).'
        self.assertFalse(self.engine.solve(goal))
    def test_integer_number(self):
        goal = 'functor(1, X, Y).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic(1), self.engine.currsubst()['X'])
        self.assertEquals(Atomic(0), self.engine.currsubst()['Y'])
    def test_float_number(self):
        goal = 'functor(X, 1.1, 0).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic(1.1), self.engine.currsubst()['X'])
    def test_list(self):
        goal = "functor([_|_], '.', 2)."
        self.assertTrue(self.engine.solve(goal))
    def test_empty_list(self):
        goal = 'functor([], [], 0).'
        self.assertTrue(self.engine.solve(goal))
    def test_term_and_name_as_variables(self):
        goal = 'functor(X, Y, 3).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_term_and_arity_as_variables(self):
        goal = 'functor(X, foo, N).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_arity_not_integer(self):
        goal = 'functor(X, foo, a).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('integer'), Atomic('a'))
        self.assertEquals(error, caught.error_term())
    def test_name_as_number(self):
        goal = 'functor(F, 1.5, 1).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atom'), Atomic(1.5))
        self.assertEquals(error, caught.error_term())            
    def test_name_as_compound(self):
        goal = 'functor(F, foo(a), 1).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atomic'), Compound('foo', Atomic('a')))
        self.assertEquals(error, caught.error_term())
    # TODO Missing max_arity implementation
##     def test_representation_error(self):
##         goal = 'current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).'
##         try:
##             self.engine.solve(goal)
##             self.fail()
##         except PrologError as e:
##             error = Compound('representation_error', Atomic('max_arity'))
##             self.assertEquals(error, e.error_term())
    def test_arity_as_negative_number(self):
        goal = 'Minus_1 is 0 - 1, functor(F, foo(a), Minus_1).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('domain_error', Atomic('not_less_than_zero'), Atomic(-1))
        self.assertEquals(error, caught.error_term())

class ArgTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_arg_ground(self):
        goal = 'arg(1, foo(a, b), a).'
        self.assertTrue(self.engine.solve(goal))
    def test_arg_as_variable(self):
        goal = 'arg(1, foo(a, b), X).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('a'), self.engine.currsubst()['X'])
    def test_variable_as_arg(self):
        goal = 'arg(1, foo(X, b), a).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('a'), self.engine.currsubst()['X'])
    def test_args_as_variables(self):
        goal = 'arg(1, foo(X, b), Y).'
        self.assertTrue(self.engine.solve(goal))
        # TODO Should be X = Y, not Y = X
        self.assertEquals(Variable('X'), self.engine.currsubst()['Y'])
    def test_wrong_arg(self):
        goal = 'arg(1, foo(a, b), b).'
        self.assertFalse(self.engine.solve(goal))
    def test_functor(self):
        goal = 'arg(0, foo(a, b), foo).'
        self.assertFalse(self.engine.solve(goal))
    def test_arg_index_out_of_bound(self):
        goal = 'arg(3, foo(3, 4), N).'
        self.assertFalse(self.engine.solve(goal))
    def test_n_as_variable(self):
        goal = 'arg(X, foo(a, b), a).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())            
    def test_term_as_variable(self):
        goal = 'arg(1, X, a).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_term_as_atom(self):
        goal = 'arg(0, atom, A).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('compound'), Atomic('atom'))
        self.assertEquals(error, caught.error_term())
    def test_term_as_integer(self):
        goal = 'arg(0, 3, A).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('compound'), Atomic(3))
        self.assertEquals(error, caught.error_term())
    # TODO Missing occur-check implementation
##     def test_undefined_unification(self):
##         goal = 'arg(1, foo(X), u(X)).'
##         self.assertFalse(self.engine.solve(goal))

class UnivTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_ground(self):
        goal = "'=..'(foo(a, b), [foo, a, b])."
        self.assertTrue(self.engine.solve(goal))
    def test_term_unification(self):
        goal = "'=..'(X, [foo, a, b])."
        self.assertTrue(self.engine.solve(goal))
        t = Compound('foo', Atomic('a'), Atomic('b'))
        self.assertEquals(t, self.engine.currsubst()['X'])
    def test_list_unification(self):
        goal = "'=..'(foo(a, b), L)."
        self.assertTrue(self.engine.solve(goal))
        t = List.from_list([Atomic('foo'), Atomic('a'), Atomic('b')])
        self.assertEquals(t, self.engine.currsubst()['L'])
    def test_unification(self):
        goal = "'=..'(foo(X, b), [foo, a, Y])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('a'), self.engine.currsubst()['X'])
        self.assertEquals(Atomic('b'), self.engine.currsubst()['Y'])
    def test_atomic_term(self):
        goal = "'=..'(1, [1])."
        self.assertTrue(self.engine.solve(goal))
    def test_wrong_arguments_order(self):
        goal = "'=..'(foo(a, b), [foo, b, a])."
        self.assertFalse(self.engine.solve(goal))
    def test_term_and_list_as_variables(self):
        goal = "'=..'(X, Y)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_list_as_partial_list(self):
        goal = "'=..'(X, [foo, a | Y])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_not_a_list(self):
        goal = "'=..'(X, [foo|bar])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('list'), List(Atomic('foo'), Atomic('bar')))
        self.assertEquals(error, caught.error_term())
    def test_functor_as_variable(self):
        goal = "'=..'(X, [Foo, bar])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_functor_as_integer(self):
        goal = "'=..'(X, [3, 1])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atom'), Atomic(3))
        self.assertEquals(error, caught.error_term())
    def test_functor_as_float(self):
        goal = "'=..'(X, [1.1, foo])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atom'), Atomic(1.1))
        self.assertEquals(error, caught.error_term())
    def test_functor_as_compound(self):
        goal = "'=..'(X, [a(b), 1])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        c = Compound('a', Atomic('b'))
        error = Compound('type_error', Atomic('atom'), c)
        self.assertEquals(error, caught.error_term())
    def test_list_as_number(self):
        goal = "'=..'(X, 4)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('list'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    # TODO Missing occur-check implementation
##     def test_undefined_unification(self):
##         goal = "'=..'(f(X), [f, u(X)])."
##         self.assertFalse(self.engine.solve(goal))

class CopyTermTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_copy_variables(self):
        self.assertTrue(self.engine.solve('copy_term(X, Y).'))
        self.assertEquals({}, self.engine.currsubst())
    def test_copy_number(self):
        self.assertTrue(self.engine.solve('copy_term(X, 3).'))
        self.assertEquals({}, self.engine.currsubst())
    def test_copy_atom(self):
        self.assertTrue(self.engine.solve('copy_term(_, a).'))
        self.assertEquals({}, self.engine.currsubst())
    def test_copy_with_atom_unification(self):
        self.assertTrue(self.engine.solve('copy_term(a+X, X+b).'))
        self.assertEquals({'X' : Atomic('a')}, self.engine.currsubst())
    def test_copy_anonymous_variable(self):
        self.assertTrue(self.engine.solve('copy_term(_, _).'))
        self.assertEquals({}, self.engine.currsubst())
    def test_copy_with_variable_unification(self):
        self.assertTrue(self.engine.solve('copy_term(X+X+Y, A+B+B).'))
        self.assertEquals({'B' : Variable('A')}, self.engine.currsubst())
    def test_copy_different_atoms(self):
        self.assertFalse(self.engine.solve('copy_term(a, b).'))
    def test_double_copy_with_unification(self):
        self.assertFalse(self.engine.solve('copy_term(a+X, X+b), copy_term(a+X, X+b).'))
    # TODO Missing occur-check implementation
##     def test_undefined_unification(self):
##         goal = 'copy_term(demoen(X, X), demoen(Y, f(Y)).'
##         self.assertFalse(self.engine.solve(goal))

###
### Arithmetic evaluation (ISO 8.6)
###

class IsTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_sum(self):
        goal = "'is'(Result, 3+11.0)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic(14.0), self.engine.currsubst()['Result'])
    def test_variable(self):
        goal = 'X = 1+2, Y is X * 3.'
        self.assertTrue(self.engine.solve(goal))
        sum = Compound('+', Atomic(1), Atomic(2))
        self.assertEquals(sum, self.engine.currsubst()['X'])
        self.assertEquals(Atomic(9), self.engine.currsubst()['Y'])
    def test_integer_equality(self):
        self.assertTrue(self.engine.solve("'is'(3, 3)."))
    def test_integer_float_equality(self):
        self.assertFalse(self.engine.solve("'is'(3, 3.0)."))
    def test_atom(self):
        self.assertFalse(self.engine.solve("'is'(foo, 77)."))
    def test_instantiation_error(self):
        goal = "'is'(77, N)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())

###
### Arithmetic evaluation (ISO 8.7)
###

class ArithmeticEvaluationTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_arithmetic_equal(self):
        self.assertFalse(self.engine.solve("'=:='(0, 1)."))
        self.assertTrue(self.engine.solve("'=:='(1.0, 1)."))
        self.assertTrue(self.engine.solve("'=:='(3*2, 7-1)."))
        caught = self.assertRaises(PrologError, self.engine.solve, "'=:='(X, 5).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_arithmetic_not_equal(self):
        self.assertTrue(self.engine.solve("'=\\='(0, 1)."))
        self.assertFalse(self.engine.solve("=\=(1.0, 1)."))
        self.assertFalse(self.engine.solve("'=\\='(3*2, 7-1)."))
        caught = self.assertRaises(PrologError, self.engine.solve, "=\=(X, 5).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_arithmetic_less_than(self):
        self.assertTrue(self.engine.solve("'<'(0, 1)."))
        self.assertFalse(self.engine.solve("'<'(1.0, 1)."))
        self.assertFalse(self.engine.solve("'<'(3*2, 7-1)."))
        caught = self.assertRaises(PrologError, self.engine.solve, "'<'(X, 5).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_arithmetic_less_than_or_equal(self):
        self.assertTrue(self.engine.solve("'=<'(0, 1)."))
        self.assertTrue(self.engine.solve("'=<'(1.0, 1)."))
        self.assertTrue(self.engine.solve("'=<'(3*2, 7-1)."))
        caught = self.assertRaises(PrologError, self.engine.solve, "'=<'(X, 5).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_arithmetic_greater_than(self):
        self.assertFalse(self.engine.solve("'>'(0, 1)."))
        self.assertFalse(self.engine.solve("'>'(1.0, 1)."))
        self.assertFalse(self.engine.solve("'>'(3*2, 7-1)."))
        caught = self.assertRaises(PrologError, self.engine.solve, "'>'(X, 5).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_arithmetic_greater_than_or_equal(self):
        self.assertFalse(self.engine.solve("'>='(0, 1)."))
        self.assertTrue(self.engine.solve("'>='(1.0, 1)."))
        self.assertTrue(self.engine.solve("'>='(3*2, 7-1)."))
        caught = self.assertRaises(PrologError, self.engine.solve, "'>='(X, 5).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())

###
### Simple arithmetic functors (ISO 9.1)
###

class EvaluableFunctorsTest(unittest.TestCase):
    '''This class contains assorted tests to exercise Prolog
    evaluable functors, dealing with mathematical operations.
    Tests exploit is/2 as a mean to trigger the functor
    evaluation on the Prolog side, instead of directly unit
    testing the functions that realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_add(self):
        self.assertTrue(self.solve("'+'(7, 35)."))
        self.assertEquals(Atomic(42), self.result())
        self.assertTrue(self.solve("'+'(0, 3+11)."))
        self.assertEquals(Atomic(14), self.result())
        self.assertTrue(self.solve("'+'(0, 3.2+11)."))
        self.assertEquals(Atomic(14.2), self.result())
        caught = self.assertRaises(PrologError, self.solve, "'+'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'+'(foo, 77).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_neg(self):
        self.assertTrue(self.solve("'-'(7)."))
        self.assertEquals(Atomic(-7), self.result())
        self.assertTrue(self.solve("'-'(3-11)."))
        self.assertEquals(Atomic(8), self.result())
        self.assertTrue(self.solve("'-'(3.2-11)."))
        self.assertEquals(Atomic(7.8), self.result())
        caught = self.assertRaises(PrologError, self.solve, "'-'(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'-'(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    # TODO FIXME Parser bug for -/2! (see TODO file)
##     def test_sub(self):
##         self.assertTrue(self.solve("'-'(7, 35)."))
##         self.assertEquals(Atomic(-28), self.result())
##         # And so on...
    def test_mul(self):
        self.assertTrue(self.solve("'*'(7, 35)."))
        self.assertEquals(Atomic(245), self.result())
        self.assertTrue(self.solve("'*'(0, 3+11)."))
        self.assertEquals(Atomic(0), self.result())
        self.assertTrue(self.solve("'*'(1.5, 3.2+11)."))
        # using values instead of terms for floating point almost-equality
        self.assertAlmostEquals(21.3, self.result().value)
        caught = self.assertRaises(PrologError, self.solve, "'*'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'*'(foo, 77).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_div(self):
        self.assertTrue(self.solve("'/'(7, 35)."))
        self.assertEquals(Atomic(0), self.result())
        self.assertTrue(self.solve("'/'(7.0, 35)."))
        self.assertEquals(Atomic(0.2), self.result())
        self.assertTrue(self.solve("'/'(140, 3+11)."))
        self.assertEquals(Atomic(10), self.result())
        self.assertTrue(self.solve("'/'(20.164, 3.2+11)."))
        # using values instead of terms for floating point almost-equality
        self.assertAlmostEquals(1.42, self.result().value)
        self.assertTrue(self.solve("'/'(7, -3)."))
        self.assertEquals(Atomic(-3), self.result())
        self.assertTrue(self.solve("'/'(-7, 3)."))
        self.assertEquals(Atomic(-3), self.result())
        caught = self.assertRaises(PrologError, self.solve, "'/'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'/'(foo, 77).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'/'(3, 0).")
        error = Compound('evaluation_error', Atomic('zero_divisor'))
        self.assertEquals(error, caught.error_term())
    # Note that intdiv tests are not included in ISO Standard
    def test_intdiv(self):
        self.assertTrue(self.solve("'//'(7, 35)."))
        self.assertEquals(Atomic(0), self.result())
        self.assertTrue(self.solve("'//'(140, 3+11)."))
        self.assertEquals(Atomic(10), self.result())
        self.assertTrue(self.solve("'//'(7, -3)."))
        self.assertEquals(Atomic(-3), self.result())
        self.assertTrue(self.solve("'//'(-7, 3)."))
        self.assertEquals(Atomic(-3), self.result())
        caught = self.assertRaises(PrologError, self.solve, "'//'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'//'(foo, 77).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "'//'(7.0, 35).")
        error = Compound('type_error', Atomic('integer'), Atomic(7.0))
        self.assertEquals(error, caught.error_term())
    def test_mod(self):
        self.assertTrue(self.solve("mod(7, 3)."))
        self.assertEquals(Atomic(1), self.result())
        self.assertTrue(self.solve("mod(0, 3+11)."))
        self.assertEquals(Atomic(0), self.result())
        self.assertTrue(self.solve("mod(7, -2)."))
        self.assertEquals(Atomic(-1), self.result())
        caught = self.assertRaises(PrologError, self.solve, "mod(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "mod(foo, 77).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "mod(7.5, 2).")
        error = Compound('type_error', Atomic('integer'), Atomic(7.5))
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "mod(7, 0).")
        error = Compound('evaluation_error', Atomic('zero_divisor'))
        self.assertEquals(error, caught.error_term())
    def test_floor(self):
        self.assertTrue(self.solve("floor(7.4)."))
        self.assertEquals(Atomic(7), self.result())
        self.assertTrue(self.solve("floor(-0.4)."))
        self.assertEquals(Atomic(-1), self.result())
    def test_round(self):
        self.assertTrue(self.solve("round(7.5)."))
        self.assertEquals(Atomic(8), self.result())
        self.assertTrue(self.solve("round(7.6)."))
        self.assertEquals(Atomic(8), self.result())
        self.assertTrue(self.solve("round(-0.6)."))
        self.assertEquals(Atomic(-1), self.result())
        caught = self.assertRaises(PrologError, self.solve, "round(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_ceiling(self):
        self.assertTrue(self.solve("ceiling(-0.5)."))
        self.assertEquals(Atomic(0), self.result())
    def test_truncate(self):
        self.assertTrue(self.solve("truncate(-0.5)."))
        self.assertEquals(Atomic(0), self.result())
        caught = self.assertRaises(PrologError, self.solve, "truncate(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_float(self):
        self.assertTrue(self.solve("float(7)."))
        self.assertEquals(Atomic(7.0), self.result())
        self.assertTrue(self.solve("float(7.3)."))
        self.assertEquals(Atomic(7.3), self.result())
        self.assertTrue(self.solve("float(5 / 3)."))
        self.assertEquals(Atomic(1.0), self.result())
        caught = self.assertRaises(PrologError, self.solve, "float(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "float(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_abs(self):
        self.assertTrue(self.solve("abs(7)."))
        self.assertEquals(Atomic(7), self.result())
        self.assertTrue(self.solve("abs(3-11)."))
        self.assertEquals(Atomic(8), self.result())
        self.assertTrue(self.solve("abs(3.2-11.0)."))
        self.assertEquals(Atomic(7.8), self.result())
        caught = self.assertRaises(PrologError, self.solve, "abs(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
        caught = self.assertRaises(PrologError, self.solve, "abs(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    # Note that sign tests are not included in ISO Standard
    def test_sign(self):
        self.assertTrue(self.solve("sign(7)."))
        self.assertEquals(Atomic(1), self.result())
        self.assertTrue(self.solve("sign(-6.7)."))
        self.assertEquals(Atomic(-1.0), self.result())
        self.assertTrue(self.solve("sign(-0.0)."))
        self.assertEquals(Atomic(0.0), self.result())
    # Note that float_integer_part tests are not included in ISO Standard
    def test_float_integer_part(self):
        self.assertTrue(self.solve("float_integer_part(7.0)."))
        self.assertEquals(Atomic(7.0), self.result())
        self.assertTrue(self.solve("float_integer_part(-0.4)."))
        self.assertEquals(Atomic(-0.0), self.result())
        caught = self.assertRaises(PrologError, self.solve, "float_integer_part(5).")
        error = Compound('type_error', Atomic('float'), Atomic(5))
        self.assertEquals(error, caught.error_term())
    # Note that float_fractional_part tests are not included in ISO Standard
    def test_float_fractional_part(self):
        self.assertTrue(self.solve("float_fractional_part(7.0)."))
        self.assertEquals(Atomic(0.0), self.result())
        self.assertTrue(self.solve("float_fractional_part(-0.4)."))
        self.assertEquals(Atomic(-0.4), self.result())
        caught = self.assertRaises(PrologError, self.solve, "float_fractional_part(5).")
        error = Compound('type_error', Atomic('float'), Atomic(5))
        self.assertEquals(error, caught.error_term())
    # TODO Missing all tests for max_integer flag and int_overflow errors

###
### Other arithmetic functors (ISO 9.3)
###

class PowerTest(unittest.TestCase):
    '''This class contains tests to exercise the **/2
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_integer_power(self):
        self.assertTrue(self.solve("'**'(5, 3)."))
        self.assertEquals(Atomic(125.0), self.result())
    def test_negative_float_base(self):
        self.assertTrue(self.solve("'**'(-5.0, 3)."))
        self.assertEquals(Atomic(-125.0), self.result())
    def test_negative_integer_exponent(self):
        self.assertTrue(self.solve("'**'(5, -1)."))
        self.assertEquals(Atomic(0.2), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "'**'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "'**'(foo, 2).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_float_exponent(self):
        self.assertTrue(self.solve("'**'(5, 3.0)."))
        self.assertEquals(Atomic(125.0), self.result())
    def test_zero_powered_to_zero(self):
        self.assertTrue(self.solve("'**'(0.0, 0)."))
        self.assertEquals(Atomic(1.0), self.result())

class SinTest(unittest.TestCase):
    '''This class contains tests to exercise the sin/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_sin_of_float_zero(self):
        self.assertTrue(self.solve("sin(0.0)."))
        self.assertEquals(Atomic(0.0), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "sin(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_sin_of_integer_zero(self):
        self.assertTrue(self.solve("sin(0)."))
        self.assertEquals(Atomic(0.0), self.result())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "sin(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_sin_with_pi(self):
        goal = 'PI is atan(1.0) * 4, X is sin(PI / 2.0).'
        self.assertTrue(self.engine.solve(goal))
        from math import pi
        self.assertEquals(Atomic(pi), self.engine.currsubst()['PI'])
        self.assertEquals(Atomic(1.0), self.engine.currsubst()['X'])

class CosTest(unittest.TestCase):
    '''This class contains tests to exercise the cos/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_cos_of_float_zero(self):
        self.assertTrue(self.solve("cos(0.0)."))
        self.assertEquals(Atomic(1.0), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "cos(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_cos_of_integer_zero(self):
        self.assertTrue(self.solve("cos(0)."))
        self.assertEquals(Atomic(1.0), self.result())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "cos(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_cos_with_pi(self):
        goal = 'PI is atan(1.0) * 4, X is cos(PI / 2.0).'
        self.assertTrue(self.engine.solve(goal))
        from math import pi
        self.assertEquals(Atomic(pi), self.engine.currsubst()['PI'])
        # using values instead of terms for floating point almost-equality
        self.assertAlmostEquals(0.0, self.engine.currsubst()['X'].value)

class AtanTest(unittest.TestCase):
    '''This class contains tests to exercise the atan/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_atan_of_float_zero(self):
        self.assertTrue(self.solve("atan(0.0)."))
        self.assertEquals(Atomic(0.0), self.result())
    def test_atan_with_pi(self):
        goal = 'PI is atan(1.0) * 4.'
        self.assertTrue(self.engine.solve(goal))
        from math import pi
        self.assertEquals(Atomic(pi), self.engine.currsubst()['PI'])
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "atan(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atan_of_integer_zero(self):
        self.assertTrue(self.solve("atan(0)."))
        self.assertEquals(Atomic(0.0), self.result())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "atan(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

class ExpTest(unittest.TestCase):
    '''This class contains tests to exercise the exp/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_exp_of_float_zero(self):
        self.assertTrue(self.solve("exp(0.0)."))
        self.assertEquals(Atomic(1.0), self.result())
    def test_exp_with_e(self):
        self.assertTrue(self.solve("exp(1.0)."))
        from math import e
        self.assertEquals(Atomic(e), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "exp(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_exp_of_integer_zero(self):
        self.assertTrue(self.solve("exp(0)."))
        self.assertEquals(Atomic(1.0), self.result())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "exp(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

class LogTest(unittest.TestCase):
    '''This class contains tests to exercise the log/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_log_of_one(self):
        self.assertTrue(self.solve("log(1.0)."))
        self.assertEquals(Atomic(0.0), self.result())
    def test_log_with_e(self):
        self.assertTrue(self.solve("log(2.7182818)."))
        # using values instead of terms for floating point almost-equality
        self.assertAlmostEquals(1.0, self.result().value)
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "log(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_log_of_integer_zero(self):
        caught = self.assertRaises(PrologError, self.solve, "log(0).")
        error = Compound('evaluation_error', Atomic('undefined'))
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "log(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
    def test_log_of_float_zero(self):
        caught = self.assertRaises(PrologError, self.solve, "log(0.0).")
        error = Compound('evaluation_error', Atomic('undefined'))
        self.assertEquals(error, caught.error_term())

class SqrtTest(unittest.TestCase):
    '''This class contains tests to exercise the sqrt/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_sqrt_of_zero(self):
        self.assertTrue(self.solve("sqrt(0.0)."))
        self.assertEquals(Atomic(0.0), self.result())
    def test_sqrt_of_one(self):
        self.assertTrue(self.solve("sqrt(1)."))
        self.assertEquals(Atomic(1.0), self.result())
    def test_sqrt_of_float(self):
        self.assertTrue(self.solve("sqrt(1.21)."))
        self.assertEquals(Atomic(1.1), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "sqrt(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_sqrt_of_negative_number(self):
        caught = self.assertRaises(PrologError, self.solve, "sqrt(-1.0).")
        error = Compound('evaluation_error', Atomic('undefined'))
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "sqrt(foo).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

###
### Bitwise functors (ISO 9.4)
###

class RightShiftTest(unittest.TestCase):
    '''This class contains tests to exercise the >>/2
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_precise_shift(self):
        self.assertTrue(self.solve("'>>'(16, 2)."))
        self.assertEquals(Atomic(4), self.result())
    def test_remainder_shift(self):
        self.assertTrue(self.solve("'>>'(19, 2)."))
        self.assertEquals(Atomic(4), self.result())
    def test_shift_negative_number(self):
        self.assertTrue(self.solve("'>>'(-16, 2)."))
        self.assertEquals(Atomic(-4), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "'>>'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "'>>'(foo, 2).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

class LeftShiftTest(unittest.TestCase):
    '''This class contains tests to exercise the <</2
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_precise_shift(self):
        self.assertTrue(self.solve("'<<'(16, 2)."))
        self.assertEquals(Atomic(64), self.result())
    def test_remainder_shift(self):
        self.assertTrue(self.solve("'<<'(19, 2)."))
        self.assertEquals(Atomic(76), self.result())
    def test_shift_negative_number(self):
        self.assertTrue(self.solve("'<<'(-16, 2)."))
        self.assertEquals(Atomic(-64), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "'<<'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "'<<'(foo, 2).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

class BitAndTest(unittest.TestCase):
    '''This class contains tests to exercise the /\/2
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_bitand(self):
        self.assertTrue(self.solve("'/\\'(10, 12)."))
        self.assertEquals(Atomic(8), self.result())
    def test_bitand_alternative_syntax(self):
        self.assertTrue(self.solve("/\(10, 12)."))
        self.assertEquals(Atomic(8), self.result())
    def test_bitand_with_operations(self):
        self.assertTrue(self.solve("'/\\'(17 * 256 + 125, 255)."))
        self.assertEquals(Atomic(125), self.result())
    def test_bitand_negative(self):
        self.assertTrue(self.solve("/\(-10, 12)."))
        self.assertEquals(Atomic(4), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "'/\\'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "'/\\'(foo, 2).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

class BitOrTest(unittest.TestCase):
    '''This class contains tests to exercise the \//2
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_bitor(self):
        self.assertTrue(self.solve("'\\/'(10, 12)."))
        self.assertEquals(Atomic(14), self.result())
    def test_bitor_alternative_syntax(self):
        self.assertTrue(self.solve("\/(10, 12)."))
        self.assertEquals(Atomic(14), self.result())
    def test_bitor_mask(self):
        self.assertTrue(self.solve("'\\/'(125, 255)."))
        self.assertEquals(Atomic(255), self.result())
    def test_bitand_negative(self):
        self.assertTrue(self.solve("\/(-10, 12)."))
        self.assertEquals(Atomic(-2), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "'\\/'(77, N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "'\\/'(foo, 2).")
        error = Compound('type_error', Atomic('number'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())

class BitNotTest(unittest.TestCase):
    '''This class contains tests to exercise the \/1
    evaluable functor. Tests exploit is/2 as a mean to
    trigger the functor evaluation on the Prolog side,
    instead of directly unit testing the functions that
    realize the functors.'''
    def setUp(self):
        self.engine = Engine()
    def solve(self, goal):
        return self.engine.solve('X is ' + goal)
    def result(self):
        return self.engine.currsubst()['X']
    def test_bitnot_identity(self):
        self.assertTrue(self.solve("'\\'('\\'(10))."))
        self.assertEquals(Atomic(10), self.result())
    def test_bitor_alternative_syntax(self):
        self.assertTrue(self.solve("\(\(10))."))
        self.assertEquals(Atomic(10), self.result())
    def test_bitor(self):
        self.assertTrue(self.solve("\(10)."))
        self.assertEquals(Atomic(-11), self.result())
    def test_variable(self):
        caught = self.assertRaises(PrologError, self.solve, "'\\'(N).")
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom(self):
        caught = self.assertRaises(PrologError, self.solve, "'\\'(2.5).")
        error = Compound('type_error', Atomic('integer'), Atomic(2.5))
        self.assertEquals(error, caught.error_term())

###
### Clause retrival and information (ISO 8.8)
###

class ClauseTest(unittest.TestCase):
    def setUp(self):
        theory = """
        :- dynamic(cat/0).
        cat.

        :- dynamic(dog/0).
        dog :- true.

        elk(X) :- moose(X).

        :- dynamic(legs/2).
        legs(A, 6) :- insect(A).
        legs(A, 7) :- A, call(A).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).
        """.lstrip()
        self.engine = Engine()
        self.engine._consult(theory)
    def tearDown(self):
        self.engine._clear()
    def test_clause_fact(self):
        self.assertTrue(self.engine.solve('clause(cat, true).'))
    def test_clause_fact_as_rule(self):
        self.assertTrue(self.engine.solve('clause(dog, true).'))
    def test_clause_rule_with_unification(self):
        self.assertTrue(self.engine.solve('clause(legs(I, 6), Body).'))
        s = {'Body' : Compound('insect', Variable('I'))}
        self.assertEquals(s, self.engine.currsubst())
    def test_clause_rule_with_convertion(self):
        self.assertTrue(self.engine.solve('clause(legs(C, 7), Body).'))
        callc = Compound('call', Variable('C'))
        body = Compound(',', callc, callc)
        self.assertEquals({'Body' : body}, self.engine.currsubst())
    def test_clause_with_reexecution(self):
        self.assertTrue(self.engine.solve('clause(insect(I), T).'))
        self.assertEquals({'I' : Atomic('ant'), 'T' : Atomic.TRUE}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'I' : Atomic('bee'), 'T' : Atomic.TRUE}, self.engine.currsubst())
    def test_undefined_predicate(self):
        self.assertFalse(self.engine.solve('clause(x, Body).'))
    def test_head_as_variable(self):
        goal = "clause(_, B)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_head_as_number(self):
        goal = "clause(4, X)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    def test_private_procedure(self):
        goal = "clause(elk(N), Body)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        pi = Compound('/', Atomic('elk'), Atomic(1))
        args = (Atomic('access'), Atomic('private_procedure'), pi)
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())
    def test_builtin_procedure(self):
        self.assertFalse(self.engine.solve("clause(atom(_), Body)."))
        # Note that the ISO standard prescribe the following error
        # to be thrown, but I believe that accessing a built-in
        # predicate from clause/2 should just fail.
        # See http://prolog.logtalk.org for details.
##         try:
##             self.engine.solve("clause(atom(_), Body).")
##             self.fail()
##         except PrologError as e:
##             args = (Atomic('access'), Atomic('private_procedure'), Atomic('atom/1'))
##             error = Compound('permission_error', *args)
##             self.assertEquals(error, e.error_term())
    # TODO Missing occur-check implementation
##     def test_undefined_unification(self):
##         goal = 'clause(legs(A, 6), insect(f(A))).'
##         self.assertFalse(self.engine.solve(goal))

class CurrentPredicateTest(unittest.TestCase):
    def setUp(self):
        theory = """
        :- dynamic(cat/0).
        cat.

        :- dynamic(dog/0).
        dog :- true.

        elk(X) :- moose(X).

        :- dynamic(legs/2).
        legs(A, 6) :- insect(A).
        legs(A, 7) :- A, call(A).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).
        """.lstrip()
        self.engine = Engine()
        self.engine._consult(theory)
    def tearDown(self):
        self.engine._clear()
    def test_fact(self):
        self.assertTrue(self.engine.solve('current_predicate(dog/0).'))
    def test_builtin_indicator(self):
        goal = 'current_predicate(current_predicate/1).'
        self.assertFalse(self.engine.solve(goal))
    def test_arity_unification(self):
        self.assertTrue(self.engine.solve('current_predicate(elk/Arity).'))
        self.assertEquals(Atomic(1), self.engine.currsubst()['Arity'])
    def test_unknown_procedure_name(self):
        self.assertFalse(self.engine.solve('current_predicate(foo/A).'))
    def test_name_unification(self):
        self.assertTrue(self.engine.solve('current_predicate(Name/1).'))
        self.assertEquals(Atomic('elk'), self.engine.currsubst()['Name'])
        self.assertTrue(self.engine.solve_next())
        self.assertEquals(Atomic('insect'), self.engine.currsubst()['Name'])
    def test_pi_as_number(self):
        goal = "current_predicate(4)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('predicate_indicator'), Atomic(4))
        self.assertEquals(error, caught.error_term())

###
### Clause creation and destruction (ISO 8.9)
###

class AssertaTest(unittest.TestCase):
    def setUp(self):
        theory = """
        :- dynamic(legs/2).
        legs(A, 6) :- insect(A).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).
        """.lstrip()
        self.engine = Engine()
        self.engine._consult(theory)
    def tearDown(self):
        self.engine._clear()
    def test_db_modification(self):
        fact = 'asserta(legs(octopus, 8)).'
        self.assertTrue(self.engine.solve(fact))
        rule = 'asserta((legs(A, 4) :- animal(A))).'
        self.assertTrue(self.engine.solve(rule))
        newrule = 'asserta((foo(X) :- X, call(X))).'
        self.assertTrue(self.engine.solve(newrule))
        result = """
        :- dynamic(legs/2).
        legs(A, 4) :- animal(A).
        legs(octopus, 8).
        legs(A, 6) :- insect(A).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).

        :- dynamic(foo/1).
        foo(X) :- call(X), call(X).""".lstrip()
        engine = Engine()
        engine._consult(result)
        self.assertEquals(engine._kb, self.engine._kb)
    def test_assert_variable(self):
        goal = "asserta(_)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_assert_number(self):
        goal = "asserta(4)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    def test_assert_body_as_number(self):
        goal = "asserta((foo :- 4))."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    def test_assert_static_procedure(self):
        goal = 'asserta((atom(_) :- true)).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        pi = Compound('/', Atomic('atom'), Atomic(1))
        args = (Atomic('modify'), Atomic('static_procedure'), pi)
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class AssertzTest(unittest.TestCase):
    def setUp(self):
        theory = """
        :- dynamic(legs/2).
        legs(A, 4) :- animal(A).
        legs(octopus, 8).
        legs(A, 6) :- insect(A).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).

        :- dynamic(foo/1).
        foo(X) :- call(X), call(X).
        """.lstrip()
        self.engine = Engine()
        self.engine._consult(theory)
    def tearDown(self):
        self.engine._clear()
    def test_db_modification(self):
        fact = 'assertz(legs(spider, 8)).'
        self.assertTrue(self.engine.solve(fact))
        rule = 'assertz((legs(B, 2) :- bird(B))).'
        self.assertTrue(self.engine.solve(rule))
        newrule = 'assertz((foo(X) :- X -> call(X))).'
        self.assertTrue(self.engine.solve(newrule))
        result = """
        :- dynamic(legs/2).
        legs(A, 4) :- animal(A).
        legs(octopus, 8).
        legs(A, 6) :- insect(A).
        legs(spider, 8).
        legs(B, 2) :- bird(B).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).

        :- dynamic(foo/1).
        foo(X) :- call(X), call(X).
        foo(X) :- call(X) -> call(X).""".lstrip()
        engine = Engine()
        engine._consult(result)
        self.assertEquals(engine._kb, self.engine._kb)
    def test_assert_variable(self):
        goal = "assertz(_)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_assert_number(self):
        goal = "assertz(4)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    def test_assert_body_as_number(self):
        goal = "assertz((foo :- 4))."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    def test_assert_static_procedure(self):
        goal = 'assertz((atom(_) :- true)).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        pi = Compound('/', Atomic('atom'), Atomic(1))
        args = (Atomic('modify'), Atomic('static_procedure'), pi)
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class RetractTest(unittest.TestCase):
    def setUp(self):
        theory = """
        :- dynamic(legs/2).
        legs(A, 4) :- animal(A).
        legs(octopus, 8).
        legs(A, 6) :- insect(A).
        legs(spider, 8).
        legs(B, 2) :- bird(B).

        :- dynamic(insect/1).
        insect(ant).
        insect(bee).

        :- dynamic(foo/1).
        foo(X) :- call(X), call(X).
        foo(X) :- call(X) -> call(X).""".lstrip()
        self.engine = Engine()
        self.engine._consult(theory)
    def tearDown(self):
        self.engine._clear()
    def test_retract_legs(self):
        fact = 'retract(legs(octopus, 8)).'
        self.assertTrue(self.engine.solve(fact))
        nonexistent = 'retract(legs(spider, 6)).'
        self.assertFalse(self.engine.solve(nonexistent))
        rule = 'retract((legs(X, 2) :- T)).'
        self.assertTrue(self.engine.solve(rule))
        bird = Compound('bird', Variable('X'))
        self.assertEquals({'T' : bird}, self.engine.currsubst())
        rule_with_alternatives = 'retract((legs(X, Y) :- Z)).'
        self.assertTrue(self.engine.solve(rule_with_alternatives))
        body = Compound('animal', Variable('X'))
        self.assertEquals({'Y' : Atomic(4), 'Z' : body}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        body = Compound('insect', Variable('X'))
        self.assertEquals({'Y' : Atomic(6), 'Z' : body}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        body = Atomic.TRUE
        self.assertEquals({'X' : Atomic('spider'), 'Y' : Atomic(8), 'Z' : body}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())
        self.assertFalse(self.engine.solve(rule))
    def test_retract_insect(self):
        goal = 'retract(insect(I)), write(I), retract(insect(bee)), fail.'
        from io import StringIO
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve(goal))
        self.assertEquals('antbee', output.getvalue())
        # restore the output stream's original value
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM
    def test_retract_foo(self):
        # TODO Missing occur-check implementation
        #goal = 'retract((foo(A) :- A, call(A))).'
        #self.assertFalse(self.engine.solve(goal))
        goal = 'retract((foo(C) :- A -> B)).'
        self.assertTrue(self.engine.solve(goal))
        callc = Compound('call', Variable('C'))
        self.assertEquals({'A' : callc, 'B' : callc}, self.engine.currsubst())
    def test_head_as_variable(self):
        goal = 'retract((X :- in_eec(Y))).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_head_as_number(self):
        goal = 'retract((4 :- X)).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())
    def test_retract_static_procedure(self):
        goal = "retract((atom(_) :- X == '[]'))."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        pi = Compound('/', Atomic('atom'), Atomic(1))
        args = (Atomic('modify'), Atomic('static_procedure'), pi)
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

class AbolishTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_abolish_procedure(self):
        self.assertTrue(self.engine.solve('abolish(foo/2).'))
    def test_pi_without_arity(self):
        goal = 'abolish(foo/_).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_abolish_not_a_pi(self):
        goal = 'abolish(foo).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('predicate_indicator'), Atomic('foo'))
        self.assertEquals(error, caught.error_term())
        goal = 'abolish(foo(_)).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = caught.error_term()
        self.assertEquals('type_error', error.name)
        self.assertEquals(Atomic('predicate_indicator'), error.value[1])
        pi = error.value[2]
        self.assertEquals('foo', pi.name)
        self.assertEquals(1, pi.arity)
        self.assertTrue(pi.value[1].isanonymous())
    def test_abolish_static_procedure(self):
        goal = 'abolish(abolish/1).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        # TODO What about using priority in term comparisons?
        pi = Compound('/', Atomic('abolish'), Atomic(1), priority=400)
        args = (Atomic('modify'), Atomic('static_procedure'), pi)
        error = Compound('permission_error', *args)
        self.assertEquals(error, caught.error_term())

###
### All solutions (ISO 8.10)
###

class FindallTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_different_solutions(self):
        goal = 'findall(X, (X=1; X=2), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_anonymous_solution(self):
        goal = 'findall(X+Y, (X=1), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(1, len(solutions))
        solution = solutions.head
        self.assertEquals('+', solution.name)
        self.assertEquals(2, len(solution.value[1:]))
        self.assertEquals(Atomic(1), solution.value[1])
        v = solution.value[2]
        self.assertTrue(isinstance(v, Variable))
        self.assertTrue(v.isanonymous())
    def test_no_solutions(self):
        goal = 'findall(X, fail, S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.EMPTY_LIST
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_same_solutions(self):
        goal = 'findall(X, (X=1; X=1), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(1)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_wrong_solution_order(self):
        goal = 'findall(X, (X=2; X=1), [1, 2]).'
        self.assertFalse(self.engine.solve(goal))
    def test_solution_with_unification(self):
        goal = 'findall(X, (X=1; X=2), [X, Y]).'
        self.assertTrue(self.engine.solve(goal))
        solutions = {'X' : Atomic(1), 'Y' : Atomic(2)}
        self.assertEquals(solutions, self.engine.currsubst())
    def test_goal_as_variable(self):
        goal = 'findall(X, Goal, S).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_goal_as_number(self):
        goal = 'findall(X, 4, S).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(4))
        self.assertEquals(error, caught.error_term())

class BagofTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_different_solutions(self):
        goal = 'bagof(X, (X=1 ; X=2), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_template_and_solution_independence(self):
        goal = 'bagof(X, (X=1 ; X=2), X).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'X' : solutions}, self.engine.currsubst())
    def test_template_binding_variables(self):
        goal = 'bagof(X, (X=Y ; X=Z), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(2, len(solutions))
        self.assertEquals(Variable('Y'), solutions.head.value)
        self.assertEquals(Variable('Z'), solutions.tail.head.value)
    def test_fail(self):
        self.assertFalse(self.engine.solve('bagof(X, fail, S).'))
    def test_with_reexecution(self):
        goal = 'bagof(1, (Y=1 ; Y=2), L).'
        self.assertTrue(self.engine.solve(goal))
        s = self.engine.currsubst()
        self.assertEquals({'L' : List(Atomic(1)), 'Y' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'L' : List(Atomic(1)), 'Y' : Atomic(2)}, self.engine.currsubst())
    def test_template_unbound_variable(self):
        goal = 'bagof(f(X, Y), (X=a ; Y=b), L).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['L']
        self.assertTrue(isinstance(solutions, List))
        self.assertTrue(2, len(solutions))
        fa, fb = solutions.head, solutions.tail.head
        self.assertTrue(3, fa.value) # compound name + 2 arguments
        self.assertTrue(3, fb.value) # compound name + 2 arguments
        self.assertEquals('f', fa.name)
        self.assertEquals('f', fb.name)
        self.assertEquals(Atomic('a'), fa.value[1])
        self.assertTrue(fa.value[2].isanonymous())
        self.assertTrue(fb.value[1].isanonymous())
        self.assertEquals(Atomic('b'), fb.value[2])
    def test_iterative_goal(self):
        goal = 'bagof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_iterative_goal_with_anonymous_alternative(self):
        goal = 'bagof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(3, len(solutions))
        a, b, c = solutions.head, solutions.tail.head, solutions.tail.tail.head
        self.assertEquals(Atomic(1), a)
        self.assertTrue(b.isanonymous())
        self.assertEquals(Atomic(2), c)
    def test_non_iterative_goal(self):
        from prologlib import core
        from prologlib.builtin import io as prologio
        flag = core._FLAGS['unknown']
        core._FLAGS['unknown'] = flag._replace(value='warning')
        from io import StringIO
        output = StringIO()
        prologio.STANDARD_OUTPUT_STREAM = output
        goal = 'bagof(X, (Y^(X=1 ; Y=2) ; X=3), S).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals('Warning: the procedure ^/2 is undefined', output.getvalue().strip())
        self.assertEquals({'S' : List(Atomic(3))}, self.engine.currsubst())
        # restore the original values of flag and output stream
        core._FLAGS['unknown'] = flag._replace(value='error')
        prologio.STANDARD_OUTPUT_STREAM = prologio.STANDARD_OUTPUT_STREAM
    def test_multiple_different_solutions(self):
        goal = 'bagof(X, (X=Y ; X=Z ; Y=1), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(2, len(solutions))
        self.assertEquals(Variable('Y'), solutions.head.value)
        self.assertEquals(Variable('Z'), solutions.tail.head.value)
        self.assertTrue(self.engine.solve_next())
        self.assertEquals(Atomic(1), self.engine.currsubst()['Y'])
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(1, len(solutions))
        self.assertTrue(solutions.head.isanonymous())
    def test_with_anonymous_facts(self):
        theory = '''
        a(1, f(_)).
        a(2, f(_)).'''.lstrip()
        self.engine._consult(theory)
        goal = 'bagof(X, a(X, Y), L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals(lst, self.engine.currsubst()['L'])
        y = self.engine.currsubst()['Y']
        self.assertEquals(2, len(y.value)) # compound name + 1 argument
        self.assertEquals('f', y.name)
        self.assertTrue(y.value[1].isanonymous())
    def test_with_ground_facts(self):
        theory = '''
        b(1, 1).
        b(1, 1).
        b(1, 2).
        b(2, 1).
        b(2, 2).
        b(2, 2).'''.lstrip()
        self.engine._consult(theory)
        goal = 'bagof(X, b(X, Y), L).'
        self.assertTrue(self.engine.solve(goal))
        l = List.from_list([Atomic(1), Atomic(1), Atomic(2)])
        self.assertEquals({'L' : l, 'Y' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        l = List.from_list([Atomic(1), Atomic(2), Atomic(2)])
        self.assertEquals({'L' : l, 'Y' : Atomic(2)}, self.engine.currsubst())
    def test_iterative_goal_as_variable(self):
        goal = 'bagof(X, Y^Z, L).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_goal_as_number(self):
        goal = 'bagof(X, 1, L).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(1))
        self.assertEquals(error, caught.error_term())

class SetofTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_different_solutions(self):
        goal = 'setof(X, (X=1 ; X=2), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_template_and_solution_independence(self):
        goal = 'setof(X, (X=1 ; X=2), X).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'X' : solutions}, self.engine.currsubst())
    def test_different_ordered_solutions(self):
        goal = 'setof(X, (X=2 ; X=1), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_no_duplicate_solutions(self):
        goal = 'setof(X, (X=2 ; X=2), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_template_binding_variables(self):
        goal = 'setof(X, (X=Y ; X=Z), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(2, len(solutions))
        self.assertEquals(Variable('Y'), solutions.head.value)
        self.assertEquals(Variable('Z'), solutions.tail.head.value)
    def test_fail(self):
        self.assertFalse(self.engine.solve('setof(X, fail, S).'))
    def test_with_reexecution(self):
        goal = 'setof(1, (Y=2 ; Y=1), L).'
        self.assertTrue(self.engine.solve(goal))
        s = self.engine.currsubst()
        self.assertEquals({'L' : List(Atomic(1)), 'Y' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'L' : List(Atomic(1)), 'Y' : Atomic(2)}, self.engine.currsubst())
    def test_template_unbound_variable(self):
        goal = 'setof(f(X, Y), (X=a ; Y=b), L).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['L']
        self.assertTrue(isinstance(solutions, List))
        self.assertTrue(2, len(solutions))
        fb, fa = solutions.head, solutions.tail.head
        self.assertTrue(3, fa.value) # compound name + 2 arguments
        self.assertTrue(3, fb.value) # compound name + 2 arguments
        self.assertEquals('f', fa.name)
        self.assertEquals('f', fb.name)
        self.assertEquals(Atomic('a'), fa.value[1])
        self.assertTrue(fa.value[2].isanonymous())
        self.assertTrue(fb.value[1].isanonymous())
        self.assertEquals(Atomic('b'), fb.value[2])
    def test_iterative_goal(self):
        goal = 'setof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'S' : solutions}, self.engine.currsubst())
    def test_iterative_goal_with_anonymous_alternative(self):
        goal = 'setof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(3, len(solutions))
        a, b, c = solutions.head, solutions.tail.head, solutions.tail.tail.head
        self.assertTrue(a.isanonymous())
        self.assertEquals(Atomic(1), b)
        self.assertEquals(Atomic(2), c)
    def test_non_iterative_goal(self):
        from prologlib import core
        from prologlib.builtin import io as prologio
        flag = core._FLAGS['unknown']
        core._FLAGS['unknown'] = flag._replace(value='warning')
        from io import StringIO
        output = StringIO()
        prologio.STANDARD_OUTPUT_STREAM = output
        goal = 'setof(X, (Y^(X=1 ; Y=2) ; X=3), S).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals('Warning: the procedure ^/2 is undefined', output.getvalue().strip())
        self.assertEquals({'S' : List(Atomic(3))}, self.engine.currsubst())
        # restore the original values of flag and output stream
        core._FLAGS['unknown'] = flag._replace(value='error')
        prologio.STANDARD_OUTPUT_STREAM = prologio.STANDARD_OUTPUT_STREAM
    def test_multiple_different_solutions(self):
        goal = 'setof(X, (X=Y ; X=Z ; Y=1), S).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(2, len(solutions))
        self.assertEquals(Variable('Y'), solutions.head.value)
        self.assertEquals(Variable('Z'), solutions.tail.head.value)
        self.assertTrue(self.engine.solve_next())
        self.assertEquals(Atomic(1), self.engine.currsubst()['Y'])
        solutions = self.engine.currsubst()['S']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(1, len(solutions))
        self.assertTrue(solutions.head.isanonymous())
    def test_with_anonymous_facts(self):
        theory = '''
        a(1, f(_)).
        a(2, f(_)).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, a(X, Y), L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals(lst, self.engine.currsubst()['L'])
        y = self.engine.currsubst()['Y']
        self.assertEquals(2, len(y.value)) # compound name + 1 argument
        self.assertEquals('f', y.name)
        self.assertTrue(y.value[1].isanonymous())
    def test_with_member_predicate_1(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, member(X, [f(U,b), f(V,c)]), L).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['L']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(2, len(solutions))
        fb, fc = solutions.head, solutions.tail.head
        self.assertTrue(3, fb.value) # compound name + 2 arguments
        self.assertTrue(3, fc.value) # compound name + 2 arguments
        self.assertEquals('f', fb.name)
        self.assertEquals('f', fc.name)
        self.assertEquals(Variable('U'), fb.value[1].value)
        self.assertEquals(Variable('V'), fc.value[1].value)
        self.assertEquals(Atomic('b'), fb.value[2])
        self.assertEquals(Atomic('c'), fc.value[2])
    def test_with_member_predicate_2(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, member(X, [f(U,b), f(V,c)]), [f(a,c), f(a,b)]).'
        self.assertFalse(self.engine.solve(goal))
    def test_with_member_predicate_3(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, member(X, [f(b,U), f(c,V)]), [f(b,a), f(c,a)]).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('a'), self.engine.currsubst()['U'])
        self.assertEquals(Atomic('a'), self.engine.currsubst()['V'])
    def test_with_member_predicate_4(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, member(X, [V, U, f(U), f(V)]), L).'
        self.assertTrue(self.engine.solve(goal))
        solutions = self.engine.currsubst()['L']
        self.assertTrue(isinstance(solutions, List))
        self.assertEquals(4, len(solutions))
        a, b = solutions.head, solutions.tail.head
        c, d = solutions.tail.tail.head, solutions.tail.tail.tail.head
        self.assertEquals(Variable('U'), a.value)
        self.assertEquals(Variable('V'), b.value)
        self.assertTrue(2, c.value) # compound name + 1 argument
        self.assertEquals('f', c.name)
        self.assertEquals(Variable('U'), c.value[1].value)
        self.assertTrue(2, d.value) # compound name + 1 argument
        self.assertEquals('f', d.name)
        self.assertEquals(Variable('V'), d.value[1].value)
    def test_with_member_predicate_5(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, member(X, [V, U, f(U), f(V)]), [a, b, f(a), f(b)]).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic('a'), self.engine.currsubst()['U'])
        self.assertEquals(Atomic('b'), self.engine.currsubst()['V'])
    def test_with_member_predicate_6(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, member(X, [V, U, f(U), f(V)]), [a, b, f(b), f(a)]).'
        self.assertFalse(self.engine.solve(goal))
    @unittest.skip('FIXME Problems with copying variables?')
    def test_with_member_predicate_7(self):
        theory = '''
        member(X, [X | _]).
        member(X, [_ | L]) :- member(X, L).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, exists(U, V)^member(X, [V, U, f(U), f(V)]), [a, b, f(b), f(a)]).'
        self.assertTrue(self.engine.solve(goal))
    def test_with_ground_facts_1(self):
        theory = '''
        b(1, 1).
        b(1, 1).
        b(1, 2).
        b(2, 1).
        b(2, 2).
        b(2, 2).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X, b(X, Y), L).'
        self.assertTrue(self.engine.solve(goal))
        l = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'L' : l, 'Y' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        l = List.from_list([Atomic(1), Atomic(2)])
        self.assertEquals({'L' : l, 'Y' : Atomic(2)}, self.engine.currsubst())
    def test_with_ground_facts_2(self):
        theory = '''
        b(1, 1).
        b(1, 1).
        b(1, 2).
        b(2, 1).
        b(2, 2).
        b(2, 2).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X-Xs, Y^setof(Y, b(X,Y), Xs), L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(1), Atomic(2)])
        first = Compound('-', Atomic(1), lst)
        second = Compound('-', Atomic(2), lst)
        solution = List.from_list([first, second])
        self.assertEquals(solution, self.engine.currsubst()['L'])
    def test_with_ground_facts_3(self):
        theory = '''
        b(1, 1).
        b(1, 1).
        b(1, 2).
        b(2, 1).
        b(2, 2).
        b(2, 2).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X-Xs, setof(Y, b(X,Y), Xs), L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(1), Atomic(2)])
        first = Compound('-', Atomic(1), lst)
        second = Compound('-', Atomic(2), lst)
        solution = List.from_list([first, second])
        self.assertEquals(solution, self.engine.currsubst()['L'])
    def test_with_ground_facts_4(self):
        theory = '''
        d(1, 1).
        d(1, 2).
        d(1, 1).
        d(2, 2).
        d(2, 1).
        d(2, 2).'''.lstrip()
        self.engine._consult(theory)
        goal = 'setof(X-Xs, bagof(Y, d(X,Y), Xs), L).'
        self.assertTrue(self.engine.solve(goal))
        first_list = List.from_list([Atomic(1), Atomic(2), Atomic(1)])
        second_list = List.from_list([Atomic(2), Atomic(1), Atomic(2)])
        first = Compound('-', Atomic(1), first_list)
        second = Compound('-', Atomic(2), second_list)
        solution = List.from_list([first, second])
        self.assertEquals(solution, self.engine.currsubst()['L'])

###
### Logic and control (ISO 8.15)
###

class NotTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_not_true(self):
        self.assertFalse(self.engine.solve("'\\+'(true)."))
    def test_cut(self):
        self.assertFalse(self.engine.solve('\+(!).'))
    def test_cut_fail(self):
        self.assertTrue(self.engine.solve("'\\+'((!, fail))."))
    def test_alternative(self):
        goal = '(X=1 ; X=2), \+((!, fail)).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic(1), self.engine.currsubst()['X'])
        self.assertTrue(self.engine.solve_next())
        self.assertEquals(Atomic(2), self.engine.currsubst()['X'])
    def test_not_unification(self):
        self.assertTrue(self.engine.solve("'\\+'(4 = 5)."))
    def test_goal_as_number(self):
        goal = '\+(3).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('callable'), Atomic(3))
        self.assertEquals(error, caught.error_term())
    def test_goal_as_variable(self):
        goal = "'\\+'(X)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_occur_check(self):
        self.assertFalse(self.engine.solve('\+(X = f(X)).'))

# We don't test infinite repeating behaviour
class RepeatTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_break_repeating(self):
        goal = 'repeat, !, fail.'
        self.assertFalse(self.engine.solve(goal))

###
### Atomic term processing (ISO 8.16)
###

class AtomLengthTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_atom(self):
        goal = "atom_length('enchanted evening', N)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'N' : Atomic(17)}, self.engine.currsubst())
    @unittest.skip('FIXME Parser does not simplify \+char expressions')
    def test_atom_with_newline(self):
        goal = "atom_length('enchanted\\\n evening', N)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'N' : Atomic(17)}, self.engine.currsubst())
    def test_empty_atom(self):
        goal = "atom_length('', N)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'N' : Atomic(0)}, self.engine.currsubst())
    def test_wrong_length(self):
        goal = "atom_length('scarlet', 5)."
        self.assertFalse(self.engine.solve(goal))
    def test_atom_as_variable(self):
        goal = 'atom_length(Atom, 4).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_atom_as_number(self):
        goal = 'atom_length(1.23, 4).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atom'), Atomic(1.23))
        self.assertEquals(error, caught.error_term())
    def test_length_as_atom(self):
        goal = "atom_length(atom, '4')."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('integer'), Atomic('4'))
        self.assertEquals(error, caught.error_term())

class AtomConcatTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_concatenation(self):
        goal = "atom_concat('hello', ' world', S3)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'S3' : Atomic('hello world')}, self.engine.currsubst())
    def test_starting_atom(self):
        goal = "atom_concat(T, ' world', 'small world')."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'T' : Atomic('small')}, self.engine.currsubst())
    def test_wrong_starting_atom(self):
        goal = "atom_concat('hello', ' world', 'small world')."
        self.assertFalse(self.engine.solve(goal))
    def test_subatom_construction(self):
        goal = "atom_concat(T1, T2, 'hello')."
        self.assertTrue(self.engine.solve(goal))
        s = self.engine.currsubst()
        self.assertEquals({'T1' : Atomic(''), 'T2' : Atomic('hello')}, s)
        self.assertTrue(self.engine.solve_next())
        s = self.engine.currsubst()
        self.assertEquals({'T1' : Atomic('h'), 'T2' : Atomic('ello')}, s)
        self.assertTrue(self.engine.solve_next())
        s = self.engine.currsubst()
        self.assertEquals({'T1' : Atomic('he'), 'T2' : Atomic('llo')}, s)
        self.assertTrue(self.engine.solve_next())
        s = self.engine.currsubst()
        self.assertEquals({'T1' : Atomic('hel'), 'T2' : Atomic('lo')}, s)
        self.assertTrue(self.engine.solve_next())
        s = self.engine.currsubst()
        self.assertEquals({'T1' : Atomic('hell'), 'T2' : Atomic('o')}, s)
        self.assertTrue(self.engine.solve_next())
        s = self.engine.currsubst()
        self.assertEquals({'T1' : Atomic('hello'), 'T2' : Atomic('')}, s)
    def test_concatenating_variable(self):
        goal = 'atom_concat(small, V2, V4).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())

# FIXME Anonymous variables are still bounded and present in substitutions?
class SubAtomTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_before_and_length(self):
        goal = 'sub_atom(abracadabra, 0, 5, _, S2).'
        self.assertTrue(self.engine.solve(goal))
        #self.assertEquals({'S2' : Atomic('abrac')}, self.engine.currsubst())
        self.assertEquals(Atomic('abrac'), self.engine.currsubst()['S2'])
    def test_length_and_after(self):
        goal = 'sub_atom(abracadabra, _, 5, 0, S2).'
        self.assertTrue(self.engine.solve(goal))
        #self.assertEquals({'S2' : Atomic('dabra')}, self.engine.currsubst())
        self.assertEquals(Atomic('dabra'), self.engine.currsubst()['S2'])
    def test_before_and_after(self):
        goal = 'sub_atom(abracadabra, 3, L, 3, S2).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'S2' : Atomic('acada'), 'L' : Atomic(5)}, self.engine.currsubst())
    def test_length_and_subatom(self):
        goal = 'sub_atom(abracadabra, B, 2, A, ab).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(9), 'B' : Atomic(0)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'A' : Atomic(2), 'B' : Atomic(7)}, self.engine.currsubst())
    def test_quoted_atom(self):
        goal = "sub_atom('Banana', 3, 2, _, S2)."
        self.assertTrue(self.engine.solve(goal))
        #self.assertEquals({'S2' : Atomic('an')}, self.engine.currsubst())
        self.assertEquals(Atomic('an'), self.engine.currsubst()['S2'])
    def test_length(self):
        goal = "sub_atom('charity', _, 3, _, S2)."
        self.assertTrue(self.engine.solve(goal))
        #self.assertEquals({'S2' : Atomic('cha')}, self.engine.currsubst())
        self.assertEquals(Atomic('cha'), self.engine.currsubst()['S2'])
        self.assertTrue(self.engine.solve_next())
        #self.assertEquals({'S2' : Atomic('har')}, self.engine.currsubst())
        self.assertEquals(Atomic('har'), self.engine.currsubst()['S2'])
        self.assertTrue(self.engine.solve_next())
        #self.assertEquals({'S2' : Atomic('ari')}, self.engine.currsubst())
        self.assertEquals(Atomic('ari'), self.engine.currsubst()['S2'])
        self.assertTrue(self.engine.solve_next())
        #self.assertEquals({'S2' : Atomic('rit')}, self.engine.currsubst())
        self.assertEquals(Atomic('rit'), self.engine.currsubst()['S2'])
        self.assertTrue(self.engine.solve_next())
        #self.assertEquals({'S2' : Atomic('ity')}, self.engine.currsubst())
        self.assertEquals(Atomic('ity'), self.engine.currsubst()['S2'])
    def test_any(self):
        goal = "sub_atom('ab', Start, Length, _, Sub_atom)."
        self.assertTrue(self.engine.solve(goal))
        #solution = {'Start' : Atomic(0), 'Length' : Atomic(0), 'Sub_atom' : Atomic('')}
        #self.assertEquals(solution, self.engine.currsubst())
        self.assertEquals(Atomic(0), self.engine.currsubst()['Start'])
        self.assertEquals(Atomic(0), self.engine.currsubst()['Length'])
        self.assertEquals(Atomic(''), self.engine.currsubst()['Sub_atom'])
        self.assertTrue(self.engine.solve_next())
        #solution = {'Start' : Atomic(0), 'Length' : Atomic(1), 'Sub_atom' : Atomic('a')}
        #self.assertEquals(solution, self.engine.currsubst())
        self.assertEquals(Atomic(0), self.engine.currsubst()['Start'])
        self.assertEquals(Atomic(1), self.engine.currsubst()['Length'])
        self.assertEquals(Atomic('a'), self.engine.currsubst()['Sub_atom'])
        self.assertTrue(self.engine.solve_next())
        #solution = {'Start' : Atomic(0), 'Length' : Atomic(2), 'Sub_atom' : Atomic('ab')}
        #self.assertEquals(solution, self.engine.currsubst())
        self.assertEquals(Atomic(0), self.engine.currsubst()['Start'])
        self.assertEquals(Atomic(2), self.engine.currsubst()['Length'])
        self.assertEquals(Atomic('ab'), self.engine.currsubst()['Sub_atom'])
        self.assertTrue(self.engine.solve_next())
        #solution = {'Start' : Atomic(1), 'Length' : Atomic(0), 'Sub_atom' : Atomic('')}
        #self.assertEquals(solution, self.engine.currsubst())
        self.assertEquals(Atomic(1), self.engine.currsubst()['Start'])
        self.assertEquals(Atomic(0), self.engine.currsubst()['Length'])
        self.assertEquals(Atomic(''), self.engine.currsubst()['Sub_atom'])
        self.assertTrue(self.engine.solve_next())
        #solution = {'Start' : Atomic(1), 'Length' : Atomic(1), 'Sub_atom' : Atomic('b')}
        #self.assertEquals(solution, self.engine.currsubst())
        self.assertEquals(Atomic(1), self.engine.currsubst()['Start'])
        self.assertEquals(Atomic(1), self.engine.currsubst()['Length'])
        self.assertEquals(Atomic('b'), self.engine.currsubst()['Sub_atom'])
        self.assertTrue(self.engine.solve_next())
        #solution = {'Start' : Atomic(2), 'Length' : Atomic(0), 'Sub_atom' : Atomic('')}
        #self.assertEquals(solution, self.engine.currsubst())
        self.assertEquals(Atomic(2), self.engine.currsubst()['Start'])
        self.assertEquals(Atomic(0), self.engine.currsubst()['Length'])
        self.assertEquals(Atomic(''), self.engine.currsubst()['Sub_atom'])

class AtomCharsTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_empty_atom(self):
        goal = "atom_chars('', L)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'L' : List.EMPTY_LIST}, self.engine.currsubst())
    def test_empty_list_atom(self):
        goal = "atom_chars([], L)."
        self.assertTrue(self.engine.solve(goal))
        l = List.from_list([Atomic('['), Atomic(']')])
        self.assertEquals({'L' : l}, self.engine.currsubst())
    @unittest.skip('FIXME Bug in parsing and simplifying single quotes')
    def test_single_quote(self):
        goal = "atom_chars('''', L)."
        self.assertTrue(self.engine.solve(goal))
        l = List(('.', Atomic("''''")))
        self.assertEquals({'L' : l}, self.engine.currsubst())
    def test_atom(self):
        goal = "atom_chars('ant', L)."
        self.assertTrue(self.engine.solve(goal))
        l = List.from_list([Atomic('a'), Atomic('n'), Atomic('t')])
        self.assertEquals({'L' : l}, self.engine.currsubst())
    def test_list(self):
        goal = "atom_chars(Str, ['s', 'o', 'p'])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'Str' : Atomic('sop')}, self.engine.currsubst())
    def test_subatom_to_list_tail(self):
        goal = "atom_chars('North', ['N' | X])."
        self.assertTrue(self.engine.solve(goal))
        x = List.from_list([Atomic('o'), Atomic('r'), Atomic('t'), Atomic('h')])
        self.assertEquals({'X' : x}, self.engine.currsubst())
    def test_wrong_list(self):
        goal = "atom_chars('soap', ['s', 'o', 'p'])."
        self.assertFalse(self.engine.solve(goal))
    def test_everything_is_a_variable(self):
        goal = 'atom_chars(X, Y).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())

class AtomCodesTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_empty_atom(self):
        goal = "atom_codes('', L)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'L' : List.EMPTY_LIST}, self.engine.currsubst())
    def test_empty_list_atom(self):
        goal = "atom_codes([], L)."
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(91), Atomic(93)])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_single_quote(self):
        goal = "atom_codes('''', L)."
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(39)])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_atom(self):
        goal = "atom_codes('ant', L)."
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(97), Atomic(110), Atomic(116)])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_list(self):
        goal = "atom_codes(Str, [0's, 0'o, 0'p])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'Str' : Atomic('sop')}, self.engine.currsubst())
    def test_subatom_to_list_tail(self):
        goal = "atom_codes('North', [0'N | X])."
        self.assertTrue(self.engine.solve(goal))
        x = List.from_list([Atomic(111), Atomic(114), Atomic(116), Atomic(104)])
        self.assertEquals({'X' : x}, self.engine.currsubst())
    def test_wrong_list(self):
        goal = "atom_codes('soap', [0's, 0'o, 0'p])."
        self.assertFalse(self.engine.solve(goal))
    def test_everything_is_a_variable(self):
        goal = 'atom_codes(X, Y).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())

class CharCodeTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_character(self):
        goal = "char_code('a', Code)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'Code' : Atomic(97)}, self.engine.currsubst())
    def test_code(self):
        goal = 'char_code(Str, 99).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'Str' : Atomic('c')}, self.engine.currsubst())
    def test_non_ascii_code(self):
        goal = 'char_code(Str, 163).'
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'Str' : Atomic('£')}, self.engine.currsubst())
    def test_wrong_code(self):
        goal = "char_code('b', 84)."
        self.assertFalse(self.engine.solve(goal))
    def test_atom(self):
        goal = "char_code('ab', Int)."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('character'), Atomic('ab'))
        self.assertEquals(error, caught.error_term())
    def test_variables(self):
        goal = 'char_code(C, I).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())

class NumberCharsTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_integer_number(self):
        goal = 'number_chars(33, L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic('3'), Atomic('3')])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_integer_number_with_list(self):
        goal = "number_chars(33, ['3', '3'])."
        self.assertTrue(self.engine.solve(goal))
    def test_real_number(self):
        goal = 'number_chars(33.0, L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic('3'), Atomic('3'), Atomic('.'), Atomic('0')])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_real_list(self):
        goal = "number_chars(X, ['3', '.', '3', 'E', '+', '0'])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'X' : Atomic(3.3)}, self.engine.currsubst())
    def test_real_number_with_list(self):
        goal = "number_chars(3.3, ['3', '.', '3', 'E', '+', '0'])."
        self.assertTrue(self.engine.solve(goal))
    def test_negative_number(self):
        goal = "number_chars(A, ['-', '2', '5'])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(-25)}, self.engine.currsubst())
    @unittest.skip('FIXME Parser does not simplify \+char expressions')
    def test_whitespace_characters(self):
        goal = "number_chars(A, ['\\n', ' ', '3'])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(3)}, self.engine.currsubst())
    @unittest.skip('FIXME Parser allows space after a number')
    def test_syntax_error(self):
        goal = "number_chars(A, ['3', ' '])."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
    def test_hex_number(self):
        goal = "number_chars(A, ['0', x, f])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(15)}, self.engine.currsubst())
    def test_character_integer_sequence(self):
        goal = "number_chars(A, ['0', '''', a])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(ord('a'))}, self.engine.currsubst())
    def test_real_list(self):
        goal = "number_chars(A, ['4', '.', '2'])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(4.2)}, self.engine.currsubst())
    def test_real_list_negative_exponent(self):
        goal = "number_chars(A, ['4', '2', '.', '0', 'e', '-', '1'])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(4.2)}, self.engine.currsubst())

class NumberCodesTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_integer_number(self):
        goal = 'number_codes(33, L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(51), Atomic(51)])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_integer_number_with_list(self):
        goal = "number_codes(33, [0'3, 0'3])."
        self.assertTrue(self.engine.solve(goal))
    def test_real_number(self):
        goal = 'number_codes(33.0, L).'
        self.assertTrue(self.engine.solve(goal))
        lst = List.from_list([Atomic(51), Atomic(51), Atomic(46), Atomic(48)])
        self.assertEquals({'L' : lst}, self.engine.currsubst())
    def test_negative_number(self):
        goal = "number_codes(A, [0'-, 0'2, 0'5])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(-25)}, self.engine.currsubst())
    def test_whitespace_codes(self):
        goal = "number_codes(A, [0' , 0'3])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(3)}, self.engine.currsubst())
    def test_hex_number(self):
        goal = "number_codes(A, [0'0, 0'x, 0'f])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(15)}, self.engine.currsubst())
    @unittest.skip('FIXME Bug in parsing and simplifying single quotes')
    def test_character_code_sequence(self):
        goal = "number_codes(A, [0'0, 0''', 0'a])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(ord('a'))}, self.engine.currsubst())
    def test_real_list(self):
        goal = "number_codes(A, [0'4, 0'., 0'2])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(4.2)}, self.engine.currsubst())
    def test_real_list_negative_exponent(self):
        goal = "number_codes(A, [0'4, 0'2, 0'., 0'0, 0'e, 0'-, 0'1])."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals({'A' : Atomic(4.2)}, self.engine.currsubst())

###
### Implementation defined hooks (ISO 8.17)
###

class SetPrologFlag(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_unknown_flag(self):
        goal = 'set_prolog_flag(unknown, fail).'
        self.assertTrue(self.engine.solve(goal))
        from prologlib import core
        flag = core._FLAGS['unknown']
        self.assertEquals(flag.value, 'fail')
        # restore the flag's original value
        core._FLAGS['unknown'] = flag._replace(value='error')
    def test_flag_as_variable(self):
        goal = 'set_prolog_flag(X, off).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Atomic('instantiation_error')
        self.assertEquals(error, caught.error_term())
    def test_flag_as_number(self):
        goal = 'set_prolog_flag(5, decimals).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atom'), Atomic(5))
        self.assertEquals(error, caught.error_term())
    def test_nonexistent_flag(self):
        goal = "set_prolog_flag(date, 'July 1988')."
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('domain_error', Atomic('prolog_flag'), Atomic('date'))
        self.assertEquals(error, caught.error_term())
    def test_invalid_value(self):
        goal = 'set_prolog_flag(debug, trace).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        culprit = Compound('+', Atomic('debug'), Atomic('trace'))
        error = Compound('domain_error', Atomic('flag_value'), culprit)
        self.assertEquals(error, caught.error_term())

class CurrentPrologFlagTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_debug_flag(self):
        goal = 'current_prolog_flag(debug, off).'
        self.assertTrue(self.engine.solve(goal))
    # WARNING This will fail as more ISO flags will be implemented
    def test_all_flags(self):
        goal = 'current_prolog_flag(F, V).'
        self.assertTrue(self.engine.solve(goal))
        s = {'F' : Atomic('unknown'), 'V' : Atomic('error')}
        self.assertEquals(s, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        s = {'F' : Atomic('debug'), 'V' : Atomic('off')}
        self.assertEquals(s, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        s = {'F' : Atomic('double_quotes'), 'V' : Atomic('atom')}
        self.assertEquals(s, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())
    def test_numeric_flag_name(self):
        goal = 'current_prolog_flag(5, _).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('atom'), Atomic(5))
        self.assertEquals(error, caught.error_term())

# We don't test halt/0 and halt/1 exiting the system
class HaltTest(unittest.TestCase):
    def setUp(self):
        self.engine = Engine()
    def test_wrong_exit_code(self):
        goal = 'halt(a).'
        caught = self.assertRaises(PrologError, self.engine.solve, goal)
        error = Compound('type_error', Atomic('integer'), Atomic('a'))
        self.assertEquals(error, caught.error_term())

###
### Tests for utility functions
###

class ListTest(unittest.TestCase):
    '''Contains tests for lists and partial lists.'''
    def test_empty_list(self):
        term = List.EMPTY_LIST
        self.assertTrue(iso.islist(term))
        self.assertFalse(iso.ispartiallist(term))
    def test_list_functional_notation(self):
        term = Compound('.', Atomic(1), List.EMPTY_LIST)
        self.assertTrue(iso.islist(term))
        self.assertFalse(iso.ispartiallist(term))
    def test_partiallist_functional_notation(self):
        term = Compound('.', Atomic(1), Variable('X'))
        self.assertFalse(iso.islist(term))
        self.assertTrue(iso.ispartiallist(term))
    def test_not_a_list(self):
        term = Compound('.', Atomic(1), Atomic(2))
        self.assertFalse(iso.islist(term))
        self.assertFalse(iso.ispartiallist(term))

class VariableSetTest(unittest.TestCase):
    def setUp(self):
        self.s = {Variable('X'), Variable('Y')}
    def test_compound_term(self):
        term = Compound('f', Variable('X'), Variable('Y'))
        self.assertEquals(self.s, iso.variable_set(term))
    def test_list_term(self):
        term = List.from_list([Variable('Y'), Atomic('f'), Variable('X')])
        self.assertEquals(self.s, iso.variable_set(term))
    def test_variable_order(self):
        term = Compound('f', Variable('Y'), Variable('X'))
        self.assertEquals(self.s, iso.variable_set(term))
    def test_operational_notation_term(self):
        from prologlib.parser import PrologParser
        p = PrologParser('X+Y.')
        term = p.read_term()
        self.assertEquals(self.s, iso.variable_set(term))
    def test_duplicate_variables(self):
        from prologlib.parser import PrologParser
        p = PrologParser('Y-X-X.')
        term = p.read_term()
        self.assertEquals(self.s, iso.variable_set(term))

class ExistentialVariableSetTest(unittest.TestCase):
    def setUp(self):
        self.s = {Variable('X'), Variable('Y')}
    def test_chained_term(self):
        from prologlib.parser import PrologParser
        p = PrologParser('X^Y^f(X,Y,Z).')
        term = p.read_term()
        self.assertEquals(self.s, iso.existential_variable_set(term))
    def test_conjunctive_term(self):
        from prologlib.parser import PrologParser
        p = PrologParser('(X,Y)^f(Z,Y,X).')
        term = p.read_term()
        self.assertEquals(self.s, iso.existential_variable_set(term))
    def test_operational_notation_term(self):
        from prologlib.parser import PrologParser
        p = PrologParser('(X+Y)^3.')
        term = p.read_term()
        self.assertEquals(self.s, iso.existential_variable_set(term))

class FreeVariableSetTest(unittest.TestCase):
    def setUp(self):
        self.s = {Variable('X'), Variable('Y')}
    def test_operational_notation_term(self):
        from prologlib.parser import PrologParser
        p = PrologParser('X+Y+Z.')
        term = p.read_term()
        wrt = Compound('f', Variable('Z'))
        self.assertEquals(self.s, iso.free_variable_set(term, wrt))
    def test_iterated_term(self):
        from prologlib.parser import PrologParser
        p = PrologParser('Z^(A+X+Y+Z).')
        term = p.read_term()
        wrt = Variable('A')
        self.assertEquals(self.s, iso.free_variable_set(term, wrt))

class IteratedGoalTermTest(unittest.TestCase):
    def test_iterated_term(self):
        igt = Compound('foo', Variable('X'))
        term = Compound('^', Variable('X'), igt)
        self.assertEquals(igt, iso.iterated_goal_term(term))

class VariantTest(unittest.TestCase):
    def test_term_with_variables(self):
        t = Compound('f', Variable('A'), Variable('B'), Variable('A'))
        v = Compound('f', Variable('X'), Variable('Y'), Variable('X'))
        self.assertTrue(iso.isvariant(t, v))
    def test_term_with_anonymous_variables(self):
        t = Compound('g', Variable('A'), Variable('B'))
        v = Compound('g', Variable('_'), Variable('_'))
        self.assertTrue(iso.isvariant(t, v))
        self.assertTrue(iso.isvariant(v, t))
    def test_same_terms(self):
        from prologlib.parser import PrologParser
        p = PrologParser('P+Q.')
        term = p.read_term()
        self.assertTrue(iso.isvariant(term, term))
        

if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestSuite()
    #suite.addTest(FindallTest('test_different_solutions'))
    #unittest.TextTestRunner().run(suite)
