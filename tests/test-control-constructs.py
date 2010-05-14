from io import StringIO
import unittest

from prologlib import core
from prologlib.parser import Atomic, Compound


class TrueTest(unittest.TestCase):

    def test_true(self):
        engine = core.Engine()
        self.assertTrue(engine.solve('true.'))


class FailTest(unittest.TestCase):

    def test_fail(self):
        engine = core.Engine()
        self.assertFalse(engine.solve('fail.'))


class CallTest(unittest.TestCase):

    def setUp(self):
        text = '''
        b(X) :-
            Y = (write(X), X),
            call(Y).
        a(1).
        a(2).'''.lstrip()
        self.engine = core.Engine()
        self.engine._consult(text)

    def tearDown(self):
        self.engine._clear()

    def test_call_cut(self):
        self.assertTrue(self.engine.solve('call(!).'))

    def test_call_fail(self):
        self.assertFalse(self.engine.solve('call(fail).'))

    def test_call_avoid_instantiation_error(self):
        self.assertFalse(self.engine.solve('call((fail, X)).'))

    def test_call_avoid_type_error(self):
        self.assertFalse(self.engine.solve('call((fail, call(1))).'))

    def test_call_indirect_instantiation_error(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertRaises(core.PrologError, self.engine.solve, 'b(_).')
        import re
        v = re.compile('_\d+')
        self.assertIsNotNone(v.match(output.getvalue()))
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_call_indirect_type_error(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertRaises(core.PrologError, self.engine.solve, 'b(3).')
        # according to the standard, the goal should output
        # '3', but this behaviour is incompatible with the
        # type_error(callable, 3) that the standard prescribes
        # the goal to throw
        self.assertEquals('', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_call_binding_no_reexecution(self):
        self.assertTrue(self.engine.solve('Z = !, call((Z=!, a(X), Z)).'))
        self.assertEquals({'X' : Atomic(1), 'Z' : Atomic('!')}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())

    def test_call_binding_with_reexecution(self):
        self.assertTrue(self.engine.solve('call((Z=!, a(X), Z)).'))
        self.assertEquals({'X' : Atomic(1), 'Z' : Atomic('!')}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'X' : Atomic(2), 'Z' : Atomic('!')}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())

    def test_call_output_with_instantiation_error(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertRaises(core.PrologError, self.engine.solve, 'call((write(3), X)).')
        self.assertEquals('3', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_call_output_with_type_error(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertRaises(core.PrologError, self.engine.solve, 'call((write(3), call(1))).')
        self.assertEquals('3', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_call_instantiation_error(self):
        self.assertRaises(core.PrologError, self.engine.solve, 'call(X).')

    def test_call_type_error(self):
        self.assertRaises(core.PrologError, self.engine.solve, 'call(1).')

    def test_call_conjunction_type_error(self):
        self.assertRaises(core.PrologError, self.engine.solve, 'call((fail, 1)).')
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertRaises(core.PrologError, self.engine.solve, 'call((write(3), 1)).')
        self.assertEquals('', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_call_disjunction_type_error(self):
        self.assertRaises(core.PrologError, self.engine.solve, 'call((1;true)).')


class CutTest(unittest.TestCase):

    def setUp(self):
        theory = """
        twice(!) :- write('C ').
        twice(true) :- write('Moss ').
        goal((twice(_), !)).
        goal(write('Three ')).
        """.lstrip()
        self.engine = core.Engine()
        self.engine._consult(theory)

    def tearDown(self):
        self.engine._clear()

    def test_cut_succeed(self):
        self.assertTrue(self.engine.solve('!.'))

    def test_cut_disjunction_fail(self):
        self.assertFalse(self.engine.solve('(!, fail; true).'))

    def test_cut_within_call(self):
        self.assertTrue(self.engine.solve('(call(!), fail; true).'))

    def test_cut_with_output_as_c_forwards(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("twice(_), !, write('Forwards '), fail."))
        self.assertEquals('C Forwards ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_output_as_cut_disjunction(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("(! ; write('No ')), write('Cut disjunction '), fail."))
        self.assertEquals('Cut disjunction ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_output_as_c_no_cut_cut(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("twice(_), (write('No ') ; !), write('Cut '), fail. "))
        self.assertEquals('C No Cut Cut ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_output_as_c(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("twice(_), (!, fail ; write('No ')). "))
        self.assertEquals('C ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_output_as_c_forwards_moss_forwards(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("twice(X), call(X), write('Forwards '), fail. "))
        self.assertEquals('C Forwards Moss Forwards ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_output_as_c_forwards_three_forwards(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("goal(X), call(X), write('Forwards '), fail. "))
        self.assertEquals('C Forwards Three Forwards ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_not_and_output(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        # FIXME Currently \+/1 is implemented as not/1
        #self.assertFalse(self.engine.solve("twice(_), \+(\+(!)), write('Forwards '), fails."))
        self.assertFalse(self.engine.solve("twice(_), not(not(!)), write('Forwards '), fail."))
        self.assertEquals('C Forwards Moss Forwards ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    @unittest.skip('once/1 not yet implemented')
    def test_cut_with_once_and_output(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("twice(_), once(!), write('Forwards '), fail."))
        self.assertEquals('C Forwards Moss Forwards ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_cut_with_call_and_output(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertFalse(self.engine.solve("twice(_), call(!), write('Forwards '), fail."))
        self.assertEquals('C Forwards Moss Forwards ', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM


class ConjunctionTest(unittest.TestCase):

    def setUp(self):
        self.engine = core.Engine()

    def test_conjunction_order_fail(self):
        self.assertFalse(self.engine.solve("','(X=1, var(X))."))

    def test_conjunction_order_succeed(self):
        self.assertTrue(self.engine.solve("','(var(X), X=1)."))
        self.assertEquals({'X': Atomic(1)}, self.engine.currsubst())

    def test_conjunction_order_with_call(self):
        self.assertTrue(self.engine.solve("','(X=true, call(X))."))
        self.assertEquals({'X' : Atomic.TRUE}, self.engine.currsubst())


class DisjunctionTest(unittest.TestCase):

    def setUp(self):
        self.engine = core.Engine()

    def test_disjunction_simple(self):
        self.assertTrue(self.engine.solve("';'(true, fail)."))

    def test_disjunction_cut_fail(self):
        self.assertFalse(self.engine.solve("';'((!, fail), true)."))

    def test_disjunction_cut_avoid_error(self):
        self.assertTrue(self.engine.solve("';'(!, call(3))."))

    def test_disjunction_with_unification(self):
        self.assertTrue(self.engine.solve("';'((X=1, !), X=2)."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())

    def test_disjunction_with_reexecution(self):
        self.assertTrue(self.engine.solve("','(';'(X=1, X=2), ';'(true, !))."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())


class IfThenTest(unittest.TestCase):

    def setUp(self):
        self.engine = core.Engine()

    def test_ifthen_true(self):
        self.assertTrue(self.engine.solve("'->'(true, true)."))

    def test_ifthen_consequent_fail(self):
        self.assertFalse(self.engine.solve("'->'(true, fail)."))

    def test_ifthen_antecedent_fail(self):
        self.assertFalse(self.engine.solve("'->'(fail, true)."))

    def test_ifthen_with_reexecution(self):
        self.assertTrue(self.engine.solve("'->'(true, X=1)."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())

    def test_ifthen_disjunctive_antecedent_with_reexecution(self):
        self.assertTrue(self.engine.solve("'->'(';'(X=1, X=2), true)."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())

    def test_ifthen_disjunctive_consequent_with_reexecution(self):
        self.assertTrue(self.engine.solve("'->'(true, ';'(X=1, X=2))."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'X' : Atomic(2)}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())


class IfThenElseTest(unittest.TestCase):

    def setUp(self):
        self.engine = core.Engine()

    def test_ifthenelse_then_true(self):
        self.assertTrue(self.engine.solve("';'('->'(true, true), fail)."))

    def test_ifthenelse_else_true(self):
        self.assertTrue(self.engine.solve("';'('->'(fail, true), true)."))

    def test_ifthenelse_then_fail(self):
        self.assertFalse(self.engine.solve("';'('->'(true, fail), fail)."))

    def test_ifthenelse_else_fail(self):
        self.assertFalse(self.engine.solve("';'('->'(fail, true), fail)."))

    def test_ifthenelse_then_unify(self):
        self.assertTrue(self.engine.solve("';'('->'(true, X=1), X=2)."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())

    def test_ifthenelse_else_unify(self):
        self.assertTrue(self.engine.solve("';'('->'(fail, X=1), X=2)."))
        self.assertEquals({'X' : Atomic(2)}, self.engine.currsubst())

    def test_ifthenelse_with_then_reexecution(self):
        self.assertTrue(self.engine.solve("';'('->'(true, ';'(X=1, X=2)), true)."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve_next())
        self.assertEquals({'X' : Atomic(2)}, self.engine.currsubst())

    def test_ifthenelse_without_if_reexecution(self):
        self.assertTrue(self.engine.solve("';'('->'(';'(X=1, X=2), true), true)."))
        self.assertEquals({'X' : Atomic(1)}, self.engine.currsubst())

    def test_ifthenelse_with_cut(self):
        self.assertTrue(self.engine.solve("';'('->'((!, fail), true), true)."))


class CatchThrowTest(unittest.TestCase):

    def setUp(self):
        theory = '''
        foo(X) :- Y is X * 2, throw(test(Y)).
        bar(X) :- X = Y, throw(Y).
        coo(X) :- throw(X).
        car(X) :- X = 1, throw(X).
        g :- catch(p, B, write(h2)), coo(c).
        p.
        p :- throw(B).
        '''.lstrip()
        self.engine = core.Engine()
        self.engine._consult(theory)

    def tearDown(self):
        self.engine._clear()

    def test_catch_succeed_with_unification(self):
        self.assertTrue(self.engine.solve('catch(foo(5), test(Y), true).'))
        self.assertEquals({'Y' : Atomic(10)}, self.engine.currsubst())
        self.assertTrue(self.engine.solve('catch(bar(3), Z, true).'))
        self.assertEquals({'Z' : Atomic(3)}, self.engine.currsubst())

    def test_1(self):
        self.assertTrue(self.engine.solve('catch(car(X), Y, true).'))
        self.assertEquals({'Y' : Atomic(1)}, self.engine.currsubst())

    def test_catch_succeed(self):
        self.assertTrue(self.engine.solve('catch(true, _, 3).'))

    def test_throw_system_error(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertRaises(core.PrologError, self.engine.solve, 'catch(true, C, write(demoen)), throw(bla).')
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_catch_error_and_fail(self):
        goal = "catch(number_chars(X, ['1', 'a', '0']), error(syntax_error(_), _), fail)."
        self.assertFalse(self.engine.solve(goal))

    def test_catch_with_unification_and_output_without_reexecution(self):
        output = StringIO()
        from prologlib.builtin import io as prologio
        prologio.current_output_stream = output
        self.assertTrue(self.engine.solve('catch(g, C, write(h1)).'))
        self.assertEquals({'C' : Atomic('c')}, self.engine.currsubst())
        self.assertFalse(self.engine.solve_next())
        self.assertEquals('h1', output.getvalue())
        prologio.current_output_stream = prologio.STANDARD_OUTPUT_STREAM

    def test_catch_with_error_unification(self):
        self.assertTrue(self.engine.solve('catch(coo(X), Y, true).'))
        error = self.engine.currsubst()['Y']
        self.assertEquals(Compound, type(error))
        self.assertEquals(Atomic('instantiation_error'), error.value[1])

    def test_catch_propagate_unification_from_catcher_to_recovery(self):
        '''A non-ISO regression test to avoid a bug in propagating the substitution'''
        goal = "catch(X is float_fractional_part(5), error(type_error(_, N), _), A is N)."
        self.assertTrue(self.engine.solve(goal))
        self.assertEquals(Atomic(5), self.engine.currsubst()['N'])
        self.assertEquals(Atomic(5), self.engine.currsubst()['A'])


if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestSuite()
    #suite.addTest(CatchThrowTest('test_catch_with_error_unification'))
    #unittest.TextTestRunner().run(suite)
