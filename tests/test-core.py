from prologlib.core import Substitution, convert_to_goal, unify
from prologlib.parser import Atomic, Compound, Variable, List

import unittest

class SubstitutionTest(unittest.TestCase):
    def test_empty_semantics(self):
        s = Substitution()
        self.assertFalse(s)
    def test_get_binding(self):
        s = Substitution()
        s['A'] = Variable('B')
        s['B'] = Atomic(1)
        self.assertEquals(Atomic(1), s['A'])
        self.assertEquals(Atomic(1), s['B'])
    def test_set_binding(self):
        s = Substitution()
        s['A'] = Variable('B')
        s['A'] = Atomic(1)
        self.assertEquals(Atomic(1), s['A'])
        self.assertEquals(Atomic(1), s['B'])
    def test_set_existing_binding(self):
        s = Substitution()
        s['A'] = Atomic(1)
        s['A'] = Atomic(2)
        self.assertEquals(Atomic(1), s['A'])
    def test_update(self):
        s1 = Substitution()
        s1['A'] = Variable('B')
        s1['B'] = Atomic(1)
        s2 = Substitution()
        s2['A'] = Variable('C')
        s2['D'] = Atomic(2)
        s1.update(s2)
        b = s1.get('A')
        # A is still bound to a variable...
        self.assertTrue(isinstance(b, Variable))
        # ...but the variable has been updated to point to 1
        self.assertEquals(Atomic(1), b.value)
        # C is updated to reflect its proper status
        self.assertEquals(Atomic(1), s1['C'])
        # D is left as it is
        self.assertEquals(Atomic(2), s1['D'])
    def test_simple_flatten(self):
        s = Substitution()
        s['A'] = Variable('B')
        s['B'] = Atomic(1)
        self.assertEquals(Variable('B'), s.get('A'))
        s.flatten()
        self.assertEquals(Atomic(1), s.get('A'))
        self.assertEquals(Atomic(1), s['B'])
    def test_flatten_with_compound(self):
        s = Substitution()
        s['A'] = Variable('I')
        s['B'] = Compound('p', Variable('A'))
        s.flatten()
        self.assertEquals(Compound('p', Variable('I')), s['B'])
    def test_simple_reduce(self):
        s = Substitution()
        # variables ending with '#N' are considered as renamed
        s['A#0'] = Variable('I')
        s['I'] = Atomic('p')
        s['X'] = Variable('C#0')
        result = Substitution()
        result['I'] = Atomic('p')
        s.reduce()
        self.assertEquals(result, s)
    def test_reduce_with_compound(self):
        s = Substitution()
        # variables ending with '#N' are considered as renamed
        s['A#0'] = Variable('I')
        s['B'] = Compound('p', Variable('A#0'))
        s['I'] = Atomic('q')
        result = Substitution()
        result['I'] = Atomic('q')
        result['B'] = Compound('p', Variable('A#0'))
        s.reduce()
        self.assertEquals(result, s)

class ConvertToGoalTest(unittest.TestCase):
    def test_convert_number(self):
        term = Atomic(3)
        self.assertTrue(convert_to_goal(term) is None)
    def test_convert_variable(self):
        term = Variable('X')
        goal = Compound('call', term)
        self.assertEquals(goal, convert_to_goal(term))
    def test_convert_call(self):
        term = Compound('call', Variable('X'))
        self.assertEquals(term, convert_to_goal(term))
    def test_convert_disjunction(self):
        term = Compound(';', Variable('X'), Atomic('p'))
        goal = convert_to_goal(term)
        self.assertEquals(';/2', goal.predicate_indicator())
        self.assertEquals(Compound('call', Variable('X')), goal.value[1])
        self.assertEquals(Atomic('p'), goal.value[2])
    def test_convert_conjunction(self):
        call = Compound('call', Variable('X'))
        term = Compound(',', Variable('X'), call)
        goal = convert_to_goal(term)
        self.assertEquals(',/2', goal.predicate_indicator())
        self.assertEquals(call, goal.value[1])
        self.assertEquals(call, goal.value[2])
    def test_convert_compound(self):
        term = Compound('f', Variable('X'), Atomic('g'))
        self.assertEquals(term, convert_to_goal(term))

class UnifyTest(unittest.TestCase):
    def test_compound(self):
        args = (Variable('A'),) * 3
        foo_variable = Compound('foo', *args)
        foo_ground = Compound('foo', Atomic(1), Atomic(2), Atomic(3))
        self.assertIsNone(unify(foo_variable, foo_ground))
    def test_deep_compound(self):
        px = Compound('p', Variable('X'))
        p1 = Compound('p', Atomic(1))
        foo = Compound('foo', px, p1)
        args = (Variable('A'),) * 2
        foo_variable = Compound('foo', *args)
        mgu = unify(foo, foo_variable)
        self.assertIsNotNone(mgu)
        self.assertEquals(Atomic(1), mgu['X'])
    def test_list(self):
        aList = List.from_list([Variable('A')] * 3)
        anotherList = List.from_list([Atomic(1), Atomic(2), Atomic(3)])
        self.assertIsNone(unify(aList, anotherList))

if __name__ == '__main__':
    unittest.main()
