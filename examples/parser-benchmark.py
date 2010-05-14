from timeit import Timer

repetitions = 1000
term = "A ; B :- A =.. ['->', C, T], !, (C, !, T ; B)."
s = 'parser = PrologParser("' + term + '"); parser.read_term()'
setup = 'from prologlib.parser import PrologParser'
timer = Timer(s, setup)
try:
    result = timer.timeit(repetitions)
    # 5.25x w.r.t. tuProlog 2.1 (851 ms)
    print('Time parsing %d terms: %f seconds.' % (repetitions, result))
except:
    timer.print_exc()
