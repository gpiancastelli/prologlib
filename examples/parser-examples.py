'''
Examples of usage for the Prolog parser module.

Should this eventually merged with real unit/functional tests? How?
'''

from prologlib.parser import PrologTokenizer, EOF, PrologParser

def tokenizer_sample():
    f = open('sample.pl', 'r')
    lexer = PrologTokenizer(f)
    token = lexer.read_token()
    while token.type != EOF:
        print(token)
        token = lexer.read_token()
    f.close()

def parser_sample():
    f = open('sample.pl', 'r')
    parser = PrologParser(f)
    # the sample comes from a Prolog system where the
    # following non-ISO operator has been defined
    parser._ot._table['#'] = [(500, 'yfx')]
    term = parser.read_term()
    while term:
        print(term)
        term = parser.read_term()
    f.close()

def parser_family():
    f = open('family.pl', 'r')
    parser = PrologParser(f)
    term = parser.read_term()
    while term:
        print(term)
        term = parser.read_term()
    f.close()


if __name__ == '__main__':
    import sys
    selected = sys.argv[1]
    # runners = {'p' : 'parser_sample', 't' : 'tokenizer_sample'}
    # runner = runners[selected]
    if selected == 'p':
        parser_sample()
    else:
        tokenizer_sample()
    # parser_family()
