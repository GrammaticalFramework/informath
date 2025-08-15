import pgf
import json
import sys

usage = "stdin | python3 test_tile.py pgf source index?"

MAX_PARSES = 9999

if sys.argv[2:]:
    PGF_FILE = sys.argv[1]
    CNC_NAME = sys.argv[2]
else:
    print(usage)

grammar = pgf.readPGF(PGF_FILE)
source = grammar.languages[CNC_NAME]

targets = [(lang, cnc) for
             (lang, cnc) in grammar.languages.items() if lang != CNC_NAME]

indexfile = sys.argv[3] if sys.argv[3:] else None

if indexfile:
    with open(indexfile) as file:
        termindex = json.load(file)
else:
    termindex = None

        
def apply_index(termindex, s):
    if not termindex:
        return s
    toks = []
    words = s.split()
    while words:
        w = words.pop(0)
        if w == '\\INDEXEDTERM':
            words.pop(0)  # {
            i = words.pop(0)
            toks.append(termindex[i])
            words.pop(0)  # }
        else:
            toks.append(w)
    return ' '.join(toks)


def var_check(tree):
    fargs = tree.unpack()
    if isinstance(fargs, tuple):
        f, args = fargs
        if f == 'StrIdent':
            x = args[0].unpack()
            return len(x) == 1 and x.isalpha() and x not in "CNQRZ"
        return all(var_check(arg) for arg in args)
    return True

sent = 1
fails = 0
successes = 0
for s in sys.stdin:
    if s.strip():
        try:
            p = source.parse(s)
            print(apply_index(termindex, s))
            print()
            i = 0
            succ = 0
            for _, t in p:
                if var_check(t):
                    succ = 1
                    print('-- TREE', t)
                    if targets:
                        for (lang, cnc) in targets:
                            print(apply_index(termindex, cnc.linearize(t)))
                            print()
                i += 1            
                if i > MAX_PARSES:
                    break
            successes += succ
        except pgf.ParseError as pe:
            print('-- NOTREE', sent, s)
            print('-- NOTREE CAUSE', pe)
            fails += 1
        sent += 1
        print()

print('success', successes, 'failure', fails, 'sentences', sent)

