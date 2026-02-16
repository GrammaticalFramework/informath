# FILE = 'grammars/mathterms/DerivedTerms.gf'
FILE = 'grammars/mathterms/DerivedTermsEng.gf'

import sys

MODE = sys.argv[1]

def convertabs(line):
    line = line.replace('AP', 'Adj')
    line = line.replace('CN', 'Noun')
    ws = line.split()
    if ws[:1] == ["fun"]:
        fun = ws[1].split('_')
        fun = '_'.join([fun[0], fun[-1]])
        ws[1] = fun
        return ' '.join(ws[:5])  # omitting Q ids
    else:
        return line

def convertcnc(line):
    line = line.replace('AP', 'Adj')
    line = line.replace('CN', 'Noun')
    line = line.replace('PastPartAdj', 'PastPartAP')
    ws = line.split()
    if ws[:1] == ["lin"]:
        fun = ws[1].split('_')
        cat = fun[-1]
        fun = '_'.join([fun[0], fun[-1]])
        ws[1] = fun
        oper = 'mk' + cat
        ws[3:] = [oper, "("] + ws[3:-1] + [")", ws[-1]] 
        return ' '.join(ws)
    else:
        return line

def pick_funs(line):
    words = line.replace(')', ' ').split()
    return '\n'.join([w for w in words if
                      any([w.endswith(s) for s in
                           ['_A', '_N', "_A'", "_N'", "_V"]])])
    
with sys.stdin as file:
    prev = ''
    for line in file:
        if MODE=='abs':
            line = convertabs(line)
        elif MODE=='cnc':
            line = convertcnc(line)
        else:
            line = pick_funs(line)
        if line != prev:
            print(line)
        prev = line


