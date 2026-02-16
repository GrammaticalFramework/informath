# FILE = 'grammars/mathterms/DerivedTerms.gf'
FILE = 'grammars/mathterms/DerivedTermsEng.gf'

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

with open(FILE) as file:
    prev = ''
    for line in file:
#        line = convertabs(line)
        line = convertcnc(line)
        if line != prev:
            print(line)
        prev = line


