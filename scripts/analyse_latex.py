"Analyse Naproch-ZF code"

import sys

FILE = sys.argv[1]
MATHMARK = 'LATEXMATH'
#MATHMARK = '$X$'

def untex(s):
    begtex = s.strip()[:1] == '$'   # line starts with $  
    ss = s.split('$')               # every other word is MATH
    ws = []
    if begtex:
        ws.append('')               # the first word is never MATH
    for (i, w) in zip(range(100), ss):
        if i%2 == 0:
            ws.append(w)
        else:
            ws.append(MATHMARK)
    return ' '.join(ws)
        

#PROCESS = lambda line: ' '.join(untex(line).split()[:3])
PROCESS = untex

def chop_lines(file):
    for line in file:
        yield line

def chop_sentences(file):
    content = ' '.join([line.strip() for line in file if line.strip()[:1] != "\\"])
    for line in content.split('. '):
        line = line + '.'
        yield line
        
CHOP = chop_sentences

with open(FILE) as file:
    freqs = {}
    for line in CHOP(file):
        line = PROCESS(line)
        freqs[line] = freqs.get(line, 0) + 1
        

for it in sorted(list(freqs.items()), key=lambda li: 0-li[1]):
#    print(it)      # show all distinct sentences and their frequencies in descending order 
    print(it[0])  # show just the sentences
    print()

    
