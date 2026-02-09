"Analyse Naproch-ZF code"

FILE = 'nzf-lines.tex'
#MATHMARK = 'LATEXMATH'
MATHMARK = '$X$'

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

with open(FILE) as file:
    freqs = {}
    for line in file:
        line = PROCESS(line)
        freqs[line] = freqs.get(line, 0) + 1
        

for it in sorted(list(freqs.items()), key=lambda li: 0-li[1]):
#    print(it)
    print(it[0])      

    
