import sys

FILE = sys.argv[1]

drop = len(r'#MACRO \newcommend{')

def get_macro(line):
    if line.startswith('#MACRO'):
        line = line[drop:]
        rpars = line.split('}')
        macro = rpars[0]
        defi = rpars[1]
        defi = defi.split('{')[1]
        return [(macro, f'${defi}$')]
    else:
        return []        

with open(FILE) as file:
    macros = {m: d for line in file for (m, d) in get_macro(line)}


def apply_macro(line):
    if line.startswith('#MACRO'):
        return ''
    elif line.startswith('#'):
        return line
    else:
        ws = line.split()
        ws = [macros.get(w, w) for w in ws]
        return ' '.join(ws)
    
    
with open(FILE) as file:
    for line in file:
        print(apply_macro(line))

