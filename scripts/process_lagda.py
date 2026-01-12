from os import listdir
from os.path import isfile, join
import sys

# def equality = 61
# prop equality ＝ 65309

# usage: process_lagda (<file.lagda.md>|<dir>) (all | funs | odd_chars) clean_idents?

MODE = 'all' 
if sys.argv[2:]:
    MODE = sys.argv[2]

FLAGS = sys.argv[3:]


def comment(s):
    return f'-- {s}'


def extract_agda(mdfile):
    agdacode = [comment(mdfile)]
    inside = False
    with open(mdfile) as file:
        for line in file:
            if line.startswith("```agda"):
                inside = True
            elif line.startswith("```"):
                inside = False
            elif inside:
                agdacode.append(line)
    return agdacode


def new_constant(line):
    if not line[0].isspace() and (ws := line.split())[2:] and ws[1] == ':':
        return ws[0]


def clean_idents(s):
    ss = []
    for c in s:
        o = ord(c)
        if o > 256:
            ss.append('_U'+str(o)+'_')
        else:
            ss.append(c)
    return ''.join(ss)

def very_clean_idents(s):
    ss = []
    for c in s:
        o = ord(c)
        if not (o < 256 and (c.isalpha() or c.isdigit() or c in "'_")):
            ss.append('_U'+str(o)+'_')
        else:
            ss.append(c)
    return ''.join(ss)


def bnfc_infix(line):
    "format: infix* int _op_"
    keyw, prec, op = line.split()
    fop = very_clean_idents(op)   # to work as bnfc ident
    op = op[1:-1]    # from _op_ to op
    fun = f'E{fop}'
    cat = f'Exp{prec}'
    nextcat = f'Exp{int(prec)+1}'
    if keyw == 'infixl':
        return f'{fun}. {cat} ::= {cat} "{op}" {nextcat} ;'
    else:
        return f'{fun}. {cat} ::= {nextcat} "{op}" {nextcat} ;' 


if __name__ == '__main__':
    path = sys.argv[1]
    if path.endswith('lagda.md'):
        files = [path]
    else:
        files = [join(path, file) for file in listdir(path) if file.endswith('lagda.md')]
    odds = set()
    for file in files:
        code = extract_agda(file)
        if 'clean_idents' in FLAGS:
            code = [clean_idents(line) for line in code]
        elif MODE == 'funs':
            for line in code:
                if c := new_constant(line):
                    print(c)
        elif MODE == 'infixes':
            for line in code:
                if line.startswith('infix'):
                    print(line)
                    print(bnfc_infix(line))
        elif MODE == 'odd_chars':
            for line in code:
                for k in line:
                    if ord(k) > 255:
                        odds.add(k)
        else:
            for line in code:
                print(line.rstrip())
    if MODE == 'odd_chars':
        print(''.join(odds))
 

        
        
