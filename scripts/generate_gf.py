import sys

"""
Generating GF rules for (multi)words, one per line.
Usage example: python3 scripts/generate_gf.py Adj <tmp/nap-adj.txt
"""

CAT = sys.argv[1]

def mk_fun(s):
    s = '_'.join(s.split())  # spaces replaced by underscores
    
    if (s[0].isalpha() and
        all(ord(c)<256 and (c.isdigit() or c.isalpha() or c in "_'")
            for c in s)):  # test if legal GF identifier
        return s
    else:
        return "'" + s.replace("'", "\\'") + "'"  # if not, single quotes make it legal


def mk_fun_from_strs(ss):
    return mk_fun('_'.join(ss))


def quote(s):
    return '"' + s + '"'

def mk_lin(oper, words, params):
    return ' '.join([oper] + [quote(w) for w in words] + params) 


def mk_fun_rule(fun, cat, comment=None):
    co = '--' + comment if comment else ''
    return ' '.join(['fun', fun, ':', cat, ';', co, '\n'])


def mk_lin_rule(fun, lin, comment=None):
    co = '--' + comment if comment else ''
    return ' '.join(['lin', fun, '=', lin, ';', co, '\n'])


def gfrules(words, cat, oper=None, params=[]):
    oper = oper if oper else 'mk' + cat
    fun = mk_fun_from_strs(words + [cat])
    lin = mk_lin(oper, words, params)
    return [
        mk_fun_rule(fun, cat),
        mk_lin_rule(fun, lin)
      ]


for line in sys.stdin:
    line = ''.join([c for c in line if c.isalpha() or c in "-'"])
    for rule in gfrules(line.split(), CAT):
        print(rule)




