import sys

"""
Generating GF rules for (multi)words, one per line.
Usage examples:
  python3 scripts/generate_gf.py Adj <tmp/nap-adj.txt
  python3 scripts/wikiterms.py w <grammars/mathterms/MathTermsEng.gf | sort -u | python3 scripts/generate_gf.py FOO funlist
"""

CAT = sys.argv[1]

MODE = 'wordlist'
if sys.argv[2:]:
    MODE = sys.argv[2]

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

cats = {'A': 'Adj', 'N': 'Noun', 'V': 'Verb'}

def gffun_rules(line):
    line = line.strip()
    words = line.split('_')
    cat0 = words[-1]
    if cat0 in cats:
        cat = cats[cat0]
        fun = mk_fun_from_strs(words[:-1] + [cat])
        lin = mk_lin ('mk' + cat, [], [line])
        return [
            mk_fun_rule(fun, cat),
            mk_lin_rule(fun, lin)
            ]
    else:
        return []


if MODE == 'wordlist':  # bare words, CAT given as command-line argument
    for line in sys.stdin:
        line = ''.join([c for c in line if c.isalpha() or c in "-'"])
        for rule in gfrules(line.split(), CAT):
            print(rule)
elif MODE == 'funlist':  # lexical funs, CAT given as suffix
    for line in sys.stdin:
        if line.strip():
            for rule in gffun_rules(line):
                print(rule)




