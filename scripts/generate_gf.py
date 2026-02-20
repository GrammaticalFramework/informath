import sys

"""
Generating GF rules for (multi)words, one per line.
Usage examples:
  python3 generate_gf.py Adj <nap-adj.txt

  python3 scripts/wikiterms.py w < MathTermsEng.gf | sort -u | python3 generate_gf.py FOO funlist

  pandoc god.tex -o god0.txt
  python3 analyse_latex.py god0.txt >god.txt
  cat god.txt | deptreepy txt2conllu >god.conllu
  grep ADJ tmp/godement.conllu | cut -f3 | sort -u >godADJ.txt
  # possible manual editing
  # in gf: Informath.pgf> gt -cat=Adj -depth=0 | l -lang=Eng | wf -file=oldadjs.tmp
  cat godADJ.txt | python3 generate_gf.py Adj wordlist oldadjs.tmp | grep "fun " >God.gf
  cat godADJ.txt | python3 generate_gf.py Adj wordlist oldadjs.tmp | grep "lin " >GodEng.gf

Some words are automatically excluded:

- those with non-alpha chars other than ' amd '
- those shorter than 3 chars
- those that appear in the optional OLD list (generated e.g. from Informath grammar)

Irregularities: to be checked separately.
"""

CAT = sys.argv[1] # Adj, Noun, Verb

MODE = 'wordlist'
if sys.argv[2:]:
    MODE = sys.argv[2]  # funlist


# list of words to be ignored one 
OLD = set()
if sys.argv[3:]:
    with open(sys.argv[3]) as file:
        OLD = {word for line in file for word in line.split()}


def mk_fun(s):
    s = '_'.join(s.split())  # spaces replaced by underscores
    
    if (s[0].isalpha() and
          s[-1].isalpha() and
          all(ord(c)<256 and
          (c.isdigit() or c.isalpha() or c in "_'") for c in s)):
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
        if line.strip() in OLD:
            line = '-- ' + line
        if line.startswith('--'):  # preserve commented-out words for information
            print(line)
        line = line.strip()
        if len(line) > 2 and all([c.isalpha() or c in "-'" for c in line]):
            for rule in gfrules(line.split(), CAT):
                print(rule)
elif MODE == 'funlist':  # lexical funs, CAT given as suffix
    for line in sys.stdin:
        if line.startsWith('--'):
            print(line)
        if line.strip():
            for rule in gffun_rules(line):
                print(rule)




