"""
A simple and useful script in lexicon extraction from CoNLLU. Example:

  grep NOUN data/hott-book.conllu | cut -f3 | python3 scripts/words_only.py | sort -u >tmp/hottnoun.tex
  RunInformath -unknown-words tmp/hottnoun.tex | cut -f1 >tmp/hottnoun.tmp
  # at this point, it is good to read through the generated file and remove bogus words
  cat tmp/hottnoun.tmp | python3 scripts/generate_gf.py Noun | grep "fun " >>tmp/hotlex.gf
  cat tmp/hottnoun.tmp | python3 scripts/generate_gf.py Noun | grep "lin " >>tmp/hotlexeng.gf

"""

import sys

for line in sys.stdin:
    line = line.strip()
    if all([c.isalpha() or c in " '-" for c in line]) and len(line) > 2 and line[-1].isalpha():
        print(line)

