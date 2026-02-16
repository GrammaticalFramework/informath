"""
Remove fun and lin rules of funs that occur in MAINFILE. Example:
  python3 scripts/remove_overlap.py grammars/NaprocheLexiconEng.gf
"""

import sys

MAINFILE = 'grammars/WikidataWords.gf'

NEWFILE = sys.argv[1]

with open(MAINFILE) as file:
    mainfuns = {line.split()[1] for line in file if line.startswith('fun ')}

with open(NEWFILE) as file:
    for line in file:
        if (line.startswith('fun ') or line.startswith('lin ')) and line.split()[1] in mainfuns:
            print('---OVERLAP', line)
        else:
            print(line.strip())

