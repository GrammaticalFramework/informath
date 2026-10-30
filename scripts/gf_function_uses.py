"""Count how many times each GF function of src/Informath.hs is used in the Haskell code.

    python3 scripts/gf_function_uses.py                    # all functions, most used first
    python3 scripts/gf_function_uses.py -unused-syntactic  # the unused ones that take arguments

The functions are the constructors of the datatype Tree in src/Informath.hs.
A use is any occurrence of the name, in a pattern or in an expression, in the
modules that stack builds (app, src, web), except src/Informath.hs itself;
comments are not counted. Syntactic functions are those that take arguments,
except the Lex constructors. The output is a Markdown table.
"""

import collections
import glob
import os
import re
import sys

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
INFORMATH_HS = os.path.join(ROOT, 'src', 'Informath.hs')
SOURCE_DIRS = ['app', 'src', 'web', 'src/typetheory']


def tree_functions():
    "the constructors of Tree, as (name, category, type)"
    funs = []
    in_tree = False
    with open(INFORMATH_HS) as file:
        for line in file:
            if line.startswith('data Tree'):
                in_tree = True
            elif in_tree:
                m = re.match(r'  (\w+) :: (.*) -> Tree G(\w+)_$|  (\w+) :: Tree G(\w+)_$', line)
                if m and m.group(1):
                    funs.append((m.group(1), m.group(3), m.group(2) + ' -> ' + m.group(3)))
                elif m:
                    funs.append((m.group(4), m.group(5), m.group(5)))
                elif not line.strip():
                    break
    return funs


def strip_comments(s):
    s = re.sub(r'\{-.*?-\}', '', s, flags=re.S)
    return re.sub(r'--.*', '', s)


def count_uses(names):
    "for each name, a Counter of its uses per module"
    counts = {n: collections.Counter() for n in names}
    for d in SOURCE_DIRS:
        for path in sorted(glob.glob(os.path.join(ROOT, d, '*.hs'))):
            if os.path.samefile(path, INFORMATH_HS):
                continue
            module = os.path.basename(path)[:-3]
            with open(path) as file:
                for tok in re.findall(r"\b[A-Za-z_][\w']*", strip_comments(file.read())):
                    if tok in counts:
                        counts[tok][module] += 1
    return counts


def main():
    funs = tree_functions()
    counts = count_uses([n for n, _, _ in funs])
    if '-unused-syntactic' in sys.argv:
        rows = sorted([(c, n, t) for n, c, t in funs
                       if not counts[n] and '->' in t and not n.startswith('Lex')])
        print('| category | function | type |')
        print('|---|---|---|')
        for c, n, t in rows:
            print(f'| {c} | {n} | `{t}` |')
        print(f'\n{len(rows)} unused syntactic functions')
    else:
        rows = sorted(funs, key=lambda f: (-sum(counts[f[0]].values()), f[0]))
        print('| function | category | uses | modules |')
        print('|---|---|---:|---|')
        for n, c, _ in rows:
            cs = counts[n]
            mods = ', '.join(f'{m} {k}' for m, k in cs.most_common())
            print(f'| {n} | {c} | {sum(cs.values())} | {mods} |')
        unused = sum(1 for n, _, _ in funs if not counts[n])
        print(f'\n{len(funs)} functions, {unused} of them unused')


if __name__ == '__main__':
    main()
