#!/usr/bin/env python3
"""Check that no declaration name of coq/Chapter*.v (constants, theorems,
records, their constructors and fields) occurs in two chapters: Rocq
accepts the same short name in two files, but the export to Dedukti and
the index name constants by their short names.

    python3 names.py        # lists the duplicates; exit status 1 if any

It also checks that no constant has an entry in two symbol tables
(dkgf/ChapterN.dkgf), where the later one would silently win.
"""
import collections, glob, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
DECL = re.compile(r"^(?:Definition|Fixpoint|Theorem|Lemma|Corollary|Proposition|Axiom|Parameter|Record)\s+([\w']+)", re.M)
RECORD = re.compile(r"^Record\s+[\w']+[^:]*:[^:]*:=\s*([\w']+)\s*\{(.*?)\}", re.M | re.S)


def main():
    seen = collections.defaultdict(list)
    files = sorted(glob.glob(os.path.join(HERE, 'coq', 'Chapter*.v')),
                   key=lambda f: int(re.findall(r'\d+', os.path.basename(f))[0]))
    for f in files:
        s = open(f).read()
        b = os.path.basename(f)
        for m in DECL.finditer(s):
            seen[m.group(1)].append(b)
        for m in RECORD.finditer(s):
            seen[m.group(1)].append(b)
            for fl in re.findall(r"(?:^|;)\s*([\w']+)\s*:", m.group(2)):
                seen[fl].append(b)
    dups = {k: v for k, v in seen.items() if len(v) > 1}
    for k, v in sorted(dups.items()):
        print('duplicate name %s: %s' % (k, ' '.join(v)))
    # a constant worded in two symbol tables: the later entry wins, silently
    entries = collections.defaultdict(list)
    for f in sorted(glob.glob(os.path.join(HERE, 'dkgf', 'Chapter*.dkgf')),
                    key=lambda f: int(re.findall(r'\d+', os.path.basename(f))[0])):
        for l in open(f):
            m = re.match(r"([A-Za-z][\w']*) :", l)
            if m:
                entries[m.group(1)].append(os.path.basename(f))
    tdups = {k: v for k, v in entries.items() if len(v) > 1}
    for k, v in sorted(tdups.items()):
        print('constant worded in two tables %s: %s' % (k, ' '.join(v)))
    sys.exit(1 if dups or tdups else 0)


if __name__ == '__main__':
    main()
