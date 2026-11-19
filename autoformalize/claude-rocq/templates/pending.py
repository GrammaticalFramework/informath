#!/usr/bin/env python3
"""List what is pending in the Rocq formalization: the statements made
axioms only because their proof is not given yet, marked by a comment
(* PENDING: why, or what it waits for *) right before the declaration, and
any Admitted proofs.  The book's own axioms are marked (* AXIOM: ... *)
and are not listed.

    python3 pending.py          # one line per pending statement
    python3 pending.py -count   # the numbers per chapter
"""
import glob, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
DECL = re.compile(r"(Axiom|Theorem|Lemma|Corollary|Proposition|Definition|Parameter)\s+([A-Za-z_][\w']*)")


def pending(path):
    src = open(path).read()
    out = []
    for m in re.finditer(r'\(\*\s*PENDING:?(.*?)\*\)', src, re.S):
        d = DECL.search(src, m.end())
        name = d.group(2) if d else '?'
        out.append((src.count('\n', 0, m.start()) + 1, name, ' '.join(m.group(1).split())))
    for m in re.finditer(r'(?:Theorem|Lemma|Corollary|Proposition)\s+([A-Za-z_][\w\']*)[^.]*?(?:\.\s*Proof\.(?:(?!Qed\.).)*?)?Admitted\.', src, re.S):
        out.append((src.count('\n', 0, m.start()) + 1, m.group(1), 'Admitted'))
    return sorted(out)


def main():
    files = sorted(glob.glob(os.path.join(HERE, 'coq', 'Chapter*.v')), key=lambda f: int(re.findall(r'\d+', os.path.basename(f))[0]))
    total = 0
    for f in files:
        ps = pending(f)
        total += len(ps)
        if '-count' in sys.argv:
            if ps:
                print('%s: %d pending' % (os.path.basename(f), len(ps)))
            continue
        for line, name, why in ps:
            print('%s:%d %s -- %s' % (os.path.relpath(f, HERE), line, name, why or '(no reason given)'))
    print('%d pending' % total)


if __name__ == '__main__':
    main()
