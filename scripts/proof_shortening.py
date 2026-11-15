"""Shortening proof texts made from tactic proofs, by rules on proof units.

A tactic proof read step by step (mathlib-informath's tactic-units.py for
Lean, coq-tactic-units.py for Coq) has one proof unit for each step: an
assumption (hypos), a derived or concluded proposition (conclusion), what is
left to show (enough), a fact to verify first (verify), a case split (cases,
induction). Many steps change only the form of a goal or a hypothesis
(rewriting, unfolding, simplification), or say again what the previous line
said. These rules merge such steps. They look at the units only, never at
the prover or the tactics: a merged unit keeps the tactics of its steps, in
order, as its justification, so that the text can be checked against the
script.

The rules, applied in this order until none applies, each only to units
next to each other at the same depth (the same scope of assumptions):

R1 goal-chain:   enough P1, enough P2, ..., enough Pn      ->  enough Pn
                 the goal transformed step by step: only the last form matters
R2 goal-closed:  enough P, conclusion P                    ->  conclusion G
                 "it is enough to show P. P." proves the goal G that P was
                 enough for (the unit's 'from', if the units have it), else P
R3 fact-chain:   conclusion P1, ..., conclusion Pn, each a new form of the
                 same hypothesis (rewrite ... in H, simp at h) ->  conclusion Pn
R4 restatement:  conclusion P, conclusion P                ->  conclusion P
                 a line that says again what the line before it says
                 (pose proof H as H0, exact H after H was derived); an
                 assumption followed by the same proposition is kept, as
                 the second line closes the case
R5 assumptions:  hypos A, hypos B, both made by tactics     ->  hypos A, B
                 "Let x be ... . Assume that ..." made by consecutive intros;
                 the assumptions of a case (made by no tactic) are kept apart
R6 by-assumption: hypos A (to show G), conclusion A        ->  hypos A, conclusion G
                 a step closing the goal G with the assumption just made
                 (after goal-chain and goal-closed merged the steps from G
                 to A) proves G, not A: "to derive a contradiction, assume
                 j = j'. A contradiction."

A unit that a rule drops gives its tactics to the unit that stays; the
method of a merged unit ("by rewriting") is kept only if all its steps have
one, and is the method of the last of them.

    import proof_shortening            # with $INFORMATH_ROOT/scripts on sys.path
    short, counts = proof_shortening.shorten(units)

`units` is the list of proof units of one proof ({'unit': ..., 'prop': ...,
'by': ..., 'depth': ...}); `counts` tells how many times each rule applied.
outline(units) keeps only the main line of a proof: each claim without the
steps of its own proof (for an informal text that follows the script).

Used by kgah (von Plato's Coq files).
"""

import re

RULES = ['goal-chain', 'goal-closed', 'fact-chain', 'restatement', 'assumptions', 'by-assumption']
BINDER = re.compile(r"([A-Za-z_][A-Za-z0-9_']*)\s*=>")


def normal(prop):
    """A proposition with its white space normalized, its outer parentheses
    dropped, and its bound variables named by their order: two that differ
    only in these are the same."""
    p = re.sub(r'\s+', ' ', prop or '').strip()
    while p.startswith('(') and p.endswith(')') and balanced(p[1:-1]):
        p = p[1:-1].strip()
    names = []
    for b in BINDER.findall(p):
        if b not in names:
            names.append(b)
    ren = {b: '_b%d' % i for i, b in enumerate(names)}
    return re.sub(r"(?<![A-Za-z0-9_'])[A-Za-z_][A-Za-z0-9_']*(?![A-Za-z0-9_'])",
                  lambda m: ren.get(m.group(0), m.group(0)), p)


def balanced(s):
    depth = 0
    for c in s:
        depth += c == '('
        depth -= c == ')'
        if depth < 0:
            return False
    return depth == 0


def joined(*bys):
    """The tactics of merged steps, in order, as one justification."""
    out = []
    for b in bys:
        for t in (b or '').split(' ;; '):
            if t.strip():
                out.append(t.strip())
    return ' ;; '.join(out)


def merged(first, last):
    """last, with the tactics of first before its own, and a method only if
    both have one; the goal that first is enough for ('from') is kept."""
    u = dict(last)
    u['by'] = joined(first.get('by'), last.get('by'))
    if first.get('from'):
        u['from'] = first['from']
    if not (first.get('method') and last.get('method')):
        u.pop('method', None)
    return u


def step(units, counts):
    """One pass of the rules over the units: the units after it, and whether a rule applied."""
    out = []
    changed = False
    for u in units:
        prev = out[-1] if out else None
        if prev is None or prev['depth'] != u['depth']:
            out.append(u)
            continue
        a, b = prev['unit'], u['unit']
        # R1 goal-chain
        if a == 'enough' and b == 'enough':
            out[-1] = merged(prev, u)
            counts['goal-chain'] += 1
            changed = True
            continue
        # R2 goal-closed
        if a == 'enough' and b == 'conclusion' and normal(prev['prop']) == normal(u['prop']):
            out[-1] = merged(prev, u)
            # what the steps prove is the goal the first of them was enough for
            if prev.get('from'):
                out[-1]['prop'] = prev['from']
                del out[-1]['from']
            counts['goal-closed'] += 1
            changed = True
            continue
        # R3 fact-chain
        if a == 'conclusion' and b == 'conclusion' and prev.get('of') and prev.get('of') == u.get('of'):
            out[-1] = merged(prev, u)
            counts['fact-chain'] += 1
            changed = True
            continue
        # R4 restatement
        if a == 'conclusion' and b == 'conclusion' and normal(prev['prop']) == normal(u['prop']):
            out[-1] = dict(prev, by=joined(prev.get('by'), u.get('by')))
            counts['restatement'] += 1
            changed = True
            continue
        # R6 by-assumption
        if a == 'hypos' and b == 'conclusion' and prev.get('goal') and \
                normal(u['prop']) in [normal(t[len('Proof '):]) for _, t in prev.get('hypos', [])
                                      if t.startswith('Proof ')] and \
                normal(u['prop']) != normal(prev['goal']):
            out.append(dict(u, prop=prev['goal']))
            counts['by-assumption'] += 1
            changed = True
            continue
        # R5 assumptions
        if a == 'hypos' and b == 'hypos' and prev.get('by') and u.get('by'):
            h = dict(u)
            h['hypos'] = prev['hypos'] + u['hypos']
            h['by'] = joined(prev.get('by'), u.get('by'))
            out[-1] = h
            counts['assumptions'] += 1
            changed = True
            continue
        out.append(u)
    return out, changed


def shorten(units):
    """The units of a proof with the rules applied until none applies, and
    how many times each rule applied."""
    counts = {r: 0 for r in RULES}
    changed = True
    while changed:
        units, changed = step(units, counts)
    return units, counts


def outline(units):
    """The outline of a proof: each claim (a verify unit: assert (H : P) in
    Coq, have h : P := by ... in Lean) is kept, the steps of its own proof are
    left out. These steps are the units in the block of the script that
    follows the claim (their 'path', the bullets and braces they are in,
    extends the claim's), or without paths, those deeper than the claim up
    to the next unit at its depth. What is left are the steps of the main
    line of the proof, which are what an informal proof writes; the claims
    are marked 'outlined'."""
    out = []
    skip = None          # the claim whose steps are being left out
    for u in units:
        if skip is not None:
            p, q = skip.get('path'), u.get('path')
            if p is not None and q is not None:
                inside = len(q) > len(p) and q[:len(p)] == p
            else:
                inside = u['depth'] > skip['depth']
            if inside:
                continue
            skip = None
        if u['unit'] == 'verify':
            u = dict(u, outlined=True)
            skip = u
        out.append(u)
    return out
