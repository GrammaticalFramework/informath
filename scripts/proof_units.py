#!/usr/bin/env python3
"""What makes the proof texts of tactic proofs read better, on their proof
units (scripts/rocq/rocq-tactic-units.py, scripts/lean/lean-tactic-units.py),
shared by scripts/rocq/rocq-index.py and RunInformath -from-formalism:

* the steps shortened by Informath's scripts/proof_shortening.py;
* what each step uses (with_uses): the lemmas of the export by name ("by L,
  P"), the hypotheses and facts of the proof by their labels ("by (h)"), or by
  their statements when they were made or last cited more than FAR lines above
  ("since A, P"), and the labels shown where the cited ones are made;
* claims stated forward (forward_verifications): "by L, P (h)" for a claim
  proved in one step, "P (h). Indeed:" otherwise;
* the steps that only introduce variables of #IMPLICIT types left out
  (without_implicit_variables).

    python3 proof_units.py <units.json> <export dir> [-tables=a.dkgf,b.dkgf] [-full] [-keep-citations] [-lean]
    # -> <units>-short.json (or, with -full, <units>-full.json)

-keep-citations keeps the citations the units already have (Lean's units
make their own) instead of finding them in the tactics as for Rocq.  OUT is
the export directory (its <M>.json files give the lemmas), TABLE the symbol
tables (their #IMPLICIT lines).
"""
import glob, json, os, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import proof_shortening

OUT = 'out'
TABLE = ''
# Lean's tactics (-lean): their names and lists (rw [...], simp [...]), dotted
# lemma names, and the hypotheses among the arguments of a library term
# (exact Or.inr hp); Rocq's tactic texts keep the rules made for them
LEAN = False

# the tactics that name the fact they use first: apply L, exact (L x), pose proof
# (L x) as h, rewrite <- L, destruct (L x) as [...], specialize (L x)
USING = re.compile(r"(?:^|\.\s*|;;\s*)(?:e?apply|exact|pose\s+proof|specialize|e?destruct|rewrite|erewrite|"
                   r"elim|inversion|injection)\s+(?:(?:<-|->)\s*)?\(?\s*@?([A-Za-z_][A-Za-z0-9_']*)")
_NAMES = None
_THEOREMS = None
# the tactics whose terms name the facts they use: every lemma of the project
# in them is cited, not only the head (exact (L1 _ (L2 x) h) cites L1 and
# L2), and assert (h := L x) cites L
TERM_TACTICS = re.compile(r"^\s*(?:e?apply|exact|pose\s+proof|specialize|e?destruct|rewrite|erewrite|elim|"
                          r"inversion|injection|refine|assert\s*\(\s*[A-Za-z_][\w']*\s*:=|pose|set)\b")
IDENT = re.compile(r"(?<![\w'.@])@?([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)")
ROCQ_IDENT = re.compile(r"(?<![\w'.@])@?([A-Za-z_][A-Za-z0-9_']*)")
# Lean's tactics: their heads (exact L, apply h, have h := L, obtain ... := h,
# cases h, rcases h, specialize h, refine L) and the lists of rw, simp
LEAN_USING = re.compile(r"(?:^|;;\s*|<;>\s*)(?:exact|apply|refine|specialize|cases|rcases|induction|"
                        r"have\s+[^:]*?:=|obtain\s+[^:]*?:=|let\s+[^:]*?:=)\s*\(?\s*@?"
                        r"([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)")
LEAN_LISTS = re.compile(r"\b(?:rw|rwa|rewrite|nth_rewrite\s+\d+|simp(?:\s+only)?|simp_all|simpa(?:\s+only)?|"
                        r"field_simp|norm_num|linarith|nlinarith|positivity)\s*(?:\[([^\]]*)\])")
LEAN_TERM_TACTICS = re.compile(r"^\s*(?:exact|apply|refine|have|obtain|let|calc|specialize|use|exists|constructor|"
                               r"rw|rwa|rewrite|nth_rewrite|simp|simp_all|simpa|field_simp|linarith|nlinarith)\b")


def declared_names():
    """The names of the declarations of all the modules of the export: the
    lemmas a step can use (not the local hypotheses, nor the library's)."""
    global _NAMES
    if _NAMES is None:
        _NAMES = set()
        for f in glob.glob(os.path.join(OUT, '*.json')):
            if f.endswith('-tactics.json') or '-tactic-units' in f:
                continue
            try:
                for d in json.load(open(f)):
                    for k in ('coq', 'lean', 'constructor'):
                        if isinstance(d, dict) and isinstance(d.get(k), str):
                            _NAMES.add(d[k])
                            _NAMES.add(d[k].split('.')[-1])
                    for fld in (d.get('fields') or []) if isinstance(d, dict) else []:
                        if isinstance(fld, str):
                            _NAMES.add(fld)
                        elif isinstance(fld, (list, tuple)) and fld and isinstance(fld[0], str):
                            _NAMES.add(fld[0])
            except (ValueError, TypeError):
                pass
    return _NAMES


# a local hypothesis cited more than this many lines after it was made or last
# cited is cited by its statement ("since A, ..."), else by its label ("by (h)")
FAR = 10


def plain(p):
    """A proposition in Dedukti without its spaces and outer parentheses."""
    p = re.sub(r'\s+', '', p or '')
    while p.startswith('(') and p.endswith(')'):
        p = p[1:-1]
    return p


def theorem_names():
    """The theorems and axioms of the export: the lemmas that a term can
    name as an argument (not the definitions it applies)."""
    global _THEOREMS
    if _THEOREMS is None:
        _THEOREMS = set()
        for f in glob.glob(os.path.join(OUT, '*.json')):
            if f.endswith('-tactics.json') or '-tactic-units' in f:
                continue
            try:
                for d in json.load(open(f)):
                    if isinstance(d, dict) and d.get('kind') in ('theorem', 'axiom', 'lemma'):
                        for k in ('coq', 'lean'):
                            if isinstance(d.get(k), str):
                                _THEOREMS.add(d[k])
                                _THEOREMS.add(d[k].split('.')[-1])
            except (ValueError, TypeError):
                pass
    return _THEOREMS


def with_uses(proofs):
    """Each step with what its tactics apply, in order, which the proof text
    cites: the lemmas of the formalization ("uses": "by L, P"), the local
    hypotheses and derived facts by their labels ("cites": "by (h), P"), or by
    their statements if they were made or last cited more than FAR lines
    above ("since": "since A, P"); and each step that makes a hypothesis or
    fact cited by its label with that label ("labels": "assume that A (h)")."""
    names = declared_names()
    theorems = theorem_names()
    for p in proofs:
        steps = p['steps']
        made, last = {}, {}     # label -> (statement, line of the step that makes it), last line
        for i, u in enumerate(steps):
            for k in ('uses', 'cites', 'since', 'labels'):
                u.pop(k, None)
            used, cites, since = [], [], []
            if u.get('unit') in ('conclusion', 'enough'):
                # the lemmas named anywhere in the terms of the tactics
                for tac in re.split(r'\s*;;\s*|\.\s+(?=[a-z])', u.get('by', '')):
                    if TERM_TACTICS.match(tac) or LEAN and LEAN_TERM_TACTICS.match(tac):
                        for m in (IDENT if LEAN else ROCQ_IDENT).finditer(tac):
                            h = m.group(1)
                            if h in theorems and h not in used and h not in made:
                                used.append(h)
                # the hypotheses where they are applied (the head of the term),
                # and the hypotheses and lemmas of Lean's rw [...], simp [...]
                by = u.get('by', '')
                heads = [m.group(1) for m in USING.finditer(by)]
                if LEAN:
                    heads += [m.group(1) for m in LEAN_USING.finditer(by)]
                for m in (LEAN_LISTS.finditer(by) if LEAN else []):
                    heads += [x.group(1) for x in IDENT.finditer((m.group(1) or '').replace('←', ' '))]
                # a term headed by a constant of the library (Or.inr hp, conj h1 h2):
                # the hypotheses among its arguments
                # (not the names the step gives: destruct (H n) as [h1 h2])
                own = {x for x, _ in u.get('facts', [])} | {x for x, _ in u.get('hypos', [])}
                for tac in (re.split(r'\s*;;\s*|\s*<;>\s*|\.\s+(?=[a-z])', by) if LEAN else []):
                    if TERM_TACTICS.match(tac) or LEAN_TERM_TACTICS.match(tac):
                        term = re.split(r'\s+(?:as|eqn:|with)\s+|\s+in\s+[A-Za-z_]', tac)[0]
                        ids = [x.group(1) for x in IDENT.finditer(term)][1:]
                        if ids and ids[0] not in made and ids[0] not in theorems and ids[0] not in names:
                            heads += [x for x in ids[1:] if x in made and x not in own]
                for h in dict.fromkeys(heads):
                    if h in made and h not in cites and made[h][0] not in since:
                        # not "since A, A": a hypothesis that is what the step
                        # concludes is cited by its label, however far
                        if i - last[h] > FAR and plain(made[h][0]) != plain(u.get('prop', '')):
                            since.append(made[h][0])
                        else:
                            cites.append(h)
                        last[h] = i
                    elif (h in theorems if LEAN else h in names) and h not in used and h not in made:
                        # (Lean: not the definitions of rw [...] lists, nor constants)
                        used.append(h)
            if used:
                u['uses'] = used
            if cites:
                u['cites'] = cites
            if since:
                u['since'] = since
            # what the step makes, by its labels
            new = []
            if u.get('unit') == 'hypos':
                new = [(x, t[len('Proof '):]) for x, t in u.get('hypos', []) if t.startswith('Proof ')]
            elif u.get('unit') in ('conclusion', 'verify'):
                new = [(x, t) for x, t in u.get('facts', [])]
            for x, t in new:
                made[x], last[x] = (t, i), i
        # the labels shown where they are made: those cited by label
        cited = {h for u in steps for h in u.get('cites', [])}
        for i, u in enumerate(steps):
            if u.get('unit') == 'hypos':
                labels = [x for x, t in u.get('hypos', []) if x in cited and t.startswith('Proof ')]
            elif u.get('unit') in ('conclusion', 'verify'):
                labels = [x for x, _ in u.get('facts', []) if x in cited]
            else:
                labels = []
            if labels:
                u['labels'] = labels
    return proofs


_IMPLICIT = None


def implicit_types():
    """The types of the symbol table's #IMPLICIT lines (Elem Obj), whose
    variables a reading leaves unbound."""
    global _IMPLICIT
    if _IMPLICIT is None:
        _IMPLICIT = set()
        for t in TABLE.split(','):
            if os.path.exists(t):
                for l in open(t):
                    if l.startswith('#IMPLICIT'):
                        _IMPLICIT.add(' '.join(l.split()[1:]).replace('(', '').replace(')', ''))
    return _IMPLICIT


def without_implicit_variables(proofs):
    """The steps that introduce only variables of implicit types (intros x),
    which say nothing, as the variables are read as being of those types:
    left out, their tactics given to the next step."""
    imp = implicit_types()

    def only_implicit(u):
        return (u.get('unit') == 'hypos' and u.get('hypos') and
                all(t.replace('(', '').replace(')', '').strip() in imp for _, t in u['hypos']))
    for p in proofs:
        out, carry = [], None
        for u in p['steps']:
            if only_implicit(u):
                carry = u['by'] if carry is None else carry + ' ;; ' + u['by']
                continue
            if carry is not None:
                u = dict(u, by=carry + ' ;; ' + u.get('by', ''))
                carry = None
            out.append(u)
        if carry is not None and out:
            out[-1] = dict(out[-1], by=out[-1].get('by', '') + ' ;; ' + carry)
        p['steps'] = out
    return proofs


def forward_verifications(proofs):
    """A claim verified in one step (assert (P) as h by one tactic) as that
    step, stated forward: "by L, P (h)" rather than "first verify that P. By
    L, P." A claim verified in more steps stays a claim ("we have P (h).
    Indeed:"), with the steps folded under it (proof_folds.verification_collapses)."""
    for p in proofs:
        steps, out, i = p['steps'], [], 0
        while i < len(steps):
            u = steps[i]
            if u.get('unit') == 'verify':
                path = list(u.get('path') or [])
                block = []
                j = i + 1
                if j < len(steps):
                    q = list(steps[j].get('path') or [])
                    if len(q) > len(path) and q[:len(path)] == path:
                        prefix = q[:len(path) + 1]
                        while j < len(steps) and list(steps[j].get('path') or [])[:len(prefix)] == prefix:
                            block.append(steps[j])
                            j += 1
                if len(block) == 1 and block[0].get('unit') == 'conclusion':
                    b = block[0]
                    merged = dict(b, prop=u['prop'], by=u['by'] + ' ;; ' + b.get('by', ''),
                                  path=u.get('path', []), depth=u.get('depth', b.get('depth')),
                                  facts=u.get('facts', []))
                    merged.pop('of', None)
                    out.append(merged)
                    i = j
                    continue
            out.append(u)
            i += 1
        p['steps'] = out
    return proofs


def improve(proofs, shorten=True, citations=True):
    """The units of each proof with all of the above (shortened unless shorten is False)."""
    proofs = without_implicit_variables(proofs)
    if shorten:
        proofs = [dict(p, steps=proof_shortening.shorten(p['steps'])[0]) for p in proofs]
        proofs = forward_verifications(proofs)
    if citations:
        proofs = with_uses(proofs)
    return proofs


def main():
    global OUT, TABLE, LEAN
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    flags = [a for a in sys.argv[1:] if a.startswith('-')]
    units_file, OUT = args[0], args[1]
    TABLE = next((f.split('=', 1)[1] for f in flags if f.startswith('-tables=')), '')
    full = '-full' in flags
    LEAN = '-lean' in flags
    proofs = improve(json.load(open(units_file)), shorten=not full, citations='-keep-citations' not in flags)
    out = units_file[:-len('.json')] + ('-full.json' if full else '-short.json')
    json.dump(proofs, open(out, 'w'), ensure_ascii=False, indent=1)
    print(out)


if __name__ == '__main__':
    main()
