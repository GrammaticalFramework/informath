#!/usr/bin/env python3
"""The tactic proofs of a Coq file as proof units, for RunInformath -tactic-proof:
mathlib-informath's tactic-units.py with Coq's tactics.

    python3 rocq-export.py coq/<M>.v                # -> out/<M>-tactics.json
    python3 rocq-tactic-units.py <M> [<dir>]        # -> out/<M>-tactic-units.json (or <dir>/...)

out/<Module>-tactics.json has, for each step of a tactic proof (a tactic of a
tactic sequence), the main goal before it and the goals after it, with their
hypotheses and targets in Dedukti. A step becomes one or two proof units,
by comparing the goals:

* hypotheses that are there before the step without an earlier step having
  made them (the case hypotheses of by_cases, rcases, induction, at the
  beginning of a case) are assumed: "Assume that n = 1";
* new hypotheses after the step: assumed if the step introduces them (intro,
  rintro), "let n be ... such that ..." if they include variables (obtain,
  rcases), and stated if they are propositions only (have, rw at, simp at);
* a goal closed by the step is stated, justified by the step: "m = p";
* a changed goal is what is enough to show: "it is enough to show that ...",
  the conjunction of the goals if there are several;
* a step that splits the goal into cases (by_cases, rcases with |, cases,
  induction) is "we reason by cases" or "by induction".

Each unit has its depth: the number of assumptions open at it (those of an
intro and of the beginning of a case, while they are hypotheses of the goal;
not the derived facts, nor the statement's hypotheses), for RunInformath's
-proof-indent; and an assumption of propositions by an intro has its goal,
what is left to show under it ("to prove that m = p, assume that n = 1"),
for -proof-goals.

Each unit has the step's tactic as its justification. The declaration of a
step is found by its line in out/<Module>.json.
"""
import json, re, sys

INTRO = ('intro', 'intros')
# the tactics whose new hypotheses are derived facts, not assumptions; "in H"
# makes a rewriting or simplification one too
DERIVED = ('assert', 'pose', 'specialize', 'destruct', 'edestruct', 'set', 'injection', 'inversion', 'apply_in')
CASES = ('destruct', 'case', 'elim', 'inversion')
INDUCTION = ('induction',)
# how a step that closes a goal proves it, by its tactic: "by rewriting, P"
# (RunInformath reads the method); a step that applies a lemma (exact, apply)
# has none, as its tactic names the lemma
METHODS = {
    'definition': ('reflexivity', 'simpl', 'cbn', 'cbv', 'unfold', 'compute', 'red', 'hnf', 'change', 'f_equal'),
    'rewriting': ('rewrite', 'subst', 'replace', 'symmetry', 'transitivity'),
    'computation': ('lia', 'omega', 'discriminate', 'congruence', 'ring', 'nia'),
    'routine': ('auto', 'eauto', 'tauto', 'trivial', 'intuition', 'assumption', 'contradiction', 'easy',
                'firstorder', 'exfalso'),
}
METHOD = {t: m for m, ts in METHODS.items() for t in ts}


def tactic_name(step):
    """The tactic's name: the first word of its text (rw for rw [...])."""
    m = re.match(r"[A-Za-z_][A-Za-z_0-9'?!]*", step['tactic'].strip())
    name = m.group(0) if m else ''
    if name == 'pose' and step['tactic'].strip().startswith('pose proof'):
        return 'pose'
    return name


HYGIENE = re.compile(r"([A-Za-z_][A-Za-z0-9_']*)\._@\.[A-Za-z0-9_.']*?_hyg\.\d+")


# As Export.lean names variables (freshName, asciiName): a Greek type
# variable by a Latin capital (α : Type is X), any other Greek letter by its
# name (ε is epsilon, which Informath shows as \epsilon), subscript digits as
# digits (x₀ is x0): Informath reads ASCII identifiers only
LATIN = {'α': 'X', 'β': 'Y', 'γ': 'Z', 'δ': 'W', 'ι': 'I', 'κ': 'J', 'σ': 'S', 'τ': 'T', 'ω': 'U'}
GREEK = dict(zip('αβγδεζηθικλμνξπρστυφχψωϕΓΔΘΛΞΠΣΦΨΩ',
                 ['alpha', 'beta', 'gamma', 'delta', 'epsilon', 'zeta', 'eta', 'theta', 'iota',
                  'kappa', 'lambda', 'mu', 'nu', 'xi', 'pi', 'rho', 'sigma', 'tau', 'upsilon',
                  'phi', 'chi', 'psi', 'omega', 'phi', 'Gamma', 'Delta', 'Theta', 'Lambda', 'Xi',
                  'Pi', 'Sigma', 'Phi', 'Psi', 'Omega']))
SUBSCRIPT = {chr(0x2080 + i): str(i) for i in range(10)}
WORD = re.compile(r"[\w']+")


def clean(s, sorts=()):
    """Names with Lean's macro scopes (m._@.M._hyg.42, shown as m✝) as m',
    and non-ASCII letters in ASCII; the names of types (sorts) as Latin capitals."""
    s = HYGIENE.sub(lambda m: m.group(1) + "'", s)
    def word(m):
        w = m.group(0)
        table = LATIN if w in sorts else {}
        return ''.join(table.get(c) or GREEK.get(c) or SUBSCRIPT.get(c, c) for c in w)
    return WORD.sub(word, s)


def sorts_of(goal):
    """The names of the types among the hypotheses of a goal (α : Type, as Set)."""
    return {h[0] for h in (goal or {}).get('hyps', []) if h[1].strip() == 'Set'}


DEFS = {}      # the values of the local definitions (set k := v) of the proof at hand


def hyps(goal):
    """The accessible hypotheses of a goal, as (name, type) pairs; the values
    of local definitions go to DEFS."""
    sorts = sorts_of(goal)
    for h in (goal or {}).get('hyps', []):
        if len(h) > 3:
            DEFS[clean(h[0], sorts)] = clean(h[3], sorts)
    return [(clean(h[0], sorts), clean(h[1], sorts)) for h in (goal or {}).get('hyps', []) if not h[2]]


def target(goal):
    return clean(goal['target'], sorts_of(goal))


def method(st, name):
    """The method of a step that closes a goal or derives a fact, if its tactic tells one."""
    if re.match(r'exact\s+rfl\b', st['tactic'].strip()):
        return 'definition'
    return METHOD.get(name.rstrip('!'))


BINDER = re.compile(r"([A-Za-z_][A-Za-z0-9_']*)\s*=>")


def alpha(target):
    """A target with its bound variables named by their order: two targets
    that differ only in those names are the same goal."""
    names = []
    for b in BINDER.findall(target):
        if b not in names:
            names.append(b)
    ren = {b: '_b%d' % i for i, b in enumerate(names)}
    return re.sub(r"(?<![A-Za-z0-9_'])[A-Za-z_][A-Za-z0-9_']*(?![A-Za-z0-9_'])",
                  lambda m: ren.get(m.group(0), m.group(0)), target)


def is_prop(ty):
    return ty.startswith('Proof ')


def conj(props):
    out = props[-1]
    for p in reversed(props[:-1]):
        out = 'and (%s) (%s)' % (p, out)
    return out


def split_if(target):
    """if A B -> (A, B), for a target in Dedukti, else None."""
    if not target.startswith('if '):
        return None
    args = top_args(target[3:])
    return (args[0], args[1]) if len(args) == 2 else None


def top_args(s):
    """The arguments of an application, as written: 'A (B C) D' -> ['A', 'B C', 'D']."""
    out, depth, cur = [], 0, ''
    for ch in s.strip():
        if ch == '(':
            depth += 1
            if depth == 1:
                continue
        elif ch == ')':
            depth -= 1
            if depth == 0:
                out.append(cur)
                cur = ''
                continue
        if ch == ' ' and depth == 0:
            if cur:
                out.append(cur)
            cur = ''
            continue
        cur += ch
    if cur:
        out.append(cur)
    return out


def first_line(tactic):
    """The justification: the tactic's first line, without a nested proof's "by"."""
    line = tactic.strip().split('\n')[0].strip()
    return re.sub(r'\s*:=\s*by$', '', line)


def units_of_proof(steps):
    """The proof units of the steps of one proof, in order, with their depths."""
    units = []
    DEFS.clear()

    class Seen:
        """The hypotheses seen, each in the cases (bullets) where it was seen:
        a hypothesis of a case is new in another case."""
        def __init__(self, hs=()):
            self.at = [(h, path or []) for h in hs]
        def __contains__(self, h):
            cur = path or []
            return any(x == h and cur[:len(p)] == p for x, p in self.at)
        def __ior__(self, hs):
            self.at += [(h, path or []) for h in hs]
            return self
    seen = Seen()
    groups = None        # the assumptions made: sets of hypotheses, or [] for one that substitutes
    open_substs = []     # those that substitute (rintro rfl), open until a goal is closed

    def add(unit, context):
        """A unit, with the number of assumptions open in its context: in a
        Coq script, those made in the cases (bullets) that the step is in."""
        ctx = set(context)
        if path is not None:
            unit['depth'] = sum(1 for g, p in zip(groups, paths) if g and path[:len(p)] == p)
        else:
            unit['depth'] = sum(1 for g in groups if g and g <= ctx) + len(open_substs)
        if path is not None:
            unit['path'] = path      # the cases (bullets, braces) it is in, for proof_shortening.outline
        if unit['unit'] == 'hypos':
            defs = {n: DEFS[n] for n, _ in unit['hypos'] if n in DEFS}
            if defs:
                unit['defs'] = defs  # local definitions, read as equations k = v
        units.append(unit)

    class Groups(list):
        """The assumptions made, each with the cases it is made in."""
        def append(self, g):
            list.append(self, g)
            paths.append(path)
    paths = []
    path = None
    groups = Groups()

    asserted = set()     # the names of the facts of assert (H : P), proved first and used after
    if steps and steps[0]['before']:
        seen = Seen(hyps(steps[0]['before']))
    for st in steps:
        path = st.get('path')
        before, after = st['before'], st['after']
        if not before or 'error' in before or any('error' in a for a in after):
            add({'unit': 'obvious', 'by': st['tactic']}, hyps(before) if before and 'error' not in before else [])
            continue
        name = tactic_name(st)
        hb = hyps(before)
        # hypotheses there without an earlier step's making them: a case's
        entered = [h for h in hb if h not in seen and h[0] not in asserted]
        if entered:
            groups.append(set(entered))      # the assumption of a case
            add({'unit': 'hypos', 'hypos': entered, 'by': ''}, hb)
        seen |= set(hb)
        n_before = st.get('nBefore', 1)
        closed = len(after) < n_before
        new_goals = after[:max(0, len(after) - n_before + 1)]
        # the hypotheses a step makes count as seen, but not those of the cases
        # it splits the goal into: they are assumed where each case begins
        if len(new_goals) <= 1:
            for a in new_goals:
                seen |= set(hyps(a))
        if name == '·':
            continue
        main = new_goals[0] if new_goals else None
        new = [h for h in hyps(main) if h not in hb] if main else []
        if name in INDUCTION:
            add({'unit': 'induction', 'by': st['tactic']}, hb)
            continue
        if len(new_goals) > 1 and name in CASES:
            add({'unit': 'cases', 'by': st['tactic']}, hb)
            continue
        ma = re.match(r'assert\s*\(\s*([A-Za-z_][A-Za-z0-9_\']*)\s*:', st['tactic'])
        if name == 'assert' and len(new_goals) == 2:
            # assert (H : P): to show P, which the step's nested steps prove, and then use it
            if ma:
                asserted.add(ma.group(1))
            add({'unit': 'verify', 'prop': target(new_goals[0]), 'by': st['tactic']}, hb)
            continue
        derived = name in DERIVED or ' in ' in st['tactic']
        if not derived:
            # a hypothesis whose type changes by a substitution (rintro rfl) is not new
            names_before = {n for n, _ in hb}
            new = [h for h in new if h[0] not in names_before]
        if new and name in ('have', 'haveI') and re.search(r':=\s*by\b', st['tactic']) \
                and all(is_prop(t) for _, t in new):
            # a fact proved by a nested proof, which follows
            add({'unit': 'verify', 'prop': conj([t[len('Proof '):] for _, t in new]),
                 'by': st['tactic']}, hb)
        elif new:
            if not derived:
                # an assumption, with what is left to show as its goal
                groups.append(set(new))
                unit = {'unit': 'hypos', 'hypos': new, 'by': st['tactic']}
                # (not where it introduces variables, which the goal would
                # mention before they are introduced)
                if name in INTRO and main and all(is_prop(t) for _, t in new):
                    unit['goal'] = target(main)
                add(unit, hyps(main))
            elif all(is_prop(t) for _, t in new):
                # 'of': the hypotheses it is a new form of (rewrite ... in H), for proof_shortening
                add(with_method({'unit': 'conclusion', 'prop': conj([t[len('Proof '):] for _, t in new]),
                                 'by': st['tactic'], 'of': sorted(n for n, _ in new),
                                 # the facts by their labels, which a later step may cite
                                 'facts': [[n, t[len('Proof '):]] for n, t in new]}, st, name),
                    hyps(main) if main else hb)
            else:
                add({'unit': 'hypos', 'hypos': new, 'by': st['tactic']}, hyps(main) if main else hb)
        if closed:
            add(with_method({'unit': 'conclusion', 'prop': target(before), 'by': st['tactic']}, st, name), hb)
            if open_substs:
                open_substs.pop()
        elif main and alpha(target(main)) != alpha(target(before)) or len(new_goals) > 1:
            if name in INTRO:
                # an assumption that substitutes (rintro rfl) leaves no hypothesis
                ab = split_if(target(before))
                if not new and ab:
                    open_substs.append(True)
                    add({'unit': 'hypos', 'hypos': [['_h', 'Proof (%s)' % ab[0]]],
                         'goal': ab[1], 'by': st['tactic']}, hb)
            else:
                # not the goal as it was, nor a fact just stated to be verified
                said = {alpha(target(before))} | {alpha(u['prop']) for u in units[-1:] if u['unit'] == 'verify'}
                targets = [target(g) for g in new_goals if alpha(target(g)) not in said]
                if targets:
                    # 'from': the goal it is enough for, for proof_shortening (goal-closed)
                    add({'unit': 'enough', 'prop': conj(targets), 'from': target(before), 'by': st['tactic']}, hb)
    return units


def with_method(unit, st, name):
    m = method(st, name)
    if m:
        unit['method'] = m
    return unit


def inside(s, c):
    """Whether step s is a part of the step c: another step that starts within it."""
    return s is not c and (s['line'], s['col']) >= (c['line'], c['col']) and s['line'] <= c['endLine']


def main():
    mod = sys.argv[1]
    outdir = sys.argv[2] if sys.argv[2:] else 'out'
    decls = json.load(open('%s/%s.json' % (outdir, mod)))
    steps = json.load(open('%s/%s-tactics.json' % (outdir, mod)))
    proofs = []
    for d in decls:
        if d['kind'] != 'theorem':
            continue
        mine = [s for s in steps if s.get('decl') == d['coq']]
        if mine:
            units = units_of_proof(mine)
            for u in units:
                u['by'] = first_line(u['by'])
            proofs.append({'name': d['dk'], 'coq': d['coq'], 'steps': units})
    out = '%s/%s-tactic-units.json' % (outdir, mod)
    json.dump(proofs, open(out, 'w'), ensure_ascii=False, indent=1)
    print('%s: %d tactic proofs, %d units -> %s' % (mod, len(proofs), sum(len(p['steps']) for p in proofs), out))


if __name__ == '__main__':
    main()
