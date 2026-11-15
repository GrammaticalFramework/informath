"""Proof texts of tactic proofs written symbolically: each line a formula in
a short sentence frame, for articles that show their formally verified code.

The input is the proof units of a tactic proof (mathlib-informath's
tactic-units.py, coq-tactic-units.py of kgah and kgb), usually shortened by
proof_shortening.shorten and outlined by proof_shortening.outline. Each
proposition and each list of variables is turned into a formula by one call
of RunInformath -to-symbolic-latex, and each unit into a sentence:

    hypos        Let $x \\in A$ such that $P$.   /   Assume $P$.
                 (with its goal: To show $G$, ...; To derive a contradiction, ...)
    conclusion   $P$.   By computation, $P$.   By Lemma 5, $P$.   A contradiction.
    enough       It suffices to show $P$.
    verify       We have $P$. (an outlined claim)   First we show $P$.
    cases        By cases.        induction  By induction.

The negation of a relation, not (R a b), is written with its own symbol,
a \\notin b, a \\neq b, when `negated` maps R to a constant whose entry in the
symbol table gives that symbol (In -> notIn with $#1 \\notin #2$).

    import symbolic_proofs          # with $INFORMATH_ROOT/scripts on sys.path
    sp = symbolic_proofs.SymbolicProofs(run, table, workdir, negated={'In': 'notIn'})
    plan = sp.plan(units)           # for each proof; also sp.statement(dedukti)
    sp.run()                        # reads all the formulas
    for n, depth, text in sp.lines(plan): ...

Used by kgah (von Plato's Gödel article) and kgb (von Plato's article on
Gödel's epsilon_0).
"""

import os
import re
import subprocess

TOKEN = re.compile(r"\s*(=>|\(|\)|[^\s()]+)")


def dk_parse(s):
    """A Dedukti term as nested lists: an application is a list, x => b is ('lam', x, b)."""
    toks = TOKEN.findall(s)
    pos = [0]

    def seq(stop):
        items = []
        while pos[0] < len(toks) and toks[pos[0]] not in stop:
            t = toks[pos[0]]
            if t == '(':
                pos[0] += 1
                items.append(seq({')'}))
                pos[0] += 1
            elif pos[0] + 1 < len(toks) and toks[pos[0] + 1] == '=>':
                pos[0] += 2
                items.append(('lam', t, seq(stop | {')'})))
                break
            else:
                items.append(t)
                pos[0] += 1
        return items[0] if len(items) == 1 else items
    return seq(set())


def dk_show(t):
    if isinstance(t, tuple):
        return '%s => %s' % (t[1], dk_show(t[2]))
    if isinstance(t, list):
        return ' '.join(x if isinstance(x, str) else '(%s)' % dk_show(x) for x in t)
    return t


METHOD_WORDS = {'computation': 'By computation', 'definition': 'By definition', 'rewriting': 'By rewriting',
                'simplification': 'By simplification', 'routine': 'Routinely'}
FALSE = ('false', '(false)')


def set_of(t):
    """The set of a variable's Dedukti type Elem A: A."""
    t = t.strip()
    return '(%s)' % t[len('Elem '):] if t.startswith('Elem ') else '(%s)' % t


def and_list(xs):
    return xs[0] if len(xs) == 1 else ', '.join(xs[:-1]) + ' and ' + xs[-1]


def citations(names, label=None):
    """A cite function for SymbolicProofs: the declarations (names) that a
    step's tactics mention, each as label(name) (an article's "Lemma 5"),
    or as its name in typewriter type; each result once, in order."""
    label = label or (lambda n: None)

    def cite(by):
        out = []
        for w in re.findall(r"[A-Za-z_][A-Za-z0-9_']*", by):
            if w in names:
                c = label(w) or '\\texttt{%s}' % w.replace('_', '\\_')
                if c not in out:
                    out.append(c)
        return out
    return cite


class SymbolicProofs:
    def __init__(self, run, table, workdir, negated=None, fix=None, env=None, cite=None):
        """run: RunInformath; table: the symbol table (.dkgf); workdir: where
        the formulas' Dedukti file is written; negated: relation -> its
        negation's constant; fix: a function applied to each formula; cite:
        a function from a step's tactics to the results it uses, as they are
        to be named in the text ("Lemma 5")."""
        self.runinformath, self.table, self.workdir = run, table, workdir
        self.cite = cite or (lambda by: [])
        self.negated = negated or {}
        self.fix = fix or (lambda f: f)
        self.env = env
        self.items = []
        self.out = {}

    # ---- the negated relations
    def negations(self, t):
        if isinstance(t, tuple):
            return ('lam', t[1], self.negations(t[2]))
        if isinstance(t, list):
            t = [self.negations(x) for x in t]
            if len(t) == 2 and t[0] == 'not' and isinstance(t[1], list) and t[1] and t[1][0] in self.negated:
                return [self.negated[t[1][0]]] + t[1][1:]
            return t
        return t

    def neat(self, prop):
        return dk_show(self.negations(dk_parse(prop))) if self.negated else prop

    def neat_text(self, s):
        """The negated relations of a Dedukti declaration, as in neat."""
        if not self.negated:
            return s
        out, i = [], 0
        pat = re.compile(r'not \((%s) ' % '|'.join(map(re.escape, self.negated)))
        while True:
            m = pat.search(s, i)
            if not m:
                out.append(s[i:])
                return ''.join(out)
            depth, j = 0, m.start() + 4
            for j in range(m.start() + 4, len(s)):
                depth += s[j] == '('
                depth -= s[j] == ')'
                if depth == 0:
                    break
            out.append(s[i:m.start()] + '(' + self.negated[m.group(1)] + ' ' + self.neat_text(s[m.end():j]) + ')')
            i = j + 1

    # ---- the formulas
    def prop(self, p):
        self.items.append('Proof (%s)' % self.neat(p))
        return len(self.items) - 1

    def context(self, hyps):
        binders = ' -> '.join('(%s : %s)' % (n, t) for n, t in hyps)
        self.items.append('%s -> Proof truth' % binders)
        return len(self.items) - 1

    def statement(self, dedukti):
        """A theorem's declaration, its formula read with the others."""
        self.items.append(('STATEMENT', self.neat_text(dedukti)))
        return len(self.items) - 1

    def run(self):
        dk = os.path.join(self.workdir, 'symbolic-proofs.dk')

        def decl(i, t):
            if isinstance(t, tuple):        # a statement: its own declaration, renamed
                return re.sub(r'^\S+', 'fax%d' % i, t[1].strip()).rstrip('.') + '.'
            return 'fax%d : %s.' % (i, t)
        open(dk, 'w').write('\n'.join(decl(i, t) for i, t in enumerate(self.items)) + '\n')
        r = subprocess.run([self.runinformath, '-add-symboltables=' + self.table, '-to-symbolic-latex', dk],
                           capture_output=True, text=True, env=self.env)
        for l in r.stdout.split('\n'):
            m = re.match(r'\\textbf\{Axiom\} \\texttt\{\\detokenize\{fax(\d+)\}\}\.\s*\$(.*)\$\s*$', l.strip())
            if m:
                self.out[int(m.group(1))] = self.fix(m.group(2).strip())
        missing = len(self.items) - len(self.out)
        if missing:
            print('symbolic_proofs: %d formulas missing; RunInformath: %s' % (missing, r.stderr[-300:]))
        return self

    def get(self, i, part=None):
        f = self.out.get(i, '?')
        if part == 'context':
            return f.split('\\vdash')[0].strip().rstrip(',')
        return f

    # ---- the proofs
    def plan(self, units):
        """For each unit its depth and what its sentence needs, the formulas
        requested; the sentences are made after run()."""
        plan = []
        for u in units:
            k = u['unit']
            if k == 'hypos':
                defs = u.get('defs') or {}
                # local definitions as equations, k = v
                dis = [self.prop('Eqt %s %s (%s)' % (set_of(t), n, defs[n])) for n, t in u['hypos'] if n in defs]
                vs = [(n, t) for n, t in u['hypos'] if not t.startswith('Proof ') and n not in defs]
                ps = [t[len('Proof '):] for n, t in u['hypos'] if t.startswith('Proof ')]
                ci = self.context(vs) if vs else None
                pis = [self.prop(p) for p in ps]
                goal = (u.get('goal') or '').strip()
                gi = self.prop(goal) if goal and goal not in FALSE else None
                plan.append((u['depth'], ('hypos', ci, pis, gi, goal in FALSE, dis)))
            elif k in ('conclusion', 'enough', 'verify'):
                plan.append((u['depth'], (k, self.prop(u['prop']), u.get('method'), u.get('outlined'),
                                          u['prop'].strip() in FALSE, self.cite(u.get('by') or ''))))
            elif k in ('cases', 'induction'):
                plan.append((u['depth'], (k,)))
        return plan

    def sentence(self, step):
        k = step[0]
        if k == 'hypos':
            _, ci, pis, gi, contra, dis = step
            props = ' and '.join('$%s$' % self.get(i) for i in pis)
            lets = (['$%s$' % self.get(ci, 'context')] if ci is not None else []) + ['$%s$' % self.get(i) for i in dis]
            if lets:
                s = 'Let %s' % ', '.join(lets) + (' such that %s' % props if props else '')
            else:
                s = 'Assume %s' % props
            if contra:
                s = 'To derive a contradiction, ' + s[0].lower() + s[1:]
            elif gi is not None:
                s = 'To show $%s$, ' % self.get(gi) + s[0].lower() + s[1:]
            return s + '.'
        if k in ('cases', 'induction'):
            return 'By induction.' if k == 'induction' else 'By cases.'
        _, i, method, outlined, false, cites = step
        f = 'a contradiction' if false else '$%s$' % self.get(i)
        by = 'By %s' % and_list(cites) if cites else METHOD_WORDS.get(method)
        if k == 'enough':
            return ('%s, it suffices to show %s.' % (by, f)) if cites else 'It suffices to show %s.' % f
        if k == 'verify':
            return ('We have %s.' if outlined else 'First we show %s.') % f
        if false:
            return '%s, a contradiction.' % by if by else 'A contradiction.'
        if by:
            return '%s, %s.' % (by, f)
        return f + '.'

    def lines(self, plan):
        """The numbered lines of a planned proof: (number, depth, sentence)."""
        return [(n, depth, self.sentence(step)) for n, (depth, step) in enumerate(plan, 1)]
