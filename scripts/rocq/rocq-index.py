#!/usr/bin/env python3
"""Build index.json of a project: for each declaration of its Coq files, its
Dedukti translation, Informath's reading in English and as a formula, and the
proof text of its tactic proof.  The project's own scripts make pages
(kg-heritage's kgah: build-html.py) and articles (kgah: build-article.py,
kgb: build-tex.py) from it.

    cd <project>         # its config.py, symbol table, coq/, out/
    python3 $INFORMATH/scripts/rocq/rocq-export.py coq/M.v   # -> out/M.dk, .json, -tactics.json
    python3 $INFORMATH/scripts/rocq/rocq-index.py            # -> index.json

The symbol table is the project's TABLE (config.py).  Each declaration is
read by a RunInformath call of its own, eight at a time: a definition by
rewrite rules (a Coq Fixpoint) is read rule by rule, as Informath's reading
of all the cases at once can take very long.  The tactic proofs are turned
into proof units by rocq-tactic-units.py and read by RunInformath
-tactic-proof, all steps and shortened (Informath's scripts/proof_shortening.py).
"""
import concurrent.futures, glob, json, os, re, subprocess, sys, tempfile

import project
HERE = os.path.dirname(os.path.abspath(__file__))
MODULES = project.get('MODULES', [])
TABLE = os.path.join(project.DIR, project.get('TABLE', 'table.dkgf'))
OUT = os.path.join(project.DIR, 'out')
INDEX = os.path.join(project.DIR, 'index.json')
LANGUAGES = os.environ.get('LANGUAGES', 'Eng').split(',')
TIMEOUT = 120
TMP = tempfile.mkdtemp(prefix='rocq-informath-')
FAILURES = []   # the RunInformath runs on proofs that failed
LABEL = re.compile(r'^\S+\.\s+')
FORMULA_LABEL = re.compile(r'^\\textbf\{(\w+)\}(?: \\texttt\{\\detokenize\{([^}]*)\}\})?\.\s*')


def informath():
    """RunInformath: the newest build in this Informath checkout."""
    root = os.environ['INFORMATH_ROOT'] = project.informath_root()
    built = glob.glob(os.path.join(root, '.stack-work', 'dist', '*', '*', 'build', 'RunInformath', 'RunInformath'))
    return max(built, key=os.path.getmtime) if built else 'RunInformath'


RUN = informath()
sys.path.insert(0, os.path.join(os.environ['INFORMATH_ROOT'], 'scripts'))
import proof_shortening


def blocks_of(mod):
    """Source name -> its Dedukti block, from out/<M>.dk (//COQ: from
    rocq-export.py, //AGDA: from agda-export.py)."""
    text = open(os.path.join(OUT, mod + '.dk')).read()
    out = {}
    for m in re.finditer(r'\(; //(?:COQ|AGDA):(\S+) ;\)\n(.*?)(?=\n\n\(; //(?:COQ|AGDA):|\Z)', text, re.S):
        out[m.group(1)] = m.group(2).strip()
    return out


def items_of(name, block, skip=()):
    """The pieces read apart: the declaration, and each rule of a definition by rules.
    The lines declaring the constants in skip (a record's constructor) are left out."""
    lines = [l for l in block.split('\n') if l.split(' ')[0] not in skip]
    block = '\n'.join(lines)
    rules = [l.rstrip('.') for l in lines if '-->' in l]
    if not rules:
        return [((name, 'stmt'), block)]
    decl = lines[0]
    return [((name, 'stmt'), decl)] + [((name, 'rule', i), decl + '\n' + r + '.') for i, r in enumerate(rules)]


def run(args, dk, timeout=TIMEOUT):
    f = os.path.join(TMP, 'i%d.dk' % abs(hash(dk)))
    open(f, 'w').write(dk + '\n')
    try:
        r = subprocess.run([RUN] + args + [f], capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return None
    return r.stdout if r.returncode == 0 else None


# flags for every statement reading, from the project (config.py): e.g.
# ['-statement-labels'], so that a hypothesis read as a proposition ("A is
# contractible") keeps its label when the statement mentions it
STATEMENT_FLAGS = list(project.get('STATEMENT_FLAGS', []))


def reading(key, dk, flags=()):
    """language -> reading, and the formula, of one piece."""
    out = {}
    flags = STATEMENT_FLAGS + [f for f in flags if f not in STATEMENT_FLAGS]
    js = run(['-json', '-no-back-to-dedukti', '-languages=' + ','.join(LANGUAGES), '-add-symboltables=' + TABLE]
             + list(flags), dk)
    if js:
        lines = [json.loads(l) for l in js.split('\n') if l.strip()]
        rule = len(key) == 3
        # a rule, or the declarations of a block (a record: its type, constructor and fields)
        for j in lines:
            if rule != ('-->' in j['originalDedukti']):
                continue
            for lang in LANGUAGES:
                ranks = j['nlgResults'].get('Informath' + lang)
                if ranks:
                    t = LABEL.sub('', ranks[0]['lin'], count=1) if not rule else ranks[0]['lin']
                    if rule:
                        m = re.search(r'\\item (.*?)\s*\\end\{itemize\}', t, re.S)
                        t = m.group(1) if m else t
                    out[lang] = (out[lang] + ' ' if lang in out else '') + t.strip()
            if rule:
                break
    tex = run(['-add-symboltables=' + TABLE, '-to-symbolic-latex'] + list(flags), dk)
    if tex:
        ls = [l for l in tex.split('\n') if l.strip()]
        if len(key) == 3:
            ls = [l for l in ls if l.startswith('\\textbf{Rules}')]
        if ls:
            out['formula'] = FORMULA_LABEL.sub('', ls[0]).strip()
            out['formulas'] = [FORMULA_LABEL.sub('', l).strip() for l in ls]
    return key, out


# ---------------------------------------------------------------- proofs (as in mathlib-informath)

def rows(table):
    out = []
    for row in re.split(r'\\\\\s*\n', table):
        mt = re.match(r'\s*(\d+\.)\s*&(.*)&(.*)$', row, re.S)
        if mt:
            out.append([g.strip() for g in mt.groups()])
    return out


def proof_tables(stdout, langs):
    out = []
    for sec in re.split(r'\\subsection\*', stdout)[1:]:
        if '% PROOF FAILED' in sec:
            out.append(None)
            continue
        tables = re.findall(r'\\begin\{tabular\}\{[^\n]*\}\n(.*?)\\end\{tabular\}', sec, re.S)
        if len(tables) < len(langs):
            break
        out.append({lang: rows(t) for lang, t in zip(langs, tables)})
    return out


# the binder macros of the symbol table (a bound variable and a body) and
# their forms for a function given as such, without a lambda: \\CoqAll {f}
# is \\CoqAllF {f}
BINDER_MACROS = project.get('BINDER_MACROS', {})
FUNCTION_FORMS = project.get('FUNCTION_FORMS', {})
# macros whose forms are single symbols (\\tau_{j,j'}), not to be put in parentheses
ATOMIC = tuple(project.get('ATOMIC', ()))


def untuple(t):
    """(\\trMACRoX {a}{b}) as \\trMACRoX {a}{b}, for the ATOMIC macros."""
    if not ATOMIC:
        return t
    return re.sub(r'\((\\(?:%s)[A-Za-z]*\s*\{[^{}]*\}\{[^{}]*\})\)' % '|'.join(ATOMIC), r'\1', t)


def brace_groups(t, i):
    """The number of {...} groups at t[i:], and where they end."""
    n = 0
    while True:
        j = i
        while j < len(t) and t[j] == ' ':
            j += 1
        if j >= len(t) or t[j] != '{':
            return n, i
        depth = 0
        for k in range(j, len(t)):
            depth += t[k] == '{'
            depth -= t[k] == '}'
            if depth == 0:
                break
        n, i = n + 1, k + 1


def fix_macros(t, macros):
    """A binder macro applied to a function, not to a lambda, as its function
    form; any other macro applied to fewer arguments than it has (a
    function iter F a, without its argument) with the missing ones as dots:
    F^a(\\cdot).  The changes are insertions, made from the right so that
    a macro inside the arguments of another one is changed too."""
    if not t:
        return t
    t = t.replace('{_}', '{\\_}')         # a wildcard of a rule's pattern
    t = untuple(t)
    inserts = []
    for m in re.finditer(r'\\([A-Za-z]+)(?![A-Za-z])', t):
        name = m.group(1)
        arity = BINDER_MACROS.get(name)
        if arity is None:
            d = macros.get(name)
            arity = d[1] if isinstance(d, list) else 0
        if not arity:
            continue
        n, end = brace_groups(t, m.end())
        if n >= arity:
            continue
        if name in BINDER_MACROS:
            inserts.append((m.end(), 'F'))
        else:
            inserts.append((end, '{\\cdot}' * (arity - n)))
    for pos, text in sorted(inserts, reverse=True):
        t = t[:pos] + text + t[pos:]
    return t


def macros_of(latex):
    out = {}
    for l in latex.split('\n'):
        m = re.match(r'\\newcommand\{\\(\w+)\}(?:\[(\d+)\])?\{(.*)\}\s*$', l.strip())
        if m:
            out[m.group(1)] = [m.group(3), int(m.group(2))] if m.group(2) else m.group(3)
    return out


def proof_texts(units, units_file):
    """Coq name -> language -> rows of the proof texts of the units in units_file,
    each row with the tactics of its step as justification; and the macros."""
    r = subprocess.run([RUN, '-tactic-proof', '-proof-indent', '-proof-goals', '-languages=' + ','.join(LANGUAGES),
                        '-add-symboltables=' + TABLE, units_file], capture_output=True, text=True)
    if r.returncode != 0:
        # a failure here (an unparsable wording in the table, ...) would leave
        # all the proof texts of the module empty without notice
        sys.stderr.write('rocq-index.py: RunInformath -tactic-proof failed on %s:\n%s\n'
                         % (units_file, r.stderr[-2000:]))
        FAILURES.append(units_file)
    texts = proof_tables(r.stdout, LANGUAGES)
    out = {}
    for p, t in zip(units, texts):
        if t:
            for lang in t:
                # each row with the path of its step (the bullets and braces it is
                # in) and its unit, for the folds of the pages (proof_folds.py)
                t[lang] = [[n, text, 'TACTIC:' + u['by'].replace(' ;; ', '. '), u.get('path', []), u.get('unit', '')]
                           for (n, text, _), u in zip(t[lang], p['steps'])]
        out[p['coq']] = t
    return out, macros_of(r.stdout)


# the side conditions of the formalization, which an article does not state
# (kg-heritage's kgb: that an object is extensional, good)
SIDE_CONDITIONS = tuple(project.get('SIDE_CONDITIONS', ()))


def drop_side_conditions(steps):
    """The proof units without the lines that only state a side condition."""
    def side(u):
        p = (u.get('prop') or '').strip()
        while p.startswith('(') and p.endswith(')'):
            p = p[1:-1].strip()
        return u['unit'] in ('conclusion', 'enough') and p.startswith(SIDE_CONDITIONS)
    return [u for u in steps if not side(u)] if SIDE_CONDITIONS else steps


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
IDENT = re.compile(r"(?<![\w'.@])@?([A-Za-z_][A-Za-z0-9_']*)")


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
                    for k in ('coq', 'constructor'):
                        if isinstance(d, dict) and isinstance(d.get(k), str):
                            _NAMES.add(d[k])
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
                        _THEOREMS.add(d.get('coq'))
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
                    if TERM_TACTICS.match(tac):
                        for m in IDENT.finditer(tac):
                            h = m.group(1)
                            if h in theorems and h not in used and h not in made:
                                used.append(h)
                # the hypotheses where they are applied (the head of the term)
                for m in USING.finditer(u.get('by', '')):
                    h = m.group(1)
                    if h in made and h not in cites and made[h][0] not in since:
                        # not "since A, A": a hypothesis that is what the step
                        # concludes is cited by its label, however far
                        if i - last[h] > FAR and plain(made[h][0]) != plain(u.get('prop', '')):
                            since.append(made[h][0])
                        else:
                            cites.append(h)
                        last[h] = i
                    elif h in names and h not in used and h not in made:
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


def tactic_proofs(mod):
    """The proof texts of a module's tactic proofs, all steps and shortened
    (Informath's scripts/proof_shortening.py), and the rules applied."""
    units_file = os.path.join(OUT, mod + '-tactic-units.json')
    if not os.path.exists(os.path.join(OUT, mod + '-tactics.json')):
        # no tactic proofs (agda-export.py): only the macros of the symbol table,
        # from a run on no proofs, which numbers them as the readings do
        empty = os.path.join(TMP, 'no-units.json')
        open(empty, 'w').write('[]')
        _, macros = proof_texts([], empty)
        return {}, {}, macros, {}
    subprocess.run([sys.executable, os.path.join(HERE, 'rocq-tactic-units.py'), mod, OUT], check=True,
                   capture_output=True)
    units = with_uses(without_implicit_variables(json.load(open(units_file))))
    json.dump(units, open(units_file, 'w'), ensure_ascii=False, indent=1)
    full, macros = proof_texts(units, units_file)
    short_units, counts = [], {}
    for p in units:
        steps, c = proof_shortening.shorten(drop_side_conditions(p['steps']))
        short_units.append(dict(p, steps=steps, rules=c))
        for k, v in c.items():
            counts[k] = counts.get(k, 0) + v
    with_uses(forward_verifications(short_units))
    short_file = os.path.join(OUT, mod + '-tactic-units-short.json')
    json.dump(short_units, open(short_file, 'w'), ensure_ascii=False, indent=1)
    short, _ = proof_texts(short_units, short_file)
    return full, short, macros, counts


# the proof texts of proof terms (agda-export.py: out/<M>-proofs.dk), as
# mathlib-informath's term proofs: each theorem by a RunInformath -proof-text
# of its own, with the eliminators and all the statements as its base
TERM_PROOF_FLAGS = ['-proof-indent', '-proof-goals', '-proof-merge-hypos', '-proof-no-cite']


def term_proofs(modules):
    """Dedukti name -> language -> proof text rows, for the proof terms of the modules."""
    files = [os.path.join(OUT, m + '-proofs.dk') for m in modules]
    blocks = []
    for f in files:
        if os.path.exists(f):
            blocks += [b.strip() for b in re.split(r'\n\n(?=def )', open(f).read()) if b.strip().startswith('def ')]
    if not blocks:
        return {}
    base = os.path.join(TMP, 'proof-base.dk')
    # the statements in the reading that the proofs use (out/<M>-prop.dk), if there is one
    parts = [os.path.join(OUT, 'eliminators.dk')] + [
        os.path.join(OUT, m + '-prop.dk') if os.path.exists(os.path.join(OUT, m + '-prop.dk'))
        else os.path.join(OUT, m + '.dk') for m in modules]
    open(base, 'w').write('\n\n'.join(open(f).read() for f in parts if os.path.exists(f)) + '\n')

    def one(b):
        name = b.split()[1]
        f = os.path.join(TMP, 'proof-%s.dk' % re.sub(r'[^A-Za-z0-9_]', '_', name))
        open(f, 'w').write(b + '\n')
        try:
            r = subprocess.run([RUN, '-proof-text'] + TERM_PROOF_FLAGS + ['-languages=' + ','.join(LANGUAGES),
                                '-base=' + base, '-add-symboltables=' + TABLE, f],
                               capture_output=True, text=True, timeout=TIMEOUT)
        except subprocess.TimeoutExpired:
            return name, None
        t = proof_tables(r.stdout, LANGUAGES)
        return name, t[0] if t else None
    out = {}
    with concurrent.futures.ThreadPoolExecutor(8) as ex:
        for name, t in ex.map(one, blocks):
            out[name] = t
    return out


# the statements of the theorems as propositions (agda-export.py: out/<M>-prop.dk,
# where an identification is a proof, Proof (Id A x y)): "Then x + y = y + x",
# besides "Then commutative-add(x, y) is an element of x + y = y + x". The
# declared constant is renamed, so that its symbol table entry (if any) does
# not make it an operation, and labels that the statement mentions are kept
# (-statement-labels): "assume that x = y (p)"
PROP_FLAGS = ['-statement-labels']
# with TYPING_READINGS in the project's config.py, every statement is also read
# with typings as in type theory (-typing): "let p : x = y", "then there is an
# identification c : T" (#TYPENOUN in the symbol table)
TYPING_FLAGS = ['-typing']


def prop_pieces(modules, theorems):
    """[(key, Dedukti)] for the proposition readings of the theorems (Dedukti names)."""
    out = []
    for m in modules:
        f = os.path.join(OUT, m + '-prop.dk')
        if not os.path.exists(f):
            continue
        for block in open(f).read().split('\n\n'):
            line = '\n'.join(l for l in block.strip().split('\n') if not l.startswith('(;')).strip()
            mm = re.match(r"([A-Za-z_][\w']*) :", line)
            if mm and mm.group(1) in theorems:
                out.append(((mm.group(1), 'prop'), mm.group(1) + '__thm' + line[len(mm.group(1)):]))
    return out


def main():
    global TABLE
    # the symbol table with the implicit arguments of the export hidden
    # (out/<M>-hide.dkgf, written by agda-export.py)
    hides = [os.path.join(OUT, m + '-hide.dkgf') for m in MODULES if os.path.exists(os.path.join(OUT, m + '-hide.dkgf'))]
    if hides:
        combined = os.path.join(TMP, 'table.dkgf')
        open(combined, 'w').write(''.join(open(f).read() + '\n' for f in [TABLE] + hides))
        TABLE = combined
    decls, pieces = [], []
    for mod in MODULES:
        blocks = blocks_of(mod)
        for d in json.load(open(os.path.join(OUT, mod + '.json'))):
            d['module'] = mod
            d['dedukti'] = blocks.get(d['coq'], d.get('dedukti'))
            decls.append(d)
            if d.get('kind') == 'structure' and re.match(r'%s : (.* -> )?Structure\.' % re.escape(d.get('dk', '')), d['dedukti'] or ''):
                # a record exported as a structure: read as its definition, the
                # constructor ("A group is ... where ..."), not field by field
                lines = d['dedukti'].split('\n')
                block = '\n'.join(lines[:2])
                pieces += [((d['coq'], 'stmt'), block)]
            else:
                pieces += items_of(d['coq'], d['dedukti'], [d['constructor']] if 'constructor' in d else [])
    print('%d declarations, %d pieces to read with %s' % (len(decls), len(pieces), RUN))
    results = {}
    with concurrent.futures.ThreadPoolExecutor(8) as ex:
        for key, out in ex.map(lambda p: reading(*p), pieces):
            results[key] = out
        if project.get('TYPING_READINGS', False):
            stmts = [(k, dk) for k, dk in pieces if k[1] == 'stmt']
            for key, out in ex.map(lambda p: reading(*p, flags=TYPING_FLAGS), stmts):
                results[(key[0], 'typing')] = out
        theorems = {d['dk'] for d in decls if d.get('proofTerm')}
        for key, out in ex.map(lambda p: reading(*p, flags=PROP_FLAGS), prop_pieces(MODULES, theorems)):
            results[key] = out
    macros = {}
    proofs, shorts, counts = {}, {}, {}
    for mod in MODULES:
        p, s, ms, c = tactic_proofs(mod)
        proofs.update(p)
        shorts.update(s)
        macros.update(ms)
        for k, v in c.items():
            counts[k] = counts.get(k, 0) + v
    base_share = os.path.join(os.environ['INFORMATH_ROOT'], 'share', 'baseconstants.dkgf')
    for l in open(base_share):
        if l.startswith('#MACRO '):
            macros.update(macros_of(l[len('#MACRO '):]))
    for l in open(TABLE):
        if l.startswith('#MACRO '):
            macros.update(macros_of(l[len('#MACRO '):]))
    macros.update(FUNCTION_FORMS)
    for out in results.values():
        for k in out:
            out[k] = [fix_macros(x, macros) for x in out[k]] if isinstance(out[k], list) else fix_macros(out[k], macros)
    for p in list(proofs.values()) + list(shorts.values()):
        for lang in (p or {}):
            for r in p[lang]:
                r[1] = fix_macros(r[1], macros)
    terms = term_proofs(MODULES)
    for t in terms.values():
        for lang in (t or {}):
            for r in t[lang]:
                r[1] = fix_macros(r[1], macros)
    for d in decls:
        if d.get('dk') in terms:
            d['proof'] = terms[d['dk']]
    for d in decls:
        if (d['coq'], 'typing') in results:
            d['typingReadings'] = results[(d['coq'], 'typing')]
    for d in decls:
        if (d.get('dk'), 'prop') in results:
            d['propReadings'] = results[(d['dk'], 'prop')]
    for d in decls:
        n = d['coq']
        d['readings'] = results.get((n, 'stmt'), {})
        rules = sorted(k for k in results if k[0] == n and len(k) == 3)
        if rules:
            d['rules'] = [results[k] for k in rules]
        if n in proofs:
            d['proof'] = proofs[n]
        if shorts.get(n):
            d['proofShort'] = shorts[n]
    index = {'shortening': counts, 'languages': LANGUAGES, 'modules': MODULES, 'macros': macros, 'declarations': decls,
             'informath': subprocess.run(['git', '-C', os.environ['INFORMATH_ROOT'], 'rev-parse', 'HEAD'],
                                         capture_output=True, text=True).stdout.strip()}
    json.dump(index, open(INDEX, 'w'), ensure_ascii=False, indent=1)
    # the statistics in the first of the languages (English by default)
    L = LANGUAGES[0]
    worded = sum(bool(d['readings'].get(L)) for d in decls)
    print('%d declarations -> %s: %d readings (%s), %d formulas, %d proof texts (%d failed)'
          % (len(decls), INDEX, worded, L, sum(bool(d['readings'].get('formula')) for d in decls),
             sum(bool(d.get('proof')) for d in decls), sum(1 for d in decls if 'proof' in d and not d['proof'])))
    lines = lambda key: sum(len(d[key].get(L, [])) for d in decls if d.get(key))
    print('proof lines: %d, shortened %d (%s)' % (lines('proof'), lines('proofShort'),
                                                 ', '.join('%s %d' % kv for kv in counts.items())))
    missing = [d['coq'] for d in decls if not d['readings'].get(L)]
    if missing:
        print('no reading:', ' '.join(missing))
    # theorems with a tactic proof and no proof text
    noproof = [d['coq'] for d in decls if d.get('proofSource') and not d.get('proof')]
    if noproof:
        print('%d tactic proofs without a proof text: %s' % (len(noproof), ' '.join(noproof[:20])))
    if FAILURES or noproof:
        sys.exit(1)


if __name__ == '__main__':
    main()
