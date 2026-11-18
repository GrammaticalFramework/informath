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
    texts = proof_tables(r.stdout, LANGUAGES)
    out = {}
    for p, t in zip(units, texts):
        if t:
            for lang in t:
                t[lang] = [[n, text, 'TACTIC:' + u['by'].replace(' ;; ', '. ')]
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
    units = json.load(open(units_file))
    full, macros = proof_texts(units, units_file)
    short_units, counts = [], {}
    for p in units:
        steps, c = proof_shortening.shorten(drop_side_conditions(p['steps']))
        short_units.append(dict(p, steps=steps, rules=c))
        for k, v in c.items():
            counts[k] = counts.get(k, 0) + v
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
            if d.get('kind') == 'structure' and '%s : Structure.' % d.get('dk') in (d['dedukti'] or ''):
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
    worded = sum(bool(d['readings'].get('Eng')) for d in decls)
    print('%d declarations -> %s: %d English readings, %d formulas, %d proof texts (%d failed)'
          % (len(decls), INDEX, worded, sum(bool(d['readings'].get('formula')) for d in decls),
             sum(bool(d.get('proof')) for d in decls), sum(1 for d in decls if 'proof' in d and not d['proof'])))
    lines = lambda key: sum(len(d[key]['Eng']) for d in decls if d.get(key))
    print('proof lines: %d, shortened %d (%s)' % (lines('proof'), lines('proofShort'),
                                                 ', '.join('%s %d' % kv for kv in counts.items())))
    missing = [d['coq'] for d in decls if not d['readings'].get('Eng')]
    if missing:
        print('no reading:', ' '.join(missing))


if __name__ == '__main__':
    main()
