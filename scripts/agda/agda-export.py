#!/usr/bin/env python3
"""Export the definitions of Agda modules to Informath's Dedukti dialect, as
rocq-export.py does for Rocq.

    cd <project>          # its config.py (MODULES, AGDA_ROOT, AGDA_TABLE, TABLE), out/
    python3 $INFORMATH/scripts/agda/agda-export.py      # -> out/<M>.dk, .json, -hide.dkgf
    python3 $INFORMATH/scripts/agda/agda-export.py File.agda -out=<dir>   # one file, without a project

Agda type-checks each module and the Agda backend agda-export (Main.hs in
this directory, built with cabal) writes its definitions in Agda's internal
syntax as JSON (out/agda/<M>.json): every implicit argument there, binder
names kept.  agdaterm.py translates them: a type is a Set, its elements are
Elem A, universe levels are dropped, a definition by pattern matching is
rewrite rules (one for each clause), a single clause of variables a
definiens.  The implicit arguments are kept and listed in out/<M>-hide.dkgf
(#HIDE), so that the readings show what the source writes.

out/<M>.json lists the declarations with their kind, source lines, source
text, the comment before them (-- Definition 5.1.1) and their section
(-- Section 5.2 ...), in the format of rocq-export.py, which rocq-index.py
reads (the source name is under "coq").
"""
import glob, json, os, re, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, '..', 'rocq')]
import project                      # the project's config.py (scripts/rocq/project.py)
import agdaterm, agdaproof
from agdaterm import Translator, Names, paren, short

OUT = os.path.join(project.DIR, 'out')
AGDA_ROOT = os.path.expanduser(project.get('AGDA_ROOT', '.'))
MODULES = project.get('MODULES', [])
# the type families read as propositions (Curry-Howard): Rijke's identity types
PROPOSITIONS = project.get('PROPOSITIONS', [])
# the names of variables by their types (agdaterm.Translator.VARIABLE_NAMES), as the text names them
VARIABLE_NAMES = project.get('VARIABLE_NAMES', {})
# the comments naming the numbered items of the source (-- Definition 5.1.1):
# a definition belongs to the last one above it in its section
ITEM = re.compile(r'--\s*(Definition|Lemma|Proposition|Theorem|Corollary|Remark|Example|Exercise|Construction)'
                  r'\s+(\d+(?:\.\d+)*\.?)')
# generated definitions (with-functions, pattern lambdas), not shown
GENERATED = re.compile(r'\.(with-\d+|-\d+|\.extendedlambda\d*|\.absurdlambda)|\.(extendedlambda|absurdlambda)\d*$')


ND_RULES = '''ifI : (A : Prop) -> (B : Prop) -> (b : ((x : Proof A) -> Proof B)) -> Proof (if A B).
def ifE : (A : Prop) -> (B : Prop) -> Proof (if A B) -> Proof A -> Proof B.
forallI : (A : Set) -> (B : ((x : Elem A) -> Prop)) -> (b : ((x : Elem A) -> Proof (B x))) -> Proof (forall A (x => B x)).
def forallE : (A : Set) -> (B : ((x : Elem A) -> Prop)) -> Proof (forall A (x => B x)) -> (a : Elem A) -> Proof (B a).

'''


def backend():
    """The agda-export executable, built in this directory: the newest one in
    dist-newstyle (cabal list-bin depends on the GHC on the PATH, which stack
    exec changes), else what cabal list-bin says."""
    built = glob.glob(os.path.join(HERE, 'dist-newstyle', 'build', '*', '*', 'agda-export-*', 'x',
                                   'agda-export', 'build', 'agda-export', 'agda-export'))
    if built:
        return max(built, key=os.path.getmtime)
    r = subprocess.run(['cabal', 'list-bin', 'agda-export'], cwd=HERE, capture_output=True, text=True)
    exe = r.stdout.strip()
    if r.returncode or not os.path.exists(exe):
        sys.exit('agda-export is not built: cd %s && cabal build agda-export' % HERE)
    return exe


def run_backend(module):
    f = module.replace('.', '/') + '.agda'
    r = subprocess.run([backend(), '--agda-export', '--agda-export-dir=' + os.path.join(OUT, 'agda'), f],
                       cwd=AGDA_ROOT, capture_output=True, text=True)
    if r.returncode:
        sys.exit('agda-export failed on %s:\n%s' % (f, (r.stdout + r.stderr)[-2000:]))


def load_all():
    env, order = {}, {}
    for f in glob.glob(os.path.join(OUT, 'agda', '*.json')):
        mod = os.path.basename(f)[:-len('.json')]
        defs = [json.loads(l) for l in open(f) if l.strip()]
        order[mod] = defs
        for d in defs:
            d['module'] = mod
            env[d['name']] = d
    return env, order


# ------------------------------------------------------------------ the source

def source_info(module, defs):
    """For each definition: its source text, the comment before it, its section."""
    path = os.path.join(AGDA_ROOT, module.replace('.', '/') + '.agda')
    lines = open(path).read().split('\n')
    starts = sorted({d['line'] for d in defs if d['line']})
    info = {}
    for d in defs:
        i = d['line'] - 1
        if i < 0:
            continue
        nxt = min([s for s in starts if s > d['line']] + [len(lines) + 1]) - 1
        # a signature may come after the comment lines, the clauses after it
        end = nxt
        while end > i + 1 and (not lines[end - 1].strip() or lines[end - 1].lstrip().startswith('--')):
            end -= 1
        comment, k = [], i - 1
        while k >= 0 and lines[k].lstrip().startswith('--') and not set(lines[k].strip()) <= set('-'):
            comment.insert(0, lines[k].strip().lstrip('-').strip())
            k -= 1
        section, item = '', ''
        for k in range(i, -1, -1):
            l = lines[k].strip()
            m = ITEM.match(l)
            if m and not item:
                item = '%s %s' % (m.group(1), m.group(2).rstrip('.'))
            m = re.match(r'--\s*(Section\s+\S+.*)$', l)
            if m:
                section = m.group(1).strip()
                break
        info[d['name']] = {'source': '\n'.join(lines[i:end]).rstrip(), 'comment': ' '.join(comment),
                           'section': section, 'item': item, 'endLine': end}
    return info


# ------------------------------------------------------------------ declarations

def is_generated(d):
    return bool(GENERATED.search(d['name'])) or d.get('extlam') or not d.get('line')


def clause_ctx(tr, clause):
    """The context of a clause's telescope: variable -> level | type | elem."""
    ctx = {}
    for x, t in clause['tel']:
        v = tr.var(x, ctx)
        ctx[v] = 'level' if tr.is_level_type(t) else ('type' if tr.is_kind(t) else 'elem')
    return ctx


def pattern_vars(p):
    """The variables of a pattern, those of its dot patterns included."""
    out = set()
    def go(q):
        if isinstance(q, list):
            if len(q) >= 2 and q[0] == 'var' and isinstance(q[1], str):
                out.add(q[1])
            # a tagged node ["con", c, args]: its parts after the tag; a list of arguments: all
            for x in (q[1:] if q and isinstance(q[0], str) else q):
                go(x)
    go(p)
    return out


def complete(c):
    """A clause with the patterns of the leading hidden arguments that Agda
    leaves out of some clauses (pr1 (pair a _) = a, left-unit = refl): the
    variables of its telescope that no pattern mentions, in their order, as
    hidden variable patterns before the others."""
    seen = set()
    for h, p in c['pats']:
        seen |= pattern_vars(p)
    missing = [x for x, t in c['tel'] if x not in seen]
    return dict(c, pats=[['h', ['var', x]] for x in missing] + c['pats'])


# Dedukti name -> the variable that its proof does induction on (agdaproof.py)
INDUCTION_ON = {}


def function(tr, d, dn, ty, env=None, proofs=None):
    cs = [complete(c) for c in d.get('clauses', []) if c.get('body') is not None]
    # a theorem (its type a proposition): its proof term, for the proof texts,
    # in out/<M>-proofs.dk; the module has its statement
    if proofs is not None and ty.rstrip().split(' -> ')[-1].startswith('Proof') and cs:
        c = cs[0]
        if len(cs) == 1 and all(p[1][0] == 'var' for p in c['pats']):
            ctx = clause_ctx(tr, c)
            xs = [v for v, k in ctx.items() if k != 'level']
            term = ''.join('%s => ' % x for x in xs) + tr.term(c['body'], ctx)
        else:
            tr.induction_on = None
            term = agdaproof.proof_term(tr, tr.names, d, env, cs) or \
                agdaproof.case_tree_proof(tr, tr.names, d, env, cs)
            if term and tr.induction_on:
                INDUCTION_ON[dn] = tr.induction_on
        if term:
            proofs.append('def %s : %s :=\n  %s.' % (dn, ty, term))
            return '%s : %s.' % (dn, ty)
    if any(p[1][0] == 'proj' for c in d.get('clauses', []) for p in c['pats']):
        return '%s : %s.' % (dn, ty)          # a projection: its type
    if not cs:
        return '%s : %s.' % (dn, ty)          # no clauses with a body (absurd)
    # one clause of variables: a definiens x => ... => body
    if len(cs) == 1 and all(p[1][0] == 'var' for p in cs[0]['pats']):
        c = cs[0]
        ctx = clause_ctx(tr, c)
        # the clause's variables are its telescope (its patterns may leave
        # out the hidden ones: left-unit = refl)
        xs = [v for v, k in ctx.items() if k != 'level']
        return 'def %s : %s :=\n  %s%s.' % (dn, ty, ''.join('%s => ' % x for x in xs), tr.term(c['body'], ctx))
    rules = []
    for c in cs:
        ctx = clause_ctx(tr, c)
        pats = [paren(tr.pattern(p, ctx)) for h, p in c['pats'] if not tr.level_pattern(p, ctx)]
        vs = [v for v, k in ctx.items() if k != 'level']
        rules.append('[%s] %s %s --> %s' % (', '.join(vs), dn, ' '.join(pats), tr.term(c['body'], ctx)))
    return 'def %s : %s.\n%s.' % (dn, ty, '\n'.join(rules))


def export(module, env, defs, names, propositions=()):
    tr = Translator(env, names, propositions, VARIABLE_NAMES)
    info = source_info(module, defs)
    decls, blocks, hides, proofs = [], [], [], []
    shown = [d for d in defs if not is_generated(d) and d['kind'] != 'constructor']
    for d in sorted(shown, key=lambda d: d['line']):
        dn = names(d['name'])
        tr.declaring = short(d['name'])
        try:
            ty, hidden, _ = tr.decl_type(d['type'])
            if hidden:
                hides.append('#HIDE %s %s' % (dn, ' '.join(map(str, hidden))))
            k = d['kind']
            if k == 'function':
                block = function(tr, d, dn, ty, env, proofs if propositions else None)
                if block.endswith('.') and ':=' not in block and '-->' not in block and \
                        (proofs and proofs[-1].startswith('def %s :' % dn)):
                    block = block   # the statement; the proof is in out/<M>-proofs.dk
                # a function into a universe defines a type (family); any other
                # constructs an element of a type, as the book's propositions do;
                # the comment before it (-- Lemma 5.2.1) says which it is
                kind = 'definition' if tr.is_kind(d['type']) else 'construction'
                m = re.match(r'(Definition|Lemma|Proposition|Theorem|Corollary|Remark|Example|Exercise)\b',
                             info.get(d['name'], {}).get('item', ''))
                if m:
                    kind = m.group(1).lower()
            elif k in ('data', 'record'):
                lines = ['%s : %s.' % (dn, ty)]
                cons = d.get('constructors') or [d.get('constructor')]
                for c in cons:
                    cd = env.get(c)
                    if cd:
                        tr.declaring = short(c)
                        cty, chidden, _ = tr.decl_type(cd['type'], params=cd.get('pars', 0))
                        cn = names(c)
                        lines.append('%s : %s.' % (cn, cty))
                        if chidden:
                            hides.append('#HIDE %s %s' % (cn, ' '.join(map(str, chidden))))
                block = '\n'.join(lines)
                kind = 'inductive' if k == 'data' else 'structure'
            else:
                block = '%s : %s.' % (dn, ty)
                kind = 'axiom' if k == 'postulate' else 'definition'
        except Exception as e:
            block = '(; %s: not translated: %s ;)' % (dn, str(e).replace(';)', ''))
            kind = d['kind']
            print('%s: %s' % (d['name'], e), file=sys.stderr)
        blocks.append('(; //AGDA:%s ;)\n%s' % (short(d['name']), block))
        entry = dict({'coq': short(d['name']), 'agda': d['name'], 'dk': dn, 'kind': kind,
                      'line': d['line'], 'dedukti': block}, **info.get(d['name'], {}))
        if proofs and proofs[-1].startswith('def %s :' % dn):
            entry['proofTerm'] = proofs[-1]
            if dn in INDUCTION_ON:
                entry['inductionOn'] = INDUCTION_ON[dn]
        decls.append(entry)
    return decls, blocks, hides, proofs


def hidden_positions(env, names):
    """Dedukti name -> the positions (1-based, levels not counted) of its
    hidden arguments, for every definition and constructor of the export."""
    tr = Translator(env, names, (), VARIABLE_NAMES)
    out = {}
    for d in env.values():
        try:
            _, hidden, _ = tr.decl_type(d['type'], params=d.get('pars', 0) if d['kind'] == 'constructor' else 0)
        except Exception:
            continue
        out[names(d['name'])] = (set(hidden), arity_of(d, tr))
    return out


def arity_of(d, tr):
    """The number of arguments a constant's applications have (levels and a
    constructor's parameters not counted)."""
    t, n, k = d['type'], 0, 0
    params = d.get('pars', 0) if d['kind'] == 'constructor' else 0
    while t[0] == 'pi':
        k += 1
        if not tr.is_level_type(t[3]) and k > params:
            n += 1
        t = t[4]
    return n


def latex_name(qname):
    """An Agda name as a LaTeX operator: left-unit-law-add-ℕ is
    \\mathsf{left\\text{-}unit\\text{-}law\\text{-}add}_{\\mathbb{N}}."""
    s = short(qname).strip('_')
    sub = ''
    m = re.search(r'-([ℕℤℚℝ])$', s)
    if m:
        s, sub = s[:m.start()], '_{\\mathbb{%s}}' % {'ℕ': 'N', 'ℤ': 'Z', 'ℚ': 'Q', 'ℝ': 'R'}[m.group(1)]
    s = s.replace('\\', '').replace('{', '').replace('}', '').replace('_', '\\_')
    s = re.sub(r'([ℕℤℚℝ])', lambda m: '}\\mathbb{%s}\\mathsf{' % {'ℕ': 'N', 'ℤ': 'Z', 'ℚ': 'Q', 'ℝ': 'R'}[m.group(1)], s)
    return '\\mathsf{%s}%s' % (s.replace('-', '\\text{-}'), sub)


def full_table(source, hides, target, defaults):
    """The project's symbol table (AGDA_TABLE), whose #k count the explicit
    arguments (#{k} all of them), with the #k renumbered to count all the arguments, as
    Informath's profiles do: concat : $#1 \\cdot #2$, with four hidden
    arguments before p and q, is $#5 \\cdot #6$.  Informath's #HIDE does
    not reach the head of a declaration (the definiendum, built from its
    hypotheses), a profile does.  Returns the constants with entries."""
    out, entries = [], set()
    # #VERBAL constants (read in words outside formulas): one without a
    # symbolic form gets the generated one, for use inside formulas
    verbal = {c for l in open(source) if l.startswith('#VERBAL') for c in l.split()[1:]}

    def generated(c):
        qname = defaults.get(c)
        if qname is None or c not in hides:
            return None
        h, n = hides[c]
        explicit = [i for i in range(1, n + 1) if i not in h]
        args = '(%s)' % ', '.join('#%d' % i for i in explicit) if explicit else ''
        return '$%s%s$' % (latex_name(qname), args)
    for l in open(source):
        m = re.match(r"^([A-Za-z_][A-Za-z0-9_']*) : (.*)$", l.rstrip('\n'))
        if not m or l.startswith('#'):
            out.append(l.rstrip('\n'))
            continue
        c, forms = m.group(1), m.group(2)
        entries.add(c)
        h = sorted(hides.get(c, (set(), 0))[0])
        if h and '#' in forms:
            explicit = [i for i in range(1, 100) if i not in h]
            forms = re.sub(r'#(\d+)', lambda mm: '#%d' % explicit[int(mm.group(1)) - 1], forms)
            # a profile [1,2] of a tree form, likewise
            forms = re.sub(r'\[(\d+(?:,\d+)*)\]', lambda mm: '[%s]' % ','.join(
                str(explicit[int(k) - 1]) for k in mm.group(1).split(',')), forms)
        # #{k}: the k'th argument counting all of them, implicit ones too (the
        # type A of center : {A} -> is-contr A -> A, "the center of contraction of #{1}")
        forms = re.sub(r'#\{(\d+)\}', r'#\1', forms)
        if c in verbal and '$' not in forms and generated(c):
            forms += ' | ' + generated(c)
        out.append('%s : %s' % (c, forms))
    # the constants without an entry: their Agda name applied to their explicit arguments
    out.append('\n# generated for the constants without an entry: the Agda name, the explicit arguments')
    for c, qname in sorted(defaults.items()):
        if c in entries or c not in hides:
            continue
        out.append('%s : %s' % (c, generated(c)))
        entries.add(c)
    open(target, 'w').write('# generated by agda-export.py from %s: #k count all the arguments\n' % os.path.basename(source)
                            + '\n'.join(out) + '\n')
    return entries


def single_file(path):
    """The library root and the module of an Agda file given on the command
    line: the root is the file's directory less the module's qualifiers
    (module A.B.C where, in A/B/C.agda)."""
    path = os.path.abspath(path)
    m = re.search(r'^module\s+(\S+)\s+where', open(path).read(), re.M)
    module = m.group(1) if m else os.path.basename(path)[:-len('.agda')]
    root = os.path.dirname(path)
    for _ in module.split('.')[:-1]:
        root = os.path.dirname(root)
    return root, module


def main():
    global AGDA_ROOT, MODULES, OUT, PROPOSITIONS
    # a single file, without a project: agda-export.py File.agda [-out=<dir>]
    files = [a for a in sys.argv[1:] if not a.startswith('-')]
    for a in sys.argv[1:]:
        if a.startswith('-out='):
            OUT = os.path.abspath(a[len('-out='):])
    if files:
        if len(files) != 1 or not files[0].endswith('.agda'):
            sys.exit(__doc__)
        AGDA_ROOT, module = single_file(files[0])
        MODULES = [module]
        # without a project's PROPOSITIONS, the identity types are propositions,
        # so that their proofs get proof texts
        if not hasattr(project.config, 'PROPOSITIONS'):
            PROPOSITIONS = ['_≡_', 'Id', '_＝_']
    os.makedirs(os.path.join(OUT, 'agda'), exist_ok=True)
    for m in MODULES:
        run_backend(m)
    env, order = load_all()
    names = Names()
    # names are given in the order of the modules, so that they are stable
    for mod in sorted(order):
        for d in order[mod]:
            names(d['name'])
    # the symbol table with all arguments counted, and the constants whose
    # hidden arguments it handles (the others get #HIDE)
    entries = set()
    if project.get('AGDA_TABLE', None):
        defaults = {names(q): q for q in env if not is_generated(env[q]) and not q.startswith('Agda.')}
        entries = full_table(os.path.join(project.DIR, project.get('AGDA_TABLE', '')),
                             hidden_positions(env, names), os.path.join(project.DIR, project.get('TABLE', '')), defaults)
    for m in MODULES:
        # the statements as Rijke states them: an identification is an element
        # of an identity type, named (p : x = y), for the readings
        decls, blocks, hides, _ = export(m, env, order.get(m, []), names)
        hides = [h for h in hides if h.split()[1] not in entries]
        proofs = []
        if PROPOSITIONS:
            # the Curry-Howard reading of the PROPOSITIONS (Proof (Id A x y)), for
            # the proof texts: the statements (out/<M>-prop.dk) and the proof terms
            pdecls, pblocks, _, proofs = export(m, env, order.get(m, []), names, PROPOSITIONS)
            terms = {pd['dk']: pd['proofTerm'] for pd in pdecls if 'proofTerm' in pd}
            for d in decls:
                if d['dk'] in terms:
                    d['proofTerm'] = terms[d['dk']]
                    if d['dk'] in INDUCTION_ON:
                        d['inductionOn'] = INDUCTION_ON[d['dk']]
            open(os.path.join(OUT, m + '-prop.dk'), 'w').write(
                '(; %s: the statements with the PROPOSITIONS as propositions (agda-export.py) ;)\n\n' % m
                + '\n\n'.join(pblocks) + '\n')
        open(os.path.join(OUT, m + '-proofs.dk'), 'w').write(
            '(; %s: the proof terms of its theorems (agda-export.py) ;)\n\n' % m + '\n\n'.join(proofs) + '\n')
        open(os.path.join(OUT, m + '.dk'), 'w').write(
            '(; %s: declarations exported from Agda by agda-export.py ;)\n\n' % m + '\n\n'.join(blocks) + '\n')
        json.dump(decls, open(os.path.join(OUT, m + '.json'), 'w'), ensure_ascii=False, indent=1)
        open(os.path.join(OUT, m + '-hide.dkgf'), 'w').write(
            '# %s: the implicit arguments, hidden in the readings (agda-export.py)\n' % m + '\n'.join(hides) + '\n')
        print('%s: %d declarations, %d proof terms -> %s.dk, .json, -hide.dkgf, -proofs.dk'
              % (m, len(decls), len(proofs), os.path.join(OUT, m)))
    # the eliminators into propositions of the data types, for the proof texts
    if PROPOSITIONS:
        tr = Translator(env, names, PROPOSITIONS, VARIABLE_NAMES)
        elims = []
        for q, d in env.items():
            if d['kind'] == 'data':
                tr.declaring = None
                for kind in agdaproof.elim_kinds(tr, d, env):
                    try:
                        e = agdaproof.eliminator(tr, names, d, env, kind)
                    except Exception as ex:
                        e = None
                    if e:
                        elims.append(e)
        open(os.path.join(OUT, 'eliminators.dk'), 'w').write(
            '(; the eliminators into propositions of the data types (agdaproof.py), and the rules\n'
            '   of natural deduction that generalized motives use ;)\n\n' + ND_RULES + '\n\n'.join(elims) + '\n')


if __name__ == '__main__':
    main()
