#!/usr/bin/env python3
"""Export a Coq file's statements, definitions and tactic proofs to Informath's
Dedukti dialect, as mathlib-informath's Export.lean does for Lean.

    cd <project>         # its config.py, coq/, out/
    python3 $INFORMATH/scripts/rocq/rocq-export.py coq/M.v    # -> out/M.dk, .json, -tactics.json
    python3 $INFORMATH/scripts/rocq/rocq-export.py M.v -out=<dir>   # without a project

The file is compiled first if its .vo is missing or older than it.
The file is compiled in coq/ (rocq compile); Rocq prints its declarations
fully elaborated (Set Printing All): Check for a theorem, Print for a
definition, an inductive type or a record. rocqterm.py parses them and
translates them. A Fixpoint becomes rewrite rules, one for each case.

The tactic proofs are replayed with Show before and after each tactic: the
goals, with their hypotheses and targets in Dedukti, go to
out/<M>-tactics.json in the format of mathlib-informath's --tactics export,
which rocq-tactic-units.py turns into proof units for RunInformath -tactic-proof.

The file is loaded with the load paths of a _CoqProject next to it (-Q and
-R lines), else with -Q . "".  Declarations of other kinds than those
exported (Variable, Context, Class, Module, ...) are counted and reported.
"""
import json, os, re, subprocess, sys
import rocqterm as coqterm
from rocqterm import parse, Env, Translator, T, Var, App, subst, head, short, paren, pattern_vars, free_vars

import project
OUT = os.path.join(project.DIR, 'out')
PREAMBLE = 'From Stdlib Require Import List Arith Lia Bool.\nImport ListNotations.\n'
SETTINGS = 'Set Printing All.\nSet Printing Width 100000.\nUnset Printing Notations.\n'
SEP = 'Check tt.'
SEP_OUT = re.compile(r'^tt\n\s+: unit\n', re.M)
# the declarations exported, with attributes and Local/Global/Program before them
PREFIX = r'(?:(?:#\[[^\]]*\]|Local|Global|Program|Polymorphic|Monomorphic)\s+)*'
DECL = re.compile(r'^' + PREFIX + r'(Lemma|Theorem|Corollary|Proposition|Fact|Remark|Example|Definition|Instance|Fixpoint'
                  r'|Record|Structure|Inductive|CoInductive|Axiom|Parameter|Conjecture)\s+([A-Za-z_][A-Za-z0-9_\']*)', re.M)
THEOREM_KINDS = ('Lemma', 'Theorem', 'Corollary', 'Proposition', 'Fact', 'Remark')
AXIOM_KINDS = ('Axiom', 'Parameter', 'Conjecture')
# the declarations not exported, reported
OTHER = re.compile(r'^' + PREFIX + r'(Variables?|Hypothes[ie]s|Context|Let|Class|Module|Equations|Theorem\s+\w+\s+with'
                   r'|Fixpoint\s+\w+.*\bwith\b|CoFixpoint|Canonical|Coercion)\b', re.M)


# ------------------------------------------------------------------ the source

def strip_comments(s):
    """s with its comments blanked out, keeping the offsets."""
    out, i, depth = list(s), 0, 0
    while i < len(s):
        if s.startswith('(*', i):
            depth += 1
            out[i] = out[i + 1] = ' '
            i += 2
            continue
        if depth and s.startswith('*)', i):
            depth -= 1
            out[i] = out[i + 1] = ' '
            i += 2
            continue
        if depth and s[i] != '\n':
            out[i] = ' '
        i += 1
    return ''.join(out)


def sentence_end(s, i):
    """The offset after the sentence starting at i: its period followed by white space."""
    depth = 0
    while i < len(s):
        c = s[i]
        if c in '([{':
            depth += 1
        elif c in ')]}':
            depth -= 1
        elif c == '.' and depth <= 0 and (i + 1 == len(s) or s[i + 1].isspace()):
            return i + 1
        i += 1
    return len(s)


def line_of(s, i):
    return s.count('\n', 0, i) + 1


def scan(src):
    """The declarations of a source in order: kind, name, lines, source text,
    the comment before it, its section, and for a theorem its tactic script."""
    code = strip_comments(src)
    items = []
    section = ''
    sections = [(m.start(), m.group(1).strip()) for m in
                re.finditer(r'\(\* -{20,} \*\)\n\(\*\s*(\d+\..*?)\s*\*\)\n\(\* -{20,} \*\)', src)]
    for m in DECL.finditer(code):
        kind, name = m.group(1), m.group(2)
        # Example and Instance are theorems if proved by tactics, else definitions;
        # Structure is Record, CoInductive like Inductive
        if kind in ('Example', 'Instance'):
            head_ = code[m.start():sentence_end(code, m.start())]
            kind = 'Definition' if ':=' in head_ else 'Lemma'
        kind = {'Structure': 'Record', 'CoInductive': 'Inductive'}.get(kind, kind)
        start = m.start()
        end = sentence_end(code, start)
        item = {'kind': kind, 'coq': name, 'line': line_of(src, start)}
        secs = [t for p, t in sections if p < start]
        item['section'] = secs[-1] if secs else ''
        # the comment right before it
        before = src[:start].rstrip()
        if before.endswith('*)'):
            cs = before.rfind('(*')
            # a block of comment lines, each (* ... *)
            k = cs
            while True:
                prev = src[:k].rstrip()
                if prev.endswith('*)') and not prev.endswith('-- *)') and '\n' in src[:k] and \
                        src[:k].rstrip('\n').endswith('*)') and src[k - 1] == '\n' and \
                        not re.search(r'-{20,}', prev[prev.rfind('(*'):]):
                    k2 = prev.rfind('(*')
                    if src[k2:k].count('\n') <= 1:
                        k = k2
                        continue
                break
            text = src[k:len(before)]
            if not re.search(r'-{20,}|={20,}', text):
                item['comment'] = re.sub(r'\s+', ' ', re.sub(r'\(\*|\*\)', ' ', text)).strip()
        if kind in THEOREM_KINDS:
            stmt_end = end
            item['source'] = src[start:stmt_end].strip()
            # the proof right after the statement: Proof. ... Qed. (or Defined.,
            # Admitted.), or Admitted. alone for a theorem stated without proof
            am = re.compile(r'\s*Admitted\.').match(code, stmt_end)
            pm = re.compile(r'\s*(?:Proof\.)').match(code, stmt_end)
            qm = re.compile(r'(Qed|Defined|Admitted)\.').search(code, pm.end()) if pm else None
            if am:
                item['admitted'] = True
                item['endLine'] = line_of(src, am.end())
            elif pm and qm:
                if qm.group(1) == 'Admitted':
                    item['admitted'] = True
                item['script'] = (pm.end(), qm.start())
                item['proofSource'] = src[pm.start():qm.end()].strip()
                item['endLine'] = line_of(src, qm.end())
            else:
                item['endLine'] = line_of(src, end)
        else:
            item['source'] = src[start:end].strip()
            item['endLine'] = line_of(src, end)
            if kind == 'Inductive':
                item['constructors'] = re.findall(r'\|\s*([A-Za-z_][A-Za-z0-9_\']*)\s*:', code[start:end])
            if kind == 'Record':
                body = code[start:end]
                m2 = re.search(r':=\s*([A-Za-z_][A-Za-z0-9_\']*)\s*\{(.*)\}', body, re.S)
                item['constructor'] = m2.group(1)
                item['fields'] = re.findall(r'(?:^|;)\s*([A-Za-z_][A-Za-z0-9_\']*)\s*:', m2.group(2))
        items.append(item)
    return items


# ------------------------------------------------------------------ Rocq

def loadpath(vdir):
    """The load path options of a _CoqProject in vdir (its -Q and -R lines),
    else -Q . ""."""
    f = os.path.join(vdir, '_CoqProject')
    if not os.path.exists(f):
        return ['-Q', '.', '']
    out = []
    for l in open(f):
        ws = l.split('#')[0].split()
        if len(ws) == 3 and ws[0] in ('-Q', '-R'):
            out += ws
    return out or ['-Q', '.', '']


def rocq(vfile_dir, text):
    path = os.path.abspath(os.path.join(vfile_dir, 'query_tmp_%d.v' % os.getpid()))
    open(path, 'w').write(text)
    r = subprocess.run(['rocq', 'repl'] + loadpath(vfile_dir) + ['-batch', '-l', path], cwd=vfile_dir,
                       capture_output=True, text=True)
    os.remove(path)
    if 'Error' in r.stdout + r.stderr:
        sys.stderr.write((r.stdout + r.stderr)[-3000:])
        raise SystemExit('rocq failed')
    return r.stdout


def query(vdir, mod, items):
    cmds = []
    for it in items:
        n = it['coq']
        if it['kind'] in THEOREM_KINDS + AXIOM_KINDS:
            cmds.append(('check', n, 'Check @%s.' % n))
        elif it['kind'] in ('Definition', 'Fixpoint'):
            cmds.append(('print', n, 'Print %s.' % n))
        elif it['kind'] == 'Inductive':
            cmds.append(('check', n, 'Check @%s.' % n))
            for c in it['constructors']:
                cmds.append(('check', c, 'Check @%s.' % c))
        elif it['kind'] == 'Record':
            cmds.append(('check', n, 'Check @%s.' % n))
            for c in [it['constructor']] + it['fields']:
                cmds.append(('check', c, 'Check @%s.' % c))
            cmds.append(('print', n, 'Print %s.' % n))
    text = PREAMBLE + 'Require Import %s.\n' % ' '.join(IMPORTS + [mod]) + SETTINGS + SEP + '\n' + \
        '\n'.join('%s\n%s' % (c, SEP) for _, _, c in cmds) + '\n'
    out = rocq(vdir, text)
    chunks = SEP_OUT.split(out)[1:]
    res = {}
    for (k, n, _), ch in zip(cmds, chunks):
        res[(k, n)] = ch
    return res


def parse_check(out):
    """name\n     : type -> the type."""
    m = re.match(r'@?\S+\s*\n\s*:\s*(.*)$', out.strip(), re.S)
    return parse(' '.join(m.group(1).split('\n')))


def parse_print(out):
    """name = body\n     : type\n\nArguments ... -> (body, type, implicit argument names)."""
    text = out.split('\nArguments')[0]
    args = re.findall(r'^Arguments .*$', out, re.M)
    implicit = []
    for a in args:
        implicit += re.findall(r'\{(\w+)\}', a)
    m = re.match(r'(\S+) = (.*?)\n\s+: (.*)$', text.strip(), re.S)
    body = ' '.join(l.strip() for l in m.group(2).split('\n'))
    ty = ' '.join(l.strip() for l in m.group(3).split('\n'))
    return parse(body), parse(ty), implicit


# ------------------------------------------------------------------ translation

def simplify(t):
    """negb on literals, after a case split."""
    if t.kind == 'app':
        f, args = t.f, [simplify(a) for a in t.args]
        if f.kind == 'var' and short(f.name) == 'negb' and len(args) == 1 and args[0].kind == 'var' \
                and args[0].name in ('true', 'false'):
            return Var('false' if args[0].name == 'true' else 'true')
        return App(simplify(f), args)
    if t.kind in ('pi', 'lam'):
        return T(t.kind, name=t.name, dom=t.dom, body=simplify(t.body))
    if t.kind == 'match':
        return T('match', scrut=simplify(t.scrut), asn=t.asn, branches=[(p, simplify(b)) for p, b in t.branches])
    return t


def ctor_arg_types(tr, ctor, scrut_ty, nargs):
    """The types of the arguments of a constructor pattern, its parameters
    taken from the type of the matched variable."""
    ty = tr.env.typeof_const(ctor)
    _, params = head(tr.whnf(scrut_ty)) if scrut_ty is not None else (None, [])
    out = []
    k = 0
    while ty is not None and ty.kind == 'pi':
        if k < len(params):
            ty = subst(ty.body, {ty.name: params[k]})
        else:
            out.append(ty.dom)
            ty = ty.body
        k += 1
    return out


def fix_rules(tr, name, binders, body):
    """The rewrite rules of a Fixpoint: its matches on its arguments (and
    on the variables of their patterns) split into cases."""
    rules = []
    ctx = {}
    for n, a in binders:
        ctx[n] = a

    def lhs_vars(lhs, ctx):
        seen = []

        def walk(t):
            if t.kind == 'var':
                if t.name != '_' and t.name in ctx and t.name not in seen:
                    seen.append(t.name)
            elif t.kind == 'app':
                for a in t.args:
                    walk(a)
        for l in lhs:
            walk(l)
        return seen

    def go(lhs, b, ctx):
        b = simplify(b)
        if b.kind == 'match' and b.scrut.kind == 'var' and b.scrut.name in lhs_vars(lhs, ctx):
            v = b.scrut.name
            vty = ctx.get(v)
            for p, rhs in b.branches:
                h, pargs = head(p)
                ctx2 = dict(ctx)
                del ctx2[v]
                if pargs:
                    tys = ctor_arg_types(tr, h.name, vty, len(pargs))
                    k0 = len(pargs) - len(tys) if tys else 0
                    # a wildcard of the pattern (S _, printed so when its variable is
                    # unused) gets a name, since a nested match may use the whole
                    pargs = pargs[:k0] + [Var(tr.fresh('k', dict(ctx, **ctx2))) if a.kind == 'var' and a.name == '_'
                                          else a for a in pargs[k0:]]
                    p = App(h, pargs)
                    real_args = pargs[k0:]
                    for a, ty in zip(real_args, tys):
                        if a.kind == 'var' and a.name != '_':
                            ctx2[a.name] = ty
                lhs2 = [simplify(subst(l, {v: p})) for l in lhs]
                go(lhs2, subst(rhs, {v: p}), ctx2)
            return
        # a value that is a function: its arguments in the pattern too
        while b.kind == 'lam':
            ctx = dict(ctx, **{b.name: b.dom})
            lhs = lhs + [Var(b.name)]
            b = b.body
        # a value that is a constant function (heq 0 --> oeq): eta-expanded
        bty = tr.typeof(b, ctx)
        names = iter('xyzuvw')
        while bty is not None and b.kind == 'var' and tr.whnf(bty).kind == 'pi':
            w = tr.whnf(bty)
            x = tr.fresh(next(names), ctx)
            ctx = dict(ctx, **{x: w.dom})
            lhs = lhs + [Var(x)]
            b = App(b, [Var(x)])
            bty = subst(w.body, {w.name: Var(x)})
            if b.kind == 'app':
                b0 = b
                while bty is not None and tr.whnf(bty).kind == 'pi':
                    w = tr.whnf(bty)
                    x = tr.fresh(next(names), ctx)
                    ctx = dict(ctx, **{x: w.dom})
                    lhs = lhs + [Var(x)]
                    b0 = App(b0, [Var(x)])
                    bty = subst(w.body, {w.name: Var(x)})
                b = b0
                break
        vs = lhs_vars(lhs, ctx)
        rhs = tr.term(b, ctx) if not tr.is_prop_term(b, ctx) else tr.prop(b, ctx)
        # the pattern translated as a term: the arguments the readings leave out are left out
        rules.append('[%s] %s --> %s' % (', '.join(vs), tr.term(App(Var(name), lhs), ctx), rhs))

    go([Var(n) for n, _ in binders], body, ctx)
    return rules


def pattern(tr, p, ctx):
    if p.kind == 'var':
        if p.name == '_' or p.name in ctx:
            return p.name
        return tr.term(p, ctx)
    return tr.term(p, ctx)


def dk_names(env, items):
    """Dedukti names of the Coq constants: as they are, unless they clash with
    Informath's base constants (neg is negation of numbers there)."""
    for it in items:
        names = [it['coq']] + it.get('constructors', []) + ([it['constructor']] if 'constructor' in it else []) + it.get('fields', [])
        for n in names:
            env.dk[n] = n + '_' if n in coqterm.BASE_NAMES or n in ('In', 'max', 'map', 'ite', 'setof', 'bmem', 'pair', 'triple') else n


FIELD_VARIABLES = project.get('FIELD_VARIABLES', {})

# the local modules that the exported file requires (set by main)
IMPORTS = []


def local_imports(vdir, src, seen=None):
    """The modules of vdir that src requires, transitively, in dependency order."""
    seen = [] if seen is None else seen
    for m in re.finditer(r'Require\s+(?:Import\s+|Export\s+)?((?:[A-Za-z_][\w\']*\s*)+)\.', strip_comments(src)):
        for name in m.group(1).split():
            f = os.path.join(vdir, name + '.v')
            if name not in seen and os.path.exists(f):
                local_imports(vdir, open(f).read(), seen)
                if name not in seen:
                    seen.append(name)
    return seen


def export_decls(vdir, mod, items, dep_items=()):
    """The Dedukti of items; dep_items are the declarations of the local
    modules the file requires, whose types the translation needs, but
    which are not exported again."""
    dep_items = list(dep_items)
    res = query(vdir, mod, dep_items + items)
    env = Env()
    dk_names(env, dep_items + items)
    env.user = set(env.dk)
    tr = Translator(env)
    blocks = []
    # first the types of all constants, so that the translation knows them
    for it in dep_items + items:
        n = it['coq']
        if it['kind'] in THEOREM_KINDS + AXIOM_KINDS:
            it['_type'] = parse_check(res[('check', n)])
            env.types[n] = it['_type']
        elif it['kind'] in ('Definition', 'Fixpoint'):
            body, ty, implicit = parse_print(res[('print', n)])
            it['_body'], it['_type'], it['implicit'] = body, ty, implicit
            env.bodies = getattr(env, 'bodies', {})
            env.bodies[n] = body
            env.types[n] = ty
            res_ty = ty
            params = []
            b = body
            while res_ty.kind == 'pi' and b.kind == 'lam':
                params.append(b.name)
                res_ty, b = subst(res_ty.body, {res_ty.name: Var(b.name)}), b.body
            if res_ty.kind == 'sort':
                env.aliases[n] = (params, b)
        elif it['kind'] == 'Inductive':
            env.types[n] = parse_check(res[('check', n)])
            for c in it['constructors']:
                env.types[c] = parse_check(res[('check', c)])
                coqterm.CONSTRUCTORS.add(c)
        elif it['kind'] == 'Record':
            env.types[n] = parse_check(res[('check', n)])
            for c in [it['constructor']] + it['fields']:
                env.types[c] = parse_check(res[('check', c)])
            coqterm.CONSTRUCTORS.add(it['constructor'])
            coqterm.STRUCTURES.add(n)
    for it in items:
        n = it['coq']
        dn = tr.const_name(n)
        it['dk'] = dn
        try:
            if it['kind'] in THEOREM_KINDS + AXIOM_KINDS:
                ty, _, _ = tr.decl_type(it['_type'])
                block = '%s : %s.' % (dn, ty)
                it['kind_'] = 'theorem' if it['kind'] in THEOREM_KINDS else 'axiom'
            elif it['kind'] == 'Inductive':
                lines = ['%s : %s.' % (dn, tr.decl_type(env.types[n])[0])]
                for c in it['constructors']:
                    lines.append('%s : %s.' % (tr.const_name(c), tr.decl_type(env.types[c])[0]))
                block = '\n'.join(lines)
                it['kind_'] = 'inductive'
            elif it['kind'] == 'Record':
                # a structure, as Informath declares them (test/algebra.dk): the
                # kind, its constructor with the fields as arguments, the selectors
                # the kind, with the parameters of the record (Str Ring -> Structure)
                kty, _, _ = tr.decl_type(env.types[n])
                lines = ['%s : %s.' % (dn, re.sub(r'(Set|Type)$', 'Structure', kty))]
                # the constructor's arguments: the parameters, then the fields, named
                # as the project says (config.py FIELD_VARIABLES: field -> the
                # variable of the definition, X for lawSet), else by the fields
                params, t = [], env.types[n]
                while t.kind == 'pi':
                    params.append(t.name)
                    t = t.body
                names = params + [FIELD_VARIABLES.get(f, f) for f in it['fields']]
                lines.append('%s : %s.' % (tr.const_name(it['constructor']),
                                           tr.decl_type(env.types[it['constructor']], names=names)[0]))
                for c in it['fields']:
                    lines.append('%s : %s.' % (tr.const_name(c), tr.decl_type(env.types[c])[0]))
                block = '\n'.join(lines)
                it['kind_'] = 'structure'
            elif n in coqterm.PREDICATE_SETS:
                block = '%s : Set.' % dn
                it['kind_'] = 'definition'
            else:
                block = definition(tr, it)
                it['kind_'] = 'definition'
        except Exception as e:
            block = '(; %s: not translated: %s ;)' % (n, str(e).replace(';)', ''))
            print('%s: %s' % (n, e), file=sys.stderr)
        it['dedukti'] = block
        blocks.append('(; //COQ:%s ;)\n%s' % (n, block))
    return env, tr, blocks


def contains_match_on(t, names):
    if t.kind == 'match':
        if t.scrut.kind == 'var' and t.scrut.name in names:
            return True
        return contains_match_on(t.scrut, names) or any(contains_match_on(b, names) for _, b in t.branches)
    if t.kind == 'app':
        return contains_match_on(t.f, names) or any(contains_match_on(a, names) for a in t.args)
    if t.kind in ('lam', 'pi'):
        return contains_match_on(t.body, names)
    return False


def definition(tr, it):
    body, ty = it['_body'], it['_type']
    n = it['coq']
    dn = tr.const_name(n)
    # a Fixpoint in a section: fun G1 G2 => fix f ... with the section's
    # variables as the first arguments of f
    outer, b0 = [], body
    while b0.kind == 'lam':
        outer.append((b0.name, b0.dom))
        b0 = b0.body
    if b0.kind == 'fix':
        params = [Var(x) for x, _ in outer]
        fbody = subst(b0.body, {b0.name: App(Var(n), params)}) if outer else b0.body
        binders = outer + b0.binders
        fty, _, _ = tr.decl_type(ty, names=[x for x, _ in binders])
        rules = fix_rules(tr, n, binders, fbody)
        return 'def %s : %s.\n%s.' % (dn, fty, '\n'.join(rules))
    names = []
    b, rty = body, ty
    ctx = {}
    while b.kind == 'lam' and rty.kind == 'pi':
        names.append(b.name)
        ctx[b.name] = b.dom
        rty = subst(rty.body, {rty.name: Var(b.name)})
        b = b.body
    dty, _, _ = tr.decl_type(ty, names=names)
    # a definition of a constant with arguments that the readings leave out,
    # or by cases on an argument: as rewrite rules
    if names and (n in coqterm.DROP or contains_match_on(b, set(names))):
        rules = fix_rules(tr, n, list(zip(names, [ctx[x] for x in names])), b)
        return 'def %s : %s.\n%s.' % (dn, dty, '\n'.join(rules))
    if tr.is_bset(rty) and b.kind == 'lam':
        # a truth value given by its conditions: s is in c x, if and only if P
        ctx2 = dict(ctx, **{b.name: b.dom})
        lhs = ' '.join([dn] + names)       # not eta-expanded: sup I f, as in the declaration
        rhs = tr.prop(b.body, ctx2)
        return 'def %s : %s.\n[%s] bmem %s %s --> %s.' % (dn, dty, ', '.join(names + [b.name]), paren(lhs), b.name, rhs)
    if rty.kind == 'sort' and rty.name == 'Prop':
        val = tr.prop(b, ctx)
    elif rty.kind == 'sort':
        val = tr.set(b, ctx)
    elif tr.is_prop_term(b, ctx):
        val = tr.prop(b, ctx)
    else:
        val = tr.term(b, ctx, rty)
    lam = ''.join('%s => ' % x for x in names)
    return 'def %s : %s :=\n  %s%s.' % (dn, dty, lam, val)


# ------------------------------------------------------------------ tactic proofs

BULLET = re.compile(r'[-+*]+|[{}]')


def script_items(code, start, end):
    """The bullets, braces and tactic sentences of a proof script, with their offsets."""
    out, i = [], start
    while i < end:
        while i < end and code[i].isspace():
            i += 1
        if i >= end:
            break
        m = BULLET.match(code, i)
        if m and (code[i] in '{}' or not code[m.end():m.end() + 1].isalnum() or True) and \
                (code[i] in '{}' or code[m.end():m.end() + 1].isspace()):
            out.append(('bullet', m.group(0), i, m.end()))
            i = m.end()
            continue
        j = sentence_end(code, i)
        j = min(j, end)
        out.append(('tactic', code[i:j], i, j))
        i = j
    return out


def instrumented(src, items):
    """The source with each tactic of each proof between two Show commands."""
    code = strip_comments(src)
    pieces, last = [], 0
    steps = []
    for it in items:
        if 'script' not in it:
            continue
        s, e = it['script']
        pieces.append(code[last:s])
        pieces.append('\n')
        # the bullets and braces open at each tactic: a bullet of a kind already
        # open is the next case of that level; each case is numbered
        stack, count = [], 0
        for kind, text, a, b in script_items(code, s, e):
            if kind == 'bullet':
                if text == '}':
                    while stack and stack.pop()[0] != '{':
                        pass
                else:
                    if text != '{' and any(k == text for k, _ in stack):
                        while stack[-1][0] != text:
                            stack.pop()
                        stack.pop()
                    count += 1
                    stack.append((text, count))
                pieces.append(' %s ' % text)
                if text not in ('}',):
                    if text != '{':
                        pieces.append('idtac.\n')
                continue
            steps.append({'decl': it['coq'], 'tactic': ' '.join(text.split()).rstrip('.'),
                          'line': line_of(src, a), 'col': a - src.rfind('\n', 0, a) - 1, 'endLine': line_of(src, b),
                          'path': [c for _, c in stack]})
            pieces.append('\n%s Show.\n%s\n%s Show.\n' % (SEP, text, SEP))
        last = e
    pieces.append(code[last:])
    text = ''.join(pieces)
    # Printing All after the imports
    m = re.search(r'Import ListNotations\.\n|Require Import [^\n]*\n', text)
    k = text.find('Require Import')
    if k < 0:
        k = 0   # no imports: at the start
    else:
        k = text.find('\n', k) + 1
    while text[k:].startswith('Import'):
        k = text.find('\n', k) + 1
    return text[:k] + SETTINGS + text[k:], steps


def parse_goals(out):
    """Show's output: the goals in focus, the first with its hypotheses."""
    out = out.strip()
    if out.startswith('No more') or out.startswith('This subproof is complete') or out.startswith('All the remaining'):
        return []
    lines = out.split('\n')
    m = re.match(r'(\d+) goals?', lines[0])
    if not m:
        return []
    n = int(m.group(1))
    body = '\n'.join(lines[1:])
    first, _, rest = body.partition('\n\ngoal 2 is:')
    hyps_part, _, target = first.partition('============================')
    hyps = []
    joined = []
    for l in hyps_part.split('\n'):
        if re.match(r'  \S', l) or not joined:
            joined.append(l)
        else:
            joined[-1] += ' ' + l.strip()
    for l in joined:
        l = ' '.join(l.split())
        if not l:
            continue
        mm = re.match(r"([A-Za-z_0-9', ]+?) : (.*)$", l)
        md = re.match(r"([A-Za-z_0-9']+) := (.*)$", l)
        if md:
            # a local definition, name := value : type (not `rest`, the other goals)
            defn = md.group(2)
            for mk in re.finditer(r' : ', defn):
                try:
                    parse(defn[:mk.start()]), parse(defn[mk.end():])
                except Exception:
                    continue
                hyps.append(([md.group(1)], defn[mk.end():], defn[:mk.start()]))
                break
        elif mm:
            hyps.append(([x.strip() for x in mm.group(1).split(',')], mm.group(2), None))
    goals = [{'hyps': hyps, 'target': ' '.join(target.split())}]
    for g in re.split(r'\ngoal \d+ is:\n', '\ngoal 2 is:' + rest)[1:] if rest else []:
        goals.append({'hyps': None, 'target': ' '.join(g.split())})
    return goals[:n]


def defix(t):
    """A goal with the fixpoints that simpl unfolded (fix heq ... ) folded back to their names."""
    k = t.kind
    if k == 'fix':
        return Var(t.name)
    if k == 'app':
        return App(defix(t.f), [defix(a) for a in t.args])
    if k in ('pi', 'lam'):
        return T(k, name=t.name, dom=defix(t.dom) if t.dom else None, body=defix(t.body))
    if k == 'match':
        return T('match', scrut=defix(t.scrut), asn=t.asn, branches=[(p, defix(b)) for p, b in t.branches])
    if k == 'let':
        return T('let', name=t.name, ty=t.ty, val=defix(t.val), body=defix(t.body))
    return t


SECTION_VARS = tuple(project.get('SECTION_VARS', ()))


def section_vars(tr, name):
    """The section variables that a constant takes as its first arguments outside its section."""
    ty = tr.env.types.get(name)
    body = getattr(tr.env, 'bodies', {}).get(name)
    out = []
    while ty is not None and ty.kind == 'pi':
        # Print names the binders of a definition's type _: its definiens names them
        nm = body.name if ty.name == '_' and body is not None and body.kind == 'lam' else ty.name
        if nm not in SECTION_VARS:
            break
        out.append(nm)
        ty = ty.body
        body = body.body if body is not None and body.kind == 'lam' else None
    return out


def resection(tr, t, sec):
    """A term of a goal inside the section, with the section variables given
    to the constants that take them outside it."""
    k = t.kind
    if k == 'var':
        vs = sec.get(t.name)
        return App(t, [Var(v) for v in vs]) if vs else t
    if k == 'app':
        return App(resection(tr, t.f, sec), [resection(tr, a, sec) for a in t.args])
    if k in ('pi', 'lam'):
        return T(k, name=t.name, dom=resection(tr, t.dom, sec) if t.dom else None, body=resection(tr, t.body, sec))
    if k == 'match':
        return T('match', scrut=resection(tr, t.scrut, sec), asn=t.asn,
                 branches=[(p, resection(tr, b, sec)) for p, b in t.branches])
    if k == 'let':
        return T('let', name=t.name, ty=t.ty, val=resection(tr, t.val, sec), body=resection(tr, t.body, sec))
    return t


def goal_dk(tr, goal, base_ctx, sec=None):
    """A goal in Dedukti: hypotheses [name, type, false], target."""
    ctx = {}
    hyps = []
    fix = (lambda t: resection(tr, defix(t), sec)) if sec else defix
    if goal['hyps'] is not None:
        for names, ty, val in goal['hyps']:
            t = fix(parse(ty))
            for nm in names:
                if val is not None:
                    tr.env.types.pop(nm, None)
                bt = tr.binder_type(t, ctx)
                hyps.append([nm, bt, False])
                if val is not None:
                    # a local definition (set k := v): its value, for the proof texts
                    try:
                        hyps[-1].append(tr.term(parse(val), ctx))
                    except Exception:
                        pass
                ctx[nm] = t
    tg = fix(parse(goal['target']))
    target = tr.prop(tg, ctx) if tr.sort(tg, ctx) == 'Prop' else tr.term(tg, ctx)
    return {'hyps': hyps, 'target': target}


def export_tactics(vdir, mod, src, items, tr):
    text, steps = instrumented(src, items)
    path = os.path.abspath(os.path.join(vdir, 'replay_tmp_%d.v' % os.getpid()))
    open(path, 'w').write(text)
    r = subprocess.run(['rocq', 'repl'] + loadpath(vdir) + ['-batch', '-l', path], cwd=vdir,
                       capture_output=True, text=True)
    os.remove(path)
    if 'Error' in r.stdout + r.stderr:
        sys.stderr.write((r.stdout + r.stderr)[-3000:])
        raise SystemExit('replay failed')
    chunks = SEP_OUT.split(r.stdout)[1:]
    assert len(chunks) == 2 * len(steps), (len(chunks), len(steps))
    out = []
    bad = 0
    # the constants of the section, which take its variables outside it
    secmap = {n: section_vars(tr, n) for n in tr.env.user}
    secmap = {n: v for n, v in secmap.items() if v}
    for k, st in enumerate(steps):
        before, after = parse_goals(chunks[2 * k]), parse_goals(chunks[2 * k + 1])
        sec = secmap if st['decl'] in secmap else None
        try:
            b = goal_dk(tr, before[0], {}, sec) if before else None
            a = []
            for g in after:
                try:
                    a.append(goal_dk(tr, g, {}, sec) if g['hyps'] is not None else {'hyps': [], 'target': goal_dk(tr, {'hyps': before[0]['hyps'], 'target': g['target']}, {}, sec)['target']})
                except Exception:
                    a.append({'hyps': [], 'target': 'unknown_goal', 'error': True})
        except Exception as e:
            bad += 1
            print('%s, %s: %s' % (st['decl'], st['tactic'], e), file=sys.stderr)
            b, a = {'error': str(e)}, []
        tac = st['tactic']
        out.append(dict(st, kind=re.match(r"[A-Za-z_']*", tac).group(0) or tac, nBefore=len(before), before=b, after=a))
    return out, bad


def compile_if_needed(vfile):
    """Compile vfile in its directory, with its load paths, unless its .vo is newer."""
    vdir, base = os.path.split(os.path.abspath(vfile))
    vo = os.path.join(vdir, base[:-2] + '.vo')
    if os.path.exists(vo) and os.path.getmtime(vo) >= os.path.getmtime(vfile):
        return
    r = subprocess.run(['rocq', 'compile'] + loadpath(vdir) + [base], cwd=vdir, capture_output=True, text=True)
    if r.returncode:
        sys.stderr.write((r.stdout + r.stderr)[-3000:])
        raise SystemExit('rocq compile %s failed' % vfile)


def main():
    global OUT
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    for a in sys.argv[1:]:
        if a.startswith('-out='):
            OUT = os.path.abspath(a[len('-out='):])
    if len(args) != 1 or not args[0].endswith('.v'):
        sys.exit(__doc__)
    vfile = args[0]
    vdir, base = os.path.split(os.path.abspath(vfile))
    mod = base[:-2]
    compile_if_needed(vfile)
    src = open(vfile).read()
    items = scan(src)
    dep_items = []
    IMPORTS[:] = local_imports(vdir, src)
    for dep in IMPORTS:
        compile_if_needed(os.path.join(vdir, dep + '.v'))
        dep_items += scan(open(os.path.join(vdir, dep + '.v')).read())
    env, tr, blocks = export_decls(vdir, mod, items, dep_items)
    os.makedirs(OUT, exist_ok=True)
    open(os.path.join(OUT, mod + '.dk'), 'w').write(
        '(; %s: declarations exported from %s by rocq-export.py ;)\n\n' % (mod, base) + '\n\n'.join(blocks) + '\n')
    decls = [{k: v for k, v in it.items() if not k.startswith('_') and k != 'script'} for it in items]
    for d in decls:
        d['kind'] = d.pop('kind_', d['kind'])
    json.dump(decls, open(os.path.join(OUT, mod + '.json'), 'w'), ensure_ascii=False, indent=1)
    steps, bad = export_tactics(vdir, mod, src, items, tr)
    json.dump(steps, open(os.path.join(OUT, mod + '-tactics.json'), 'w'), ensure_ascii=False, indent=1)
    print('%s: %d declarations, %d tactic steps (%d not translated) -> %s.dk, .json, -tactics.json'
          % (mod, len(items), len(steps), bad, os.path.join(OUT, mod)))
    other = {}
    for m in OTHER.finditer(strip_comments(src)):
        k = m.group(1).split()[0]
        other[k] = other.get(k, 0) + 1
    if other:
        print('%s: not exported: %s' % (mod, ', '.join('%d %s' % (v, k) for k, v in sorted(other.items()))))


if __name__ == '__main__':
    main()
