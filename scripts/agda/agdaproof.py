"""Proof terms from Agda's definitions by pattern matching, for proof texts.

With the identity types read as propositions (agdaterm.py, PROPOSITIONS), a
definition whose type is a proposition is a theorem, and its clauses are a
proof by induction or by cases.  This module makes a Dedukti proof term of
them, which RunInformath -proof-text writes as a proof text:

* for every data type D, its eliminator into propositions, ind_D_P, made from
  its constructors as the induction principle of the book: a motive P over
  the indices and D, a case for each constructor with an induction
  hypothesis for each recursive argument (for N, ordinary induction; for
  the identity type, path induction; for coprod and Sigma, cases);
* for a definition whose clauses match on one argument with a constructor
  (and on nothing else but variables and dot patterns), the term
  x1 => ... => ind_D_P params motive case1 ... indices x_i, where the motive
  is the statement with the argument (and the indices of its type)
  abstracted, and a case is the clause's body with the clause's variables
  abstracted and each recursive call on a recursive argument replaced by
  its induction hypothesis.

A definition that does not have this shape (matching on two arguments,
nested patterns, a later argument whose type depends on the matched one)
keeps its rewrite rules.
"""
import os, sys
from agdaterm import paren, short


def is_recursive(d, env):
    """Whether a data type has a constructor with an argument of the type itself."""
    for c in d.get('constructors', []):
        t = env.get(c, {}).get('type')
        while t and t[0] == 'pi':
            if t[3][0] == 'def' and t[3][1] == d['name']:
                return True
            t = t[4]
    return False


def elim_kinds(tr, d, env):
    """The eliminators made for a data type: 'ind' (induction, with induction
    hypotheses) for a recursive type and for the PROPOSITIONS (path
    induction), 'cases' (case analysis) for every type but the PROPOSITIONS."""
    out = []
    if is_recursive(d, env) or short(d['name']) in tr.propositions:
        out.append('ind')
    if short(d['name']) not in tr.propositions:
        out.append('cases')
    return out


def elim_name(names, dqname, kind='ind'):
    """The Dedukti name of D's eliminator into propositions: ind_D_P, or cases_D_P."""
    key = dqname + '.%s-prop' % kind
    if key not in names.dk:
        n = '%s_%s_P' % (kind, names(dqname))
        names.dk[key] = n
        names.taken.add(n)
    return names.dk[key]


def binders(tr, t, ctx=None):
    """The leading binders of an Agda type: (agda name, Dedukti name, domain,
    hiding, is a level), as decl_type names them; and the rest, and the context."""
    ctx = dict(ctx or {})
    out = []
    while t[0] == 'pi':
        _, h, x, dom, cod = t
        if tr.is_level_type(dom):
            v = tr.var(x, ctx)
            ctx[v] = 'level'
            out.append((x, v, dom, h, True))
            t = cod
            continue
        v = tr.binder(x, ctx, dom)
        ctx[v] = 'type' if tr.is_kind(dom) else ('proof' if tr.is_prop_family(dom) else 'elem')
        out.append((x, v, dom, h, False))
        t = tr.rename(cod, x, v)
    return out, t, ctx


def eliminator(tr, names, d, env, kind='ind'):
    """The Dedukti declaration of D's eliminator into propositions, or None;
    kind 'cases' leaves out the induction hypotheses."""
    pars = d.get('pars', 0)
    bs, res, ctx = binders(tr, d['type'])
    params, indices = bs[:pars], bs[pars:]
    dn = names(d['name'])
    pv = [v for _, v, _, _, lev in params if not lev]
    iv = [v for _, v, _, _, lev in indices if not lev]
    app = ' '.join([dn] + pv + iv)
    z = tr.fresh('z', ctx)
    zt = ('Proof %s' if short(d['name']) in tr.propositions else 'Elem %s') % paren(app)
    motive_ty = ' -> '.join(['(%s : %s)' % (v, tr.binder_type(dom, ctx)) for _, v, dom, _, lev in indices if not lev]
                            + [zt, 'Prop'])
    P = tr.fresh('P', ctx)
    cases = []
    for c in d.get('constructors', []):
        cd = env.get(c)
        if not cd:
            return None
        cbs, cres, cctx = binders(tr, cd['type'])
        cparams, cargs = cbs[:pars], cbs[pars:]
        # the constructor's parameters are the data type's
        ren = {cv: v for (_, cv, _, _, clev), (_, v, _, _, lev) in zip(cparams, params) if not clev}
        cctx = dict(ctx, **{v: k for v, k in cctx.items() if v not in ren})
        parts, ihs = [], []
        for x, v, dom, h, lev in cargs:
            if lev:
                continue
            parts.append('(%s : %s)' % (v, rename_str(tr.binder_type(dom, cctx), ren)))
            if dom[0] == 'def' and dom[1] == d['name'] and kind == 'ind':
                ih_idx = [rename_str(tr.term(e[2], cctx), ren) for e in dom[2][pars:] if e[0] == 'app'
                          and not tr.is_level(e[2], cctx)]
                ihs.append('Proof (%s)' % ' '.join([P] + [paren(i) for i in ih_idx] + [v]))
        cidx = [rename_str(tr.term(e[2], cctx), ren) for e in cres[2][pars:] if e[0] == 'app'
                and not tr.is_level(e[2], cctx)] if cres[0] == 'def' else []
        capp = ' '.join([names(c)] + [v for _, v, _, _, lev in cargs if not lev])
        concl = 'Proof (%s)' % ' '.join([P] + [paren(i) for i in cidx] + [paren(capp)])
        cases.append('(' + ' -> '.join(parts + ihs + [concl]) + ')')
    ty = ' -> '.join(['(%s : %s)' % (v, tr.binder_type(dom, ctx)) for _, v, dom, _, lev in params if not lev]
                     + ['(%s : (%s))' % (P, motive_ty)] + cases
                     + ['(%s : %s)' % (v, tr.binder_type(dom, ctx)) for _, v, dom, _, lev in indices if not lev]
                     + ['(%s : %s)' % (z, zt), 'Proof (%s)' % ' '.join([P] + iv + [z])])
    return '%s : %s.' % (elim_name(names, d['name'], kind), ty)


def rename_str(s, ren):
    import re
    if not ren:
        return s
    return re.sub(r"(?<![A-Za-z0-9_'])([A-Za-z_][A-Za-z0-9_']*)(?![A-Za-z0-9_'])", lambda m: ren.get(m.group(1), m.group(1)), s)


def rename_json(t, ren):
    """A JSON term with its variables renamed (not under a binder of the same name)."""
    if isinstance(t, list):
        if len(t) >= 2 and t[0] == 'var' and t[1] in ren:
            return ['var', ren[t[1]]] + [rename_json(u, ren) for u in t[2:]]
        if t and t[0] == 'lam' and t[2] in ren:
            r2 = {k: v for k, v in ren.items() if k != t[2]}
            return t[:3] + [rename_json(t[3], r2)]
        if t and t[0] == 'pi' and t[2] in ren:
            r2 = {k: v for k, v in ren.items() if k != t[2]}
            return t[:3] + [rename_json(t[3], ren), rename_json(t[4], r2)]
        return [rename_json(u, ren) for u in t]
    return t


def replace_calls(t, fname, i, recvars, ihs):
    """The body with each recursive call f ... a ... on a recursive argument a
    (at the matched position i) replaced by a's induction hypothesis."""
    if isinstance(t, list):
        if len(t) >= 3 and t[0] == 'def' and t[1] == fname:
            args = [e for e in t[2] if e[0] == 'app']
            if len(args) > i and args[i][2][0] == 'var' and args[i][2][1] in recvars and len(args) == len(t[2]):
                return ['var', ihs[args[i][2][1]], []]
        return [replace_calls(u, fname, i, recvars, ihs) for u in t]
    return t


def mentions_calls(t, fname):
    if isinstance(t, list):
        if len(t) >= 2 and t[0] == 'def' and t[1] == fname:
            return True
        return any(mentions_calls(u, fname) for u in t)
    return False


def proof_term(tr, names, d, env, clauses):
    """A proof term of a definition by pattern matching on one argument, or None."""
    if not clauses:
        return None
    bs, res, ctx = binders(tr, d['type'])
    n = len(bs)
    if any(len(c['pats']) != n for c in clauses):
        return None
    # the matched position: a constructor pattern there in every clause,
    # variables and dot patterns elsewhere
    cands = [i for i in range(n) if all(c['pats'][i][1][0] == 'con' for c in clauses)]
    for i in cands:
        if all(c['pats'][j][1][0] in ('var', 'dot') for c in clauses for j in range(n) if j != i):
            break
    else:
        return None
    dom = bs[i][2]
    if dom[0] != 'def' or dom[1] not in env or env[dom[1]]['kind'] != 'data':
        return None
    D = env[dom[1]]
    pars = D.get('pars', 0)
    elims = [e for e in dom[2] if e[0] == 'app']
    if len(elims) != pars + (len([b for b in binders(tr, D['type'])[0]]) - pars):
        return None
    pargs, iargs = elims[:pars], elims[pars:]
    # the indices must be variables bound before the matched argument; the
    # binders after it must not depend on it or on them
    agda_names = [b[0] for b in bs]
    idx_pos = []
    for e in iargs:
        if tr.is_level(e[2], ctx):
            continue
        if e[2][0] != 'var' or e[2][1] not in [b[1] for b in bs[:i]]:
            return None
        idx_pos.append([b[1] for b in bs].index(e[2][1]))
    deps = {bs[i][1]} | {bs[k][1] for k in idx_pos}
    # the binders after the matched argument: if their types depend on it or
    # on its indices, the motive generalizes over them (for all z, if q : y = z
    # then ...), and the cases and the conclusion introduce and eliminate them
    gen = []
    if any(tr.occurs(v, b[2]) for b in bs[i + 1:] for v in deps):
        gen = [b for b in bs[i + 1:] if not b[4]]
        if any(tr.is_kind(b[2]) or (tr.is_prop_family(b[2]) and not tr.is_prop_type(b[2])) for b in gen):
            return None

    def gen_prop(k):
        if k == len(gen):
            return tr.term(res, ctx)
        x, v, gdom, h, lev = gen[k]
        if tr.is_prop_type(gdom):
            return 'if (%s) (%s)' % (tr.term(gdom, ctx), gen_prop(k + 1))
        return 'forall (%s) (%s => %s)' % (tr.set(gdom, ctx), v, gen_prop(k + 1))

    def introduce(body, sub):
        """A case's body with the generalized binders introduced (forallI, ifI)."""
        for k in range(len(gen) - 1, -1, -1):
            x, v, gdom, h, lev = gen[k]
            rest = rename_str(gen_prop(k + 1), sub)
            if tr.is_prop_type(gdom):
                body = 'ifI (%s) (%s) (%s => %s)' % (rename_str(tr.term(gdom, ctx), sub), rest, v, body)
            else:
                body = 'forallI (%s) (%s => %s) (%s => %s)' % (rename_str(tr.set(gdom, ctx), sub), v, rest, v, body)
        return body

    def eliminate(e):
        """The conclusion applied to the generalized binders (forallE, ifE)."""
        for k in range(len(gen)):
            x, v, gdom, h, lev = gen[k]
            rest = gen_prop(k + 1)
            if tr.is_prop_type(gdom):
                e = 'ifE (%s) (%s) (%s) %s' % (tr.term(gdom, ctx), rest, e, v)
            else:
                e = 'forallE (%s) (%s => %s) (%s) %s' % (tr.set(gdom, ctx), v, rest, e, v)
        return e
    # the motive: the statement with the indices and the matched argument abstracted
    motive = ' => '.join([bs[k][1] for k in idx_pos] + [bs[i][1]]) + ' => ' + gen_prop(0)
    cases = []
    for c in D.get('constructors', []):
        cl = [cl for cl in clauses if cl['pats'][i][1][1] == c]
        cd = env.get(c)
        if not cd:
            return None
        cbs = binders(tr, cd['type'])[0][pars:]
        if not cl:
            return None                      # an absurd case
        cl = cl[0]
        sub = [p for h, p in cl['pats'][i][1][2]]
        if any(p[0] != 'var' for p in sub):
            return None
        # the clause's variables at the other positions: the binders' names
        ren = {}
        for j in range(n):
            if j != i and cl['pats'][j][1][0] == 'var':
                ren[cl['pats'][j][1][1]] = bs[j][1]
        avars = [p[1] for p in sub]
        cctx = dict(ctx)
        rec = {}
        lams = []
        kept = [b for b in cbs if not b[4]]
        if len(avars) != len(kept):
            return None
        for (x, v, cdom, h, lev), a in zip(kept, avars):
            av = tr.var(a, cctx)
            if av in cctx and cctx[av] != 'level':
                av = tr.fresh(av, cctx)
                ren[a] = av
            cctx[av] = 'proof' if tr.is_prop_family(cdom) else ('type' if tr.is_kind(cdom) else 'elem')
            lams.append(av)
            if cdom[0] == 'def' and cdom[1] == D['name']:
                rec[av] = None
        ihs = {}
        for av in rec:
            ih = tr.fresh('ih', cctx)
            cctx[ih] = 'proof'
            ihs[av] = ih
            lams.append(ih)
        # the clause's variables renamed, then its recursive calls on a recursive
        # argument (now named as in lams) replaced by their induction hypotheses
        body = replace_calls(rename_json(cl['body'], ren), d['name'], i, set(rec), ihs)
        if mentions_calls(body, d['name']):
            return None                      # a recursive call that is not an induction hypothesis
        # the indices and the matched argument as this constructor gives them
        cres = binders(tr, cd['type'])[1]
        cidx = [tr.term(e[2], cctx) for e in (cres[2][pars:] if cres[0] == 'def' else [])
                if e[0] == 'app' and not tr.is_level(e[2], cctx)]
        sub = {bs[k][1]: paren(t) for k, t in zip(idx_pos, cidx)}
        sub[bs[i][1]] = paren(' '.join([tr.names(c)] + lams[:len(kept)]))
        cases.append(''.join('%s => ' % l for l in lams) + introduce(tr.term(body, cctx), sub))
    if len(cases) != len(D.get('constructors', [])):
        return None
    params = [paren(tr.arg(e[2], ctx)) for e in pargs if not tr.is_level(e[2], ctx)]
    term = ' '.join([elim_name(names, D['name'], elim_kinds(tr, D, env)[0])] + params + ['(%s)' % motive] + ['(%s)' % c for c in cases]
                    + [bs[k][1] for k in idx_pos] + [bs[i][1]])
    tr.induction_on = [bs[i][1]]        # the variable of the induction, for the proof texts
    return ''.join('%s => ' % b[1] for b in bs if not b[4]) + eliminate(term)


# ---------------------------------------------------------------------------
# Pattern matching on several arguments and nested patterns: a case tree.
#
# Agda splits the arguments one at a time, in the order of the first clause's
# constructor patterns. Each split becomes the eliminator of the argument's
# data type, with the statement (the arguments as the patterns so far have
# them) as its motive: a case analysis, or an induction if the type is
# recursive. A recursive call whose arguments are those of the current
# pattern with an argument of the constructor in place of the split one is
# the induction hypothesis of that argument. So ap-binary f refl refl is
# path induction on p and then on q, and a proof on the integers matching
# inr (inr (succ n)) is cases on z and on its inr part, and induction on n
# with the motive "the statement for inr (inr n)".

class NoProof(Exception):
    pass


def subst_json(t, s):
    """A JSON term with variables replaced by JSON terms (s: name -> term)."""
    if not s or not isinstance(t, list) or not t:
        return t
    if t[0] == 'var' and t[1] in s:
        new, elims = s[t[1]], [subst_json(e, s) for e in t[2]]
        if not elims:
            return new
        if new[0] in ('var', 'def', 'con'):
            return new[:2] + [new[2] + elims]
        raise NoProof('applied substitution')
    if t[0] == 'lam':
        s2 = {k: v for k, v in s.items() if k != t[2]}
        return t[:3] + [subst_json(t[3], s2)]
    if t[0] == 'pi':
        s2 = {k: v for k, v in s.items() if k != t[2]}
        return t[:3] + [subst_json(t[3], s), subst_json(t[4], s2)]
    return [subst_json(u, s) for u in t]


def free_in(v, t):
    if isinstance(t, list) and t:
        if t[0] == 'var' and t[1] == v:
            return True
        if t[0] == 'lam' and t[2] == v:
            return False
        if t[0] == 'pi' and t[2] == v:
            return free_in(v, t[3])
        return any(free_in(v, u) for u in t)
    return False


def unfold(T, env):
    """A type defined as another (Z = coprod N (coprod unit N)) unfolded to a data type."""
    for _ in range(20):
        if T[0] != 'def' or T[1] not in env:
            return T
        d = env[T[1]]
        cs = d.get('clauses') or []
        if d['kind'] != 'function' or len(cs) != 1 or cs[0].get('body') is None:
            return T
        pats = cs[0]['pats']
        args = [e for e in T[2] if e[0] == 'app']
        if len(args) != len(T[2]) or len(pats) != len(args) or any(p[1][0] != 'var' for p in pats):
            return T
        T = subst_json(cs[0]['body'], {p[1][1]: a[2] for p, a in zip(pats, args)})
    return T


def _norm(s):
    return ' '.join(s.replace('(', ' ').replace(')', ' ').split())


def case_tree_proof(tr, names, d, env, clauses):
    """A proof term of a definition by pattern matching (any arguments, nested
    patterns) as nested eliminators, or None."""
    try:
        return _case_tree(tr, names, d, env, clauses)
    except (NoProof, KeyError, IndexError, ValueError) as e:
        if os.environ.get('AGDAPROOF_DEBUG'):
            sys.stderr.write('no proof term: %s: %s %s\n' % (short(d['name']), type(e).__name__, e))
        return None


def _case_tree(tr, names, d, env, clauses):
    if not clauses:
        return None
    bs, res, ctx = binders(tr, d['type'])
    n = len(bs)
    if any(len(c['pats']) != n for c in clauses):
        return None
    taken = dict(ctx)
    real = [j for j in range(n) if not bs[j][4]]
    # the arguments as the patterns so far have them (original binder -> JSON)
    val = {bs[j][1]: ['var', bs[j][1], []] for j in range(n)}
    types = {bs[j][1]: bs[j][2] for j in range(n)}
    kinds = {bs[j][1]: ctx[bs[j][1]] for j in range(n)}
    rows = []
    for c in clauses:
        pats = {bs[j][1]: c['pats'][j][1] for j in range(n)}
        rows.append({'pats': pats, 'body': c['body'], 'ren': {}})
    opened = [bs[j][1] for j in range(n)]
    splits = []

    def fresh(x, kind):
        v = tr.fresh(tr.var(x, taken) if x and x != '_' else 'x', taken)
        taken[v] = kind
        return v

    def settle(rows, opened):
        """Variable and dot patterns recorded: a row's variable is the open one."""
        for r in rows:
            for v in opened:
                p = r['pats'].get(v)
                if p and p[0] == 'var':
                    r['ren'][p[1]] = ['var', v, []]

    def args_key(vals):
        return tuple(_norm(tr.term(vals[bs[j][1]], taken)) for j in real)

    def leaf(rows, val, ihs):
        r = rows[0]
        body = subst_json(r['body'], r['ren'])

        def repl(t):
            if isinstance(t, list) and t:
                if t[0] == 'def' and t[1] == d['name']:
                    args = [e for e in t[2] if e[0] == 'app']
                    if len(args) == n and len(t[2]) == n:
                        key = tuple(_norm(tr.term(args[j][2], taken)) for j in real)
                        for k, ih in ihs:
                            if k == key:
                                return ['var', ih, []]
                    raise NoProof('recursive call not an induction hypothesis')
                return [repl(u) for u in t]
            return t
        return tr.term(repl(body), taken)

    def build(rows, opened, val, types, ihs):
        settle(rows, opened)
        # the split: the first constructor pattern of the first row
        r0 = rows[0]
        v = next((v for v in opened if r0['pats'].get(v, ['var'])[0] == 'con'), None)
        if v is None:
            return leaf(rows, val, ihs)
        T = unfold(types[v], env)
        if T[0] != 'def' or T[1] not in env or env[T[1]]['kind'] != 'data':
            raise NoProof('not a data type')
        D = env[T[1]]
        pars = D.get('pars', 0)
        elims = [e for e in T[2] if e[0] == 'app']
        dbs = binders(tr, D['type'])[0]
        if len(elims) != len(dbs):
            raise NoProof('partial type')
        pargs = elims[:pars]
        idx = []
        for (x, dv, ddom, h, lev), e in zip(dbs[pars:], elims[pars:]):
            if lev:
                continue
            if e[2][0] != 'var' or e[2][2] or e[2][1] not in opened or e[2][1] == v:
                raise NoProof('index not a variable')
            idx.append(e[2][1])
        if len(set(idx)) != len(idx) or any(free_in(y, e[2]) for y in idx for e in pargs):
            raise NoProof('indices')
        # no other open argument may depend on the split one or its indices
        for u in opened:
            if u != v and u not in idx and any(free_in(w, types[u]) for w in [v] + idx):
                raise NoProof('dependent argument')
        rest = [u for u in opened if u != v and u not in idx]
        goal = tr.term(subst_json(res, val), taken)
        motive = ' => '.join(idx + [v]) + ' => ' + goal
        cases = []
        for c in D.get('constructors', []):
            cd = env[c]
            cbs, cres, _ = binders(tr, cd['type'])
            cpar, cargs = cbs[:pars], cbs[pars:]
            s = {}
            for (x, cv, cdom, h, lev), e in zip(cpar, pargs):
                s[cv] = e[2]
            my = [r for r in rows if r['pats'][v][0] != 'con' or r['pats'][v][1] == c]
            if not my:
                raise NoProof('absurd case')
            first = next((r for r in my if r['pats'][v][0] == 'con'), None)
            fsub = [p for h, p in first['pats'][v][2]] if first else []
            new, lams, recs, atypes = [], [], [], {}
            kept = [b for b in cargs if not b[4]]
            if first and len(fsub) != len(kept):
                if len(fsub) == len(cargs):
                    fsub = [p for p, b in zip(fsub, cargs) if not b[4]]
                else:
                    raise NoProof('constructor arity')
            for k, (x, cv, cdom, h, lev) in enumerate(kept):
                hint = fsub[k][1] if fsub and fsub[k][0] == 'var' else x
                ty = subst_json(cdom, s)
                kind = 'proof' if tr.is_prop_family(ty) else ('type' if tr.is_kind(ty) else 'elem')
                a = fresh(hint, kind)
                s[cv] = ['var', a, []]
                new.append(a)
                atypes[a] = ty
                if cdom[0] == 'def' and cdom[1] == D['name']:      # as declared, not as substituted
                    recs.append(a)
            con = ['con', c, [['app', b[3], ['var', a, []]] for b, a in zip(kept, new)]]
            # the indices as the constructor gives them
            cidx = [e[2] for e, b in zip([e for e in cres[2] if e[0] == 'app'][pars:], dbs[pars:]) if not b[4]] \
                if cres[0] == 'def' else []
            sub = {v: con}
            for y, t in zip(idx, [subst_json(t, s) for t in cidx]):
                sub[y] = t
            val2 = {k: subst_json(t, sub) for k, t in val.items()}
            types2 = {k: subst_json(t, sub) for k, t in types.items() if k not in sub}
            types2.update(atypes)
            ihs2 = list(ihs)
            ihnames = []
            for a in recs:
                # the induction hypothesis: the statement for the recursive argument
                at = atypes[a]
                aidx = [e[2] for e, b in zip([e for e in at[2] if e[0] == 'app'][pars:], dbs[pars:]) if not b[4]]
                s2 = {v: ['var', a, []]}
                for y, t in zip(idx, aidx):
                    s2[y] = t
                ih = fresh('ih', 'proof')
                ihnames.append(ih)
                ihs2.append((args_key({k: subst_json(t, s2) for k, t in val.items()}), ih))
            rows2 = []
            for r in my:
                p = r['pats'][v]
                pats = {u: q for u, q in r['pats'].items() if u != v}
                ren = {k: subst_json(t, sub) for k, t in r['ren'].items()}
                if p[0] == 'con':
                    sp = [q for h, q in p[2]]
                    if len(sp) != len(kept):
                        sp = [q for q, b in zip(sp, cargs) if not b[4]]
                    for a, q in zip(new, sp):
                        pats[a] = q
                for y in idx:
                    q = r['pats'].get(y)
                    if q and q[0] == 'con':
                        raise NoProof('pattern on an index')
                    if q and q[0] == 'var':
                        ren[q[1]] = sub[y]
                rows2.append({'pats': pats, 'body': r['body'], 'ren': ren})
            pos = opened.index(v)
            opened2 = [u for u in opened[:pos] if u not in idx] + new + [u for u in opened[pos + 1:] if u not in idx]
            body = build(rows2, opened2, val2, types2, ihs2)
            cases.append((new, ihnames, body))
        splits.append(v)
        params = [paren(tr.arg(subst_json(e[2], {}), taken)) for e in pargs if not tr.is_level(e[2], taken)]
        import re as _re
        used = any(_re.search(r'(?<![\w\'])%s(?![\w\'])' % _re.escape(ih), b) for _, ihs_, b in cases for ih in ihs_)
        kinds = elim_kinds(tr, D, env)
        kind = 'ind' if used or 'cases' not in kinds else 'cases'
        cases = [''.join('%s => ' % l for l in new + (ihs_ if kind == 'ind' else [])) + b for new, ihs_, b in cases]
        return ' '.join([elim_name(names, D['name'], kind)] + params + ['(%s)' % motive]
                        + ['(%s)' % c for c in cases] + idx + [v])

    term = build(rows, opened, val, types, [])
    # the variables of the splits, in the order of their lines in the proof
    # text (inner splits before outer ones, cases left to right)
    tr.induction_on = splits
    return ''.join('%s => ' % b[1] for b in bs if not b[4]) + term
