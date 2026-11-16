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
from agdaterm import paren, short


def elim_name(names, dqname):
    """The Dedukti name of D's eliminator into propositions: ind_D_P."""
    key = dqname + '.ind-prop'
    if key not in names.dk:
        n = 'ind_%s_P' % names(dqname)
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


def eliminator(tr, names, d, env):
    """The Dedukti declaration of D's eliminator into propositions, or None."""
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
            if dom[0] == 'def' and dom[1] == d['name']:
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
    return '%s : %s.' % (elim_name(names, d['name']), ty)


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
    term = ' '.join([elim_name(names, D['name'])] + params + ['(%s)' % motive] + ['(%s)' % c for c in cases]
                    + [bs[k][1] for k in idx_pos] + [bs[i][1]])
    tr.induction_on = bs[i][1]          # the variable of the induction, for the proof texts
    return ''.join('%s => ' % b[1] for b in bs if not b[4]) + eliminate(term)
