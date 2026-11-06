#!/usr/bin/env python3
"""Selectors for the structures of a Dedukti file.

    python3 scripts/structure_selectors.py X.dk     # -> X_selectors.dk, X_selectors.dkgf

A structure is declared as a constant whose type ends in Structure, as in
test/algebra.dk:

    Magma : (S : Set) -> ({|*|} : Oper S) -> Structure.

For each argument of such a constant, a selector returns it from the
structure: it is named by the argument and the structure, and its type is
the argument's type, in which the earlier arguments are their selectors
applied to the structure:

    def S_Magma : (m : Structure) -> Set.
    [S, {|*|}] S_Magma (Magma S {|*|}) --> S.
    def {|*_Magma|} : (m : Structure) -> Oper (S_Magma m).
    [S, {|*|}] {|*_Magma|} (Magma S {|*|}) --> {|*|}.

An argument without a name is named by its position (arg5_Monoid for the
fifth argument of Monoid).  X_selectors.dk uses the constants of X.dk by
their names, so the two are checked together: the script runs dk check on
them concatenated (-no-check to skip it).

X_selectors.dkgf has an initial symbol table entry for each selector: the
argument's name as a formula ($*$, $S$, $\\mathrm{inv}(#2)$), without the
structure, so that the entries of different structures are the same
(overloaded); and an #EXCLUDE line of the selectors, whose declarations the
structure definitions already informalize.
"""

import os
import re
import subprocess
import sys
import tempfile

# ---------------------------------------------------------------- terms
# Var(name), App(fun, arg), Lam(name, type or None, body), Pi(name or None, type, body)


class Var:
    def __init__(self, name): self.name = name


class App:
    def __init__(self, fun, arg): self.fun, self.arg = fun, arg


class Lam:
    def __init__(self, name, typ, body): self.name, self.typ, self.body = name, typ, body


class Pi:
    def __init__(self, name, typ, body): self.name, self.typ, self.body = name, typ, body


TOKEN = re.compile(r'\(;.*?;\)|\{\|.*?\|\}|->|=>|-->|:=|[()\[\],.:]|[^\s()\[\],:.]+(?:\.[^\s()\[\],:.]+)?',
                   re.S)


def tokens(text):
    return [t for t in TOKEN.findall(text) if not t.startswith('(;')]


class Parser:
    """Dedukti terms as in its official grammar:
    term ::= aterm | x : aterm -> term | (x : aterm) -> term | aterm -> term
           | x => term | x : aterm => term
    aterm ::= sterm+    sterm ::= ident | ( term )"""

    def __init__(self, toks):
        self.toks, self.i = toks, 0

    def peek(self, k=0):
        return self.toks[self.i + k] if self.i + k < len(self.toks) else None

    def eat(self, t):
        if self.peek() != t:
            raise SyntaxError('expected %s, found %s at token %d' % (t, self.peek(), self.i))
        self.i += 1

    def ident(self, t):
        return t is not None and t not in ('(', ')', '[', ']', ',', '.', ':', '->', '=>', '-->', ':=')

    def term(self):
        t, t1 = self.peek(), self.peek(1)
        if self.ident(t) and t1 == ':':
            self.i += 2
            a = self.aterm()
            if self.peek() == '->':
                self.i += 1
                return Pi(t, a, self.term())
            self.eat('=>')
            return Lam(t, a, self.term())
        if self.ident(t) and t1 == '=>':
            self.i += 2
            return Lam(t, None, self.term())
        if t == '(' and self.ident(t1) and self.peek(2) == ':':
            # (x : aterm) -> term, or else a parenthesized term
            save = self.i
            self.i += 3
            a = self.aterm()
            if self.peek() == ')' and self.peek(1) == '->':
                self.i += 2
                return Pi(t1, a, self.term())
            self.i = save
        a = self.aterm()
        if self.peek() == '->':
            self.i += 1
            return Pi(None, a, self.term())
        return a

    def aterm(self):
        f = self.sterm()
        while self.peek() == '(' or (self.ident(self.peek()) and self.peek(1) not in (':', '=>')):
            f = App(f, self.sterm())
        return f

    def sterm(self):
        t = self.peek()
        if t == '(':
            self.i += 1
            e = self.term()
            self.eat(')')
            return e
        if not self.ident(t):
            raise SyntaxError('unexpected %s at token %d' % (t, self.i))
        self.i += 1
        return Var(t)


def show(e, prec=0):
    """Dedukti text; prec 0: term, 1: aterm (argument position of ->), 2: sterm."""
    if isinstance(e, Var):
        return e.name
    if isinstance(e, App):
        s = '%s %s' % (show(e.fun, 1), show(e.arg, 2))
        return '(%s)' % s if prec > 1 else s
    if isinstance(e, Lam):
        s = '%s%s => %s' % (e.name, ' : ' + show(e.typ, 1) if e.typ else '', show(e.body))
    else:
        if e.name is None:
            s = '%s -> %s' % (show(e.typ, 1), show(e.body))
        else:
            s = '(%s : %s) -> %s' % (e.name, show(e.typ, 1), show(e.body))
    return '(%s)' % s if prec > 0 else s


def subst(e, sub):
    """e with the free variables of sub replaced; the substituted terms are
    selector applications, whose only variable is the structure variable,
    which no binder of the structure's type has, so there is no capture."""
    if isinstance(e, Var):
        return sub.get(e.name, e)
    if isinstance(e, App):
        return App(subst(e.fun, sub), subst(e.arg, sub))
    inner = {k: v for k, v in sub.items() if k != e.name}
    typ = subst(e.typ, sub) if e.typ is not None else None
    return type(e)(e.name, typ, subst(e.body, inner))


def names(e, acc):
    """All the identifiers of a term."""
    if isinstance(e, Var):
        acc.add(e.name)
    elif isinstance(e, App):
        names(e.fun, acc)
        names(e.arg, acc)
    else:
        acc.add(e.name)
        if e.typ is not None:
            names(e.typ, acc)
        names(e.body, acc)
    return acc


# ---------------------------------------------------------------- the file

def declarations(text):
    """(name, type) of the static declarations c : T. of a Dedukti file."""
    out = []
    toks = tokens(text)
    i = 0
    while i < len(toks):
        # one judgement: up to the '.' outside brackets
        j, depth = i, 0
        while j < len(toks) and not (toks[j] == '.' and depth == 0):
            depth += {'(': 1, ')': -1, '[': 1, ']': -1}.get(toks[j], 0)
            j += 1
        jmt = toks[i:j]
        i = j + 1
        if len(jmt) >= 3 and jmt[1] == ':' and jmt[0] not in ('def', 'thm', 'injective', 'private'):
            p = Parser(jmt[2:])
            typ = p.term()
            if p.i != len(jmt) - 2:
                raise SyntaxError('declaration of %s not parsed to the end' % jmt[0])
            out.append((jmt[0], typ))
    return out


def telescope(typ):
    """The arguments (name or None, type) and the result of a Pi type."""
    args = []
    while isinstance(typ, Pi):
        args.append((typ.name, typ.typ))
        typ = typ.body
    return args, typ


def selector_name(field, structure):
    if field.startswith('{|') and field.endswith('|}'):
        return '{|%s_%s|}' % (field[2:-2], structure)
    return '%s_%s' % (field, structure)


def formula(field, arity):
    """The initial entry of a selector: the field's name as a formula, applied
    to the arguments after the structure (#2, #3, ...)."""
    s = field[2:-2] if field.startswith('{|') else field
    s = s.replace('_', '\\_')
    if len(s) > 1 and s.isalnum():
        s = '\\mathrm{%s}' % s
    if arity:
        s += '(%s)' % ', '.join('#%d' % k for k in range(2, arity + 2))
    return '$%s$' % s


def arity(typ):
    """The number of arguments of a function type."""
    n = 0
    while isinstance(typ, Pi):
        n += 1
        typ = typ.body
    return n


def selectors(name, typ, var):
    """The Dedukti text and the symbol table entries of a structure's selectors."""
    args, _ = telescope(typ)
    taken = names(typ, set())
    # the pattern variables: the arguments' names, or fresh ones
    pvars, fields = [], []
    for k, (x, _) in enumerate(args, 1):
        field = x if x is not None else 'arg%d' % k
        fields.append(field)
        v = x if x is not None else 'x%d' % k
        while x is None and (v in taken or v in pvars):
            v += "'"
        pvars.append(v)
    pattern = '%s %s' % (name, ' '.join(pvars)) if pvars else name
    ctx = ', '.join(pvars)
    dk, table, sub = [], [], {}
    for (x, a), field, v in zip(args, fields, pvars):
        sel = selector_name(field, name)
        styp = subst(a, sub)
        dk.append('def %s : (%s : Structure) -> %s.' % (sel, var, show(styp)))
        dk.append('[%s] %s (%s) --> %s.' % (ctx, sel, pattern, v))
        dk.append('')
        table.append((sel, formula(field, arity(styp))))
        if x is not None:
            sub[x] = App(Var(sel), Var(var))
    return dk, table


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    if len(args) != 1 or not args[0].endswith('.dk'):
        sys.exit(__doc__)
    path = args[0]
    text = open(path).read()
    decls = declarations(text)
    structures = [(n, t) for n, t in decls if isinstance(telescope(t)[1], Var)
                  and telescope(t)[1].name == 'Structure' and telescope(t)[0]]
    if not structures:
        sys.exit('no structures in %s' % path)
    # the structure variable: a name that no structure's type uses
    used = set()
    for _, t in structures:
        names(t, used)
    var = 'm'
    while var in used or var in {n for n, _ in decls}:
        var += "'"
    base = path[:-3]
    dk = ['(; selectors of the structures of %s, by scripts/structure_selectors.py ;)' % os.path.basename(path), '']
    table, sels = [], []
    for n, t in structures:
        d, tb = selectors(n, t, var)
        dk += ['(; %s ;)' % n] + d
        table += tb
        sels += [s for s, _ in tb]
    open(base + '_selectors.dk', 'w').write('\n'.join(dk))
    with open(base + '_selectors.dkgf', 'w') as f:
        f.write('(; selectors of the structures of %s: their declarations are not shown,\n'
                '   as the structure definitions informalize them ;)\n' % os.path.basename(path))
        f.write('#EXCLUDE %s\n\n' % ' '.join(sels))
        for s, e in table:
            f.write('%s : %s\n' % (s, e))
    print('%d structures, %d selectors -> %s_selectors.dk, %s_selectors.dkgf'
          % (len(structures), len(sels), base, base))
    if '-no-check' not in sys.argv:
        # X_selectors.dk uses the constants of X.dk unqualified: checked together
        with tempfile.TemporaryDirectory() as d:
            both = os.path.join(d, 'selectors_check.dk')
            open(both, 'w').write(text + '\n' + '\n'.join(dk) + '\n')
            r = subprocess.run(['dk', 'check', both], capture_output=True, text=True)
            out = (r.stdout + r.stderr).strip()
            print('dk check %s + %s_selectors.dk: %s' % (path, base, out.split('\n')[-1] if out else r.returncode))
            if r.returncode:
                sys.exit(1)


if __name__ == '__main__':
    main()
