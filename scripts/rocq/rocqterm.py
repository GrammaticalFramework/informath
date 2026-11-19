"""Coq terms as Rocq prints them with Set Printing All, and their translation
to Informath's Dedukti dialect.

The parser reads the fully elaborated terms of `Check`, `Print` and `Show`
(every implicit argument written with @, arrows as forall (_ : A), B).
The translator needs the types of the constants (Env) and of the variables
in scope (a context), to tell propositions from sets: a Coq forall over a
proposition is an implication, over a set a quantifier.

Encoding, as in mathlib-informath's Export.lean where it applies:

* a type is a Set, its elements Elem A, a proposition a Prop, its proofs Proof A;
* a function type in a binder is a Dedukti arrow, (f : (Elem A -> Elem B)),
  in a set position (a quantifier's domain) the set function A B;
* a type defined as T -> Prop that the project lists in PREDICATE_SETS (kgah's
  bset, the truth values, cond -> Prop) is a set of its own: D s is
  bmem D s ("s is in D"), fun s => P at the type is setof (s => P);
* fun x => match x with ... in a Fixpoint becomes rewrite rules (rules());
  any other match on a boolean or a decision is ite;
* the constants of the standard library are mapped to Informath's base
  (eq to Eq / Eqt, le to Leq, ...) or to the constants of base-coq.dk.
"""
import re

KEYWORDS = {'forall', 'fun', 'fix', 'match', 'with', 'end', 'return', 'as', 'in', 'let', 'struct'}
TOKEN = re.compile(r"\s*(=>|:=|->|[(){}|,:;@]|[A-Za-z_][A-Za-z0-9_'.]*|[0-9]+)")


def tokenize(s):
    out, i = [], 0
    s = s.rstrip()
    while i < len(s):
        m = TOKEN.match(s, i)
        if not m:
            if s[i:].strip() in ('', '.'):
                break
            raise SyntaxError('cannot tokenize at: %r' % s[i:i + 40])
        tok = m.group(1)
        # a trailing period of a sentence, not part of a name
        if tok.endswith('.') and len(tok) > 1:
            tok = tok[:-1]
            i = m.end() - 1
        else:
            i = m.end()
        out.append(tok)
    return out


# ------------------------------------------------------------------ syntax

class T:
    """A term: kind is var (a name), app, pi, lam, match, fix, sort, let."""
    def __init__(self, kind, **kw):
        self.kind = kind
        self.__dict__.update(kw)

    def __repr__(self):
        return show(self)


def Var(n):
    return T('var', name=n)


def App(f, args):
    if not args:
        return f
    if f.kind == 'app':
        return T('app', f=f.f, args=f.args + list(args))
    return T('app', f=f, args=list(args))


def show(t):
    k = t.kind
    if k == 'var':
        return t.name
    if k == 'app':
        return '(' + ' '.join(show(x) for x in [t.f] + t.args) + ')'
    if k == 'pi':
        return '(forall (%s : %s), %s)' % (t.name, show(t.dom), show(t.body))
    if k == 'lam':
        return '(fun (%s : %s) => %s)' % (t.name, show(t.dom), show(t.body))
    if k == 'sort':
        return t.name
    if k == 'match':
        return '(match %s with %s end)' % (show(t.scrut), ' | '.join('%s => %s' % (show(p), show(b)) for p, b in t.branches))
    if k == 'let':
        return '(let %s := %s in %s)' % (t.name, show(t.val), show(t.body))
    if k == 'fix':
        return '(fix %s ...)' % t.name
    return '?'


class Parser:
    def __init__(self, s):
        self.toks = tokenize(s) if isinstance(s, str) else s
        self.i = 0

    def peek(self, k=0):
        return self.toks[self.i + k] if self.i + k < len(self.toks) else None

    def next(self):
        t = self.peek()
        self.i += 1
        return t

    def expect(self, t):
        got = self.next()
        if got != t:
            raise SyntaxError('expected %s, got %s at %d: %s' % (t, got, self.i, ' '.join(self.toks[max(0, self.i - 8):self.i + 8])))

    def term(self):
        t = self.peek()
        if t == 'forall':
            self.next()
            bs = self.binders(',')
            body = self.term()
            for n, a in reversed(bs):
                body = T('pi', name=n, dom=a, body=body)
            return body
        if t == 'fun':
            self.next()
            bs = self.binders('=>')
            body = self.term()
            for n, a in reversed(bs):
                body = T('lam', name=n, dom=a, body=body)
            return body
        if t == 'fix':
            self.next()
            name = self.next()
            bs = []
            while self.peek() == '(' or self.peek() == '{' and self.peek(1) != 'struct':
                bs += self.paren_binder()
            struct = None
            if self.peek() == '{':
                self.next()
                self.expect('struct')
                struct = self.next()
                self.expect('}')
            self.expect(':')
            ret = self.term()
            self.expect(':=')
            body = self.term()
            return T('fix', name=name, binders=bs, struct=struct, ret=ret, body=body)
        if t == 'let':
            self.next()
            name = self.next()
            ty = None
            if self.peek() == ':':
                self.next()
                ty = self.term()
            self.expect(':=')
            val = self.term()
            self.expect('in')
            body = self.term()
            return T('let', name=name, ty=ty, val=val, body=body)
        return self.app()

    def binders(self, stop):
        """(x y : A) (z : B) ... or x y : A, up to the stop token (consumed)."""
        bs = []
        if self.peek() in ('(', '{'):
            while self.peek() in ('(', '{'):
                bs += self.paren_binder()
        else:
            names = []
            while self.peek() not in (':', stop):
                if self.peek() is None:
                    raise SyntaxError('unfinished binder')
                names.append(self.next())
            if self.peek() == ':':
                self.next()
                a = self.term()
            else:
                a = None
            bs = [(n, a) for n in names]
        self.expect(stop)
        return bs

    def paren_binder(self):
        close = ')' if self.next() == '(' else '}'
        names = []
        while self.peek() != ':':
            if self.peek() is None:
                raise SyntaxError('unfinished binder')
            names.append(self.next())
        self.next()
        a = self.term()
        self.expect(close)
        return [(n, a) for n in names]

    def app(self):
        f = self.atom()
        args = []
        while True:
            t = self.peek()
            if t is None or t in (')', ',', '=>', ':=', '|', ':', ';', '}', '{', 'with', 'end', 'return', 'as', 'in') \
                    or t in ('forall', 'fun', 'fix', 'let'):
                break
            args.append(self.atom())
        return App(f, args)

    def atom(self):
        t = self.next()
        if t == '(':
            x = self.term()
            self.expect(')')
            return x
        if t == '@':
            return Var(self.next())
        if t in ('Prop', 'Set', 'Type'):
            return T('sort', name=t)
        if t == 'match':
            scrut = self.term()
            asn = None
            if self.peek() == 'as':
                self.next()
                asn = self.next()
            if self.peek() == 'in':
                self.next()
                self.app()
            if self.peek() == 'return':
                self.next()
                self.term()
            self.expect('with')
            branches = []
            while self.peek() == '|':
                if self.peek() is None:
                    break
                self.next()
                p = self.app()
                self.expect('=>')
                b = self.term()
                branches.append((p, b))
            self.expect('end')
            return T('match', scrut=scrut, branches=branches, asn=asn)
        if t is None or t in KEYWORDS - {'struct'} and t not in ('in',):
            raise SyntaxError('unexpected %s at %d: %s' % (t, self.i, ' '.join(self.toks[max(0, self.i - 8):self.i + 8])))
        return Var(t)


def parse(s):
    p = Parser(s)
    t = p.term()
    if p.peek() is not None:
        raise SyntaxError('trailing tokens: %s' % ' '.join(p.toks[p.i:p.i + 10]))
    return t


# ------------------------------------------------------------------ terms

def free_vars(t, bound=frozenset()):
    k = t.kind
    if k == 'var':
        return set() if t.name in bound else {t.name}
    if k == 'app':
        out = free_vars(t.f, bound)
        for a in t.args:
            out |= free_vars(a, bound)
        return out
    if k in ('pi', 'lam'):
        return (free_vars(t.dom, bound) if t.dom else set()) | free_vars(t.body, bound | {t.name})
    if k == 'let':
        return free_vars(t.val, bound) | free_vars(t.body, bound | {t.name})
    if k == 'match':
        out = free_vars(t.scrut, bound)
        for p, b in t.branches:
            out |= free_vars(b, bound | pattern_vars(p))
        return out
    return set()


def pattern_vars(p):
    if p.kind == 'var':
        return set() if p.name == '_' or is_constructor_name(p.name) else {p.name}
    if p.kind == 'app':
        out = set()
        for a in p.args:
            out |= pattern_vars(a)
        return out
    return set()


CONSTRUCTORS = set()          # filled by Env: the constructors of inductive types
# the record types of the exported file, read as Informath's structures: a
# variable of a record type R has the type Str R ("let G be a group")
STRUCTURES = set()


def is_constructor_name(n):
    return n in CONSTRUCTORS or n in ('true', 'false', 'O', 'S', 'nil', 'cons', 'pair', 'Some', 'None', 'left', 'right', 'tt')


def beta(t):
    """t with its beta-redexes (fun x => b) a reduced, as Rocq leaves them in
    the hypotheses that `pose proof` makes from a lemma applied to a lambda."""
    k = t.kind
    if k == 'app':
        f, args = beta(t.f), [beta(a) for a in t.args]
        while f.kind == 'lam' and args:
            f = beta(subst(f.body, {f.name: args[0]}))
            args = args[1:]
        return App(f, args) if args else f
    if k in ('lam', 'pi'):
        return T(k, name=t.name, dom=beta(t.dom), body=beta(t.body))
    return t


def subst(t, s):
    """t with the variables of s (a dict name -> term) replaced, capture ignored
    (Coq's printed names are distinct enough for the terms of these files)."""
    if not s:
        return t
    k = t.kind
    if k == 'var':
        return s.get(t.name, t)
    if k == 'app':
        return App(subst(t.f, s), [subst(a, s) for a in t.args])
    if k in ('pi', 'lam'):
        s2 = {x: v for x, v in s.items() if x != t.name}
        return T(k, name=t.name, dom=subst(t.dom, s) if t.dom else None, body=subst(t.body, s2))
    if k == 'let':
        s2 = {x: v for x, v in s.items() if x != t.name}
        return T('let', name=t.name, ty=t.ty, val=subst(t.val, s), body=subst(t.body, s2))
    if k == 'match':
        return T('match', scrut=subst(t.scrut, s), asn=t.asn,
                 branches=[(p, subst(b, {x: v for x, v in s.items() if x not in pattern_vars(p)})) for p, b in t.branches])
    return t


def head(t):
    return (t.f, t.args) if t.kind == 'app' else (t, [])


def short(name):
    return name.split('.')[-1]


# ------------------------------------------------------------------ environment

class Env:
    """The constants: their Coq types, the bodies of the type aliases, the
    implicit arguments."""
    def __init__(self):
        self.types = {}
        self.aliases = {}       # name -> body, for definitions of types (cond, bset, SE)
        self.dk = {}            # Coq name -> Dedukti name
        self.user = set()       # the constants of the exported file
        for n, ty in STDLIB_TYPES.items():
            self.types[n] = parse(ty)

    def typeof_const(self, n):
        if n in self.types:
            return self.types[n]
        if short(n) in self.types:
            return self.types[short(n)]
        return None


STDLIB_TYPES = {
    'nat': 'Set', 'bool': 'Set', 'unit': 'Set',
    'list': 'forall (A : Type), Type', 'option': 'forall (A : Type), Type',
    'prod': 'forall (A : Type) (B : Type), Type',
    'sig': 'forall (A : Type) (P : forall _ : A, Prop), Type',
    'proj1_sig': 'forall (A : Type) (P : forall _ : A, Prop) (_ : sig A P), A',
    'exist': 'forall (A : Type) (P : forall _ : A, Prop) (x : A) (_ : P x), sig A P',
    'eq': 'forall (A : Type) (_ : A) (_ : A), Prop',
    'not': 'forall _ : Prop, Prop', 'and': 'forall (_ : Prop) (_ : Prop), Prop',
    'or': 'forall (_ : Prop) (_ : Prop), Prop', 'iff': 'forall (_ : Prop) (_ : Prop), Prop',
    'ex': 'forall (A : Type) (P : forall _ : A, Prop), Prop',
    'True': 'Prop', 'False': 'Prop', 'I': 'True',
    'le': 'forall (_ : nat) (_ : nat), Prop', 'lt': 'forall (_ : nat) (_ : nat), Prop',
    'ge': 'forall (_ : nat) (_ : nat), Prop', 'gt': 'forall (_ : nat) (_ : nat), Prop',
    'In': 'forall (A : Type) (_ : A) (_ : list A), Prop',
    'O': 'nat', 'S': 'forall _ : nat, nat',
    'Some': 'forall (A : Type) (_ : A), option A', 'None': 'forall (A : Type), option A',
    'nil': 'forall (A : Type), list A', 'cons': 'forall (A : Type) (_ : A) (_ : list A), list A',
    'pair': 'forall (A : Type) (B : Type) (_ : A) (_ : B), prod A B',
    'true': 'bool', 'false': 'bool', 'tt': 'unit',
    'andb': 'forall (_ : bool) (_ : bool), bool', 'orb': 'forall (_ : bool) (_ : bool), bool',
    'negb': 'forall _ : bool, bool',
    'eqb': 'forall (_ : nat) (_ : nat), bool', 'ltb': 'forall (_ : nat) (_ : nat), bool',
    'leb': 'forall (_ : nat) (_ : nat), bool',
    'max': 'forall (_ : nat) (_ : nat), nat', 'min': 'forall (_ : nat) (_ : nat), nat',
    'add': 'forall (_ : nat) (_ : nat), nat', 'mul': 'forall (_ : nat) (_ : nat), nat',
    'sub': 'forall (_ : nat) (_ : nat), nat',
    'map': 'forall (A : Type) (B : Type) (f : forall _ : A, B) (_ : list A), list B',
    'app': 'forall (A : Type) (_ : list A) (_ : list A), list A',
}

# Coq constants (by their last name component) -> Dedukti, with the number of
# leading (implicit) type arguments dropped
STDLIB_DK = {
    'nat': ('Nat', 0), 'bool': ('bool', 0), 'list': ('List', 0), 'option': ('Option', 0),
    'prod': ('prod', 0), 'unit': ('unit', 0),
    'not': ('not', 0), 'and': ('and', 0), 'or': ('or', 0), 'iff': ('iff', 0),
    'False': ('false', 0), 'True': ('truth', 0),
    'le': ('Leq', 0), 'lt': ('Lt', 0), 'ge': ('Geq', 0), 'gt': ('Gt', 0),
    'In': ('In', 0),
    'O': ('0', 0), 'Some': ('Some', 1), 'None': ('None', 1), 'nil': ('lnil', 1), 'cons': ('lcons', 1),
    'pair': ('pair', 2), 'true': ('btrue', 0), 'false': ('bfalse', 0), 'tt': ('tt', 0),
    'andb': ('andb', 0), 'orb': ('orb', 0), 'negb': ('negb', 0),
    'eqb': ('eqb', 0), 'ltb': ('ltb', 0), 'leb': ('leb', 0),
    'eq_dec': ('eqdec', 0), 'max': ('max', 0), 'min': ('min', 0), 'pred': ('pred', 0), 'add': ('plus', 0), 'mul': ('times', 0),
    'sub': ('minus', 0),
    'map': ('map', 2), 'app': ('lappend', 1),
}

# the project's tables (its config.py, read by project.py)
import project
# constants whose function arguments are bound variables and bodies of a
# binder notation (raw macros in the symbol table): name -> position of that
# argument, or a tuple of positions; eta-expanded, f to x => f x
BINDER_CONSTS = project.get('BINDER_CONSTS', {})
# the arguments of constants that the readings leave out (kgb: type levels)
DROP = project.get('DROP', {})
# the constants whose level argument n stands for the index n+1 of an article
SHIFT1 = project.get('SHIFT1', {})
# record fields whose application to the record alone is the record (kgah: rv x is x)
VALUE_FIELDS = set(project.get('VALUE_FIELDS', ()))
# types defined as T -> Prop read as sets with membership (kgah: bset)
PREDICATE_SETS = set(project.get('PREDICATE_SETS', ()))

# Informath base constants that Coq names would clash with
BASE_NAMES = set('''0 1 2 3 4 5 6 7 8 9 Cardinal Circle Complex Dig Elem Enum Eq Eqt Equ Geq Gt Int Leq List Lt Nat
Neq Num Polynomial Proof Prop Rat Real Set Vector abs and angleBetween arccos arcsin arctan area binomial bool
cardinality cartesian combinations combinationsFromSet complement congruent cons cos degree denumerable difference
disjoint div divide divisible divisor dotProduct element empty emptyset enum123 enumset equalset even exists
factorial false finite forall fst function gcd if iff infinite infinity inhabited integral intersection irrational
isRoot legendre length log member minus nd neg negative negativePart ni nil nn norm not notelement odd or orthogonal
pi plus positive positivePart pow powerset prime radius rational sigma sin sqrt square subset subseteq succ suchthat
superset superseteq tan times union universeset vectorPlus'''.split())


class Translator:
    """Coq terms to Dedukti, in the context of the constants of env."""
    def __init__(self, env, prefix=''):
        self.env = env
        self.fresh_count = 0

    # ---- names
    def const_name(self, n):
        if n in self.env.dk:
            return self.env.dk[n]
        s = short(n)
        if s in STDLIB_DK and n not in self.env.user:
            return STDLIB_DK[s][0]
        return n.replace('.', '__')

    def var_name(self, n, ctx):
        return n

    def fresh(self, base, ctx):
        i = 0
        while True:
            n = base if i == 0 else '%s%d' % (base, i)
            if n not in ctx and n not in self.env.types:
                return n
            i += 1

    # ---- types
    def whnf(self, ty):
        """Unfold aliases at the head until a pi or a sort or a constant that is not an alias."""
        for _ in range(10):
            h, args = head(ty)
            if h.kind == 'var' and h.name in self.env.aliases:
                params, body = self.env.aliases[h.name]
                if len(params) <= len(args):
                    ty = App(subst(body, dict(zip(params, args[:len(params)]))), args[len(params):])
                    continue
            return ty
        return ty

    def typeof(self, t, ctx):
        k = t.kind
        if k == 'var':
            if t.name in ctx:
                return ctx[t.name]
            ty = self.env.typeof_const(t.name)
            return ty
        if k == 'sort':
            return T('sort', name='Type')
        if k == 'app':
            ty = self.typeof(t.f, ctx)
            for a in t.args:
                if ty is None:
                    return None
                ty = self.whnf(ty)
                if ty.kind != 'pi':
                    return None
                ty = subst(ty.body, {ty.name: a})
            return ty
        if k == 'pi':
            s = self.sort(t.body, dict(ctx, **{t.name: t.dom}))
            return T('sort', name='Prop' if s == 'Prop' else 'Type')
        if k == 'lam':
            b = self.typeof(t.body, dict(ctx, **{t.name: t.dom}))
            return T('pi', name=t.name, dom=t.dom, body=b) if b is not None else None
        if k == 'match':
            for p, b in t.branches:
                ty = self.typeof(b, dict(ctx, **{v: Var('?') for v in pattern_vars(p)}))
                if ty is not None and not (ty.kind == 'var' and ty.name == '?'):
                    return ty
            return None
        if k == 'let':
            return self.typeof(t.body, dict(ctx, **{t.name: t.ty or self.typeof(t.val, ctx)}))
        return None

    def sort(self, ty, ctx):
        """Kind (ty is a sort), Prop (ty is a proposition), Type (a set) or None."""
        if ty is None:
            return None
        if ty.kind == 'sort':
            return 'Kind'
        if ty.kind == 'pi':
            s = self.sort(ty.body, dict(ctx, **{ty.name: ty.dom}))
            return 'Kind' if s == 'Kind' else s
        tt = self.typeof(ty, ctx)
        if tt is None:
            return None
        tt = self.whnf(tt)
        if tt.kind == 'sort':
            return 'Prop' if tt.name == 'Prop' else 'Type'
        if tt.kind == 'pi':
            return 'Kind'
        return None

    def is_bset(self, ty):
        """Whether ty is one of the project's PREDICATE_SETS (kgah: bset)."""
        return ty is not None and ty.kind == 'var' and ty.name in PREDICATE_SETS

    # ---- translation
    def binder_type(self, a, ctx):
        """The Dedukti type of a variable of the Coq type a."""
        if a.kind == 'sort':
            return 'Prop' if a.name == 'Prop' else 'Set'
        if a.kind == 'pi':
            s = self.sort(a, ctx)
            if s == 'Prop':
                return 'Proof (%s)' % self.prop(a, ctx)
            dom = self.binder_type(a.dom, ctx)
            ctx2 = dict(ctx, **{a.name: a.dom})
            cod = self.binder_type(a.body, ctx2)
            if a.name != '_' and a.name in free_vars(a.body):
                return '((%s : %s) -> %s)' % (a.name, dom, cod)
            return '(%s -> %s)' % (dom, cod)
        s = self.sort(a, ctx)
        if s == 'Prop':
            return 'Proof (%s)' % self.prop(a, ctx)
        if a.kind == 'var' and a.name in STRUCTURES:
            return 'Str %s' % self.env.dk.get(a.name, a.name)
        if a.kind == 'app' and a.f.kind == 'var' and a.f.name in STRUCTURES:
            # a structure with parameters: Str (LeftModule K)
            return 'Str (%s)' % ' '.join([self.env.dk.get(a.f.name, a.f.name)] + [paren(self.term(x, ctx)) for x in a.args])
        return 'Elem %s' % paren(self.set(a, ctx))

    def set(self, a, ctx):
        """A Coq type in a set position."""
        if a.kind == 'sort':
            return 'Prop' if a.name == 'Prop' else 'Set'
        if a.kind == 'pi':
            ctx2 = dict(ctx, **{a.name: a.dom})
            if self.sort(a.body, ctx2) == 'Kind' and a.body.kind == 'sort' and a.body.name == 'Prop':
                return 'function %s Prop' % paren(self.set(a.dom, ctx))
            return 'function %s %s' % (paren(self.set(a.dom, ctx)), paren(self.set(a.body, ctx2)))
        h, args = head(a)
        if h.kind == 'var' and short(h.name) == 'sig' and len(args) == 2:
            return 'suchthat %s %s' % (paren(self.set(args[0], ctx)), paren(self.fun_arg(args[1], args[0], ctx, prop=True)))
        return self.term(a, ctx)

    def fun_arg(self, f, dom, ctx, prop=False):
        """A function argument x => body, eta-expanded if f is not a lambda."""
        if f.kind == 'lam':
            ctx2 = dict(ctx, **{f.name: f.dom})
            body = self.prop(f.body, ctx2) if prop else self.term(f.body, ctx2)
            return '%s => %s' % (f.name, body)
        x = self.fresh('x', ctx)
        ctx2 = dict(ctx, **{x: dom})
        body = App(f, [Var(x)])
        return '%s => %s' % (x, self.prop(body, ctx2) if prop else self.term(body, ctx2))

    def prop(self, p, ctx):
        k = p.kind
        if k == 'pi':
            ctx2 = dict(ctx, **{p.name: p.dom})
            s = self.sort(p.dom, ctx)
            if s == 'Prop' and (p.name == '_' or p.name not in free_vars(p.body)):
                return 'if %s %s' % (paren(self.prop(p.dom, ctx)), paren(self.prop(p.body, ctx2)))
            name = p.name if p.name != '_' else self.fresh('x', ctx)
            if re.fullmatch(r'H\d*', name):       # a name Coq made for an anonymous binder
                name = self.fresh('n' if p.dom.kind == 'var' and p.dom.name == 'nat' else 'x', ctx)
                body = subst(p.body, {p.name: Var(name)})
                p = T('pi', name=name, dom=p.dom, body=body)
            if name != p.name:
                ctx2 = dict(ctx, **{name: p.dom})
            else:
                ctx2 = dict(ctx, **{name: p.dom})
            return 'forall %s (%s => %s)' % (paren(self.set(p.dom, ctx)), name, self.prop(p.body, ctx2))
        h, args = head(p)
        if h.kind == 'var' and h.name not in ctx:
            s = short(h.name)
            if s == 'eq' and len(args) == 3 and h.name not in self.env.dk:
                a = self.whnf(args[0])
                if a.kind == 'var' and a.name == 'nat':
                    return 'Eq %s %s' % (paren(self.term(args[1], ctx)), paren(self.term(args[2], ctx)))
                return 'Eqt %s %s %s' % (paren(self.set(args[0], ctx)), paren(self.term(args[1], ctx)), paren(self.term(args[2], ctx)))
            if s == 'ex' and len(args) == 2:
                return 'exists %s (%s)' % (paren(self.set(args[0], ctx)), self.fun_arg(args[1], args[0], ctx, prop=True))
            if s in ('not', 'and', 'or', 'iff') and h.name not in self.env.user:
                return ' '.join([STDLIB_DK[s][0]] + [paren(self.prop(a, ctx)) for a in args])
            if s == 'In' and len(args) == 3:
                return 'In %s %s %s' % (paren(self.set(args[0], ctx)), paren(self.term(args[1], ctx)), paren(self.term(args[2], ctx)))
        return self.term(p, ctx)

    def term(self, t, ctx, expected=None):
        k = t.kind
        if k == 'var':
            if t.name in ctx:
                return t.name
            s = short(t.name)
            if s == 'O' and t.name not in self.env.user:
                return '0'
            return self.const_name(t.name)
        if k == 'sort':
            return 'Prop' if t.name == 'Prop' else 'Set'
        if k == 'pi':
            if self.sort(t, ctx) == 'Prop':
                return self.prop(t, ctx)
            return self.set(t, ctx)
        if k == 'lam':
            if t.name == '_':          # an unused variable, named for the readings
                t = T('lam', name=self.fresh('k', ctx), dom=t.dom, body=t.body)
            ctx2 = dict(ctx, **{t.name: t.dom})
            if self.is_bset(expected):
                return 'setof (%s => %s)' % (t.name, self.prop(t.body, ctx2))
            exp = self.whnf(expected) if expected is not None else None
            cod = None
            if exp is not None and exp.kind == 'pi':
                cod = subst(exp.body, {exp.name: Var(t.name)})
            if self.is_prop_term(t.body, ctx2):
                body = self.prop(t.body, ctx2)
            else:
                body = self.term(t.body, ctx2, cod)
            return '%s => %s' % (t.name, body)
        if k == 'match':
            return self.match_term(t, ctx)
        if k == 'let':
            return self.term(subst(t.body, {t.name: t.val}), ctx, expected)
        if k == 'app':
            return self.app(t, ctx)
        return '?'

    def is_prop_term(self, t, ctx):
        """Whether t is a proposition (its type is Prop)."""
        if t.kind == 'pi':
            return self.sort(t, ctx) == 'Prop'
        ty = self.typeof(t, ctx)
        if ty is None:
            return False
        ty = self.whnf(ty)
        return ty.kind == 'sort' and ty.name == 'Prop'

    def app(self, t, ctx):
        h, args = head(t)
        if h.kind == 'var' and h.name not in ctx and h.name not in self.env.dk:
            s = short(h.name)
            if s in ('eq', 'ex', 'not', 'and', 'or', 'iff', 'In') and h.name not in self.env.user:
                return self.prop(t, ctx)
            if s == 'S' and len(args) == 1 and h.name not in self.env.user:
                # S (S ... x): a numeral, or x + k
                k, base = 1, args[0]
                while True:
                    hb, ab = head(base)
                    if hb.kind == 'var' and short(hb.name) == 'S' and len(ab) == 1 and hb.name not in self.env.user:
                        k, base = k + 1, ab[0]
                    else:
                        break
                if base.kind == 'var' and short(base.name) == 'O' and base.name not in self.env.user:
                    return str(k) if k < 10 else 'plus %s %d' % (paren(self.term(base, ctx)), k)
                return 'plus %s %d' % (paren(self.term(base, ctx)), k)
            if s == 'proj1_sig' and len(args) == 3:          # a coercion
                return self.term(args[2], ctx)
            if s == 'pair' and len(args) == 4 and h.name not in self.env.user:
                a, b = args[2], args[3]
                ha, aa = head(a)
                if ha.kind == 'var' and short(ha.name) == 'pair' and len(aa) == 4:
                    return 'triple %s %s %s' % (paren(self.term(aa[2], ctx)), paren(self.term(aa[3], ctx)), paren(self.term(b, ctx)))
                return 'pair %s %s' % (paren(self.term(a, ctx)), paren(self.term(b, ctx)))
            if s == 'sig':
                return self.set(t, ctx)
            if s == 'map' and len(args) == 4 and args[2].kind != 'lam' and h.name not in self.env.user:
                x = self.fresh('x', ctx)
                args = args[:2] + [T('lam', name=x, dom=args[0], body=App(args[2], [Var(x)]))] + args[3:]
            if s in STDLIB_DK and h.name not in self.env.user:
                name, drop = STDLIB_DK[s]
                return ' '.join([name] + [paren(self.arg(a, None, ctx)) for a in args[drop:]])
        # a record's value function without its argument, rv y, is the object y itself, as in the article
        # (the field selector, not a bound variable of that name: the
        # constructor's type forall rv, (forall n, Reg (rv n)) -> ...)
        if h.kind == 'var' and h.name in VALUE_FIELDS and h.name not in ctx and len(args) == 1:
            return self.term(args[0], ctx)
        # the generating functions and their companions, indexed as in the
        # article: FA F1 n is F_(n+1)
        if h.kind == 'var' and h.name in SHIFT1 and h.name in self.env.user and len(args) > SHIFT1[h.name]:
            i = SHIFT1[h.name]
            args = args[:i] + [App(Var('S'), [args[i]])] + args[i + 1:]
        # application with the types of the arguments known: values of PREDICATE_SETS applied are memberships
        ty = self.typeof(h, ctx)
        out = self.term(h, ctx)
        binder = BINDER_CONSTS.get(h.name) if h.kind == 'var' else None
        # the arguments that the readings leave out (type levels, parameters),
        # and the arity: the arguments beyond it are applications of the value
        user = h.kind == 'var' and h.name in self.env.user and h.name not in ctx
        drop = DROP.get(h.name, ()) if user else ()
        arity = None
        if user:
            cty, arity = self.env.typeof_const(h.name), 0
            while cty is not None and cty.kind == 'pi':
                arity, cty = arity + 1, cty.body
        for i, a in enumerate(args):
            # a value of a PREDICATE_SETS type (kgah's truth values) applied: membership, s is in D
            if self.is_bset(ty):
                out = 'bmem %s %s' % (paren(out), paren(self.term(a, ctx)))
                ty = T('sort', name='Prop')
                continue
            if arity is not None and i == arity:
                out = paren(out)
            if arity is not None and i >= arity:
                w = self.whnf(ty) if ty is not None else None
                exp = w.dom if w is not None and w.kind == 'pi' else None
                out = 'ap %s %s' % (paren(out), paren(self.arg(a, exp, ctx)))
                ty = subst(w.body, {w.name: a}) if w is not None and w.kind == 'pi' else None
                continue
            if i in drop:
                w = self.whnf(ty) if ty is not None else None
                ty = subst(w.body, {w.name: a}) if w is not None and w.kind == 'pi' else None
                continue
            if binder is not None and i in (binder if isinstance(binder, tuple) else (binder,)) and a.kind != 'lam':
                w0 = self.whnf(ty) if ty is not None else None
                dom = self.whnf(w0.dom) if w0 is not None and w0.kind == 'pi' else None
                if dom is not None and dom.kind == 'pi' and self.sort(dom, ctx) != 'Prop':
                    x = self.fresh('n' if dom.dom.kind == 'var' and dom.dom.name == 'nat' else 'x', ctx)
                    a = T('lam', name=x, dom=dom.dom, body=App(a, [Var(x)]))
            w = self.whnf(ty) if ty is not None else None
            exp = w.dom if w is not None and w.kind == 'pi' else None
            out = '%s %s' % (out, paren(self.arg(a, exp, ctx)))
            ty = subst(w.body, {w.name: a}) if w is not None and w.kind == 'pi' else None
        return out

    def arg(self, a, expected, ctx):
        if a.kind == 'lam':
            return self.term(a, ctx, expected)
        if self.is_prop_term(a, ctx):
            return self.prop(a, ctx)
        if expected is not None and expected.kind == 'sort' or a.kind == 'pi':
            return self.set(a, ctx)
        return self.term(a, ctx)

    def match_term(self, t, ctx):
        """A match outside a Fixpoint: if-then-else on a boolean or a decision;
        a tuple taken apart by its projections."""
        bs = t.branches
        conds = {short(head(p)[0].name): b for p, b in bs if head(p)[0].kind == 'var'}
        # the condition as a boolean term (andb, eqb, eq_dec), which a formula shows as it is
        if set(conds) == {'true', 'false'}:
            return 'ite %s %s %s' % (paren(self.term(t.scrut, ctx)), paren(self.term(conds['true'], ctx)),
                                     paren(self.term(conds['false'], ctx)))
        if set(conds) == {'left', 'right'}:
            return 'ite %s %s %s' % (paren(self.term(t.scrut, ctx)), paren(self.term(conds['left'], ctx)),
                                     paren(self.term(conds['right'], ctx)))
        if set(conds) == {'Some', 'None'}:
            # match o with Some x => A | None => B end: optcase o (x => A) B;
            # o, else B where A is Some x (optElse)
            sp = [p for p, b in bs if short(head(p)[0].name) == 'Some'][0]
            x = head(sp)[1][-1]
            sb, sargs = head(conds['Some'])
            if sb.kind == 'var' and short(sb.name) == 'Some' and sargs and sargs[-1].kind == 'var' \
                    and sargs[-1].name == x.name:
                return 'optElse %s %s' % (paren(self.term(t.scrut, ctx)), paren(self.term(conds['None'], ctx)))
            sty = self.typeof(t.scrut, ctx)
            dom = head(self.whnf(sty))[1][0] if sty is not None and head(self.whnf(sty))[1] else Var('nat')
            ctx2 = dict(ctx, **{x.name: dom})
            return 'optcase %s (%s => %s) %s' % (paren(self.term(t.scrut, ctx)), x.name,
                                                 self.term(conds['Some'], ctx2), paren(self.term(conds['None'], ctx)))
        if len(bs) == 1 and short(head(bs[0][0])[0].name) == 'pair':
            p, b = bs[0]
            _, pargs = head(p)
            x, y = pargs[-2], pargs[-1]
            s = {}
            if x.kind == 'var' and x.name != '_':
                s[x.name] = App(Var('pr1'), [t.scrut])
            if y.kind == 'var' and y.name != '_':
                s[y.name] = App(Var('pr2'), [t.scrut])
            return self.term(subst(b, s), ctx)
        return 'match_ %s' % paren(self.term(t.scrut, ctx))

    def bool_prop(self, b, ctx):
        """A boolean expression as the proposition that it is true."""
        h, args = head(b)
        if h.kind == 'var' and h.name not in ctx:
            s = short(h.name)
            if s == 'andb':
                return 'and %s %s' % (paren(self.bool_prop(args[0], ctx)), paren(self.bool_prop(args[1], ctx)))
            if s == 'orb':
                return 'or %s %s' % (paren(self.bool_prop(args[0], ctx)), paren(self.bool_prop(args[1], ctx)))
            if s == 'negb':
                return 'not %s' % paren(self.bool_prop(args[0], ctx))
            if s == 'eqb':
                return 'Eq %s %s' % (paren(self.term(args[0], ctx)), paren(self.term(args[1], ctx)))
            if s == 'ltb':
                return 'Lt %s %s' % (paren(self.term(args[0], ctx)), paren(self.term(args[1], ctx)))
            if s == 'leb':
                return 'Leq %s %s' % (paren(self.term(args[0], ctx)), paren(self.term(args[1], ctx)))
        return 'Eqt bool %s btrue' % paren(self.term(b, ctx))

    def decision_prop(self, d, ctx):
        h, args = head(d)
        if h.kind == 'var' and short(h.name) == 'eq_dec' and len(args) == 2:
            return 'Eq %s %s' % (paren(self.term(args[0], ctx)), paren(self.term(args[1], ctx)))
        return 'decides %s' % paren(self.term(d, ctx))

    # ---- declarations
    def decl_type(self, ty, ctx=None, names=None):
        """A Coq type as a Dedukti declaration type: its leading binders as
        Dedukti binders, (x : Elem A) -> (h : Proof P) -> ... -> Proof C.
        names: binder names to use (those of the definiens' lambdas)."""
        ctx = dict(ctx or {})
        parts, i, hyp = [], 0, 0
        while ty.kind == 'pi':
            name = ty.name
            if names and i < len(names) and names[i] != '_':
                if name != names[i]:
                    ty = T('pi', name=names[i], dom=ty.dom, body=subst(ty.body, {name: Var(names[i])}) if name != '_' else ty.body)
                    name = names[i]
            bt = self.binder_type(ty.dom, ctx)
            if name == '_':
                parts.append('(_ : %s)' % bt if bt.startswith('Proof') else bt)
            else:
                parts.append('(%s : %s)' % (name, bt))
            ctx[name] = ty.dom
            ty = ty.body
            i += 1
        s = self.sort(ty, ctx)
        if ty.kind == 'sort':
            res = 'Prop' if ty.name == 'Prop' else 'Set'
        elif s == 'Prop':
            res = 'Proof (%s)' % self.prop(ty, ctx)
        elif ty.kind == 'var' and ty.name in STRUCTURES:
            res = 'Str %s' % self.env.dk.get(ty.name, ty.name)
        elif ty.kind == 'app' and ty.f.kind == 'var' and ty.f.name in STRUCTURES:
            res = 'Str (%s)' % ' '.join([self.env.dk.get(ty.f.name, ty.f.name)] + [paren(self.term(x, ctx)) for x in ty.args])
        else:
            res = 'Elem %s' % paren(self.set(ty, ctx))
        return ' -> '.join(parts + [res]), ctx, ty


def paren(s):
    s = s.strip()
    if re.fullmatch(r"[A-Za-z0-9_']+", s):
        return s
    if s.startswith('(') and s.endswith(')'):
        depth = 0
        for i, c in enumerate(s):
            depth += c == '('
            depth -= c == ')'
            if depth == 0 and i < len(s) - 1:
                break
        else:
            return s
    return '(%s)' % s
