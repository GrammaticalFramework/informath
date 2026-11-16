"""Agda's internal terms, as agda-export (Main.hs) writes them in JSON, and
their translation to Informath's Dedukti dialect.

The encoding is shallow, as in the hott-llm formalization of Rijke's book
(version 1): a type is a Set, its elements are Elem A, and a universe UU l is
Set itself; universe levels are dropped, as the book leaves them out.  A
function type is a Dedukti arrow in a binder, (f : (Elem A -> Elem B)), and
in the position of a type Pi A (x => B) (dependent) or Arr A B.  Agda's
implicit arguments are kept, so that the Dedukti has all arguments, and
hide.dkgf lists them (#HIDE) for the readings, which show what Rijke writes.

Terms (agda-export):

    ["var", name, elims]  ["def", qname, elims]  ["con", qname, elims]
    ["lam", hiding, name, body]  ["pi", hiding, name, dom, cod]
    ["sort", text]  ["lit", text]  ["level"]  ["other", text]

with elims [["app", hiding, term] | ["proj", qname]].
"""
import re
import unicodedata

LEVEL = 'Agda.Primitive.Level'
LEVEL_OPS = ('Agda.Primitive.lzero', 'Agda.Primitive.lsuc', 'Agda.Primitive._⊔_')
UNIVERSE_NAMES = ('UU',)          # the book's UU l, which is Set l

# characters of Agda names and their Dedukti spelling
CHARS = {
    '-': '_', 'ℕ': 'N', 'ℤ': 'Z', 'ℚ': 'Q', 'ℝ': 'R', 'Σ': 'Sigma', 'Π': 'Pi', '𝟙': 'unit', '𝟘': 'empty',
    '𝟚': 'two', '∙': 'concat', '⁻¹': 'inv', '∘': 'comp', '×': 'prod', '↔': 'iff', '≃': 'equiv', '~': 'htpy',
    '＝': 'Id', '≡': 'eq', '≤': 'leq', '<': 'lt', '+': 'plus', '·': 'dot', '¬': 'neg', '⊔': 'join', '→': 'to',
    '∀': 'all', '∃': 'ex', '⊢': 'turnstile', 'λ': 'lambda', '∈': 'in', '′': "'", '″': "''",
}
# Informath's base constants, which the names must not take
BASE_NAMES = set('''0 1 2 3 4 5 6 7 8 9 Cardinal Circle Complex Dig Elem Enum Eq Eqt Equ Geq Gt Int Leq List Lt Nat
Neq Num Polynomial Proof Prop Rat Real Set Vector abs and angleBetween arccos arcsin arctan area binomial bool
cardinality cartesian combinations combinationsFromSet complement congruent cons cos degree denumerable difference
disjoint div divide divisible divisor dotProduct element empty emptyset enum123 enumset equalset even exists
factorial false finite forall fst function gcd if iff infinite infinity inhabited integral intersection irrational
isRoot legendre length log member minus nd neg negative negativePart ni nil nn norm not notelement odd or orthogonal
pi plus positive positivePart pow powerset prime radius rational sigma sin sqrt square subset subseteq succ suchthat
superset superseteq tan times union universeset vectorPlus def thm Type'''.split())


def ident(name):
    """An Agda name (the last part of a qualified one) as a Dedukti identifier."""
    s = name
    if s.startswith('_') or s.endswith('_'):      # a mixfix operator: _∙_ is concat
        s = s.strip('_')
    out = []
    for ch in s:
        if ch in CHARS:
            out.append(CHARS[ch])
        elif ch.isascii() and (ch.isalnum() or ch in "_'"):
            out.append(ch)
        elif unicodedata.category(ch).startswith('L'):
            n = unicodedata.name(ch, '')
            m = re.search(r'(?:LETTER|SMALL|CAPITAL) (\w+)$', n)
            out.append(m.group(1).lower() if m and 'SMALL' in n else (m.group(1).capitalize() if m else 'u%X' % ord(ch)))
        elif ch.isdigit():
            out.append(str(unicodedata.digit(ch, 0)))
        else:
            out.append('u%X' % ord(ch))
    s = re.sub(r'_+', '_', ''.join(out)).strip('_') or 'x'
    if s[0].isdigit():
        s = 'n' + s
    return s


def short(qname):
    """The last part of a qualified Agda name: book.05-identity-types.concat is concat."""
    parts = qname.split('.')
    # a module name has dots, an operator name may too (_._ is rare): the last part
    return parts[-1] if parts[-1] else qname


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


class Names:
    """The Dedukti names of the Agda constants, distinct from each other and
    from Informath's base constants."""
    def __init__(self):
        self.dk = {}
        self.taken = set(BASE_NAMES)

    def __call__(self, qname):
        if qname not in self.dk:
            base = ident(short(qname))
            n, i = base, 1
            while n in self.taken:
                # a base constant's name gets _ (neg_), a second one a number (unit2)
                n = base + '_' if base in BASE_NAMES and i == 1 else '%s%d' % (base, i + 1)
                i += 1
            self.dk[qname] = n
            self.taken.add(n)
        return self.dk[qname]


class Translator:
    """Agda terms to Dedukti.  ctx: variable name -> 'level' | 'type' (a type
    or a family of types) | 'elem' (an element or a function)."""
    def __init__(self, env, names, propositions=()):
        self.env = env            # qname -> definition (agda-export JSON)
        self.names = names
        self.fresh_count = 0
        # the type families read as propositions (Curry-Howard): their elements
        # are proofs, Proof (Id A x y), and a definition into one is a theorem
        self.propositions = set(propositions)

    def is_prop_type(self, t):
        """Whether t is an application of one of the PROPOSITIONS families (Id x y)."""
        return t[0] in ('def', 'con') and short(t[1]) in self.propositions

    def is_prop_family(self, t):
        """Whether t is a function type into a proposition: (x : A) -> x = x."""
        while t[0] == 'pi':
            t = t[4]
        return self.is_prop_type(t)

    # ---- variables
    def var(self, x, ctx):
        """The Dedukti name of a bound variable (its Agda name, made an identifier)."""
        v = ident(x) if x not in ('_', '') else 'x'
        if v in BASE_NAMES:
            v = v + '_'
        return v

    def fresh(self, base, ctx):
        i = 0
        while True:
            n = base if i == 0 else '%s%d' % (base, i)
            if n not in ctx:
                return n
            i += 1

    # names for anonymous variables, by their types, as a mathematician would choose them
    NAMES_BY_TYPE = [('Id', 'pqrs'), ('N', 'nmkl'), ('Z', 'kl'), ('function', 'fgh'), ('type', 'ABCD'), ('', 'xyzuvw')]

    def binder(self, x, ctx, typ=None):
        """A name for a new bound variable: Agda's, or for _ one chosen by its
        type (p, q for identifications, n, m for natural numbers, f, g for
        functions), and a fresh one for shadowing."""
        v = self.var(x, ctx)
        if x in ('_', ''):
            for c in self.name_candidates(typ):
                if c not in ctx and c not in self.env_names:
                    return c
            return self.fresh('x', ctx)
        if v in ctx:
            v = self.fresh(v, ctx)
        return v

    env_names = set()
    declaring = None      # the short name of the constant being declared

    def name_candidates(self, t):
        cls = ''
        if t is not None:
            if t[0] == 'pi':
                cls = 'type' if self.is_kind(t) else 'function'
            elif self.is_universe(t):
                cls = 'type'
            elif t[0] == 'def':
                s = short(t[1])
                cls = 'Id' if s in ('Id', '_＝_') else ('N' if s == 'ℕ' else ('Z' if s == 'ℤ' else ''))
        for c, names in self.NAMES_BY_TYPE:
            if c == cls:
                return list(names) + [n + "'" for n in names]
        return list('xyzuvw')

    # ---- what a term is
    def is_level_type(self, t):
        return t[0] == 'def' and t[1] == LEVEL

    def is_level(self, t, ctx):
        if t[0] == 'level':
            return True
        if t[0] == 'var':
            return ctx.get(self.var(t[1], ctx)) == 'level'
        if t[0] == 'def':
            return t[1] in LEVEL_OPS
        return False

    def is_universe(self, t):
        return t[0] == 'sort' or (t[0] == 'def' and short(t[1]) in UNIVERSE_NAMES)

    def is_kind(self, t):
        """Whether the type t is a universe or a function into one (A -> UU l):
        its elements are types or families of types."""
        while t[0] == 'pi':
            t = t[4]
        return self.is_universe(t)

    def kept_args(self, elims, ctx):
        """The applied arguments without the levels: (hiding, term)."""
        return [(e[1], e[2]) for e in elims if e[0] == 'app' and not self.is_level(e[2], ctx)]

    # ---- types of bound variables (binders), and terms
    def binder_type(self, t, ctx):
        """The Dedukti type of a variable of the Agda type t."""
        if self.is_universe(t):
            return 'Set'
        if t[0] == 'pi':
            _, h, x, dom, cod = t
            if self.is_level_type(dom):
                return self.binder_type(cod, dict(ctx, **{self.var(x, ctx): 'level'}))
            v = self.binder(x, ctx, dom)
            d = self.binder_type(dom, ctx)
            ctx2 = dict(ctx, **{v: 'type' if self.is_kind(dom) else 'elem'})
            c = self.binder_type(self.rename(cod, x, v), ctx2)
            if x not in ('_', '') and self.occurs(x, cod):
                return '((%s : %s) -> %s)' % (v, d, c)
            return '(%s -> %s)' % (d, c)
        if self.is_prop_type(t):
            return 'Proof %s' % paren(self.term(t, ctx))
        return 'Elem %s' % paren(self.set(t, ctx))

    def set(self, t, ctx):
        """A type in the position of a set (an argument of a type former)."""
        if self.is_universe(t):
            return 'UU'
        if t[0] == 'pi':
            _, h, x, dom, cod = t
            if self.is_level_type(dom):
                return self.set(cod, dict(ctx, **{self.var(x, ctx): 'level'}))
            v = self.binder(x, ctx, dom)
            ctx2 = dict(ctx, **{v: 'type' if self.is_kind(dom) else 'elem'})
            if x not in ('_', '') and self.occurs(x, cod):
                return 'Pi %s (%s => %s)' % (paren(self.set(dom, ctx)), v, self.set(self.rename(cod, x, v), ctx2))
            return 'Arr %s %s' % (paren(self.set(dom, ctx)), paren(self.set(cod, ctx2)))
        return self.term(t, ctx)

    def term(self, t, ctx):
        k = t[0]
        if k == 'var':
            return self.app(self.var(t[1], ctx), t[2], ctx)
        if k in ('def', 'con'):
            if self.is_universe(t):
                return 'UU'
            return self.app(self.names(t[1]), t[2], ctx)
        if k == 'lam':
            _, h, x, body = t
            v = self.binder(x, ctx)
            # a lambda over a level is dropped with the level
            if self.is_level_lambda(t):
                return self.term(body, dict(ctx, **{self.var(x, ctx): 'level'}))
            return '%s => %s' % (v, self.term(self.rename(body, x, v), dict(ctx, **{v: 'elem'})))
        if k == 'pi':
            return self.set(t, ctx)
        if k == 'sort':
            return 'UU'
        if k == 'lit':
            return t[1].strip('"')
        if k == 'level':
            return 'lzero'
        return 'unknown'

    def is_level_lambda(self, t):
        return False

    def app(self, head, elims, ctx):
        out = head
        for e in elims:
            if e[0] == 'proj':
                out = '%s %s' % (self.names(e[1]), paren(out))
            elif not self.is_level(e[2], ctx):
                out = '%s %s' % (out, paren(self.arg(e[2], ctx)))
        return out

    def arg(self, a, ctx):
        if a[0] == 'pi' or self.is_universe(a):
            return self.set(a, ctx)
        return self.term(a, ctx)

    # ---- variables in terms
    def occurs(self, x, t):
        if isinstance(t, list):
            if len(t) >= 2 and t[0] == 'var' and t[1] == x:
                return True
            if t and t[0] in ('lam',) and t[2] == x:
                return False
            if t and t[0] == 'pi' and t[2] == x:
                return self.occurs(x, t[3])
            return any(self.occurs(x, u) for u in t[1:])
        return False

    def rename(self, t, x, v):
        """t with the variable x named v (the Dedukti name chosen for it); the
        JSON names variables by their Agda names, which the translation keeps
        unless they clash, so that renaming is rare."""
        if v == self.var(x, {}) or x in ('_', ''):
            return t
        def go(u):
            if isinstance(u, list):
                if len(u) >= 2 and u[0] == 'var' and u[1] == x:
                    return ['var', v] + [go(w) for w in u[2:]]
                if u and u[0] == 'lam' and u[2] == x:
                    return u
                if u and u[0] == 'pi' and u[2] == x:
                    return u[:3] + [go(u[3]), u[4]]
                return [go(w) for w in u]
            return u
        return go(t)

    # ---- declarations
    def decl_type(self, t, ctx=None, params=0):
        """An Agda type as a Dedukti declaration type: its leading binders as
        binders, (A : Set) -> (x : Elem A) -> Elem B; and the hidden argument
        positions among those kept (1-based), and the context.  The first
        params binders (a constructor's parameters, which Agda leaves out of
        the constructor's applications) are not counted in the positions."""
        ctx = dict(ctx or {})
        parts, hidden, i, n = [], [], 0, 0
        while t[0] == 'pi':
            _, h, x, dom, cod = t
            n += 1
            if self.is_level_type(dom):
                ctx[self.var(x, ctx)] = 'level'
                t = cod
                continue
            v = self.binder(x, ctx, dom)
            bt = self.binder_type(dom, ctx)
            parts.append('(%s : %s)' % (v, bt))
            if n > params:
                i += 1
                if h != 'v':
                    hidden.append(i)
            ctx[v] = 'type' if self.is_kind(dom) else ('proof' if self.is_prop_family(dom) else 'elem')
            t = self.rename(cod, x, v)
        if self.is_universe(t):
            # a family of the PROPOSITIONS (Id) is a predicate
            res = 'Prop' if self.declaring in self.propositions else 'Set'
        elif self.is_prop_type(t):
            res = 'Proof %s' % paren(self.term(t, ctx))
        else:
            res = 'Elem %s' % paren(self.set(t, ctx))
        return ' -> '.join(parts + [res]), hidden, ctx

    def pattern(self, p, ctx):
        """A clause pattern as a Dedukti pattern."""
        k = p[0]
        if k == 'var':
            return self.var(p[1], ctx)
        if k == 'con':
            args = [self.pattern(q, ctx) for h, q in p[2] if not self.level_pattern(q, ctx)]
            return ' '.join([self.names(p[1])] + [paren(a) for a in args])
        if k == 'lit':
            return p[1].strip('"')
        return '_'

    def level_pattern(self, p, ctx):
        return p[0] == 'var' and ctx.get(self.var(p[1], ctx)) == 'level'
