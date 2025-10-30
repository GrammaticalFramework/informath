abstract Categories =
  Identifiers
** {

cat
-- syntax
  Jmt ;
  Exp ;
  [Exp] {2} ;
  Exps ;
  Prop ;
  [Prop] {2} ;
  Kind ;
  ArgKind ;
  [ArgKind] {1} ;
  Hypo ;
  [Hypo] ;
  Local ;
  Proof ;
  [Proof] {0} ;
  ProofExp ;
  Rule ;
  [Rule] {1} ;
  Coercion ;
  [Ident] {1} ;

-- lexicon, verbal
  Noun ;    -- Kind -- set
  Fam ;     -- Kind -> Kind         -- list of integers
  Fam2 ;    -- Kind -> Kind -> Kind -- function from reals to integers
  Noun1 ;   -- Exp -> Prop          -- (a) prime
  Noun2 ;   -- Exp -> Exp -> Prop   -- divisor of
  Adj ;     -- Exp -> Prop          -- even
  Adj2 ;    -- Exp -> Exp -> Prop   -- divisible by
  AdjE ;    -- [Exp] -> Prop        -- equal (equivalence relation)
  AdjC ;    -- [Exp] -> Prop        -- distinct (collective)
  Verb ;    -- Exp -> Exp           -- converge
  Verb2 ;   -- Exp -> Exp -> Prop   -- divide
  Name ;    -- Exp                  -- the empty set
  Fun ;     -- Exp -> Exp           -- the square of
  Fun2 ;    -- Exp -> Exp -> Exp    -- the quotient of
  FunC ;    -- [Exp] -> Exp         -- the sum of
  Label ;   -- Exp                  -- theorem 1
  Adj3 ;    -- Exp -> Exp -> Exp -> Prop -- congruent to y modulo z


}