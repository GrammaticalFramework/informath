abstract Categories =
  Identifiers
** {

cat
-- syntax
  Jmt ;
  Exp ;
  Exps ;
  Prop ;
  [Prop] {2} ;
  Kind ;
  ArgKind ;
  [ArgKind] {1} ;
  Hypo ;
  [Hypo] ;
  Proof ;
  [Proof] {0} ;
  ProofExp ;
  Rule ;
  [Rule] {1} ;
  Coercion ;
  [Ident] {1} ;

-- lexicon, verbal
  Noun ;    -- Kind -- set
  Fam ;     -- Kind -> Kind        -- list of integers
  Adj ;     -- Exp -> Prop         -- even
  Verb ;    -- Exp -> Exp          -- converge
  Reladj ;  -- Exp -> Exp -> Prop  -- divisible by
  Relverb ; -- Exp -> Exp -> Prop  -- divide
  Relnoun ; -- Exp -> Exp -> Prop  -- divisor of
  Name ;    -- Exp                 -- the empty set
  Fun ;     -- [Exp] -> Exp        -- the sum of
  Label ;   -- Exp                 -- theorem 1
  Pred3 ;   -- Exp -> Exp -> Exp -> Prop -- congruent to y modulo z

-- lexicon, symbolic
  Set ;     -- Kind                      -- Z
  Const ;   -- Exp                       -- Ø
  Oper ;    -- [Exp] -> Exp symbol       -- +
  Compar ;  -- Exp -> Exp -> Prop symbol -- >


}