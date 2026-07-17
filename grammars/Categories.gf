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
  Unit ;

-- lexicon, verbal
  Noun ;    -- Kind                      -- set
  Fam ;     -- Kind -> Kind              -- list of integers
  Fam2 ;    -- Kind -> Kind -> Kind      -- function from reals to integers
  Noun1 ;   -- Exp -> Prop               -- (a) prime
  Noun2 ;   -- Exp -> Exp -> Prop        -- divisor of
  Noun3 ;   -- Exp -> Exp -> Exp -> Prop -- isomorphism between A and B
  NounC ;   -- [Exp] -> Prop             -- relative primes (collective)
  Adj ;     -- Exp -> Prop               -- even
  Adj2 ;    -- Exp -> Exp -> Prop        -- divisible by
  Adj3 ;    -- Exp -> Exp -> Exp -> Prop -- congruent to y modulo z
  AdjE ;    -- [Exp] -> Prop             -- equal (equivalence relation)
  AdjC ;    -- [Exp] -> Prop             -- distinct (collective)
  Verb ;    -- Exp -> Prop               -- converge
  Verb2 ;   -- Exp -> Exp -> Prop        -- divide
  VerbC ;   -- [Exp] -> Prop             -- intersect (collective)
  Name ;    -- Exp                       -- the empty set
  Fun ;     -- Exp -> Exp                -- the square of
  Fun2 ;    -- Exp -> Exp -> Exp         -- the quotient of
  FunC ;    -- [Exp] -> Exp              -- the sum of
  Label ;   -- ProofExp                  -- theorem 1
  Dep ;     -- Exp -> Kind               -- divisor of x
  Dep2 ;    -- Exp -> Exp -> Kind        -- path from x to y
  DepC ;    -- [Exp] -> Kind             -- connection between x and y
  Adv ;     -- Exp -> Prop               -- X is in the universe
  Adv2 ;    -- Exp -> Exp -> Prop        -- X is outside Y
  AdvC ;    -- [Exp] -> Prop             -- X and Y are in opposition
  
  Binder  ; -- Exp -> Ident -> Exp ?
  Binder1 ; -- Exp -> Ident -> Kind -> Exp ?
  Binder2 ; -- Exp -> Ident -> Exp -> Exp -> Exp ?
  
  Adverb ;     -- used for modifying adjectives, as "uniformly"
  Prep ;       -- used for symbol table extensions of grammar
  ProperName ; -- names of mathematicians occurring in concepts

}
