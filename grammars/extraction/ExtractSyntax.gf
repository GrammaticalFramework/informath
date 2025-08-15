abstract ExtractSyntax = Structural ** {

flags startcat = MT ;

cat
  MT ;

fun
  NPMT : NP -> MT ;   -- noun phrase term, including CN
  APMT : AP -> MT ;   -- adjectival phrase
  AdvMT : Adv -> MT ; -- adverbial phrase

  UseN : N -> CN ;                    -- bijection
  AdjCN : AP -> CN -> CN ;            -- continuous function
  CompoundN : N -> N -> N ;           -- function space
  IntCompoundCN : Int -> CN -> CN ;   -- 13-cube
  NameCompoundCN : PN -> CN -> CN ;   -- Lie group
  NounIntCN : CN -> Int -> CN ;       -- Grinberg graph 42
  NounPrepCN : CN -> Adv -> CN ;      -- ring of sets
  NounGenCN : CN -> NP -> CN ;        -- bishop's graph

  DefCN : CN -> NP ;                  -- (multiple of) the median
  DefPluralCN : CN -> NP ;            -- (law of) large numbers
  IndefCN : CN -> NP ;                -- (parity of) a permutation
  IndefPluralCN : CN -> NP ;          -- (field of) sets
  BareCN : CN -> NP ;                 -- bijection

  PositA : A -> AP ;                  -- uniform
  AdAP : AdA -> AP -> AP ;            -- almost uniform
  AAdAP : A -> AP -> AP ;             -- algebraically closed
  PastPartAP : V -> AP ;              -- connected

  PrepNP : Prep -> NP -> Adv ;        -- (integration) by parts

}