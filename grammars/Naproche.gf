abstract Naproche = MathCore ** {

-- extensions from Naproche-ZF

fun
-- combinations
  SupposePropHypo : Prop -> Hypo ;
  IffIffProp : Prop -> Prop -> Prop ;
  WeHaveProp : Prop -> Prop ;
  NoCommaAllProp : [ArgKind] -> Prop -> Prop ;
  BareIdentsArgKind : [Ident] -> ArgKind ;
  FormulaArgKind : Formula -> ArgKind ;  -- for all $ x \in N $, $ x > 0 $, etc
  --- not always meaningful, which can be hard to decide
  IndexedFormulaArgKind : Int -> ArgKind ;



-- lexicon
  inhabited_Adj : Adj ;
  empty_Adj : Adj ;
  disjoint_Compar : Compar ;
  ni_Compar : Compar ;
  
}