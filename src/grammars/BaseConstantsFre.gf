concrete BaseConstantsFre of BaseConstants =

open
  UtilitiesFre,
  SyntaxFre,
  ParadigmsFre,
  (P=ParadigmsFre),
  SymbolicFre,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {

lincat
  Noun = CN ;
  Fam = CN ;
  Set = SetT ;
  Adj = AP ;
  Reladj = RelationT ;
  Verb = VP ;
  Relverb = V2 ;
  Relnoun = N2 ;
  Name = NP ;
  Fun = FunctionT ;
  Label = LabelT ;
  Const = ConstantT ;
  Oper = OperatorT ;
  Compar = ComparisonT ;

lin
  type_Noun = mkNoun type_N ;
  set_Noun = mkNoun (mkN "ensemble" masculine) ;
  proposition_Noun = mkNoun "proposition" ;

  elements_Fun = mkFun type_N "des éléments" ;
  proofs_Fun = mkFun  type_N "des preuves" ;

  absurdity_Name = mkName "contradiction" ;
  conjunction_Fun = mkFun "conjonction" ;
  disjunction_Fun = mkFun "disjonction" ;
  implication_Fun = mkFun "implication" ;
  universal_Fun = mkFun "universel" "quantification" ;
  existential_Fun = mkFun "existentiel" "quantification" ;
  negation_Fun = mkFun "négation" ;
  equivalence_Fun = mkFun "équivalence" ;

  number_Noun = mkNoun nombre_N ;
  boolean_Noun = mkNoun (mkCN (mkA "booléen") (mkN "valeur" feminine)) ;
  list_Fam = mkNoun "liste" ;

  natural_Set = mkSet L.natural_Set "naturel" nombre_N ;
  integer_Set = mkSet L.integer_Set "entier" ;
  rational_Set = mkSet L.rational_Set "rationnel" nombre_N ;
  real_Set = mkSet L.real_Set "réel" nombre_N ;
  complex_Set = mkSet L.complex_Set "complexe" nombre_N ;

  Eq_Compar = mkCompar L.Eq_Compar (mkAP (mkA "égal")) dative ;
  Lt_Compar = mkCompar L.Lt_Compar (mkAP (mkA "inférieur")) dative ;
  Gt_Compar = mkCompar L.Gt_Compar (mkAP (mkA "supërieur")) dative ;
  Neq_Compar = mkCompar L.Neq_Compar (mkAP (mkA "inégal")) dative ; ---- ?
  Leq_Compar = mkCompar L.Leq_Compar (mkAP or_Conj (mkAP (mkA "inférieur")) (mkAP (mkA "ágal"))) dative ;
  Geq_Compar =  mkCompar L.Geq_Compar (mkAP or_Conj (mkAP (mkA "supérieur")) (mkAP (mkA "ágal"))) dative ;

  positive_Adj = mkAdj "positif" ;
  negative_Adj = mkAdj "negatif" ;

  converge_Verb = mkVP (mkV "converger") ;
  divide_Relverb = mkV2 "diviser" ;
  member_Relnoun = mkN2 (mkN "membre" masculine) genitive ;
  divisor_Relnoun = mkN2 (mkN "diviseur") genitive ;

  plus_Oper = mkOper L.plus_Oper "somme" ;
  minus_Oper = mkOper L.minus_Oper "différence" ;
  times_Oper = mkOper L.times_Oper "produit" ;
  div_Oper = mkOper L.div_Oper "quotient" ;
  pow_Oper = mkOper L.pow_Oper "puissance" ;
  neg_Oper = mkOper L.neg_Oper "négation" ;

  logarithm_Oper = mkOper L.logarithm_Oper (mkN "logarithme" masculine) ;
  square_root_Oper = mkOper L.square_root_Oper (mkCN (mkA "carré") (mkN "racine")) ;

  successor_Fun = mkFun "successeur" ;
  absolute_value_Fun = mkFun (mkCN (mkA "absolu") (mkN "valeur" feminine)) ;
  factorial_Fun = mkFun "factorielle" ;
  gcd_Fun = mkFun "plus grand" "commun" "diviseur" ; ---- should be in this order

  even_Adj = mkAdj "pair" ;
  odd_Adj = mkAdj "impair" ;
  divisible_Reladj = mkRel "divisible" "par" ;
  prime_Adj = mkAdj "premier" ;

  function_Oper = mkOper L.function_Oper "fonction" ;
  union_Oper = mkOper L.union_Oper "union" ;
  intersection_Oper = mkOper L.intersection_Oper "intersection" ;
  difference_Oper = mkOper L.difference_Oper (mkN "différence") (mkPrep "entre") ;
  powerset_Oper = mkOper L.powerset_Oper "puissance" ; ----

}