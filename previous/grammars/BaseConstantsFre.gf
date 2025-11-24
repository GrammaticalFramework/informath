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
  Fam = FamilyT ;
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
  Comparnoun = ComparnounT ;
  Pred3 = Pred3T ;

  Exp = NP ;


lin
  type_Noun = mkNoun type_N ;
  set_Noun = mkNoun ensemble_N ;
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

  digit_Noun = mkNoun (mkN "chiffre" masculine) ;
  number_Noun = mkNoun nombre_N ;
  boolean_Noun = mkNoun (mkCN (mkA "booléen") (mkN "valeur" feminine)) ;
  cardinal_Noun = mkNoun "cardinal" ;
  list_Fam = mkFam "liste" ;

  natural_Set = mkSet L.natural_Set "naturel" nombre_N ;
  integer_Set = mkSet L.integer_Set "entier" ;
  rational_Set = mkSet L.rational_Set "rationnel" nombre_N ;
  real_Set = mkSet L.real_Set "réel" nombre_N ;
  complex_Set = mkSet L.complex_Set "complexe" nombre_N ;

  Eq_Compar = mkCompar L.Eq_Compar (mkAP (mkA "égal")) dative ;
  Lt_Compar = mkCompar L.Lt_Compar (mkAP (mkA "inférieur")) dative ;
  Gt_Compar = mkCompar L.Gt_Compar (mkAP (mkA "supérieur")) dative ;
  Neq_Compar = mkCompar L.Neq_Compar (mkAP (mkA "distinct")) genitive ; ---- ?
  Leq_Compar = mkCompar L.Leq_Compar (mkAP or_Conj (mkAP (mkA "inférieur")) (mkAP (mkA "égal"))) dative ;
  Geq_Compar =  mkCompar L.Geq_Compar (mkAP or_Conj (mkAP (mkA "supérieur")) (mkAP (mkA "égal"))) dative ;

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
  absolute_value_Oper = mkOper L.absolute_value_Oper (mkCN (mkA "absolu") (mkN "valeur" feminine)) ;
  factorial_Oper = mkOper L.factorial_Oper "factorielle" ;
  gcd_Fun = mkFun "plus grand" "commun" "diviseur" ; ---- should be in this order

  even_Adj = mkAdj "pair" ;
  odd_Adj = mkAdj "impair" ;
  divisible_Reladj = mkRel "divisible" "par" ;
  prime_Adj = mkAdj "premier" ;

  function_Fam = mkFam "fonction" genitive dative ;
  union_Oper = mkOper L.union_Oper "union" ;
  intersection_Oper = mkOper L.intersection_Oper "intersection" ;
  complement_Oper = mkOper L.complement_Oper (mkN "complément") ;
  cartesian_Oper = mkOper L.cartesian_Oper (mkCN (mkA "cartésien") (mkN "produit")) ;
  difference_Oper = mkOper L.difference_Oper (mkN "différence") (mkPrep "entre") ;
  powerset_Oper = mkOper L.powerset_Oper "puissance" ; ----

  subset_Comparnoun = mkComparnoun L.subset_Comparnoun (mkCN (mkA "propre") (mkN "sous-ensemble" masculine)) ;  
  subseteq_Comparnoun = mkComparnoun L.subseteq_Comparnoun  (mkN "sous-ensemble" masculine) ;  
  superset_Comparnoun = mkComparnoun L.superset_Comparnoun (mkCN (mkA "propre") (mkN "sur-ensemble" masculine)) ;
  superseteq_Comparnoun = mkComparnoun L.superseteq_Comparnoun  (mkN "sur-ensemble" masculine) ;
  equalset_Compar = mkCompar L.equalset_Compar (mkA "égal") dative ;
  notequalset_Compar = mkCompar L.notequalset_Compar (mkA "égal") dative ; ----
  element_Comparnoun = mkComparnoun L.element_Comparnoun (mkN "élément") ;
  notelement_Comparnoun = mkComparnoun L.notelement_Comparnoun  (mkN "non-élément") ; ----

  emptyset_Const = mkConst L.emptyset_Const (mkNP the_Det (mkCN (mkA "vide") ensemble_N)) ;
  universeset_Const = mkConst L.universeset_Const (mkNP the_Det (mkCN (mkA "universel") ensemble_N)) ;

  congruent_Pred3 = mkPred3 (mkAP (mkA "congruent")) dative (mkPrep "modulo") ;

  finite_Adj = mkAdj "fini" ;
  infinite_Adj = mkAdj "infini" ;

  combinationsFromSet_Oper = mkOper L.binomial_Oper (mkCN nombre_N (SyntaxFre.mkAdv genitive (mkNP thePl_Det combinaison_N))) ;
  combinations_Oper = mkOper L.binomial_Oper (mkCN ensemble_N (SyntaxFre.mkAdv genitive (mkNP aPl_Det combinaison_N))) ;
  binomial_Oper = mkOper L.binomial_Oper (mkCN (mkA "binomial") (mkN "coefficient")) ;

  area_Fun = mkFun "aire" ;
  radius_Fun = mkFun "rayon" ;
  circle_Noun = mkNoun (mkN "cercle" masculine) ;
  pi_Const = mkConst "\\pi" (mkNP the_Det (mkCN nombre_N (symb "\\(\\pi\\)"))) ;
  legendre_symbol_Oper = mkOper L.legendre_symbol_Oper (mkCN (mkN "symbole" masculine) (SyntaxFre.mkAdv genitive (mkNP (mkPN "Legendre")))) ;
  square_Oper = mkOper L.square_Oper (mkN "carré" masculine) ;
  resultant_Oper = mkOper L.plus_Oper (mkN "addition") ;
  perpendicular_Compar = mkCompar L.perpendicular_Compar (mkA "perpendiculaire") dative ;
  length_Oper = mkOper L.length_Oper "norme" ;
  norm_Oper = mkOper L.length_Oper "norme" ;
  vector_Noun = mkNoun "vecteur" ;
  denumerable_Adj = mkAdj "dénombrable" ;
  cardinality_Oper = mkOper L.absolute_value_Oper "cardinalité" ;
  is_root_Relnoun = mkN2 (mkN "racine") genitive ;
  degree_Fun = mkFun (mkN "degré" masculine) ;
  polynomial_Noun = mkNoun (mkN "polynôme" masculine) ;
  irrational_Adj = mkAdj "irrationnel" ;
  rational_Adj = mkAdj "rationnel" ;

  sin_Oper = mkOper L.sin_Oper "sinus" ;
  cos_Oper = mkOper L.cos_Oper "cosinus" ;
  tan_Oper = mkOper L.tan_Oper "tangente" ;
  arcsin_Oper = mkOper L.arcsin_Oper "arcsinus" ;
  arccos_Oper = mkOper L.arccos_Oper "arccosinus" ;
  arctan_Oper = mkOper L.arctan_Oper "arctangente" ;
  orthogonal_Compar = mkCompar L.perpendicular_Compar (mkA "orthogonal") dative ;
  angle_between_Fun = mkFun (mkN "angle" masculine) (mkPrep "entre") ;
  dot_product_Oper = mkOper L.dot_product_Oper (mkCN (mkA "scalaire") (mkN "produit")) ;
  vector_plus_Oper = mkOper L.plus_Oper "somme" ;

  sphenic_Adj = mkAdj "sphénique" ;

}