concrete BaseConstantsGer of BaseConstants =

open
  UtilitiesGer,
  SyntaxGer,
  ParadigmsGer,
  SymbolicGer,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {

lincat
  Noun = CN ;
  Fam = FamilyT ;
  Set = SetT ;
  Adj = AP ;
  Verb = VP ;
  Reladj = RelationT ;
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

oper
  zahl_N = mkN "Zahl" "Zahlen" feminine ;
  menge_N = mkN "Menge";

lin
  type_Noun = mkNoun (mkN "Typ") ;
  set_Noun = mkNoun menge_N ;
  proposition_Noun = mkNoun "Proposition" ;

  elements_Fun = mkFun "Elementtyp" ;
  proofs_Fun = mkFun  "Prooftyp" ;

  absurdity_Name = mkName (mkN "Widerspruch") ;
  conjunction_Fun = mkFun (mkN "Konjunktion") ;
  disjunction_Fun = mkFun (mkN "Disjunktion") ;
  implication_Fun = mkFun (mkN "Implikation") ;
  universal_Fun = mkFun "universelle" "Quantifizierung" ; ---- plural
  existential_Fun = mkFun "existentielle" "Quantifizierung" ;
  negation_Fun = mkFun (mkN "Negation") ;
  equivalence_Fun = mkFun (mkN "Äquivalenz") ;

  digit_Noun = mkNoun (mkN "Ziffer" "Ziffer" masculine) ;
  number_Noun = mkNoun zahl_N ;
  boolean_Noun = mkNoun "Wahrheitswert" ;
  cardinal_Noun = mkNoun (mkN "Kardinal" zahl_N) ;
  list_Fam = mkFam "Liste" ;
  
  natural_Set = mkSet L.natural_Set "natürlich" zahl_N ;
  integer_Set = mkSet L.integer_Set "ganz" zahl_N ;
  rational_Set = mkSet L.rational_Set "rational" zahl_N ;
  real_Set = mkSet L.real_Set "reell" zahl_N ;
  complex_Set = mkSet L.complex_Set "komplex" zahl_N ;

  Eq_Compar = mkCompar L.Eq_Compar "" "gleich" ;
  Lt_Compar = mkCompar L.Lt_Compar "" "kleiner als" ; 
  Gt_Compar = mkCompar L.Gt_Compar "" "größer als" ; 
  Neq_Compar = mkCompar L.Neq_Compar "" "ungleich" ; ---- 
  Leq_Compar = mkCompar L.Leq_Compar "" "kleiner oder gleich" ; 
  Geq_Compar =  mkCompar L.Geq_Compar "" "größer oder gleich" ;

  positive_Adj = mkAdj "positiv" ;
  negative_Adj = mkAdj "negativ" ;

  converge_Verb = mkVP (mkV "konvergieren") ;
  divide_Relverb = mkV2 "teilen" ;
  member_Relnoun = mkN2 (mkN "Element") ;
  divisor_Relnoun = mkN2 (mkN "Teiler") ;

  plus_Oper = mkOper L.plus_Oper "Summe" ;
  minus_Oper = mkOper L.minus_Oper (mkN "Differenz" feminine) (mkPrep "zwischen" dative) ;
  times_Oper = mkOper L.times_Oper (mkN "Produkt" neuter) ;
  div_Oper = mkOper L.div_Oper (mkN "Quotient" "Quotienten" masculine) ;
  pow_Oper = mkOper L.pow_Oper (mkN "Potenz" feminine) ; ----
  neg_Oper = mkOper L.neg_Oper "Negation" ;
  logarithm_Oper = mkOper L.logarithm_Oper "Logarithmus" ;
  square_root_Oper = mkOper L.square_root_Oper (mkN "Quadratwurzel" feminine) ;

  successor_Fun = mkFun (mkN "Nachfolger" masculine) ;
  absolute_value_Oper = mkOper L.absolute_value_Oper (mkCN (mkA "absolut") (mkN "Wert" masculine)) ;
  factorial_Oper = mkOper L.factorial_Oper (mkN "Fakultät" feminine);
  gcd_Fun = mkFun "groß" "gemeinsam" "Teiler" ;

  even_Adj = mkAdj "gerade" ;
  odd_Adj = mkAdj "ungerade" ;
  divisible_Reladj = mkRel "teilbar" "durch" ;
  prime_Adj = mkAdj "prim" ;

  function_Fam = mkFam (mkCN (mkN "Funktion")) from_Prep to_Prep ;
  union_Oper = mkOper L.union_Oper "Vereinigung" ;
  intersection_Oper = mkOper L.intersection_Oper (mkN "Schnittmenge") ;
  complement_Oper = mkOper L.complement_Oper (mkN "Komplement") ;
  cartesian_Oper = mkOper L.cartesian_Oper (mkCN (mkA "kartesische") (mkN "Produkt")) ;
  difference_Oper = mkOper L.difference_Oper (mkN "Differenz" feminine);
  powerset_Oper = mkOper L.powerset_Oper "Potenzmenge" ;

  subset_Comparnoun = mkComparnoun L.subset_Comparnoun (mkCN (mkA "echt") (mkN "Teilmenge"));
  subseteq_Comparnoun = mkComparnoun L.subseteq_Comparnoun (mkN "Teilmenge") ;  
  superset_Comparnoun = mkComparnoun L.superset_Comparnoun (mkCN (mkA "echt") (mkN "Obermenge")) ;
  superseteq_Comparnoun = mkComparnoun L.superseteq_Comparnoun (mkN "Obermenge") ;
  equalset_Compar = mkCompar L.equalset_Compar "" "gleich" ;
  notequalset_Compar = mkCompar L.notequalset_Compar "" "ungleich" ; ----
  element_Comparnoun = mkComparnoun L.element_Comparnoun element_N ;
  notelement_Comparnoun = mkComparnoun L.notelement_Comparnoun (mkN "nicht-" element_N) ; ----

  emptyset_Const = mkConst L.emptyset_Const (mkNP the_Det (mkCN (mkA "leer") menge_N)) ;
  universeset_Const = mkConst L.universeset_Const (mkNP the_Det (mkCN (mkA "universell") menge_N)) ;

  congruent_Pred3 = mkPred3 (mkAP (mkA "kongruent")) with_Prep (mkPrep "modulo" dative) ;
  
  finite_Adj = mkAdj "endlich" ;
  infinite_Adj = mkAdj "unendlich" ;

  combinationsFromSet_Oper = mkOper L.binomial_Oper (mkCN (mkN "An" zahl_N) (SyntaxGer.mkAdv (mkPrep genitive) (mkNP thePl_Det (mkN "Kombination")))) ;
  combinations_Oper = mkOper L.binomial_Oper (mkCN menge_N (SyntaxGer.mkAdv (mkPrep genitive) (mkNP aPl_Det  (mkN "Kombination")))) ;
  binomial_Oper = mkOper L.binomial_Oper (mkCN (mkN "Binomialkoeffizient")) ;

  area_Fun = mkFun "Fläche" ;
  radius_Fun = mkFun "Radius" ;
  circle_Noun = mkNoun (mkN "Kreis") ;
  pi_Const = mkConst "\\pi" (mkNP the_Det (mkCN tal_N (symb "\\(\\pi\\)"))) ;
  legendre_symbol_Oper = mkOper L.legendre_symbol_Oper (mkN "Legendresymbol") ;
  square_Oper = mkOper L.square_Oper (mkN "Quadrat" neuter) ;
  resultant_Oper = mkOper L.plus_Oper (mkN "Ergebnis") ;
  perpendicular_Compar = mkCompar L.perpendicular_Compar "senkrecht" "zu" ;
  length_Oper = mkOper L.length_Oper (mkN "Länge") ;
  norm_Oper = mkOper L.length_Oper (mkN "Betrag" "Beträge" masculine) ;
  vector_Noun = mkNoun (mkN "Vektor") ;
  denumerable_Adj = mkAdj "abzählbar" ;
  cardinality_Oper = mkOper L.absolute_value_Oper (mkN "Kardinalität") ;
  is_root_Relnoun = mkN2 (mkN "Wurzel" feminine) ;
  degree_Fun = mkFun (mkN "Grad") ;
  polynomial_Noun = mkNoun (mkN "Polynom") ;
  irrational_Adj = mkAdj "irrational" ;
  rational_Adj = mkAdj "rational" ;
  
  sin_Oper = mkOper L.sin_Oper "Sinus" ;
  cos_Oper = mkOper L.cos_Oper "Cosinus" ;
  tan_Oper = mkOper L.tan_Oper "Tangens" ;
  arcsin_Oper = mkOper L.arcsin_Oper (mkN "Arcsinus") for_Prep ;
  arccos_Oper = mkOper L.arccos_Oper (mkN "Arccosinus") for_Prep ;
  arctan_Oper = mkOper L.arctan_Oper (mkN "Arctangens") for_Prep ;
  orthogonal_Compar = mkCompar L.perpendicular_Compar "orthogonal" "zu" ;
  angle_between_Fun = mkFun (mkN "Winkel") (mkPrep "zwischen" dative) ;
  dot_product_Oper = mkOper L.dot_product_Oper "Skalarprodukt" ;
  vector_plus_Oper = mkOper L.plus_Oper "Summe" ;

}
