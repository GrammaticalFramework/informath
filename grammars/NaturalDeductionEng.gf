concrete NaturalDeductionEng of NaturalDeduction = CategoriesEng **

open
  SyntaxEng,
  (P=ParadigmsEng),
  (Grammar=GrammarEng),
  (Extend=ExtendEng),
  UtilitiesEng

--- quick and dirty implementation, needs more structure and functorization

in {

lin
  falseEProof C c = ccText c (prefixText "hence" (mkText (topProp C))) ; 

  andIProof A B a b =
    mkText a (mkText b (prefixText "altogether" (mkText
      (mkS and_Conj (partProp A) (partProp B))))) ;
    
  andElProof A B c = mkText c (prefixText "a fortiori" (mkText (topProp B))) ;
  andErProof A B c = mkText c (prefixText "a fortiori" (mkText (topProp B))) ;

  orIlProof A B a = mkText a (prefixText "a fortiori" (mkText (mkS or_Conj A.s B.s))) ;
  orIrProof A B b = mkText b (prefixText "a fortiori" (mkText (mkS or_Conj A.s B.s))) ;

  orEProof A B C c x d y e =
    ccText c
      (embedText ("Hence we have two cases . " ++ begin_itemize_str) end_itemize_str
        (mkText
          (prefixText (item_str ++ "First ,") (mkText (assumeText x A.s) d))
          (prefixText (item_str ++ "Second ,") (mkText (assumeText y B.s) e))))
        (prefixText "Thus"
          (mkText (mkText C.s) (strText "in both cases ,"))) ;
  
  ifIProof A B h b =
    mkText (assumeText h (topProp A)) (mkText b (prefixText "we conclude" (mkText
     (Grammar.ExtAdvS (SyntaxEng.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B)))))) ;

  ifEProof A B a b = mkText a (mkText b (prefixText "we conclude" (mkText (topProp B)))) ;


  forallIProof A x B z b =
    ccText
      (prefixText "consider an arbitrary" (mkText (mkUtt (identKindCN x A))))
      b
      (strText "we have proved that")
      (mkText (mkText (topProp B)) (prefixText "for all" (mkText (mkUtt (identKindCN x A))))) ;

  forallEProof A x B b a =
    ccText
      b
      (prefixText "in particular" (topProp B))
      (prefixText ("for" ++ x ++ "set to") (mkText (mkUtt a))) ;

  existsIProof A x B a b =
    ccText
      b
      (prefixText "thus"
        (mkText (Grammar.SSubjS (mkS (Extend.ExistsNP (mkNP a_Det ((identKindCN x A)))))
	  such_that_Subj (partProp B)))) ;

  existsEProof A x B C c x y d =
    ccText
      (prefixText "consider an arbitrary" (mkText (mkUtt (identKindCN x A))))
      (prefixText "such that" (mkText (topProp B)))
      d
      (prefixText "we have proved that" (mkText (topProp C)))
      (strText ("independently of" ++ x)) ;

  hypoProof A h = prefixText ("by" ++ h ++ ",") (mkText (topProp A)) ;
  
  reflProof A a =
    ccText
      (mkText (mkS (mkCl a (P.mkV2 "equal") a)))
      (strText "by reflexivity .") ;

  NatIndProof x C d n h e =
    prefixText "we proceed by induction . First ,"
      (mkText d (prefixText ("Second , consider an arbitrary")
        (mkText (strText x) (prefixText "such that"
	  (mkText (mkText (mkText C.s) (strText ("(" ++ h ++ ")")))
	    (mkText e (prefixText ("we have proved that , for all" ++ x ++ ",") (mkText C.s)))))))) ;

  evenZeroProof = strText "by the first axiom of parity , $ 0 $ is even ." ;
  
  evenSuccProof n c = mkText c (prefixText "by the second axiom of parity ,"
    (mkText (mkS (mkCl (succNP n) (P.mkA "even"))))) ;
  oddSuccProof n c = mkText c (prefixText "by the third axiom of parity ,"
    (mkText (mkS (mkCl (succNP n) (P.mkA "odd"))))) ;
    

oper
  assumeText : Str -> S -> Text = \h, s ->
    mkText (mkText (mkUtt (mkImp (mkVP assume_VS s)))) ((strText ("(" ++ h ++ ")"))) ;

  strText : Str -> Text = \s -> lin Text {s = s} ;

  succNP : NP -> NP = \x -> mkNP the_Det (mkCN (P.mkN "successor") (mkAdv possess_Prep x)) ;  
  ccText = overload {    
    ccText : (a, b : Text) -> Text = \a, b -> mkText a b ;
    ccText : (a, b, c : Text) -> Text = \a, b, c -> mkText a (mkText b c) ;
    ccText : (a, b, c, d : Text) -> Text = \a, b, c, d -> mkText a (mkText b (mkText c d)) ;
    ccText : (a, b, c, d, e : Text) -> Text = \a, b, c, d, e ->
      mkText a (mkText b (mkText c (mkText d e))) ;
    } ;


}