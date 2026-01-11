concrete NaturalDeductionEng of NaturalDeduction = CategoriesEng **

open
  SyntaxEng,
  (P=ParadigmsEng),
  (Grammar=GrammarEng),
  UtilitiesEng

in {

lin
  andIProof A B a b =
    mkText a (mkText b (prefixText "altogether" (mkText
      (mkS and_Conj (partProp A) (partProp B))))) ;
    
  andElProof A B c = mkText c (prefixText "a fortiori" (mkText (topProp B))) ;
  andErProof A B c = mkText c (prefixText "a fortiori" (mkText (topProp B))) ;

  orIlProof A B a = mkText a (prefixText "a fortiori" (mkText (mkS or_Conj A.s B.s))) ;
  orIrProof A B b = mkText b (prefixText "a fortiori" (mkText (mkS or_Conj A.s B.s))) ;

  orEProof A B C c x d y e =
    mkText c (prefixText "Hence we have two cases . First, " 
      (mkText (assumeText x A.s) (mkText d (prefixText "Second"
      (mkText (assumeText y B.s) (mkText e (prefixText "Thus"
        (mkText (mkText C.s) (strText "in both cases ,"))))))))) ;
  
  ifIProof A B h b =
    mkText (assumeText h (topProp A)) (mkText b (prefixText "we conclude" (mkText
     (Grammar.ExtAdvS (SyntaxEng.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B)))))) ;

  ifEProof A B a b = mkText a (mkText b (prefixText "we conclude" (mkText (topProp B)))) ;

{-
  forallIProof A x B z b =

  forallEProof A x B b a = 

  existsIProof A x B a b =

  existsEProof A x B C c x y d =
-}

  hypoProof A h = prefixText ("by" ++ h ++ ",") (mkText (topProp A)) ;

--  reflProof A a =

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

}