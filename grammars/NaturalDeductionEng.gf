concrete NaturalDeductionEng of NaturalDeduction = CategoriesEng **

open
  SyntaxEng,
  (Grammar=GrammarEng),
  UtilitiesEng

in {

lin
  andIProof A B a b =
    mkText a (mkText b (prefixText "altogether" (mkText
      (mkS and_Conj (partProp A) (partProp B))))) ;
    
  andElProof A B c = mkText c (prefixText "a fortiori" (mkText (topProp A))) ;
  andErProof A B c = mkText c (prefixText "a fortiori" (mkText (topProp B))) ;
  
  ifIProof  A B h b =
    mkText (assumeText h (topProp A)) (mkText b (prefixText "we conclude" (mkText
     (Grammar.ExtAdvS (SyntaxEng.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B)))))) ;

  ifEProof A B a b = mkText a (mkText b (prefixText "we conclude" (mkText (topProp B)))) ;

  hypoProof A h = prefixText ("by" ++ h ++ ",") (mkText (topProp A)) ; 

oper
  assumeText : Str -> S -> Text = \h, s ->
    prefixText (h ++ ":") (mkText (mkUtt (mkImp (mkVP assume_VS s)))) ;

}