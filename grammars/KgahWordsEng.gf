concrete KgahWordsEng of KgahWords = CategoriesEng **
open UtilitiesEng, SyntaxEng, ParadigmsEng, (G = GrammarEng) in {
oper
  reals : CN = mkCN (mkN "real") ;
  ofReals : Adv = SyntaxEng.mkAdv possess_Prep (mkNP aPl_Det reals) ;
lin real_object_Noun = reals ;
lin set_of_reals_Noun = mkCN (mkCN (mkN "set")) ofReals ;
lin relation_between_sets_of_reals_and_reals_Noun =
  mkCN (mkCN (mkN "relation"))
       (SyntaxEng.mkAdv between_Prep
          (mkNP and_Conj (mkNP aPl_Det (mkCN (mkCN (mkN "set")) ofReals)) (mkNP aPl_Det reals))) ;
lin force_Verb = mkVerb "force" ;
lin closed_downwards_Adj = G.AdvAP (mkAP (mkA "closed")) (ParadigmsEng.mkAdv "downwards") ;
}
