incomplete concrete GodementSyntaxFunctor of GodementSyntax =
  Categories, TermsLatex
**
open
  Syntax,
  Symbolic,
  Grammar,
  Extend,
  Utilities,
  UtilitiesGodement,
  Formal,
  Prelude
in {

param
  DefMode = PassToBe | PassDirect | ByDef ;
  EnumPos = Pa | Pb | Pc | Pd | Pe | Pf | Pg | Ph ;

lincat
  DefVerb = {v2 : V2 ; adv : AdV ; mode : DefMode} ;  -- adv only used with ByDef
  Condition = AP ;
  PropEnum = {s : EnumPos => Str} ;  -- the label of the first item is the parameter
  NumDet = Det ;
  Card = Syntax.Card ;

oper
  -- the VP of a definition, given the complement as "be X" and as bare X
  defVP : {v2 : V2 ; adv : AdV ; mode : DefMode} -> VP -> Str -> VP = \dv, bex, x ->
    case dv.mode of {
      PassToBe   => mkVP (passiveVP dv.v2) (lin Adv (mkUtt bex)) ;   -- is said to be X
      PassDirect => mkVP (passiveVP dv.v2) (lin Adv {s = x}) ;      -- is called X
      ByDef      => mkVP dv.adv bex                                  -- is by definition X
      } ;

  defAPVP : {v2 : V2 ; adv : AdV ; mode : DefMode} -> AP -> VP = \dv, ap ->
    defVP dv (mkVP ap) (mkUtt ap).s ;
  defNPVP : {v2 : V2 ; adv : AdV ; mode : DefMode} -> NP -> VP = \dv, np ->
    defVP dv (mkVP np) (mkUtt np).s ;

  defJmt : LabelT -> {text : Text ; isEmpty : Bool} -> S -> Text = \label, hypos, s ->
    labelText label (thenText hypos s) ;

  strAdv : Str -> Syntax.Adv = \s -> lin Adv {s = s} ;
  strNP : Str -> NP = \s -> symb (mkSymb s) ;
  sStr : S -> Str = \s -> (mkUtt s).s ;
  optCommaG : Str = [] | "," ;  -- a closing comma is accepted in parsing
  enumPropStr : {s : EnumPos => Str} -> Str = \enum -> ":" ++ enum.s ! Pa ;

  enumLabel : EnumPos -> Str = \p -> case p of {
    Pa => "a" ; Pb => "b" ; Pc => "c" ; Pd => "d" ;
    Pe => "e" ; Pf => "f" ; Pg => "g" ; Ph => "h"
    } ++ ")" ;
  enumNext : EnumPos -> EnumPos = \p -> case p of {
    Pa => Pb ; Pb => Pc ; Pc => Pd ; Pd => Pe ; Pe => Pf ; Pf => Pg ; _ => Ph
    } ;
  enumItemStr : EnumPos -> Proposition -> Str = \p, prop ->
    enumLabel p ++ sStr (partProp prop) ;

lin
-- predicates

  AdjPred adj = mkVP adj ;
  Adj2Pred rel y = mkVP (Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep y)) ;
  VerbPred verb = verb ;
  Verb2Pred verb y = mkVP verb.vp (Syntax.mkAdv verb.prep y) ;
  Noun2Pred rel y = mkVP (mkNP a_Det (mkCN rel.cn (Syntax.mkAdv rel.prep y))) ;
  KindPred kind = mkVP (mkNP a_Det (useKind kind)) ;
  HavePred det noun = mkVP have_V2 (mkNP det noun) ;

-- propositions

  KindProp x kind = simpleProp (mkS (mkCl x (mkNP a_Det (useKind kind)))) ;
  NotKindProp x kind = simpleProp (mkS negPol (mkCl x (mkNP a_Det (useKind kind)))) ;
  HaveProp x det noun = simpleProp (mkS (mkCl x have_V2 (mkNP det noun))) ;

  NecSuffProp cond x pred prop =
    let
      forAdv : Syntax.Adv = strAdv ((Syntax.mkAdv for_Prep x).s ++ (mkUtt pred).s) ;
      itS : S = mkS (mkCl it_NP (mkVP (Grammar.SentAP cond (mkSC (partProp prop)))))
    in simpleProp (Grammar.ExtAdvS forAdv itS | Grammar.AdvS forAdv itS) ;  -- with or without comma
  necessary_Condition = mkAP necessary_A ;
  sufficient_Condition = mkAP sufficient_A ;
  necsuff_Condition = mkAP and_Conj (mkAP necessary_A) (mkAP sufficient_A) ;

  EquivalentProp enum =
    simpleProp (postAdvS
      (mkS (mkCl (mkNP thePl_Det (mkCN following_A condition_N)) equivalent_A))
      (strAdv (enumPropStr enum))) ;
  BasePropEnum a b = {s = \\p => enumItemStr p a ++ ";" ++ enumItemStr (enumNext p) b} ;
  ConsPropEnum a bs = {s = \\p => enumItemStr p a ++ ";" ++ bs.s ! enumNext p} ;

  ExistUniqueProp argkind prop =
    let exS : S = mkS (Extend.ExistsNP (mkNP a_Det (mkCN unique_A (useKind argkind))))
    in simpleProp (Grammar.SSubjS exS such_that_Subj (partProp prop)
                 | postAdvS exS (Syntax.mkAdv such_that_Subj (partProp prop))) ;  -- with or without comma

  IfSoIsProp x pred y =
    simpleProp (Grammar.ExtAdvS
      (Syntax.mkAdv if_Subj (mkS (mkCl x pred)))
      (mkS then_Adv (symbS (so_is_Str ++ (mkUtt y).s)))) ;

  WheneverProp a b = simpleProp (postAdvS (partProp a) (Syntax.mkAdv whenever_Subj (partProp b))) ;
  GivenProp argkinds prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv given_Prep argkinds.sg) (partProp prop)) ;
  RespProp a b =
    simpleProp (postAdvS (partProp a) (strAdv ("(" ++ resp_Str ++ sStr (partProp b) ++ ")"))) ;

-- definitions

  DefAdjJmt label hypos x dv adj prop =
    defJmt label hypos (postAdvS (mkS (mkCl x (defAPVP dv adj))) (Syntax.mkAdv if_Subj (partProp prop))) ;
  DefNounJmt label hypos x dv kind prop =
    defJmt label hypos
      (postAdvS (mkS (mkCl x (defNPVP dv (mkNP a_Det (useKind kind))))) (Syntax.mkAdv if_Subj (partProp prop))) ;
  DefAdjCJmt label hypos xs dv adj prop =
    defJmt label hypos
      (postAdvS (mkS (mkCl (mkNP and_Conj xs) (defAPVP dv adj))) (Syntax.mkAdv if_Subj (partProp prop))) ;
  DefAnyKindJmt label hypos kind dv df =
    defJmt label hypos (mkS (mkCl (mkNP a_Det (useKind kind)) (defNPVP dv (mkNP any_Det (useKind df))))) ;
  NamingJmt label hypos x dv y =
    defJmt label hypos (mkS (mkCl x (defNPVP dv y))) ;
  NamingKindJmt label hypos kind dv df =
    defJmt label hypos (mkS (mkCl (mkNP aPl_Det (useKind kind)) (defNPVP dv (mkNP aPl_Det (useKind df))))) ;
  WeDefineExpJmt label hypos x y =
    defJmt label hypos (mkS (mkCl we_NP (mkVP (mkVP defineG_V2 x) (lin Adv (mkUtt (mkVP y)))))) ;
  WeSayJmt label hypos p q =
    defJmt label hypos (mkS (mkCl we_NP (mkVP say_VS (postAdvS (partProp p) (Syntax.mkAdv if_Subj (partProp q)))))) ;
  DenoteJmt label hypos t x =
    defJmt label hypos (mkS (mkCl we_NP
      (mkVP (Grammar.AdvVPSlash (mkVPSlash denote_V2) (Syntax.mkAdv by8agent_Prep t)) x))) ;
  IsDenotedJmt label hypos x t =
    defJmt label hypos (mkS (mkCl x (mkVP (passiveVP denote_V2) (Syntax.mkAdv by8agent_Prep t)))) ;

  said_DefVerb = {v2 = say_V2 ; adv = by_definition_AdV ; mode = PassToBe} ;
  called_DefVerb = {v2 = call_V2 ; adv = by_definition_AdV ; mode = PassDirect} ;
  defined_DefVerb = {v2 = defineG_V2 ; adv = by_definition_AdV ; mode = PassToBe} ;
  byDefinition_DefVerb = {v2 = defineG_V2 ; adv = by_definition_AdV ; mode = ByDef} ;

-- naming inside statements

  CalledExp x y = mkNP x (strAdv ("," ++ called_Str ++ (mkUtt y).s ++ optCommaG)) ;
  DenotedExp x t = mkNP x (strAdv ("," ++ denoted_by_Str ++ (mkUtt t).s ++ optCommaG)) ;
  SynonymAdj a b = Grammar.AdvAP a (strAdv ("(" ++ or_Str ++ (mkUtt b).s ++ ")")) ;
  SynonymKind k1 k2 = {
    cn = k1.cn ;
    adv = ccAdv k1.adv (strAdv ("(" ++ or_Str ++ (mkUtt (mkNP a_Det (useKind k2))).s ++ ")"))
    } ;
  RespAdj a b = Grammar.AdvAP a (strAdv ("(" ++ resp_Str ++ (mkUtt b).s ++ ")")) ;

-- kinds

  AdjKind adj kind = {cn = mkCN adj kind.cn ; adv = kind.adv} ;
  PostAdj2Kind kind rel y = {
    cn = kind.cn ;
    adv = ccAdv kind.adv (strAdv (mkUtt (Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep y))).s)
    } ;
  RelKind kind pred = {
    cn = mkCN (mkCN kind.cn kind.adv) (mkRS (mkRCl which_RP pred)) ;
    adv = strAdv []
    } ;
  WithPropertyKind kind x prop = {
    cn = mkCN kind.cn (latexNP (mkSymb x)) ;
    adv = ccAdv kind.adv
            (Syntax.mkAdv with_Prep (mkNP the_Det (mkCN (mkCN property_N) (mkSC (partProp prop)))))
    } ;
  PropertiesKind kind enum = {
    cn = kind.cn ;
    adv = ccAdv kind.adv
            (strAdv ((Syntax.mkAdv with_Prep (mkNP thePl_Det (mkCN following_A property_N))).s
                     ++ enumPropStr enum))
    } ;
  OfTheFormKind kind t decl = {
    cn = kind.cn ;
    adv = ccAdv kind.adv
           (strAdv ((Syntax.mkAdv possess_Prep (mkNP the_Det form_N)).s ++ (mkUtt t).s
                    ++ "," ++ (Syntax.mkAdv whereG_Subj (partProp decl)).s ++ optCommaG))
    } ;
  DeclArgKind kind decl = {
    cn = mkCN kind.cn (strNP (sStr (partProp decl))) ;
    adv = kind.adv ;
    isPl = False
    } ;
  DeclSuchThatKind kind decl prop = {
    cn = mkCN kind.cn (strNP (sStr (partProp decl))) ;
    adv = ccAdv kind.adv (Syntax.mkAdv such_that_Subj (partProp prop))
    } ;

-- quantifiers and determiners

  NumDetKindQuant det kind = mkNP det (useKind kind) ;
  a_NumDet = a_Det ;
  no_NumDet = mkDet no_Quant ;
  AtLeastNumDet c = mkDet (mkCard at_least_AdN c) ;
  AtMostNumDet c = mkDet (mkCard at_most_AdN c) ;
  ExactlyNumDet c = mkDet (mkCard exactly_AdN c) ;
  one_Card = oneG_Card ;
  two_Card = twoG_Card ;
  three_Card = threeG_Card ;
  ExpCard e = termCard (mkUtt e).s ;

-- expressions

  TheUniqueExp argkind prop =
    mkNP the_Det (mkCN unique_A (mkCN (useKind argkind) (Syntax.mkAdv such_that_Subj (partProp prop)))) ;
  NumberOfExp kind =
    mkNP the_Det (mkCN numberG_N (Syntax.mkAdv possess_Prep (mkNP aPl_Det (useKind kind)))) ;
  SetOfAllExp kind =
    mkNP the_Det (mkCN set_N (Syntax.mkAdv possess_Prep (mkNP all_Predet (mkNP aPl_Det (useKind kind))))) ;
  NamelyExp x y = mkNP x (strAdv ("," ++ namely_Str ++ (mkUtt y).s ++ optCommaG)) ;
  EndowedExp x y = mkNP x (strAdv ("," ++ endowed_with_Str ++ (mkUtt y).s ++ ",")) ;

}
