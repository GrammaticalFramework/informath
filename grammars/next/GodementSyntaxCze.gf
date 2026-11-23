concrete GodementSyntaxCze of GodementSyntax =
  MathExtensionsCze
  **
  GodementSyntaxFunctor - [
    said_DefVerb, defined_DefVerb, DefAdjJmt, DefAdjCJmt, DefNounJmt, WeSayJmt, WithPropertyKind, DenoteJmt, IsDenotedJmt,
    Condition, NecSuffProp, necessary_Condition, sufficient_Condition, necsuff_Condition,
    AtLeastNumDet, AtMostNumDet, ExactlyNumDet,
    CalledExp, DenotedExp, EndowedExp, NamelyExp, SynonymAdj, RespAdj, PostAdj2Kind
    ] with
    (Syntax = SyntaxCze),
    (Symbolic = SymbolicCze),
    (Grammar = GrammarCze),
    (Extend = ExtendCze),
    (Utilities = UtilitiesCze),
    (UtilitiesGodement = UtilitiesGodementCze)
  ** open
    ParadigmsCze,
    (S = SyntaxCze),
    (G = GrammarCze),
    (R = ResCze),
    UtilitiesCze,
    UtilitiesGodementCze,
    SymbolicCze,
    Formal,
    Prelude
in {

-- written by Claude, to be reviewed

-- Czech says "E se nazývá X" (the reflexive passive) and "E je z definice X",
-- not "se říká být X" (the English mode PassToBe)
lin
  said_DefVerb = {v2 = call_V2 ; adv = by_definition_AdV ; mode = PassDirect} ;
  defined_DefVerb = {v2 = defineG_V2 ; adv = by_definition_AdV ; mode = ByDef} ;

-- an adjective after "se nazývá" would not agree with the subject
-- ("grupa se nazývá abelovský"): "říkáme, že E je X, jestliže P" instead
oper
  defAPS : {v2 : S.V2 ; adv : S.AdV ; mode : DefMode} -> NP -> AP -> S -> S = \dv, x, ap, cond ->
    case dv.mode of {
      ByDef => postAdvS (mkS (mkCl x (mkVP dv.adv (mkVP ap)))) (ifAdvC cond) ;
      _ => mkS (mkCl weNP (mkVP say_VS (postAdvS (mkS (mkCl x ap)) (ifAdvC cond))))
      } ;

-- with the comma that Czech requires before a subordinate clause
  ifAdvC : S -> S.Adv = \s -> S.mkAdv (mkSubj ", jestliže") s ;

lin
  DefNounJmt label hypos x dv kind prop =
    defJmt label hypos
      (postAdvS (mkS (mkCl x (defNPVP dv (mkNP a_Det (useKind kind))))) (ifAdvC (partProp prop))) ;
  WeSayJmt label hypos p q =
    defJmt label hypos (mkS (mkCl weNP (mkVP say_VS (postAdvS (partProp p) (ifAdvC (partProp q)))))) ;
  -- "s vlastností, že P"
  WithPropertyKind kind x prop = {
    cn = mkCN kind.cn (latexNP (mkSymb x)) ;
    adv = ccAdv kind.adv
            (S.mkAdv with_Prep (mkNP the_Det (mkCN (mkCN property_N) (S.mkAdv (mkSubj ", že") (partProp prop)))))
    } ;
  DefAdjJmt label hypos x dv adj prop =
    defJmt label hypos (defAPS dv x adj (partProp prop)) ;
  DefAdjCJmt label hypos xs dv adj prop =
    defJmt label hypos (defAPS dv (mkNP and_Conj xs) adj (partProp prop)) ;

-- "symbolem t označujeme x", "x se označuje symbolem t": by8agent_Prep is "od"
oper
  symbolem_Prep : S.Prep = mkPrep "symbolem" nominative ;
lin
  DenoteJmt label hypos t x =
    defJmt label hypos (mkS (mkCl weNP
      (mkVP (G.AdvVPSlash (mkVPSlash denote_V2) (S.mkAdv symbolem_Prep t)) x))) ;
  IsDenotedJmt label hypos x t =
    defJmt label hypos (mkS (mkCl x (mkVP (passiveVP denote_V2) (S.mkAdv symbolem_Prep t)))) ;

-- "aby x bylo invertibilní, je nutné a stačí, aby P" needs subjunctives that
-- the propositions do not have: "x je invertibilní, jen když P / jestliže P /
-- právě tehdy, když P"
lincat
  Condition = Subj ;
lin
  NecSuffProp cond x pred prop =
    simpleProp (postAdvS (mkS (mkCl x pred)) (S.mkAdv cond (partProp prop))) ;
  necessary_Condition = mkSubj ", jen když" ;
  sufficient_Condition = mkSubj ", jestliže" ;
  necsuff_Condition = mkSubj ", právě když" ;

-- Czech has no at_least_AdN, at_most_AdN
  AtLeastNumDet c = mkDet (mkCard (ParadigmsCze.mkAdN "alespoň") c) ;
  AtMostNumDet c = mkDet (mkCard (ParadigmsCze.mkAdN "nejvýše") c) ;
  ExactlyNumDet c = mkDet (mkCard exactly_AdN c) ;

-- the participles agree with the expression they follow:
-- "jádro f, označované Ker(f)", "podgrupa G, zvaná jádro f"
oper
  agrAStr : A -> NP -> Str = \a, np ->
    (mkAP a).s ! R.agrGender np.a ! R.agrNumber np.a ! R.Nom ;
  strAdvC : Str -> S.Adv = \s -> lin Adv {s = s} ;
-- the second NP as a nominative string, and the closing comma always, which
-- Czech requires: with the variants of optCommaG the compilation took minutes
  nomStr : NP -> Str = \np -> np.s ! R.Nom ;
lin
  CalledExp x y =
    mkNP x (strAdvC ("," ++ agrAStr (mkA "zvaný") x ++ nomStr y ++ ",")) ;
  DenotedExp x t =
    mkNP x (strAdvC ("," ++ agrAStr (mkA "označovaný") x ++ nomStr t ++ ",")) ;
  EndowedExp x y =
    mkNP x (strAdvC ("," ++ (S.mkAdv (mkPrep endowed_with_Str instrumental) y).s ++ ",")) ;
  NamelyExp x y =
    mkNP x (strAdvC ("," ++ namely_Str ++ nomStr y ++ ",")) ;

-- the synonym agrees too: "normální (neboli invariantní)"
  SynonymAdj a b = a ** {
    s = \\g, n, c => a.s ! g ! n ! c ++ "(" ++ or_Str ++ b.s ! g ! n ! c ++ ")" ;
    pred = \\agr => a.pred ! agr ++ "(" ++ or_Str ++ b.pred ! agr ++ ")"
    } ;
  RespAdj a b = a ** {
    s = \\g, n, c => a.s ! g ! n ! c ++ "(" ++ resp_Str ++ b.s ! g ! n ! c ++ ")" ;
    pred = \\agr => a.pred ! agr ++ "(" ++ resp_Str ++ b.pred ! agr ++ ")"
    } ;

-- the participle agrees with the kind: "podgrupa G generovaná B"
  PostAdj2Kind kind rel y = {
    cn = mkCN (mkCN kind.cn kind.adv) (G.AdvAP rel.ap (S.mkAdv rel.prep y)) ;
    adv = strAdvC []
    } ;

}
