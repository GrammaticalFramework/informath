instance UtilitiesCze of Utilities =

open
  SyntaxCze,
  (S=SyntaxCze),
  ParadigmsCze,
  (P=ParadigmsCze),
  (R=ResCze),
  SymbolicCze,
  MarkupCze,
  (Extend=ExtendCze),
  GrammarCze,
  Formal,
  Prelude

in {

oper

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = s.s ++ adv.s} ;
  displayLatexS : Symb -> S = \x -> symb (mkSymb ("$$" ++ x.s ++ "$$")) ;

  -- the modifier in the genitive after the head: relace ekvivalence
  compoundCN : CN -> CN -> CN = \cn1, cn2 ->
    cn2 ** {s = \\n, c => cn2.s ! n ! c ++ cn1.s ! singular ! genitive} ;

  nameCompoundCN : PN -> CN -> CN = \pn, cn ->
    cn ** {s = \\n, c => pn.s ! nominative ++ Predef.BIND ++ cn.s ! n ! c} ;

  npGenNounNP : NP -> CN -> NP = \np, cn -> mkNP (Extend.GenNP np) cn ;

  negPol = negativePol ;

-- Czech RGL has no verb paradigms beyond mkV2, so we build VerbForms here.
-- Three productive present-tense classes are covered:
--   -ovat (kupovat), -at (dělat), -it/-et/-ět (prosit)


  strV2 : Str -> V2 = \s -> mkV2 (mkV s) ;

  strN : Str -> N = mkN ;
  strA : Str -> A = mkA ;
  strV : Str -> V = mkV ;
  strPN : Str -> PN = \s -> lin PN {s = \\_ => s ; g = mascInanimate} ;
  strPrep : Str -> Prep = \s -> mkPrep s nominative ;

  define_V2 : V2 = strV2 "definovat" ;
  assume_VS : VS = mkVS (mkV "předpokládat") ;
  type_CN : CN = mkCN (hradN "typ") ;
  case_N : N = hradN "případ" ;
  contradiction_N : N = hradN "spor" ;
  then_Adv : Adv = ParadigmsCze.mkAdv "pak" ;
  thenText_Adv : Adv = ParadigmsCze.mkAdv "tedy" ;
  such_that_Subj : Subj = mkSubj "takový, že" ;
  such_A : A = mkA "takový" ;
  -- "such" as an AP with a "that" clause, agreeing with the CN
  suchThatCN : CN -> S -> CN = \cn, s ->
    mkCN (AdvAP (mkAP such_A) (S.mkAdv (mkSubj ", že") s)) cn ;
  -- the subject pronoun dropped, and "předpokládejme, že" (1st person plural)
  weNP : NP = mkNP (Extend.ProDrop we_Pron) ;
  assumeUtt : VP -> Utt = \vp -> GrammarCze.ImpPl1 vp ;
  -- the NP of a kind without its clitic and pronoun parameters, which a kind's
  -- NP does not need: with them, [ArgKind] had 256^3 parameter combinations
  ArgKindNP : Type = {s, prep, before, prepBefore : R.Case => Str ; after : Str ; a : R.Agr} ;
  toArgKindNP : NP -> ArgKindNP = \np ->
    {s = np.s ; prep = np.prep ; before = np.before ; prepBefore = np.prepBefore ;
     after = np.after ; a = np.a} ;
  fromArgKindNP : ArgKindNP -> NP = \x ->
    lin NP (x ** {clit = \\_ => [] ; hasClit = False ; isDrop = False ; isPron = False}) ;
  applied_to_Prep : Prep = mkPrep "aplikovaný na" accusative ;
  defined_as_Prep : Prep = mkPrep "definovaný jako" nominative ;
  function_N : N = ruzeN "funkce" ;
  predicate_N : N = hradN "predikát" ;
  basic_type_CN : CN = mkCN (mkA "základní") (hradN "typ") ;
  algebraic_structure_CN : CN = mkCN (mkA "algebraický") (zenaN "struktura") ;
  map_V3 = mkV3 (mkV "zobrazovat")
    (lin Prep {s = [] ; c = accusative ; hasPrep = False})
    (lin Prep {s = "na" ; c = accusative ; hasPrep = True}) ;
  say_VS = mkVS (mkV "říkat") ;
  hold_V2 = mkV2 (mkV "platit") (lin Prep {s = "pro" ; c = accusative ; hasPrep = True}) ;
  form_V2 = mkV2 (mkV "tvořit") ;
  arbitrary_A = mkA "libovolný" ;
  set_N = mnozina_N ;
  proposition_N = hradN "výrok" ;

  iff_Subj : Subj = mkSubj "právě tehdy, když" ;
  commaConj : Conj = mkConj "," ;

  basic_concept_Str = "základní pojem" ;
  by_cases_Str = "rozborem případů:" ;
  proof_Str = "důkaz" ;
  axiom_Str = "axiom" ;
  theorem_Str = "věta" ;
  definition_Str = "definice" ;

  instance_N = ruzeN "instance" ;
  prove_VS = mkVS (mkV "dokazovat") ;

  as_Prep : Prep = mkPrep "jako" nominative ;
  at_Prep : Prep = mkPrep "v" locative ;
  over_Prep : Prep = mkPrep "nad" instrumental ;

  let_Str : Bool => Str = \\_ => "nechť" ;
  assuming_Str = "za následujících předpokladů:" ;


  imply_V2 : V2 = strV2 "implikovat" ;
  only_if_Subj : Subj = mkSubj "jen když" ;

  number_Noun = mkCN cislo_N ;
  range_V3 = mkV3 (mkV "probíhat")
    (lin Prep {s = "od" ; c = genitive ; hasPrep = True})
    (lin Prep {s = "do" ; c = genitive ; hasPrep = True}) ;
  sum_N = mkN "součet" "součtu" mascInanimate ;
  where_Subj = mkSubj "kde" ;

  given_A2 = mkA2 (mkA "daný") as_Prep ;
  infinity_NP = mkNP (mestoN "nekonečno") ;
  series_N = zenaN "řada" ;
  integral_N = hradN "integrál" ;

  latexSymbNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  cislo_N : N = mestoN "číslo" ;
  prazdny_A : A = mkA "prázdný" ;

  mnozina_N : N = zenaN "množina" ;
  element_N : N = mkN "prvek" "prvku" mascInanimate ;

}
