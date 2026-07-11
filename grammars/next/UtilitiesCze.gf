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
  Formal,
  Prelude

in {

oper

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = s.s ++ adv.s} ;
  displayLatexS : Symb -> S = \x -> symb (mkSymb ("$$" ++ x.s ++ "$$")) ;

  compoundCN : CN -> CN -> CN = \cn1, cn2 ->
    cn2 ** {s = \\n, c => cn1.s ! singular ! nominative ++ Predef.BIND ++ cn2.s ! n ! c} ;

  nameCompoundCN : PN -> CN -> CN = \pn, cn ->
    cn ** {s = \\n, c => pn.s ! nominative ++ Predef.BIND ++ cn.s ! n ! c} ;

  npGenNounNP : NP -> CN -> NP = \np, cn -> mkNP (Extend.GenNP np) cn ;

  negPol = negativePol ;

-- Czech RGL has no verb paradigms beyond mkV2, so we build VerbForms here.
-- Three productive present-tense classes are covered:
--   -ovat (kupovat), -at (dělat), -it/-et/-ět (prosit)

  aVerbForms : Str -> Str -> R.VerbForms = \stem, inf -> {
    inf = inf ;
    pressg1 = stem + "ám" ;
    pressg2 = stem + "áš" ;
    pressg3, negpressg3 = stem + "á" ;
    prespl1 = stem + "áme" ;
    prespl2 = stem + "áte" ;
    prespl3 = stem + "ají" ;
    pastpartsg = stem + "al" ;
    pastpartpl = stem + "ali" ;
    } ;

  iVerbForms : Str -> Str -> Str -> R.VerbForms = \stem, inf, past -> {
    inf = inf ;
    pressg1 = stem + "ím" ;
    pressg2 = stem + "íš" ;
    pressg3, negpressg3 = stem + "í" ;
    prespl1 = stem + "íme" ;
    prespl2 = stem + "íte" ;
    prespl3 = stem + "í" ;
    pastpartsg = past ;
    pastpartpl = past + "i" ;
    } ;

  guessVerbForms : Str -> R.VerbForms = \v ->
    case v of {
      _ + "ovat"  => R.iii_kupovatVerbForms v ;
      st + "ět"   => iVerbForms st v (st + "ěl") ;
      st + "et"   => iVerbForms st v (st + "el") ;
      st + "it"   => iVerbForms st v (st + "il") ;
      st + "at"   => aVerbForms st v ;
      _           => aVerbForms v v
      } ;

  mkV : Str -> V = \s -> lin V (guessVerbForms s) ;
  mkVS : Str -> VS = \s -> lin VS (guessVerbForms s) ;
  strV2 : Str -> V2 = \s -> lin V2 (mkV2 (guessVerbForms s)) ;
  mkPN : Str -> PN = \s -> lin PN {s = \\_ => s ; g = mascInanimate} ;

  strN : Str -> N = mkN ;
  strA : Str -> A = mkA ;
  strV : Str -> V = mkV ;
  strPN : Str -> PN = \s -> lin PN {s = \\_ => s ; g = mascInanimate} ;
  strPrep : Str -> Prep = \s -> mkPrep s nominative ;

  define_V2 : V2 = strV2 "definovat" ;
  assume_VS : VS = mkVS "předpokládat" ;
  type_CN : CN = mkCN (hradN "typ") ;
  case_N : N = hradN "případ" ;
  contradiction_N : N = hradN "spor" ;
  then_Adv : Adv = ParadigmsCze.mkAdv "pak" ;
  thenText_Adv : Adv = ParadigmsCze.mkAdv "tedy" ;
  such_that_Subj : Subj = mkSubj "takový, že" ;
  applied_to_Prep : Prep = mkPrep "aplikovaný na" accusative ;
  defined_as_Prep : Prep = mkPrep "definovaný jako" nominative ;
  function_N : N = ruzeN "funkce" ;
  predicate_N : N = hradN "predikát" ;
  basic_type_CN : CN = mkCN (mkA "základní") (hradN "typ") ;
  map_V3 = variants {} ; ---- Czech RGL has no V3
  say_VS = variants {} ;
  hold_V2 = variants {} ;
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
  prove_VS = mkVS "dokazovat" ;

  as_Prep : Prep = mkPrep "jako" nominative ;
  at_Prep : Prep = mkPrep "v" locative ;
  over_Prep : Prep = mkPrep "nad" instrumental ;

  let_Str : Bool => Str = \\_ => "nechť" ;
  assuming_Str = "za následujících předpokladů:" ;

  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ; ---- should be in RGL

  imply_V2 : V2 = strV2 "implikovat" ;
  only_if_Subj : Subj = mkSubj "jen když" ;

  number_Noun = mkCN cislo_N ;
  range_V3 = variants {} ;
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
