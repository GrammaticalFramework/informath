instance UtilitiesPol of Utilities =

open
  SyntaxPol,
  (S=SyntaxPol),
  ParadigmsPol,
  (P=ParadigmsPol),
  (R=ResPol),
  (M=MorphoPol),
  (A=AdjectiveMorphoPol),
  (V=VerbMorphoPol),
  (NM=NounMorphoPol),
  SymbolicPol,
  MarkupPol,
  (Extend=ExtendPol),
  Formal,
  Prelude

in {

oper

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = s.s ++ adv.s} ;
  displayLatexS : Symb -> S = \x -> symb (mkSymb ("$$" ++ x.s ++ "$$")) ;

  compoundCN : CN -> CN -> CN = \cn1, cn2 ->
    cn2 ** {s = \\n, c => cn1.s ! R.Sg ! R.Nom ++ Predef.BIND ++ cn2.s ! n ! c} ;

  nameCompoundCN : PN -> CN -> CN = \pn, cn ->
    cn ** {s = \\n, c => pn.nom ++ Predef.BIND ++ cn.s ! n ! c} ;

  npGenNounNP : NP -> CN -> NP = \np, cn -> mkNP (Extend.GenNP np) cn ;

  negPol = negativePol ;

--------------------------------------------------------------------
-- Nouns.
--
-- ParadigmsPol.mkN is unusable for this vocabulary: its 1-string guesser
-- routes "zbiór" to *zbióra, "element" to *elementowie and "przypadek" to
-- *przypadeka, and its 2-string guesser is dead code (see the note in
-- ParadigmsPol). The ~1050 mkNTable* paradigms in NounMorphoPol are correct
-- but each one bakes in the alternation of one specific exemplar
-- (mkNTable0231 turns "wektor" into *wektot), so they cannot be selected
-- without knowing the table number per word.
--
-- So the declensions are built here, in the manner of UtilitiesCze. Each
-- takes the nominative and the genitive; the genitive supplies the oblique
-- stem, which is what makes the fleeting e (przypadek/przypadku) and the
-- o/ó alternation (zbiór/zbioru) come out right.
--------------------------------------------------------------------

-- Locative/vocative singular of a hard masculine or neuter stem: Polish
-- palatalizes the stem-final consonant before -e, and velars take -u instead.

  locHard : Str -> Str = \st ->
    case st of {
      _ + ("k"|"g"|"ch"|"c"|"cz"|"sz"|"ż"|"rz"|"dz"|"dż"|"j"|"l"|"ń"|"ś"|"ź"|"ć") => st + "u" ;
      x + "st"  => x + "ście" ;
      x + "sł"  => x + "śle" ;
      x + "zł"  => x + "źle" ;
      x + "t"   => x + "cie" ;
      x + "d"   => x + "dzie" ;
      x + "r"   => x + "rze" ;
      x + "ł"   => x + "le" ;
      x + "n"   => x + "nie" ;
      x + "s"   => x + "sie" ;
      x + "z"   => x + "zie" ;
      _         => st + "ie"
      } ;

-- Dative/locative singular of a hard feminine stem. Same palatalization,
-- except that velars take -ce/-dze/-sze rather than -u.

  locFem : Str -> Str = \st ->
    case st of {
      x + "ch"  => x + "sze" ;
      x + "k"   => x + "ce" ;
      x + "g"   => x + "dze" ;
      x + "st"  => x + "ście" ;
      x + "sł"  => x + "śle" ;
      x + "t"   => x + "cie" ;
      x + "d"   => x + "dzie" ;
      x + "r"   => x + "rze" ;
      x + "ł"   => x + "le" ;
      x + "n"   => x + "nie" ;
      x + "s"   => x + "sie" ;
      x + "z"   => x + "zie" ;
      _         => st + "ie"
      } ;

  isVelarStem : Str -> Bool = \st ->
    case st of { _ + ("k"|"g") => True ; _ => False } ;

-- Softness is read off the NOMINATIVE, whose final letter still carries it
-- (pierścień, klucz); the oblique stem has already lost the diacritic.

  isSoftNom : Str -> Bool = \nom ->
    case nom of {
      _ + ("ń"|"ś"|"ź"|"ć"|"l"|"j"|"c"|"cz"|"sz"|"ż"|"rz"|"dz"|"dż") => True ;
      _ => False
      } ;

-- Masculine inanimate: typ/typu, zbiór/zbioru, przypadek/przypadku,
-- wektor/wektora, pierścień/pierścienia, klucz/klucza.

  mascN : Str -> Str -> N = \nom, gen ->
    let
      st : Str = Predef.tk 1 gen ;
      soft : Bool = isSoftNom nom ;
      velar : Bool = isVelarStem st ;
      instr : Str = case soft of {
        True  => st + "em" ;
        False => case velar of { True => st + "iem" ; False => st + "em" }
        } ;
      loc : Str = case soft of {
        True  => st + "u" ;
        False => locHard st
        } ;
      plnom : Str = case soft of {
        True  => st + "e" ;
        False => case velar of { True => st + "i" ; False => st + "y" }
        } ;
      -- a soft stem taken from the genitive already carries its softening i
      -- (pierścienia -> pierścieni), and then the plural genitive is bare
      plgen : Str = case soft of {
        True => case st of { _ + "i" => st ; _ => st + "y" } ;
        False => st + "ów"
        }
    in NM.mkN (table {
      R.SF R.Sg R.Nom => nom ;
      R.SF R.Sg R.Gen => gen ;
      R.SF R.Sg R.Dat => st + "owi" ;
      R.SF R.Sg R.Acc => nom ;
      R.SF R.Sg R.Instr => instr ;
      R.SF R.Sg R.Loc => loc ;
      R.SF R.Sg R.VocP => loc ;
      R.SF R.Pl R.Nom => plnom ;
      R.SF R.Pl R.Gen => plgen ;
      R.SF R.Pl R.Dat => st + "om" ;
      R.SF R.Pl R.Acc => plnom ;
      R.SF R.Pl R.Instr => st + "ami" ;
      R.SF R.Pl R.Loc => st + "ach" ;
      R.SF R.Pl R.VocP => plnom
      }) (R.Masc R.Inanimate) ;

-- The plural genitive of a hard feminine is the bare stem (grupa -> grup),
-- but a stem ending in a consonant cluster in -k/-g breaks it with a fleeting
-- e: płytka -> płytek, książka -> książek, wiązka -> wiązek. After a vowel
-- there is no cluster and nothing is inserted: sztuka -> sztuk.

  femPlGen : Str -> Str = \st ->
    case st of {
      x + "k" => case x of {
        _ + ("a"|"e"|"i"|"o"|"u"|"y"|"ó"|"ą"|"ę") => st ;
        _ => x + "ek"
        } ;
      x + "g" => case x of {
        _ + ("a"|"e"|"i"|"o"|"u"|"y"|"ó"|"ą"|"ę") => st ;
        _ => x + "eg"
        } ;
      _ => st
      } ;

-- Feminine in -a. A genitive in -y marks a hard stem (grupa/grupy), one in
-- -i a soft stem (funkcja/funkcji) -- EXCEPT after a velar, where -i is
-- merely orthographic (Polish cannot write ky/gy): płytka/płytki is hard,
-- with dative płytce and plural płytki, not *płytce/*płytke.

  femN : Str -> Str -> N = \nom, gen ->
    let
      st : Str = Predef.tk 1 nom ;
      velar : Bool = isVelarStem st ;
      genI : Bool = case gen of { _ + "i" => True ; _ => False } ;
      soft : Bool = case velar of { True => False ; False => genI } ;
      datloc : Str = case soft of { True => gen ; False => locFem st } ;
      plnom : Str = case soft of { True => st + "e" ; False => gen } ;
      plgen : Str = case soft of { True => gen ; False => femPlGen st }
    in NM.mkN (table {
      R.SF R.Sg R.Nom => nom ;
      R.SF R.Sg R.Gen => gen ;
      R.SF R.Sg R.Dat => datloc ;
      R.SF R.Sg R.Acc => st + "ę" ;
      R.SF R.Sg R.Instr => st + "ą" ;
      R.SF R.Sg R.Loc => datloc ;
      R.SF R.Sg R.VocP => st + "o" ;
      R.SF R.Pl R.Nom => plnom ;
      R.SF R.Pl R.Gen => plgen ;
      R.SF R.Pl R.Dat => st + "om" ;
      R.SF R.Pl R.Acc => plnom ;
      R.SF R.Pl R.Instr => st + "ami" ;
      R.SF R.Pl R.Loc => st + "ach" ;
      R.SF R.Pl R.VocP => plnom
      }) R.Fem ;

-- Feminine ending in a soft consonant: macierz/macierzy, przestrzeń/
-- przestrzeni, oś/osi.

  femConsN : Str -> Str -> N = \nom, gen ->
    let
      st : Str = Predef.tk 1 gen ;
      -- a genitive in -i marks a palatalized stem (przestrzeń/przestrzeni),
      -- one in -y a hardened one (macierz/macierzy). The palatalized stem
      -- keeps its softening i before a back vowel -- przestrzenią,
      -- przestrzeniom -- except after l and j, which are soft already.
      j : Str = case gen of {
        _ + "y" => "" ;
        _       => case st of { _ + ("l"|"j") => "" ; _ => "i" }
        }
    in NM.mkN (table {
      R.SF R.Sg R.Nom => nom ;
      R.SF R.Sg R.Gen => gen ;
      R.SF R.Sg R.Dat => gen ;
      R.SF R.Sg R.Acc => nom ;
      R.SF R.Sg R.Instr => st + j + "ą" ;
      R.SF R.Sg R.Loc => gen ;
      R.SF R.Sg R.VocP => gen ;
      R.SF R.Pl R.Nom => st + j + "e" ;
      R.SF R.Pl R.Gen => gen ;
      R.SF R.Pl R.Dat => st + j + "om" ;
      R.SF R.Pl R.Acc => st + j + "e" ;
      R.SF R.Pl R.Instr => st + j + "ami" ;
      R.SF R.Pl R.Loc => st + j + "ach" ;
      R.SF R.Pl R.VocP => st + j + "e"
      }) R.Fem ;

-- Feminine in -ość (sprzeczność, własność, równość): the productive abstract
-- suffix, always the kość declension, which NounMorphoPol gets exactly right.

  oscN : Str -> N = \nom -> NM.mkN (NM.mkNTable0475 nom) R.Fem ;

-- Neuter in -o: koło/koła. The plural genitive often lengthens o to ó
-- (koło -> kół), which cannot be guessed; use neutN2 for those.

  neutON : Str -> N = \nom ->
    let st : Str = Predef.tk 1 nom
    in NM.mkN (table {
      R.SF R.Sg R.Nom => nom ;
      R.SF R.Sg R.Gen => st + "a" ;
      R.SF R.Sg R.Dat => st + "u" ;
      R.SF R.Sg R.Acc => nom ;
      R.SF R.Sg R.Instr => st + "em" ;
      R.SF R.Sg R.Loc => locHard st ;
      R.SF R.Sg R.VocP => nom ;
      R.SF R.Pl R.Nom => st + "a" ;
      R.SF R.Pl R.Gen => st ;
      R.SF R.Pl R.Dat => st + "om" ;
      R.SF R.Pl R.Acc => st + "a" ;
      R.SF R.Pl R.Instr => st + "ami" ;
      R.SF R.Pl R.Loc => st + "ach" ;
      R.SF R.Pl R.VocP => st + "a"
      }) R.Neut ;

-- Neuter in -e: zdanie/zdania, pole/pola. A stem in -ni takes -ń in the
-- plural genitive (zdanie -> zdań).

  neutEN : Str -> N = \nom ->
    let
      st : Str = Predef.tk 1 nom ;
      plgen : Str = case st of {
        x + "ni" => x + "ń" ;
        x + "ci" => x + "ć" ;
        _ => st
        }
    in NM.mkN (table {
      R.SF R.Sg R.Nom => nom ;
      R.SF R.Sg R.Gen => st + "a" ;
      R.SF R.Sg R.Dat => st + "u" ;
      R.SF R.Sg R.Acc => nom ;
      R.SF R.Sg R.Instr => st + "em" ;
      R.SF R.Sg R.Loc => st + "u" ;
      R.SF R.Sg R.VocP => nom ;
      R.SF R.Pl R.Nom => st + "a" ;
      R.SF R.Pl R.Gen => plgen ;
      R.SF R.Pl R.Dat => st + "om" ;
      R.SF R.Pl R.Acc => st + "a" ;
      R.SF R.Pl R.Instr => st + "ami" ;
      R.SF R.Pl R.Loc => st + "ach" ;
      R.SF R.Pl R.VocP => st + "a"
      }) R.Neut ;

-- Adjectival nouns: normalna/normalnej, równoległa/równoległej,
-- pochodna, składowa, stała, prosta. These end in -a but take ADJECTIVAL
-- endings (gen -ej, not -y), so they need the adjective table, not femN.

  adjFemN : Str -> N = \nom ->
    let
      st : Str = Predef.tk 1 nom ;
      masc : Str = case st of {
        _ + ("k"|"g") => st + "i" ;
        _             => st + "y"
        } ;
      t : R.AForm => Str = R.mkAtable (A.guess_model masc)
    in NM.mkN (table {
      R.SF R.Sg c => t ! R.AF R.FemSg c ;
      R.SF R.Pl c => t ! R.AF R.OthersPl c
      }) R.Fem ;

-- Indeclinable: acronyms (ADE) and -um loans (kontinuum), which do not
-- decline in the Polish singular.

  invarN : Str -> N = \nom ->
    NM.mkN (\\_ => nom) (R.Masc R.Inanimate) ;

-- A declining head with a frozen modifier ("liczba heptanacci"), for the few
-- terms whose modifier is not an adjective and so cannot agree.

  tailN : N -> Str -> N = \n, tail ->
    NM.mkN (\\f => n.s ! f ++ tail) n.g ;

-- The dispatcher. Given the nominative and the genitive, the declension is
-- determined: -a is feminine, -o/-e neuter, -ość the kość type, and a noun
-- in a consonant is masculine if its genitive is in -u/-a and feminine if it
-- is in -y/-i (macierz/macierzy).

  polN : Str -> Str -> N = \nom, gen ->
    case nom of {
      _ + "ość" => oscN nom ;
      _ + "a"   => case gen of {
        _ + "ej" => adjFemN nom ;   -- normalna/normalnej
        _        => femN nom gen
        } ;
      _ + "o"   => neutON nom ;
      _ + "e"   => neutEN nom ;
      _ => case gen of {
        _ + ("u"|"a") => mascN nom gen ;
        _             => femConsN nom gen
        }
      } ;

-- One-argument fallback: guess the genitive, then decline. Only safe for the
-- shapes below; anything else gets a masculine -u genitive, which is the
-- commonest default for inanimate loans (graf, typ, wektor).

  polN1 : Str -> N = \nom ->
    case nom of {
      _ + "ość" => oscN nom ;
      st + "a"  => femN nom (st + "y") ;
      _ + "o"   => neutON nom ;
      _ + "e"   => neutEN nom ;
      _         => mascN nom (nom + "u")
      } ;

--------------------------------------------------------------------
-- Adjectives.
--------------------------------------------------------------------

-- The comparative is the analytic "bardziej X", built by prefixing every
-- form: A.guess_model cannot match a multiword string, so the RGL's own
-- mkCompAdj would break on it.

  prefixAForms : Str -> R.adj11forms -> R.adj11forms = \p, f -> {
    s1 = p ++ f.s1 ; s2 = p ++ f.s2 ; s3 = p ++ f.s3 ; s4 = p ++ f.s4 ;
    s5 = p ++ f.s5 ; s6 = p ++ f.s6 ; s7 = p ++ f.s7 ; s8 = p ++ f.s8 ;
    s9 = p ++ f.s9 ; s10 = p ++ f.s10 ; s11 = p ++ f.s11
    } ;

  mkA : Str -> A = \s ->
    let f : R.adj11forms = A.guess_model s in lin A {
      pos = f ;
      comp = prefixAForms "bardziej" f ;
      super = prefixAForms "najbardziej" f ;
      advpos = adjAdverb s ;
      advcomp = "bardziej" ++ adjAdverb s ;
      advsuper = "najbardziej" ++ adjAdverb s
      } ;

-- A classifying adjective follows its noun in Polish: "liczba naturalna",
-- "iloczyn kartezjański", not *"naturalna liczba". PositA always builds a
-- preposed AP, so isPost has to be set here.

  postA : A -> AP = \a -> lin AP {
    s = R.mkAtable a.pos ;
    adv = a.advpos ;
    isPost = True
    } ;

  postAdjN : Str -> N -> CN = \a, n -> mkCN (postA (mkA a)) (mkCN n) ;

  postAdjCN : Str -> CN -> CN = \a, cn -> mkCN (postA (mkA a)) cn ;

-- Deadjectival adverbs end in -o or -ie; -ny and -ły take -ie/-le. A guess,
-- used only in AdA-style positions.

  adjAdverb : Str -> Str = \s ->
    case s of {
      st + "ny"  => st + "nie" ;
      st + "ły"  => st + "le" ;
      st + "czy" => st + "czo" ;
      st + "y"   => st + "o" ;
      st + "i"   => st + "o" ;
      _          => s
      } ;

--------------------------------------------------------------------
-- Verbs. All verbs used here are imperfective, the aspect Polish
-- mathematical prose uses for stating what holds. The conjugation class is
-- guessed from the infinitive, following Saloni's numbering as implemented
-- in VerbMorphoPol.
--------------------------------------------------------------------

  guessConj : Str -> V.ConjCl = \v ->
    case v of {
      _ + "ywać"  => V.conj54 ;    -- skazywać: -uję -ujesz
      _ + "iwać"  => V.conj55 ;    -- oszukiwać
      _ + "awać"  => V.conj57 ;    -- dawać
      _ + "ować"  => V.conj53 ;    -- ratować, definiować
      _ + "ać"    => V.conj98 ;    -- pytać, zakładać
      _ + "ścić"  => V.conj84 ;    -- opuścić
      _ + "dzić"  => V.conj80 ;    -- budzić, dowodzić
      _ + "cić"   => V.conj81 ;    -- tracić
      _ + "sić"   => V.conj83 ;    -- prosić
      _ + "lić"   => V.conj75 ;    -- dzielić
      _ + "nić"   => V.conj74 ;    -- waśnić
      _ + "yć"    => V.conj87 ;    -- męczyć, liczyć
      _ + "ić"    => V.conj72 ;    -- kupić, mówić, lubić
      _ + "eć"    => V.conj90 ;    -- myśleć
      _           => V.conj98
      } ;

  mkV : Str -> V = \s -> V.mkMonoVerb s (guessConj s) R.Imperfective ;
  mkVS : Str -> VS = \s -> lin VS (mkV s) ;
  strV2 : Str -> V2 = \s -> V.dirV2 (mkV s) ;

--------------------------------------------------------------------
-- Prepositions, adverbs, conjunctions. ParadigmsPol has none of these
-- beyond mkAdv.
--------------------------------------------------------------------

  mkPrep : Str -> R.Case -> Prep = \s, c -> lin Prep (R.mkCompl s c) ;
  mkSubj : Str -> Subj = \s -> lin Subj {s = s} ;    ---- should be in RGL
  mkConj : Str -> Conj = \s -> lin Conj {s1, sent1 = [] ; s2, sent2 = s} ;

-- Proper names are LaTeX identifiers, hence indeclinable.

  mkPN : Str -> PN = \s ->
    lin PN {nom = s ; voc = s ; dep = \\_ => s ; gn = R.MascInaniSg ; p = R.P3} ;

  strN : Str -> N = polN1 ;
  strA : Str -> A = mkA ;
  strV : Str -> V = mkV ;
  strPN : Str -> PN = mkPN ;
  strPrep : Str -> Prep = \s -> mkPrep s R.Nom ;

--------------------------------------------------------------------
-- Vocabulary.
--------------------------------------------------------------------

  define_V2 : V2 = strV2 "definiować" ;
  assume_VS : VS = mkVS "zakładać" ;
  type_CN : CN = mkCN typ_N ;
  case_N : N = mascN "przypadek" "przypadku" ;
  contradiction_N : N = oscN "sprzeczność" ;
  then_Adv : Adv = P.mkAdv "wtedy" ;
  thenText_Adv : Adv = P.mkAdv "zatem" ;
  otherwise_Adv : Adv = P.mkAdv "inaczej" ;
  such_that_Subj : Subj = mkSubj "taki, że" ;
  applied_to_Prep : Prep = mkPrep "zastosowany do" R.Gen ;
  defined_as_Prep : Prep = mkPrep "zdefiniowany jako" R.Nom ;
  function_N : N = funkcja_N ;
  predicate_N : N = mascN "predykat" "predykatu" ;
  basic_type_CN : CN = mkCN (mkA "podstawowy") typ_N ;
  map_V3 = V.mkV3 (mkV "odwzorowywać") [] "na" R.Acc R.Acc ;
  say_VS = mkVS "mówić" ;
  hold_V2 = V.mkV2 (mkV "zachodzić") "dla" R.Gen ;
  arbitrary_A = mkA "dowolny" ;
  set_N = zbior_N ;
  proposition_N = neutEN "zdanie" ;

  iff_Subj : Subj = mkSubj "wtedy i tylko wtedy, gdy" ;
  commaConj : Conj = mkConj "," ;

  basic_concept_Str = "pojęcie podstawowe" ;
  by_cases_Str = "przez rozpatrzenie przypadków:" ;
  proof_Str = "dowód" ;
  axiom_Str = "aksjomat" ;
  theorem_Str = "twierdzenie" ;
  definition_Str = "definicja" ;

  instance_N = femN "instancja" "instancji" ;
  prove_VS = mkVS "dowodzić" ;

  as_Prep : Prep = mkPrep "jako" R.Nom ;
  at_Prep : Prep = mkPrep "w" R.Loc ;
  over_Prep : Prep = mkPrep "nad" R.Instr ;

  let_Str : Bool => Str = \\_ => "niech" ;
  assuming_Str = "przy następujących założeniach:" ;

  imply_V2 : V2 = strV2 "implikować" ;
  only_if_Subj : Subj = mkSubj "tylko wtedy, gdy" ;

  number_Noun = mkCN liczba_N ;
  range_V3 = V.mkV3 (mkV "przebiegać") "od" "do" R.Gen R.Gen ;
  sum_N = femN "suma" "sumy" ;
  where_Subj = mkSubj "gdzie" ;

  given_A2 = P.mkA2 (mkA "dany") "jako" P.genPrep ;
  infinity_NP = mkNP (oscN "nieskończoność") ;
  series_N = mascN "szereg" "szeregu" ;
  integral_N = femN "całka" "całki" ;

  latexSymbNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  liczba_N : N = femN "liczba" "liczby" ;
  pusty_A : A = mkA "pusty" ;
  typ_N : N = mascN "typ" "typu" ;
  zbior_N : N = mascN "zbiór" "zbioru" ;
  funkcja_N : N = femN "funkcja" "funkcji" ;
  element_N : N = mascN "element" "elementu" ;

}
