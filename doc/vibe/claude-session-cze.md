# Session Log — Czech Grammar Completion

**Date:** 2026-07-10
**Repos:** informath (`grammars/`, branch `main`), gf-rgl (`src/czech/`, branch `master`)
**Tool:** Claude Code (Opus 4.8, 1M context)
**Goal:** Make `InformathCze.gf` compile, link, and produce Czech.

---

## Context

The Czech modules existed only as templates copied from Swedish — including the
paradigm calls, which do not exist in Czech (`mkN "typ" "typer"`). Unlike the
Finnish case, the obstacle was not vocabulary but the **resource grammar**: the
Czech RGL is a stub. Roughly half of this session was spent completing it.

Two distinct problems, in order of discovery:

1. informath's Czech modules were Swedish (vocabulary + paradigms).
2. `ResCze`/`GrammarCze` lacked the categories, lins and paradigms that
   `MathCoreFunctor` and `CategoriesFunctor` need. These are reached as
   qualified `Grammar.X`, so they cannot be shimmed from `UtilitiesCze`.

---

## Work done

### 1. `UtilitiesCze.gf` — Swedish → Czech (commit `e81159b`)

Rewrote with Czech vocabulary and Czech declension paradigms (`hradN`, `zenaN`,
`mestoN`, `ruzeN`, `muzN`, `strojN`, `kostN`, `staveniN`).

`ParadigmsCze` has **no `mkV` and no `mkPN` at all**, so the module builds
`VerbForms` itself for the three productive present classes:

| class | example | pattern |
|---|---|---|
| `-ovat` | definovat | `ResCze.iii_kupovatVerbForms` |
| `-at` | předpokládat | dělat (`-ám -áš -á …`) |
| `-it/-et/-ět` | dokazovat→dělit | prosit (`-ím -íš -í …`) |

Czech has no `V3` category, so `map_V3` and `range_V3` are `variants {}` — the
same treatment `say_VS` and `hold_V2` already had in Swedish.

`MakeStructuralCze` does not exist; its single use (`commaConj`) was replaced by
`ParadigmsCze.mkConj`, which does.

### 2. `VerbalConstantsCze.gf` — Swedish → Czech (commit `e81159b`)

~120 strings. Nouns carry explicit paradigms; prepositions carry a case
(`mkPrep` in Czech is `Str -> Case -> Prep`, unlike the other languages).

- Arithmetic: součet, rozdíl, součin, podíl, mocnina, odmocnina, faktoriál,
  největší společný dělitel
- Sets: sjednocení, průnik, doplněk, kartézský součin, potenční množina,
  podmnožina / vlastní podmnožina, nadmnožina, prvek, prázdná množina
- Comparison: rovný (+dat), menší/větší (+ `než`), různý (+ `od`)
- Geometry/analysis: obsah, poloměr, čtverec, kolmý, délka, norma, mohutnost,
  kořen, stupeň, sinus/kosinus/tangens (+ arkus-), skalární součin, úhel

### 3. Czech RGL — missing lins (commit `a79202f6` in gf-rgl)

`SentenceCze` defined 4 lins; `IdiomCze` and `QuestionCze` were near-empty. Added:

| module | added |
|---|---|
| `CatCze` | lincat `VV` |
| `SentenceCze` | `AdvS`, `ExtAdvS`, `SSubjS`, `EmbedS`, `EmbedQS`, `ImpVP` |
| `IdiomCze` | `ImpP3`, `ExistNP`, `ImpersCl`, `GenericCl` |
| `VerbCze` | `CompCN`, `ComplVS`, `ComplVV`, `PassV2` |
| `NounCze` | `SentCN`, `PredetNP` |
| `AdverbCze` | `SubjS` |
| `QuestionCze` | `QuestIAdv` |
| `PhraseCze` | `UttImpSg`, `UttImpPl`, `UttImpPol` |
| `StructuralCze` | `all_Predet`, `both7and_DConj`, `between_Prep`, `by8means_Prep`, `can_VV`, `either7or_DConj`, `every_Det`, `if_Subj`, `no_Quant`, `on_Prep`, `someSg_Det`, `that_Subj`, `under_Prep`, `where_IAdv` |
| `ExtendCze` | `ExistsNP` un-excluded |

`UttVP` was added upstream by the repo owner during the session.

**Three deliberate approximations**, because `VerbForms` lacks the forms:

- `ImpVP` uses the 1pl present, not an imperative: `PropHypo` → *předpokládáme,
  že …* ("we assume that …"). Normal register in Czech maths, but not an
  imperative.
- `PassV2` uses the reflexive passive (*číslo se dělí*) — `passpart` is
  commented out in `ResCze`.
- `ImpP3` uses *nechť*, right for informath's `VarHypo`, archaic for the RGL's
  own "let John walk" example.

### 4. `WikidataWordsCze.gf` — all 2003 terms (commit `f756218`)

Was a Swedish copy referencing a nonexistent `ExtractCze`. Regenerated from the
abstract: **1177 nouns, 740 adjectives, 86 verbs = 2003 lins**, one per `fun`
(the abstract has 2018 `fun` lines, 15 duplicate declarations).

- Translated by 8 parallel subagents, English as source, French and German lins
  supplied as hints. Those grammars are themselves partial (537 / 348 of 2018),
  so ~75% of terms leaned on English plus Czech word-formation.
- **Nouns give nominative + genitive + gender**, not a bare string. Czech's
  one-argument `guessNounForms` routes anything in `-ce` to the
  masculine-animate *soudce* paradigm, which would wreck `matice`, `funkce`,
  `kružnice`.
- Adjectives constrained to `-ý`/`-í`; verbs to `-ovat/-at/-it/-et/-ět`. The
  generator **rejects** anything else rather than letting it compile to garbage:
  4 perfectives came back (`uzavřít`, `pokrýt`, `protáhnout`, `použít`) and were
  replaced by their imperfectives.
- No morphodict lemmas remain, so no `ExtractCze` is needed. Enabled in
  `InformathCze`.

### 5. Czech RGL — noun paradigms (commit `34434201` in gf-rgl)

Auditing the 1177 nouns against `declensionNounForms` exposed two real defects:

**(a) Neutral consonants were never matched.** The selector tested only
`hardConsonant` (`d t g h k n r`), but `neutralConsonant` (`b f l m p s v`) takes
the hard endings too. 119 nouns — `graf`, `atom`, `profil`, `přístup` — silently
got the *soft* `declSTROJ`. Same gap in `guessNounForms`. Added
`hardishConsonant` (hard + neutral + foreign `z`, `x`).

**(b) The supplied genitive was thrown away.** It was used only to *pick* a
paradigm; the oblique stem was then re-derived from the nominative by
`dropFleetingE`, which guesses wrong in both directions:

| word | was | should be |
|---|---|---|
| člen | čln**u** | člen**u** |
| uzel | uzel**u** | uzl**u** |
| doplněk | doplněk**u** | doplňk**u** |
| výpočet | výpočet**u** | výpočt**u** |

`declHRAD`, `declHRADA`, `declMUZ` now have stem-taking variants and
`declensionNounForms` passes the stem it was given. One-argument
`guessNounForms` keeps the old `dropFleetingE` behaviour, so nothing else moves.

**Seven paradigms added**, all previously hitting the fallback:

| paradigm | example |
|---|---|
| `declLATINUS` / `declLATINUSA` | algoritmus – algoritmu, kosmos – kosmu |
| `declLATINUM` | kontinuum – kontinua |
| `declGREEKMA` | schéma – schématu |
| `declHRADA` | les – lesa, zákon – zákona |
| `declADJF` | proměnná – proměnné |
| `declADJM` | nultý – nultého |
| `declINVAR` | bombé, tamari, software |

Also: feminine consonant stems matched by genitive (`-i` → kost, `-e/-ě` →
píseň), masculine `-e/-ě` genitives (kužel – kužele, král – krále), neuter `-ě`
(těžiště).

**Result: fallbacks on the 1177 nouns went 225 → 1** (the acronym `ADE`).

This incidentally fixed `element_N`, which had shipped in `e81159b` as
`prvek/prveku`; it is now `prvek/prvku`.

---

## Commits

| repo | commit | summary |
|---|---|---|
| informath | `e81159b` | a Czech grammar: InformathCze compiles and links |
| gf-rgl | `a79202f6` | czech: fill in the lins needed to build an application grammar |
| informath | `f756218` | WikidataWordsCze: translate all 2003 terms and enable it |
| gf-rgl | `34434201` | czech: fix noun paradigm selection, add loanword and adjectival types |

No pushes were made.

---

## Verification

```
cd gf-rgl && make -f claude-Makefile build && make -f claude-Makefile copy
cd informath/grammars && gf InformathCze.gf     # linking ... OK
```

Note: the plain `Makefile` fails on this GHC; `claude-Makefile` (untracked in
gf-rgl) passes `-package process/filepath/directory` to `runghc`. It is the only
way the rebuild works and should probably be committed.

Sample linearizations:

```
VarHypo (StrIdent "x") (NounKind group_Noun)   -> nechť $ x $ je grupa
CoreAllProp (NounKind ring_Noun) ...           -> pro všechny okruhy $ R $ , máme spor
CoreExistProp (NounKind matrix_Noun) ...       -> existuje matice $ M $ , takový, že máme spor
CoreIfProp FalseProp FalseProp                 -> jestliže máme spor , pak máme spor
```

---

## Known defects

- **`such_that_Subj` does not agree.** *existuje matice $M$, **takový**, že …*
  should be *taková*. It is a frozen masculine string in `UtilitiesCze`; fixing
  it properly needs an agreeing `Subj`, which the Czech `Subj = {s : Str}`
  lincat cannot express.
- Vocative of fleeting-`e` inanimates is off (`doplňke`). Inanimate vocative is
  essentially unused.
- `map_V3` / `range_V3` linearize to nothing (no `V3` in Czech).
- `ADE_Noun` still takes the soft fallback declension.
- The ~12 multiword nouns (`vlastní číslo`, `orientovaný graf`) inflect only
  their head: genitive comes out `vlastní čísla`, not `vlastního čísla`.
- `mkPrep ""` with a bare case is used for `Eq_Adj2` (dative) and
  `divisible_Adj2` (instrumental); case government there is unverified.
- The Wikidata translations have had no native review. Rare terms are
  best-effort coinages.

## Possible follow-ups

- `UserExtensionsCze` is still commented out of `InformathCze`.
- Native review of the 2003 Wikidata terms.
- Add `passpart` and an imperative to `ResCze.VerbForms`, then revisit
  `PassV2`, `ImpVP`, `ImpP3`.
- Commit `claude-Makefile` to gf-rgl.
