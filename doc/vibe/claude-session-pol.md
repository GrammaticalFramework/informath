# Session Log — Polish Grammar

**Date:** 2026-07-16
**Repos:** informath (`grammars/next/`, branch `main`), gf-rgl (`src/polish/`, branch `master`)
**Tool:** Claude Code (Opus 4.8, 1M context)
**Goal:** Create the Polish concrete syntax modules, following the Cze structure,
so that `InformathPol.gf` compiles. Then, on request, `MizarWordsPol` and a
`UserExtensionsPol` linking to it.

---

## Context

Unlike Czech, the Polish RGL was in good shape: `StructuralPol` was already
complete, and `SyntaxPol`, `SymbolicPol`, `ExtendPol` and `ConstructorsPol` all
built. Only six lins were missing across the whole application surface.

The obstacle was **nouns**. Polish has a large morphology in the RGL — 1057
`mkNTable*` paradigms in `NounMorphoPol`, generated from SGJP — but no usable
way to *select* one for a given word. Roughly half the session went on that,
and on finding the two Polish alternations (§2) that a first-cut guesser gets
wrong silently.

---

## Files

New in `grammars/next/`:

| module | lines | what |
|---|---|---|
| `UtilitiesPol.gf` | 512 | the `Utilities` instance: declensions, conjugation guesser, vocabulary |
| `WikidataWordsPol.gf` | 2015 | all 2003 Wikidata terms |
| `VerbalConstantsPol.gf` | 140 | ~120 math constants |
| `MizarWordsPol.gf` | 60 | the 21 Mizar terms |
| `MizarPol.gf` | 20 | `MizarFunctor` instantiation |
| `MathCorePol.gf` | 40 | functor + three pro-drop overrides |
| `CategoriesPol`, `ExamplesPol`, `MathExtensionsPol`, `ProperNamesPol`, `UserExtensionsPol`, `InformathPol` | ~60 | functor instantiations and glue |

New in `gf-rgl/src/polish/`: `MarkupPol.gf`. Modified: `AdjectivePol`,
`ExtendPol`, `IdiomPol`, `LexiconNounPol`, `MorphoPol`, `NounMorphoPol`,
`ParadigmsPol`, `PronounMorphoPol`, `ResPol`, `SentencePol`, `StructuralPol`,
`VerbPol`.

Also: `Makefile` (`next_grammar` now builds Pol) and `grammars/next/README.md`.

---

## Work done

### 1. gf-rgl — `polish: complete the RGL enough to build an application grammar` (`8c4cd58b`)

**Missing lins.** Reached as qualified `Grammar.X` by the functors, so they
cannot be shimmed from `UtilitiesPol`:

| module | added |
|---|---|
| `AdjectivePol` | `AdvAP` |
| `SentencePol` | `SSubjS` |
| `IdiomPol` | `ImpP3` |
| `StructuralPol` | `that_Subj` |
| `MarkupPol` | new module |
| `ParadigmsPol` | `mkAdv` |

`Markup` is not part of the RGL build script (`Setup.hs`), so `MarkupPol` is
compiled on demand into `dist/alltenses` and copied, exactly as `MarkupCze` is.

**`NomPrep`.** `ComplCase` had no nominative at all, so `jako`, `niż` and
`zdefiniowany jako` — all nominative-governing — came out locative
(*mniejszy niż liczbie*). Added `NomPrep`, and covered it in the `dep` tables of
13 pronouns, `nounPN`, `mkPN`, the structural NPs, and `paris_PN`/`john_PN`
(which inline their own tables despite `LexiconNounPol` being DO NOT EDIT).

Symbolic NPs are case-invariant, so this only matters for non-symbolic
arguments — but those are exactly what `definedCN` and `Lt_Adj2` take.

**Corrections:**

- `VerbPol.CompCN` used the nominative for a predicative noun → *x jest grupa*.
  Polish uses the instrumental, as `CompNP` right below it already did.
- `ExtendPol.ExistsNP` inherited `ExistNP` from `ExtendFunctor` → *jest macierz*.
  Polish distinguishes them: "there exists" is `istnieć`.
- `guess_paradigm_basic` never matched `-ość`, the productive feminine abstract
  suffix, so `sprzeczność` got a *masculine* declension (*sprzecznościa,
  *sprzecznościowi*) despite being tagged feminine.

`ImpP3` uses the `niech` periphrasis. The copula takes the **future**
("niech x będzie grupą"), every other verb the **present** ("niech x należy do
A"): `być` is the only Polish verb with a synthetic future, and `imienne` marks
exactly the copular VPs.

### 2. informath — `a Polish grammar: InformathPol compiles and links` (`6ee105f`)

**The noun problem, and why the declensions live in `UtilitiesPol`.**

Three dead ends, in order of discovery:

1. `ParadigmsPol.mkN` (1-string) guesses badly: *zbióra*, *elementowie*,
   *przypadeka*, *cięgu*, *typiem*, *macierzu*, *pierśtnia*.
2. `mkN` (2-string) **discards the genitive** — the same bug Czech had. Fixing
   it made things *worse*: the 2-string `guess_paradigm` table is dead code and
   unsound. Its first branch `<_ + "a", _ + "">` matches every noun in `-a`
   (the suffix `""` matches anything), and its branches disagree about whether
   `mkNTable*` takes the nominative (`mkNTable0021` does `Predef.tk 1`) or the
   bare stem (`mkNTable0308` does not). Routing the genitive there turns
   `liczba`/`liczby` into *liczbaa*. **Reverted, and documented in place.**
3. The `mkNTable*` paradigms are individually correct but each bakes in one
   exemplar's alternations: `mkNTable0231` (samolot) declines `element`
   perfectly and turns `wektor` into *wektot*. Selecting one needs the table
   number per word, and there is no Polish morphodict to supply it.

So `UtilitiesPol` builds the declensions itself, as `UtilitiesCze` does:
`mascN`, `femN`, `femConsN`, `oscN`, `neutON`, `neutEN`, `adjFemN`, `invarN`,
dispatched by `polN`. Each takes nominative + genitive, and takes the oblique
stem **from the genitive** — the same insight that fixed the Czech paradigms.
Softness is read off the **nominative**, whose final letter still shows it
(`pierścień`, `klucz`); the oblique stem has already lost the diacritic.

Two Polish subtleties found by testing:

- **A velar feminine takes `-i` in the genitive orthographically only** (Polish
  cannot write *ky*/*gy*). `płytka`/`płytki` is **hard**, not soft: dative
  `płytce`, plural `płytki`, plural genitive `płytek`. Reading that `-i` as
  softness mis-declines the entire, very common, `-ka` class. Caught by random
  generation throwing *dla wszystkich płytki*.
- **The plural genitive of a hard feminine breaks a final `-k`/`-g` cluster with
  a fleeting e**: `płytka` → `płytek`, `książka` → `książek`, but `sztuka` →
  `sztuk` after a vowel.

**Pro-drop.** `MathCorePol` excludes `FalseProp`, `AxiomPropJmt` and `ProofProp`
from the functor and redefines them with `ProDrop we_Pron`. Polish says "mamy
sprzeczność"; "my mamy sprzeczność" reads as emphatic. `we_NP` is fixed to
`mkNP we_Pron` by the `Syntax` **interface**, so an instance cannot override it —
`SyntaxPolPD` was tried and rejected by the compiler.

**`WikidataWordsPol`** — all 2003 funs (1177 nouns, 740 adjectives, 86 verbs),
translated by 12 parallel subagents from English, with **Czech** as the hint (the
one complete Slavic grammar; cognates and word-formation often carry over).
Nouns give nominative + genitive. The generator validated every entry against
the paradigms and against `AdjectiveMorphoPol.guess_model`, which raises a
`Predef.error` — a **compile** error — on an unmatched adjective rather than
degrading. Result: 2003/2003 emitted, 0 rejects, 6 indeclinables (`ADE`,
`kontinuum`, `dyskontinuum`, `uniwersum`, `drzewo trie`, `morfizm étale`).

Subagents flagged Polish **adjectival nouns** (`normalna`/`normalnej`,
`zmienna`/`zmiennej`) as violating the shape rules rather than faking a
genitive — a real class the dispatcher was missing. `adjFemN` was added in
response.

### 3. `MizarWordsPol`, `MizarPol`, `UserExtensionsPol` (`c735985`, gf-rgl `36321973`)

`MizarWordsPol` reuses `WikidataWordsPol` wherever the sense agrees, as
`MizarWordsEng` reuses `WikidataWordsEng`. Two things could not be reused:

- Mizar's **field of a relation** (dom R ∪ rng R) is *pole relacji*, not the
  algebraic field, which `WikidataWordsPol` renders as *ciało*.
- `irreflexive`, `carrier`, `strongly`, `miss`, `meet` are not in `WikidataWords`
  at all. `MizarWordsEng` takes them from `NaprocheWordsEng` and the extracted
  `MathWordsEng` (in `grammars/extraction/`, generated from a morphodict);
  neither has a Polish counterpart, so they are defined locally.

The Adj+Noun terms use `postAdjCN`, not `AdjNounNoun`: a classifying adjective
follows its noun in Polish (*relacja binarna*, *struktura topologiczna*).

`MizarPol` is the `MizarFunctor` instantiation, mirroring `MizarEng`.

`UserExtensionsPol` follows `UserExtensionsFin` in commenting out what does not
exist yet, but — unlike the Cze and Fin ones — **is enabled in `InformathPol`**,
which still links. `Informath` already includes `UserExtensions`, so those funs
were previously unlinearizable; covering Mizar and MizarWords is a strict gain.
(`UserExtensionsCze` stays commented out because `HottWordsCze` still holds
Swedish.)

**`AdvVP` word order.** Testing `meet_Verb2` exposed *"π zbiór pusty przecina"*.
`AdvVP` used `setPrefix`, putting the adverbial before the verb — and Informath
passes objects through `PrepNP`, which reach `AdvVP` as an `Adv`. Polish is
neutrally SVO, so it now uses `setSufix`. This also fixed `VerbalConstantsPol`:
*"$P$ liczby π zachodzi"* → *"$P$ zachodzi dla liczby π"*.

---

## Commits

| repo | commit | summary |
|---|---|---|
| gf-rgl | `8c4cd58b` | polish: complete the RGL enough to build an application grammar |
| gf-rgl | `36321973` | polish: put the AdvVP adverbial after the verb |
| informath | `6ee105f` | a Polish grammar: InformathPol compiles and links |
| informath | `c735985` | MizarWordsPol and UserExtensionsPol, enabled in InformathPol |

No pushes were made.

---

## Verification

```
cd gf-rgl && make -f claude-Makefile build && make -f claude-Makefile copy
# plus: gf MarkupPol.gf into dist/alltenses, then copy again -- Markup is not
# in Setup.hs, so it is not built by the RGL build script
cd informath/grammars/next && gf InformathPol.gf     # linking ... OK
                              gf InformathCze.gf     # linking ... OK (unaffected)
cd informath/grammars     && gf InformathEng.gf      # linking ... OK (unaffected)
```

As in the Czech session, the plain `Makefile` fails on this GHC; `claude-Makefile`
(still untracked in gf-rgl) is the only way the rebuild works.

The RGL changes are confined to `src/polish/`, and Cze and Eng were re-linked to
confirm nothing else moved.

Sample linearizations:

```
VarHypo (StrIdent "x") (NounKind group_Noun)  -> niech $ x $ będzie grupą
VarHypo (StrIdent "V") (NounKind space_Noun)  -> niech $ V $ będzie przestrzenią
CoreAllProp (NounKind ring_Noun) ...          -> dla wszystkich pierścieni $ R $ , mamy sprzeczność
CoreExistProp (NounKind matrix_Noun) ...      -> istnieje macierz $ M $ , taki, że mamy sprzeczność
CoreIfProp FalseProp FalseProp                -> jeśli mamy sprzeczność , wtedy mamy sprzeczność
CoreNotProp FalseProp                         -> nie jest przypadkiem , że mamy sprzeczność
Adj2Prop reflexive_in_Adj2 pi_Name emptyset_Name
                                              -> liczba \(\pi\) jest zwrotna w zbiorze pustym
Verb2Prop meet_Verb2 pi_Name emptyset_Name    -> liczba \(\pi\) przecina zbiór pusty
ExistKindProp (NounKind one_sorted_Noun)      -> jest struktura jednosortowa
```

`gr -cat=Prop | l` was also used as a smoke test over the whole vocabulary; it
is what surfaced the velar-feminine bug (*dla wszystkich płytki*).

---

## Known defects

- **`such_that_Subj` does not agree.** *istnieje macierz $M$, **taki**, że …*
  should be *taka*. A frozen masculine string, exactly as in `UtilitiesCze`;
  fixing it needs an agreeing `Subj`, which `Subj = {s : Str}` cannot express.
  This is the identical defect Czech has, for the identical reason.
- Plural genitive of neuters in `-o` does not lengthen o to ó (`koło` → *koł*),
  and ę does not raise to ą (`księga` → *księg*). Neither is guessable from the
  nominative; both need a third form.
- `myśl`-type feminines get plural nominative *myśle* for `myśli`.
- The 2-string `ParadigmsPol.mkN` still ignores its genitive. Repairing
  `guess_paradigm` (ordering + the stem/nominative confusion) would let the
  1057 SGJP paradigms be selected properly, and would make `UtilitiesPol`'s
  hand-written declensions redundant. That is the right long-term fix.
- The Wikidata translations have had no native review. Rare terms are
  best-effort coinages; the subagents flagged `pushforwardowy`, `ordynał`,
  `tamaria` and `data_Noun` → `informacja` as weakest.
- `n_Noun`/`p_Noun` decline as `na`/`pa`; "na" collides with the preposition.

## What generalizes to the next language

The `next/README.md` points here for guidance, so: what actually cost time, in
order.

1. **Find out whether the RGL can select a paradigm before writing vocabulary.**
   The order that worked: get one module compiling (`MathCorePol`), linearize
   *one* noun, and check the whole declension table by hand against known forms.
   Both Czech and Polish turned out to have a `mkN` that quietly drops its
   genitive. Assume the guesser is wrong until a table proves otherwise.
2. **Let the compiler enumerate the gaps.** Writing the scaffolding first and
   compiling gave the exact list of missing lins (`AdvAP`, `SSubjS`, `ImpP3`,
   `that_Subj`) in two iterations. Reading the functors to predict them would
   have been slower and less complete.
3. **A "fix" that activates dead code is not a fix.** Passing the genitive to
   `guess_paradigm` looked obviously right and made output worse. Reverting and
   documenting the trap in place was the useful outcome.
4. **`Predef.error` in a paradigm is a compile error.** `AdjectiveMorphoPol.
   guess_model` errors on an unmatched adjective, so 2003 generated lins are
   all-or-nothing. Validating each entry in the generator, against the same
   suffix list the RGL uses, is what made one compile attempt succeed.
5. **Give the translation subagents shape rules, and let them refuse.** Telling
   them exactly which nom/gen shapes the dispatcher accepts got 2003/2003 with 0
   rejects. More useful still: several *refused* to fake a genitive and flagged
   Polish adjectival nouns (`normalna`/`normalnej`) instead — a whole declension
   class the dispatcher was missing.
6. **Random generation finds what examples miss.** `gr -cat=Prop | l` caught the
   velar-feminine bug across the `-ka` class; the hand-picked test sentences did
   not, because none of them happened to use one.

## Possible follow-ups

- Repair `guess_paradigm` and retire the hand-written declensions.
- The rest of `UserExtensionsPol`: `NaprochePol`, `NaprocheWordsPol` (57 funs),
  `HottWordsPol` (626), `GodementWordsPol` (789), `NaturalDeductionPol`,
  `ProofUnitsPol`. The two big ones would need the same subagent pipeline as
  `WikidataWordsPol`.
- Native review of the 2003 Wikidata terms.
- Commit `claude-Makefile` to gf-rgl (noted in the Czech session too).
