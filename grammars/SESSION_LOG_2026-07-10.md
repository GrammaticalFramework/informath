# Session Log — Finnish Grammar Extension Experiment

**Date:** 2026-07-10
**Repo:** informath (`grammars/`), branch `main`
**Tool:** Claude Code (Opus 4.8, 1M context)
**Goal:** Complete the Finnish concrete grammars by replacing placeholder
(mostly Swedish / English-copy) content with proper Finnish mathematical
vocabulary — part of an experiment on automated grammar completion.

---

## Context

The Finnish concrete modules had been created as compilable templates, largely
copied from the English versions. The task across this session was to translate
their lexical content into Finnish, keeping the abstract identifiers and GF
constructor structure unchanged, and verifying each step with the `gf` compiler.

Helper constructors (`mkNoun`, `mkFun`, `mkFun2`, `mkAdj`, `mkAdj2`, `mkVerb`,
…) are defined in `Utilities.gf` / `UtilitiesFin.gf`; they accept plain `Str`
arguments, so inline strings compile without a morphodict.

---

## Work done

### 1. `UtilitiesFin.gf` — Swedish → Finnish (commit `9ac0431`)

The quoted strings were mostly Swedish placeholders. Replaced with Finnish,
using the English `oper` identifiers as the meaning hints. ~23 strings, e.g.:

| identifier | was (sv) | now (fi) |
|---|---|---|
| `then_Adv` | så | niin |
| `such_that_Subj` | så att | siten että |
| `arbitrary_A` | godtycklig | mielivaltainen |
| `iff_Subj` | om och endast om | jos ja vain jos |
| `proof_Str` | bevis | todistus |
| `theorem_Str` | teorem | lause |
| `definition_Str` | definition | määritelmä |
| `only_if_Subj` | endast om | vain jos |
| `where_Subj` | där | missä |
| `infinity_NP` | äärettön *(typo)* | ääretön |

Strings already in Finnish (määritellä, olettaa, tyyppi, funktio, joukko, …)
were left untouched.

**Note:** several `mkPrep` strings (applied_to, defined_as, as, at, over) are
pre-positioned words; Finnish would normally use grammatical case. They compile
and read acceptably but are not case-governed.

### 2. `VerbalConstantsFin.gf` — English → Finnish (commit `9ac0431`)

Was an English copy (only `supeta`, `jakaa` were Finnish). Rewrote all ~120
quoted strings as Finnish math vocabulary, keeping constructors/identifiers:

- Nouns/types: propositio, numero, totuusarvo, luonnollinen luku, rationaaliluku, reaaliluku, lista, joukko
- Comparisons: yhtä suuri / pienempi / suurempi / … + `kuin`
- Arithmetic: summa, erotus, tulo, osamäärä, `korotus … potenssiin`, vastaluku, `logaritmi … kannassa`, neliöjuuri, seuraaja, itseisarvo, kertoma, `suurin yhteinen tekijä`
- Sets: yhdiste, leikkaus, komplementti, karteesinen tulo, potenssijoukko, osajoukko / aito osajoukko, ylijoukko, alkio, tyhjä joukko, universaali joukko
- Analysis/geometry: pinta-ala, säde, neliö, resultantti, kohtisuora, pituus, normi, mahtavuus, juuri, aste, sini/kosini/tangentti (+ arkus-), pistetulo, kulma
- Adverbs: kaikkialla, melkein kaikkialla, tasaisesti, ei

Both files verified with `gf InformathFin.gf` → `linking ... OK` (only
pre-existing `ProofUnitsFin` warnings remain).

### 3. `WikidataWordsFin.gf` — full translation of ~2051 terms (commit `ab82b63`)

**Diagnosis:** the file was a byte-identical copy of `WikidataWordsEng.gf`,
referencing English morphodict lemmas (`A_N`, `knot_N`, `Abelian_A`, …) that do
**not** exist in the Finnish extraction (`MathWordsFin` is a different, smaller,
genuinely-Finnish set). It therefore did **not** compile — which is why it was
commented out in `InformathFin.gf`.

**Approach:**
- Extracted the abstract identifiers and split them into 3 categories:
  **1216 nouns, 749 adjectives, 86 verbs** (2036 unique keys).
- Split into 7 batches and translated in **parallel across 7 subagents**, each
  producing a `english<TAB>finnish` TSV (nominative singular / A-infinitive).
- Merged the TSVs (full coverage, 0 missing, 0 malformed, 0 dup keys) and
  code-generated the module with a Python script: every abstract identifier
  preserved exactly, each RHS replaced by an inline Finnish string
  (`mkNoun "…"`, `mkAdj "…"`, `mkVerb "…"`).
- Because no lemma references remain, **dropped the `ExtractFin` import**
  (`open UtilitiesFin in {`), so the module compiles without the morphodict.

**Quality (spot-check):** ryhmä, rengas, kunta, joukko, avaruus, monisto,
lyhde (sheaf), ydin (kernel), ominaisarvo (eigenvalue), kohomologia,
homeomorfismi, kvasikoherentti, arkhimedinen, noetherilainen, gaussinen,
jaoton (prime), parillinen/pariton.

**Caveats:** rarest terms are best-effort coinages (e.g. `quiver → nuolikko`,
various `-hedron/-tope/-oid` forms); proper-name adjectives are capitalized
(`Abelin`, `Bairen`); pure symbol/code tokens (`A`, `P2`, `C222`, `csgn`) left
unchanged; inline strings rely on GF smart paradigms, so a few irregular stems
may want explicit paradigms on review.

### 4. `InformathFin.gf` — enable Wikidata (commit `ab82b63`)

Uncommented `WikidataWordsFin`. Full grammar compiles and links:
`gf InformathFin.gf` → `linking ... OK`.

---

## Commits

| commit | summary |
|---|---|
| `9ac0431` | translate Finnish grammar strings from Swedish/English (UtilitiesFin, VerbalConstantsFin) |
| `ab82b63` | translate WikidataWordsFin to Finnish and enable it in InformathFin |

No pushes were made.

---

## Verification commands used

```
gf InformathFin.gf        # full Finnish grammar — linking ... OK
gf WikidataWordsFin.gf    # standalone module — linking ... OK
```

## Possible follow-ups

- Native review of the ~2051 Wikidata translations, esp. rare coinages.
- Convert imperfect `mkPrep` string relations to case-governed prepositions.
- Consider lowercasing proper-name-derived adjectives per house style.
- Add explicit paradigms where GF smart guessing mis-inflects irregular stems.
