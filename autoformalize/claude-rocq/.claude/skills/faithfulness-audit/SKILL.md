---
name: faithfulness-audit
description: Audit whether each tagged declaration of the Rocq formalization says what its sentence of the text says; one report per unit, problems first. Run as agents over groups of units; read-only.
---

# Faithfulness audit

`python3 templates/dump.py N` (copied to the project as `dump.py`) prints,
for each declaration of `coq/ChapterN.v` with a tag: the sentence and the
one before it, the Rocq statement, Informath's English reading, and the
comment above the declaration (where corrections and encoding choices are
noted). Untagged helpers are listed by name.

For each tagged declaration, a verdict:

- **faithful**: it says what the sentence (or the part of it the
  declaration formalizes) says, up to encoding;
- **corrected**: it differs at an edge case on purpose, with a comment; check
  that the comment is right;
- **weaker / stronger**: it assumes more or concludes less (weaker), or the
  converse, without a comment justifying it;
- **partial**: it formalizes part of the sentence, and no other declaration
  with the same tag covers the rest;
- **different**: it says something else, has the tag of another sentence,
  or is vacuous;
- **unclear**: say why.

Encoding is not a fault: records for structures, `nat` for natural numbers,
relations for partial functions, Hilbert's choice for "the unique x", n ≥ 1
restrictions with a comment.

Write `doc/faithfulness/ChapterN.md`: a first line `# § N: <counts>`, a
section "Problems" (name, sentence id, verdict, what the text says, what
the formalization says, what should change), a section "Checked" (the
faithful and corrected ones). Then a summary `doc/faithfulness/README.md`
with the totals and the most serious problems. Edit nothing else.

The fixes are a pending pass per group of units, in copies: add the missing
statements (uniqueness, converses, further cases, exact forms) as new
declarations with the sentence's tag, comment the necessary extra
hypotheses, remove the unneeded ones, move wrong tags; never weaken a
statement other proofs use.
