---
name: formalize-chapter
description: Formalize one chapter (section) of the book in Rocq from its text, with tags, Records for structures, tactic proofs, PENDING markers and a tested symbol table; run as an agent in a copy of the project.
---

# Formalize chapter N

Work in a copy of the project (`scratchpad/wN`), never in the project
itself. Write `coq/ChapterN.v`, with `Require Export Chapter<N-1>.`, and
`dkgf/ChapterN.dkgf`. Give every new name the chapter's suffix.

1. **Read the conventions** in `README.md` / `CLAUDE.md`. Then read the
   chapters this one builds on and their tables, for the names to reuse.
2. **Read the chapter's text**: `text/chN.txt`, one `sent_id<TAB>sentence`
   per line. Formalize it in the book's order:
   - definitions as Definitions, or Records for structures;
   - theorems with their proofs, following the book's argument when it
     gives one;
   - remarks and examples where they state something.
   Tag each declaration with the sentence it formalizes. When a sentence is
   repeated, tag its first occurrence.
3. **Prove** with the elementary tactics only. Mark what is left
   `(* PENDING: what it needs *)`, or `(* AXIOM: ... *)` when the book
   takes it as given. Check every axiom at the edge cases. Prove corrected
   forms of statements that are false there, and say so in a comment.
4. **Word the new constants** in the table. Test each entry with the
   test-wording skill before adding it.
5. **Build:** `./build.sh -noread` while iterating, then `./build.sh` at
   the end. It must report:
   - 0 tactic steps not translated;
   - every declaration read;
   - every proof with a proof text;
   - `names.py` clean.
6. **Report:**
   - the counts;
   - each PENDING or AXIOM statement and why;
   - the statements corrected at edge cases;
   - the encoding choices and the names later chapters should use;
   - sample readings;
   - problems with the exporter or Informath;
   - the exact list of files changed.
