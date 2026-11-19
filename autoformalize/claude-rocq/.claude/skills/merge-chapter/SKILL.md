---
name: merge-chapter
description: Merge a chapter or a pending pass from an agent's copy into the project, rebuild, rebuild the pages and commit (never push).
---

# Merge

1. **Check the base.** The project's version of each file the agent edited
   must be the one the agent started from (`cmp` it with the agent's copy
   of the original, or `git diff --quiet HEAD -- file`).
2. **Copy** `coq/ChapterN.v` and `dkgf/ChapterN.dkgf` into the project,
   with any lines to add to `config.py` or `binders.tsv`.
   - Set the `Require Export` line to the last merged chapter (`sed`).
   - When a chapter is placed between two others, change the next
     chapter's `Require Export` line as well.
3. **Build:** `python3 names.py && ./build.sh`, then the page build.
4. **Commit** with a message that says what is formalized, what is
   PENDING, and which statements were corrected. Add only the project's
   files, never unrelated changes of the user's. Do not push.
5. **Update the memory log**, and start the next agents. Statements that
   were waiting for this chapter now get a pending pass.
