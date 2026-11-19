---
name: pending-pass
description: Prove the PENDING statements of a chapter once what they need is there, keeping their names and statements, or replace a false one by a corrected theorem.
---

# Pending pass

1. **List them:** `python3 pending.py` lists the PENDING statements with
   what each needs. Choose those whose needs are now met, for example after
   the chapter they wait for has been merged.
2. **Work in a copy.** Edit only the chapter that contains them, and give
   the new helpers a fresh suffix (`32p`, `36q`).
3. **Keep each statement's name and statement.**
   - If the statement is true, turn its `Axiom` into a `Theorem` and
     remove the PENDING comment.
   - If it is false at an edge case, delete it, and prove the corrected
     form under a new name.
4. **Run `./build.sh`**, which must be clean. Report what was proved, the
   helpers, what remains and why, and the files changed.
