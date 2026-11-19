---
name: test-wording
description: Test a new Informath symbol table entry with a one-line table before adding it to a chapter's .dkgf.
---

# Test a wording

    mkdir -p $TMP/w && cd $TMP/w
    printf 'Obj : Type.\nfoo : Elem Obj -> Elem Obj -> Elem Obj.\nthm1 : (x : Elem Obj) -> Proof (eq (foo x x) x).\n' > t.dk
    printf 'foo : "the foo of #1 and #2" | $#1 \\star #2$\n' > t.dkgf
    RunInformath -add-symboltables=t.dkgf t.dk

- **The entry parses** if the reading of `thm1` uses it. An error, or a
  reading with `\mathrm{foo}`, means it does not.
- **Declare the constant in `t.dk`:** only the entries of constants
  declared there are checked. `thm` is a Dedukti keyword, so do not use it
  as a name.
- **Arrow-typed binders** need their own parentheses:
  `(f : (Elem Obj -> Elem Obj))`.
- **Forms that fail:** adverb + adjective, "at most", hyphens, unknown
  words, comparatives, plural subjects, predicates that do not start with
  their subject, and four or more shown arguments. Try "the N of #1 at #2",
  or a predicate "#1 is ... of #2", instead.
