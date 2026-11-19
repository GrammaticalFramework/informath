# Godement Tinkered / Godement bricolé

Two books that nobody wrote:

- [godement-tinkered.pdf](./godement-tinkered.pdf) (English)
- [godement-bricole.pdf](./godement-bricole.pdf) (French)

They follow the chapters of Roger Godement's *Cours d'algèbre* (1963;
English translation *Algebra*, 1968): logic and set theory, groups, rings,
modules, linear algebra, determinants, polynomials, eigenvalues, the Jordan
form and hermitian forms.

## How they were made

1. **Formalization.** Claude agents in Claude Code, directed by Aarne Ranta,
   formalized the book in Rocq from its text. The result has 5,363
   declarations and 4,483 theorems proved by tactics, with every statement
   checked against its sentence of the book.
2. **Export.** The formalization was exported to Dedukti.
3. **Reading.** Informath read the Dedukti back in English and French.

Every definition, theorem and proof outside the framed boxes was written by
Informath. The proofs are cut to about the detail of a textbook. The framed
boxes, a preface and a short description of each chapter, were written by
Claude. Neither book contains Godement's text: they have only the titles of
his chapters and sections, the order of his results, and the mathematics.

The books are a test for human readers: can the mathematics be learned from
text generated in this way?

## Sources

- **Formalization:** the `godement-index` repository, directory `rocq/`.
  The books are made by `rocq/book/make-book.py`, and the French readings use
  the French symbol table `rocq/dkgf-fre/`.
- **Pipeline:** Informath's `scripts/rocq/` and `scripts/proof_units.py`.
- **Template** for similar projects:
  [autoformalize/claude-rocq](../autoformalize/claude-rocq/README.md).
