# Felix integration

Informath's experimental Felix integration parses Felix source and exports one
Informath GF tree per line. Ordinary `RunInformath` then performs variation,
ranking, and English generation.

```bash
make english_grammar
stack build

felix_library=$(CDPATH= cd -- ../felix/library && pwd)
NAPROCHE_LIB="$felix_library" \
  stack exec -- felix2informath "$felix_library/set.tex" \
  > /tmp/felix-set.gft

INFORMATH_ROOT="$PWD" \
  stack exec -- RunInformath -variations -nbest=3 /tmp/felix-set.gft
```

`felix2informath` writes trees to stdout and diagnostics to stderr. The absolute
path above avoids Felix rejecting `..` in a searched source path.

The translation is lossy and experimental: proofs are omitted, claims are
treated as axioms, and only English generation is supported. Reverse
formalization and compatibility of saved `.gft` files are not promised.
