#!/bin/bash
# Type-check each judgement of a Dedukti file (one judgement per line, as
# RunInformath prints them) separately, on top of a base file.
# Prints the ill-typed judgements; a count of the well-typed ones goes to stderr.
#   usage: dkcheck-each.sh base.dk judgements.dk workdir
base=$1; file=$2; work=$3
ok=0; bad=0
while IFS= read -r line; do
  [ -z "$line" ] && continue
  { cat "$base"; echo; echo "$line"; } > "$work/one.dk"
  if dk check "$work/one.dk" >/dev/null 2>&1; then
    ok=$((ok+1))
  else
    bad=$((bad+1)); echo "ill-typed: $line"
  fi
done < "$file"
echo "$ok well-typed, $bad ill-typed" >&2
