#!/bin/sh
# Compile, export to Dedukti and read the formalization:
#
#     ./build.sh            # -> index.json: readings, formulas, proof texts
#     ./build.sh -noread    # compile and export only
#
# INFORMATH is the Informath checkout (default ../informath).
set -e
cd "$(dirname "$0")"
INFORMATH=${INFORMATH:-$(cd ../informath 2>/dev/null && pwd)}
export INFORMATH_ROOT=$INFORMATH
CHAPTERS=$(ls coq | sed -n 's/^Chapter\([0-9]*\)\.v$/\1/p' | sort -n)

python3 names.py || { echo "rename the duplicates first"; exit 1; }
(cd coq && for n in $CHAPTERS; do rocq compile -Q . "" Chapter$n.v; done)
for n in $CHAPTERS; do
  python3 "$INFORMATH/scripts/rocq/rocq-export.py" coq/Chapter$n.v
done

: > table.dkgf
for n in $CHAPTERS; do
  [ -f dkgf/Chapter$n.dkgf ] && cat dkgf/Chapter$n.dkgf >> table.dkgf && echo >> table.dkgf
done

[ "$1" = "-noread" ] && { python3 pending.py -count; exit 0; }
python3 "$INFORMATH/scripts/rocq/rocq-index.py"
python3 pending.py -count
