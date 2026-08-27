#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(dirname -- "$script_dir")
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/informath-felix.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

fixture_stdout="$work_dir/fixture.stdout"
INFORMATH_ROOT="$repo_root" \
NAPROCHE_LIB="$repo_root/test" \
  RunInformath -from-felix "$repo_root/test/felix-statements.tex" \
    >"$fixture_stdout"

awk 'NF { nonempty++ } END { exit !(NR == 3 && nonempty == 3) }' \
  "$fixture_stdout"
