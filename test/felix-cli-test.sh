#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(dirname -- "$script_dir")
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/informath-felix.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

fixture_trees="$work_dir/fixture.gft"
fixture_output="$work_dir/fixture.txt"

NAPROCHE_LIB="$repo_root/test" \
  felix2informath "$repo_root/test/felix-statements.tex" \
    >"$fixture_trees"
test -s "$fixture_trees"

INFORMATH_ROOT="$repo_root" \
  RunInformath -variations -nbest=3 "$fixture_trees" \
    >"$fixture_output"
test -s "$fixture_output"
