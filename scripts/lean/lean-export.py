#!/usr/bin/env python3
"""Export the declarations of a Lean file to Informath's Dedukti dialect.

    python3 $INFORMATH/scripts/lean/lean-export.py File.lean [-out=<dir>] [-mapping=<file.tsv>]
    # -> <dir>/File.dk, File.json, File-hide.dkgf, File-deps.dk, File-proofs.dk,
    #    File-tactics.json  (default <dir>: out)
    python3 $INFORMATH/scripts/lean/lean-tactic-units.py File <dir>
    # -> <dir>/File-tactic-units.json, for RunInformath -tactic-proof

Lean elaborates the file and Export.lean (next to this script) translates its
declarations (see there). No Lake project is needed for a file that imports
Lean's own libraries only. A file in a Lake project (a lakefile.lean or
lakefile.toml in its directory or above) is elaborated with `lake env` in
the project's directory, so that it can import the project's dependencies,
Mathlib say, which must be built; the project's Lean version must be one
that Export.lean compiles with.

The constants of mapping.tsv (next to this script) become Informath's base
constants; -mapping=<file.tsv> adds the lines of a project's own mapping,
which override those.
"""
import os, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))


def lake_root(d):
    """The directory of the Lake project that d is in, if any."""
    d = os.path.abspath(d)
    while True:
        if any(os.path.exists(os.path.join(d, f)) for f in ('lakefile.lean', 'lakefile.toml')):
            return d
        parent = os.path.dirname(d)
        if parent == d:
            return None
        d = parent


def main():
    files = [a for a in sys.argv[1:] if not a.startswith('-')]
    opts = dict(a[1:].split('=', 1) for a in sys.argv[1:] if a.startswith('-') and '=' in a)
    if len(files) != 1 or not files[0].endswith('.lean'):
        sys.exit(__doc__)
    path = os.path.abspath(files[0])
    out = os.path.abspath(opts.get('out', 'out'))
    os.makedirs(out, exist_ok=True)
    mapping = open(os.path.join(HERE, 'mapping.tsv')).read()
    if 'mapping' in opts:
        mapping += '\n' + open(opts['mapping']).read()
    with tempfile.NamedTemporaryFile('w', suffix='.tsv', delete=False) as f:
        f.write(mapping)
        mfile = f.name
    try:
        cmd = ['lean', '--run', os.path.join(HERE, 'Export.lean'), path, out, mfile]
        root = lake_root(os.path.dirname(path))
        if root:
            cmd = ['lake', 'env'] + cmd
        r = subprocess.run(cmd, cwd=root or os.path.dirname(path))
    finally:
        os.remove(mfile)
    sys.exit(r.returncode)


if __name__ == '__main__':
    main()
