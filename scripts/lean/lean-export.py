#!/usr/bin/env python3
"""Export the declarations of a Lean file to Informath's Dedukti dialect.

    python3 $INFORMATH/scripts/lean/lean-export.py File.lean [-out=<dir>] [-mapping=<file.tsv>] [-project=<dir>]
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

With -project=<dir> (a project like ../rocq's, with a config.py), the
structures of the file are exported as the Rocq exporter exports Records:
as Informath structures, unless the config says RECORDS_AS_STRUCTURES =
False (see structures() below).
"""
import glob, json, os, re, subprocess, sys, tempfile

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


def load_config(d):
    """The settings of a project's config.py, as a dict."""
    path = os.path.join(d, 'config.py')
    conf = {'__file__': path}
    if os.path.exists(path):
        exec(compile(open(path).read(), path, 'exec'), conf)
    return conf


def token(name):
    return re.compile(r"(?<![\w'])%s(?![\w'])" % re.escape(name))


def structures(out, mod, conf):
    """A Lean structure R of the project as an Informath structure, as
    ../rocq/rocq-export.py exports a Record: its kind R : Structure (with its
    parameters: Str Ring -> Structure), its constructor mkR (R.mk; a
    constructor of another name keeps its name) into Str R, and its fields
    by their own names (R.groupLaw is groupLaw: field names are to be unique
    in the project), all in the block of R; a variable of type R is of type
    Str R. The constructor's arguments are named as the project says
    (config.py FIELD_VARIABLES: field -> variable, lawSet -> X). The
    structures of the modules exported before (out/*.json) are renamed in
    this module's export as well; the module's json gets the constructor and
    the fields of each structure."""
    as_structures = conf.get('RECORDS_AS_STRUCTURES', True)
    fvars = conf.get('FIELD_VARIABLES', {})
    dkf = os.path.join(out, mod + '.dk')
    dk = open(dkf).read()
    decls = json.load(open(os.path.join(out, mod + '.json')))
    # the structures of this module: their constructor and fields, from the blocks
    tags = re.findall(r'\(; //LEAN:(\S+) ;\)', dk)
    own = {}
    for d in decls:
        if d['kind'] == 'structure':
            n = d['lean']
            parts = [t[len(n) + 1:] for t in tags if t.startswith(n + '.') and '.' not in t[len(n) + 1:]]
            ctor = parts[0] if parts else 'mk'
            own[n] = {'ctor': ctor, 'fields': parts[1:]}
    known = dict(own)
    for jf in glob.glob(os.path.join(out, '*.json')):
        if os.path.basename(jf)[:-5] == mod or jf.endswith('-tactics.json') or jf.endswith('-tactic-units.json'):
            continue
        try:
            for d in json.load(open(jf)):
                if isinstance(d, dict) and d.get('kind') == 'structure' and 'structureCtor' in d:
                    known.setdefault(d['lean'], {'ctor': d['structureCtor'], 'fields': d['fields']})
        except (ValueError, KeyError):
            pass
    if not known:
        return
    mang = lambda n: n.replace('.', '__')
    renames = {}
    for n, s in known.items():
        r = mang(n)
        renames[r + '__' + s['ctor']] = 'mk' + r.split('__')[-1] if s['ctor'] == 'mk' else s['ctor']
        for fld in s['fields']:
            renames[r + '__' + fld] = fld
    kinds = [mang(n) for n in known]

    def rename(t):
        for a, b in renames.items():
            t = token(a).sub(b, t)
        if as_structures:
            for k in kinds:
                t = re.sub(r"(?<![\w'])Elem (\(?)%s(?![\w'])" % re.escape(k), r'Str \1%s' % k, t)
        return t

    def kind_line(m):
        line = m.group(0)
        if not as_structures:
            return line
        # the parameters without their names, as the Rocq exporter has them: Str Ring -> Structure
        head, ty = line.split(' : ', 1)
        ty = re.sub(r"\([\w']+ : ((?:[^()]|\([^()]*\))*)\) -> ", r'\1 -> ', ty)
        return head + ' : ' + re.sub(r'(Set|Type)\.$', 'Structure.', ty)

    for n, s in own.items():
        r = mang(n)
        # the block of R: its own tag, then the constructor's and the fields' without theirs
        for part in [s['ctor']] + s['fields']:
            dk = re.sub(r'\n*\(; //LEAN:%s ;\)\n' % re.escape(n + '.' + part), '\n', dk)
    dk = rename(dk)
    for n, s in own.items():
        r = mang(n)
        dk = re.sub(r'(?m)^%s : .*$' % re.escape(r), kind_line, dk)
        ctor = renames[r + '__' + s['ctor']]

        def ctor_line(m):
            line = m.group(0)
            for fld in s['fields']:
                if fld in fvars:
                    line = token(fld).sub(lambda _: fvars[fld], line)
            return line
        dk = re.sub(r'(?m)^%s : .*$' % re.escape(ctor), ctor_line, dk)
    open(dkf, 'w').write(dk)
    # the constants declared for the module (earlier structures among them)
    deps = os.path.join(out, mod + '-deps.dk')
    if os.path.exists(deps):
        t = rename(open(deps).read())
        for k in kinds:
            t = re.sub(r'(?m)^%s : .*$' % re.escape(k), kind_line, t)
        open(deps, 'w').write(t)
    for suffix in ('-proofs.dk', '-hide.dkgf'):
        p = os.path.join(out, mod + suffix)
        if os.path.exists(p):
            open(p, 'w').write(rename(open(p).read()))
    tac = os.path.join(out, mod + '-tactics.json')
    if os.path.exists(tac):
        steps = json.load(open(tac))

        def walk(x):
            if isinstance(x, str):
                return rename(x)
            if isinstance(x, list):
                return [walk(y) for y in x]
            if isinstance(x, dict):
                return {k: (walk(v) if k in ('before', 'after', 'hyps', 'target') else v) for k, v in x.items()}
            return x
        json.dump([walk(s) for s in steps], open(tac, 'w'), ensure_ascii=False)
    for d in decls:
        if d['lean'] in own:
            s = own[d['lean']]
            d['structureCtor'] = s['ctor']
            d['constructor'] = renames[mang(d['lean']) + '__' + s['ctor']]
            d['fields'] = s['fields']
    json.dump(decls, open(os.path.join(out, mod + '.json'), 'w'), ensure_ascii=False, indent=0)


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
    if r.returncode == 0 and 'project' in opts:
        structures(out, os.path.basename(path)[:-5], load_config(opts['project']))
    sys.exit(r.returncode)


if __name__ == '__main__':
    main()
