"""A map of the Haskell and GF code of a project, in Markdown: the files with
their sizes and descriptions, and the dependencies between them as Mermaid
diagrams. Only the project's own modules are shown, not the libraries they
import. With -check, it checks instead that a document still matches the
code: that its links lead to files, and the names it quotes exist.

    python3 scripts/code_map.py [options] DIR ...  > map.md

Options:
    -out=FILE        the file the Markdown will be in: links are made relative to it
    -hs-main=M,...   only the Haskell modules reachable from these
    -gf-top=M,...    only the GF modules reachable from these
    -collapse=P,...  show the modules P.X.Y as one node P.* (e.g. BNFC-generated ones)
    -omit=M,...      leave out the edges to these modules, and list their importers
    -exclude=D,...   skip directories with these names (default: .stack-work, dist,
                     dist-newstyle, .git, node_modules)
    -no-files        no file tables
    -check=DOC       check DOC against the code in DIR ... instead
    -check-suffixes=S,...  the files whose words the check looks for
                     (default: hs,gf,dk,dkgf,bnf)

For Informath, the map of doc/informalization-pipeline.md:

    python3 scripts/code_map.py -out=doc/map.md -hs-main=Main -gf-top=Informath,InformathEng,InformathLatex \\
      -collapse=Dedukti,Agda,Lean,Rocq '-omit=Utils,Dedukti.*' app src grammars > doc/map.md
    python3 scripts/code_map.py -check=doc/informalization-pipeline.md app src grammars share test

(The quotes keep the shell from expanding Dedukti.* as a file name pattern.)

The descriptions are the first comment of each file (a Haddock module comment
if there is one). The GF diagrams are one of the abstract syntax and one for
each concrete syntax among the -gf-top modules; in a concrete syntax, the
edges say how a module is built: extends, instantiates (a functor),
opens, with (a functor's interface given an instance).
"""

import collections
import glob
import os
import re
import sys

MAX_NODES = 80     # the largest diagram of concrete syntax drawn without -gf-top
EXCLUDE = ['.stack-work', 'dist', 'dist-newstyle', '.git', 'node_modules']


def option(name, default=None):
    for a in sys.argv[1:]:
        if a == '-' + name:
            return True
        if a.startswith('-%s=' % name):
            return a.split('=', 1)[1]
    return default


def options_list(name):
    v = option(name)
    return v.split(',') if isinstance(v, str) and v else []


def source_files(dirs, suffix, exclude):
    out = []
    for d in dirs:
        if os.path.isfile(d):
            if d.endswith(suffix):
                out.append(d)
            continue
        for root, subdirs, files in os.walk(d):
            subdirs[:] = sorted(s for s in subdirs if s not in exclude)
            out += [os.path.join(root, f) for f in sorted(files) if f.endswith(suffix)]
    return out


def strip_comments(s, line_comment='--'):
    s = re.sub(r'\{-.*?-\}', ' ', s, flags=re.S)
    return re.sub(re.escape(line_comment) + r'.*', '', s)


# the first line of code that defines something, in Haskell or GF
DEFINITION = re.compile(r"^(?:[a-z_][\w']*\s*(?:::|=|[a-z_(])|data |type |newtype |class |instance |"
                        r"(?:fun|lin|lincat|cat|oper|param|flags)\b|\s+[a-z_][\w']*\s*[:=])")


def first_comment(text):
    """A file's description: its Haddock module comment, or else the first
    comment before the first definition that says something (not a pragma,
    a path, a ruler, a TODO or code commented out)."""
    m = re.search(r'\{-\|(.*?)-\}', text, re.S)
    if m:
        return ' '.join(m.group(1).split())
    head = []
    for line in text.split('\n'):
        if DEFINITION.match(line):
            break
        head.append(line)
    lines = []
    for line in head:
        m = re.match(r'\s*--+\s?\|?\s*(.*)', line)
        if m and lines and not m.group(1).strip():
            break
        if m and m.group(1).strip() and not re.match(r'#|-+$|[=*-]{3,}|TODO|import |module ', m.group(1).strip()):
            lines.append(m.group(1).strip())
            if len(lines) == 2:
                break
        elif lines:
            break
    return ' '.join(lines)


def md_escape(s):
    return s.replace('|', '\\|').replace('<', '&lt;').replace('>', '&gt;')


def link(path, out):
    base = os.path.dirname(os.path.abspath(out)) if out else os.getcwd()
    return os.path.relpath(os.path.abspath(path), base)


# ------------------------------------------------------------------ Haskell

def haskell_modules(files):
    """module name -> (file, imported module names, text). Of two files of
    the same module, the one named after it keeps the name, and the other
    is named with its file: "ParseInformath (src/PGF2_ParseInformath.hs)"."""
    found = []
    for f in files:
        text = open(f, encoding='utf-8', errors='replace').read()
        code = strip_comments(text)
        m = re.search(r'^module\s+([A-Z][\w.]*)', code, re.M)
        name = m.group(1) if m else 'Main'
        imports = re.findall(r'^import\s+(?:safe\s+)?(?:qualified\s+)?(?:"[^"]*"\s+)?([A-Z][\w.]*)', code, re.M)
        named = os.path.basename(f)[:-3] == name.split('.')[-1]
        found.append((not named, name, f, imports, text))
    mods = {}
    for _, name, f, imports, text in sorted(found, key=lambda x: x[0]):
        if name in mods:
            name = '%s (%s)' % (name, f)
        mods[name] = (f, imports, text)
    return mods


def collapsed(name, prefixes):
    for p in prefixes:
        if name.startswith(p + '.'):
            return p + '.*'
    return name


def reachable(edges, roots):
    seen, todo = set(), list(roots)
    while todo:
        m = todo.pop()
        if m not in seen:
            seen.add(m)
            todo += edges.get(m, [])
    return seen


def haskell_graph(mods, prefixes):
    """node -> internal nodes it imports"""
    nodes = {collapsed(m, prefixes) for m in mods}
    edges = collections.defaultdict(set)
    for m, (_, imports, _) in mods.items():
        a = collapsed(m, prefixes)
        for i in imports:
            b = collapsed(i, prefixes)
            if b in nodes and b != a:
                edges[a].add(b)
    return nodes, edges


def mermaid_id(name):
    return re.sub(r'\W', '_', name)


def mermaid(nodes, edges, omit, labels=None):
    out = ['```mermaid', 'flowchart TD']
    for n in sorted(nodes):
        out.append('  %s["%s"]' % (mermaid_id(n), n))
    for a in sorted(edges):
        for b, label in sorted(edges[a]) if labels else [(b, None) for b in sorted(edges[a])]:
            if b in omit or a not in nodes or b not in nodes:
                continue
            arrow = '-->' if label in (None, 'extends', 'instantiates') else '-.->'
            text = '' if label in (None, 'extends') else '|"%s"|' % label
            out.append('  %s %s%s %s' % (mermaid_id(a), arrow, text, mermaid_id(b)))
    out.append('```')
    return '\n'.join(out)


def omitted_note(nodes, edges, omit):
    notes = []
    for o in sorted(omit):
        users = sorted(a for a in nodes if any((b if isinstance(b, str) else b[0]) == o for b in edges.get(a, [])))
        if users:
            notes.append('- the edges to **%s**, from %s' % (o, ', '.join(users)))
    return ('Left out to keep the picture readable:\n\n' + '\n'.join(notes) + '\n') if notes else ''


def haskell_section(mods, out, prefixes, mains, omit, files_table):
    nodes, edges = haskell_graph(mods, prefixes)
    if mains:
        keep = reachable(edges, [collapsed(m, prefixes) for m in mains])
        nodes = nodes & keep
    parts = ['## Haskell modules\n']
    if files_table:
        rows = ['| module | file | lines | description |', '|---|---|---:|---|']
        groups = collections.defaultdict(list)
        for m, (f, _, text) in mods.items():
            groups[collapsed(m, prefixes)].append((m, f, text))
        for n in sorted(nodes):
            ms = sorted(groups[n])
            if len(ms) == 1:
                m, f, text = ms[0]
                rows.append('| %s | [%s](%s) | %d | %s |' % (m, f, link(f, out), text.count('\n') + 1,
                                                          md_escape(first_comment(text))))
            else:
                total = sum(t.count('\n') + 1 for _, _, t in ms)
                d = os.path.dirname(ms[0][1])
                rows.append('| %s | [%s/](%s) | %d | %d modules |' % (n, d, link(d, out), total, len(ms)))
        parts.append('\n'.join(rows) + '\n')
    parts.append('An arrow means that a module imports the other.\n')
    parts.append(omitted_note(nodes, edges, omit))
    parts.append(mermaid(nodes, edges, set(omit)))
    unreached = sorted({collapsed(m, prefixes) for m in mods} - nodes)
    if mains and unreached:
        parts.append('\nNot reachable from %s: %s.\n' % (', '.join(mains), ', '.join(unreached)))
    return '\n'.join(parts)


# ----------------------------------------------------------------------- GF

def gf_header(text):
    """(kind, name, of, [(relation, module)]) of a GF module, from its header"""
    code = strip_comments(text)
    m = re.search(r'\b(abstract|concrete|resource|interface|instance|incomplete\s+concrete)\s+(\w+)'
                  r'(?:\s+of\s+(\w+))?\s*=?(.*?)(\{|;\s*$|\Z)', code, re.S)
    if not m:
        return None
    kind, name, of, body = ' '.join(m.group(1).split()), m.group(2), m.group(3), m.group(4)
    rels = []
    if of:
        rels.append(('of', of))
    # open M, (P = N) in, wherever it is in the header
    opens = ' '.join(re.findall(r'\bopen\b(.*?)(?:\bin\b|$)', body, re.S))
    body = re.sub(r'\bopen\b.*?(?:\bin\b|$)', ' ', body, flags=re.S)
    for part in body.split('**'):
        part = re.sub(r'-?\s*\[[^\]]*\]', ' ', part).strip().rstrip(',')   # M - [f, g], M [f]
        if not part:
            continue
        m2 = re.match(r'(\w+)\s+with\s*(.*)', part, re.S)
        if m2:                               # a functor instantiated
            rels.append(('instantiates', m2.group(1)))
            for _, inst in re.findall(r'\(\s*(\w+)\s*=\s*(\w+)\s*\)', m2.group(2)):
                rels.append(('with', inst))
            continue
        for w in re.findall(r'\w+', part):
            rels.append(('extends', w))
    for o in re.findall(r'(?:\(\s*\w+\s*=\s*)?(\w+)\s*\)?', opens):
        rels.append(('opens', o))
    return kind, name, rels


def gf_modules(files):
    mods = {}
    for f in files:
        text = open(f, encoding='utf-8', errors='replace').read()
        h = gf_header(text)
        if h and h[1] not in mods:
            mods[h[1]] = (f, h[0], h[2], text)
    return mods


def gf_section(mods, out, tops, omit, files_table):
    edges = {m: [b for _, b in rels if b in mods] for m, (_, _, rels, _) in mods.items()}
    nodes = reachable(edges, tops) if tops else set(mods)
    parts = ['## GF modules\n']
    if files_table:
        rows = ['| module | kind | lines | description |', '|---|---|---:|---|']
        for n in sorted(nodes):
            f, kind, _, text = mods[n]
            rows.append('| [%s](%s) | %s | %d | %s |' % (n, link(f, out), kind, text.count('\n') + 1,
                                                       md_escape(first_comment(text))))
        parts.append('\n'.join(rows) + '\n')
    external = sorted({b for m in nodes for _, b in mods[m][2] if b not in mods})
    if external:
        parts.append('Modules from outside (not shown): %s.\n' % ', '.join(external))
    abstract = {n for n in nodes if mods[n][1] == 'abstract'}
    parts.append('### The abstract syntax\n\nAn arrow means that a module extends the other.\n')
    aedges = {a: {(b, 'extends') for r, b in mods[a][2] if r == 'extends' and b in abstract} for a in abstract}
    parts.append(omitted_note(abstract, aedges, omit))
    parts.append(mermaid(abstract, aedges, set(omit), labels=True))
    concrete = {m: [b for r, b in mods[m][2] if b in mods and r != 'of'] for m in mods}
    # one diagram for each concrete syntax among the tops, or, without tops,
    # one for all the modules that are not abstract, if they are not too many
    if tops:
        diagrams = [(t, reachable(concrete, [t])) for t in tops if t in mods and mods[t][1] == 'concrete']
    else:
        rest = nodes - abstract
        diagrams = [('', rest)] if 0 < len(rest) <= MAX_NODES else []
        if len(rest) > MAX_NODES:
            parts.append('\n%d modules of concrete syntax: too many for one diagram; '
                         'choose some with -gf-top.\n' % len(rest))
    for t, cnodes in diagrams:
        cedges = {a: {(b, r) for r, b in mods[a][2] if b in cnodes and r != 'of'} for a in cnodes}
        parts.append('\n### The concrete syntax%s\n\n' % (' ' + t if t else '') +
                     'Solid arrows: extends, instantiates (a functor); dotted: opens, '
                     'with (the instance given for an interface).\n')
        parts.append(omitted_note(cnodes, cedges, omit))
        parts.append(mermaid(cnodes, cedges, set(omit), labels=True))
    return '\n'.join(parts)


# -------------------------------------------------------------------- check

def check(doc, dirs, exclude):
    """The links of doc that lead nowhere, and the names in `...` that look
    like Haskell functions or GF modules but occur in none of the files."""
    text = open(doc, encoding='utf-8').read()
    base = os.path.dirname(os.path.abspath(doc))
    bad_links = [l for l in re.findall(r'\]\(([^)#\s]+)(?:#[^)]*)?\)', text)
                 if not re.match(r'\w+:', l) and not os.path.exists(os.path.join(base, l))]
    suffixes = options_list('check-suffixes') or ['hs', 'gf', 'dk', 'dkgf', 'bnf']
    files = [f for s in suffixes for f in source_files(dirs, '.' + s, exclude)]
    words = set()
    for f in files:
        words |= set(re.findall(r"[A-Za-z_][\w']*", open(f, encoding='utf-8', errors='replace').read()))
    prose = re.sub(r'```.*?```', '', text, flags=re.S)       # quoted names, not code blocks
    names = set()
    for q in re.findall(r'`([^`\n]+)`', prose):
        q = q.strip()
        if re.fullmatch(r"[a-z][\w']*[A-Z][\w']*|[A-Z][a-z]+[A-Z]\w*", q):  # camelCase, CamelCase
            names.add(q)
        elif re.fullmatch(r"([A-Z]\w*)\.([a-z]\w*)", q):                  # Module.function
            names.add(q.split('.')[1])
    missing = sorted(n for n in names if n not in words)
    print('%s: %d links, %d leading nowhere%s' % (doc, len(re.findall(r'\]\(', text)), len(bad_links),
                                                ''.join('\n  ' + l for l in bad_links)))
    print('%d quoted names, %d not in the code%s' % (len(names), len(missing),
                                                    ''.join('\n  ' + n for n in missing)))
    return 1 if bad_links or missing else 0


def main():
    dirs = [a for a in sys.argv[1:] if not a.startswith('-')]
    if not dirs:
        sys.exit(__doc__)
    exclude = options_list('exclude') or EXCLUDE
    if option('check'):
        sys.exit(check(option('check'), dirs, exclude))
    out = option('out')
    files_table = not option('no-files')
    omit = options_list('omit')
    sections = ['# Code map of %s\n' % ', '.join(dirs),
                'Generated by `scripts/code_map.py %s`.\n' % ' '.join(sys.argv[1:])]
    hs = haskell_modules(source_files(dirs, '.hs', exclude))
    if hs:
        sections.append(haskell_section(hs, out, options_list('collapse'), options_list('hs-main'), omit, files_table))
    gf = gf_modules(source_files(dirs, '.gf', exclude))
    if gf:
        sections.append(gf_section(gf, out, options_list('gf-top'), omit, files_table))
    print('\n'.join(sections))


if __name__ == '__main__':
    main()
