"""Proof texts in HTML with folds: parts of a proof that can be hidden and
shown again, leaving the hypothesis and the conclusion.

The input is the rows of RunInformath -proof-text, as the LaTeX tables have
them: (number, sentence, justification), the sentence beginning with
\\hspace*{1.5em} for each scope open at the line (-proof-indent). Two kinds of
folds are made:

- the subproof of an assumption (a line without a justification that does not
  announce cases): the lines between the assumption and the conclusion
  reached under it, at the same or a greater depth;
- the derivation of a line that cites earlier lines in its justification
  ("∨e 6, 10, 12 (8, 11)"): the lines it depends on through the lines it
  cites, and the lines announcing cases among them, except the assumptions
  still open at the line (the hypotheses of the theorem are open everywhere). Folds that would hide the same lines as a subproof
  or an earlier fold are left out.

A page that shows proof tables includes FOLD_CSS in its style and FOLD_JS in
a script, and calls proof_table for each proof:

    import proof_folds              # with $INFORMATH_ROOT/scripts on sys.path
    html = proof_folds.proof_table(rows, sentence_html=..., justification_html=...)

The rows of the table are <tr><td class=n><td class=s><td class=j> (other
class names can be given with cells=), and the depth of a line is given to
its sentence cell as the CSS variable --depth, for lines to the left of it
as in Fitch's notation.  Used by edukera, mathlib-informath, godement-index
and hott-llm.
"""

import html
import re

INDENT = '\\hspace*{1.5em}'      # one open scope of a line (RunInformath -proof-indent)


def depth_of(sentence):
    "the number of scopes open at a line, and its sentence without them"
    depth = 0
    while sentence.startswith(INDENT):
        sentence, depth = sentence[len(INDENT):], depth + 1
    return depth, sentence


def announces_cases(sentence):
    "a line announcing cases: \"We have two cases:\", in Dedukti notation \"2 cases\""
    m = re.match(r'\\verb#(.*)#$', sentence)
    if m:
        sentence = m.group(1).strip()
    return sentence.rstrip(' .').endswith(':') or bool(re.match(r'^\d+ cases$', sentence))


def _lines(rows):
    rows = [(list(r) + ['', '', ''])[:3] for r in rows]
    depth, sentence = zip(*[depth_of(r[1].strip()) for r in rows]) if rows else ((), ())
    sentence = [t.strip() for t in sentence]
    no_just = [not r[2].strip() for r in rows]
    header = [no_just[i] and announces_cases(sentence[i]) for i in range(len(rows))]
    return rows, list(depth), list(sentence), no_just, header


def _scope_end(i, depth, assumption):
    "the conclusion of the assumption at i: the last line of its scope"
    end = i + 1
    while end < len(depth) and depth[end] >= depth[i] and not (depth[end] == depth[i] and assumption[end]):
        end += 1
    return end - 1


def subproofs(rows):
    """The subproofs of a proof text that can be folded: for each assumption
    (a line without a justification, not a line announcing cases, which ends
    with a colon), the lines of its scope, which go on until a line of
    smaller depth or the next assumption of the same depth (the next case).
    The last line of the scope is the conclusion reached under the
    assumption; folded, the lines between the two are hidden.  Returns
    {index of the assumption: (first hidden, last hidden)}."""
    rows, depth, sentence, no_just, header = _lines(rows)
    assumption = [no_just[i] and depth[i] > 0 and not header[i] for i in range(len(rows))]
    out = {}
    for i in range(len(rows)):
        if assumption[i]:
            conclusion = _scope_end(i, depth, assumption)
            if conclusion - 1 >= i + 1:
                out[i] = (i + 1, conclusion - 1)
    return out


def derivations(rows, folds=None):
    """The derivations of the lines that cite earlier lines, to be folded too:
    for line n, the lines that it depends on through the lines it cites (the
    premisses and the discharged assumptions in the justification, "∨e 6, 10,
    12 (8, 11)"), and the lines announcing cases among them, but not the
    assumptions still open at n (an assumption that n discharges is closed).
    Folds that hide fewer than two lines, or the same lines as an
    assumption's fold or an earlier one, are left out.
    Returns {index of the line: sorted list of hidden indices}."""
    if folds is None:
        folds = subproofs(rows)
    rows, depth, sentence, no_just, header = _lines(rows)
    index = {}
    for i, r in enumerate(rows):
        m = re.match(r'(\d+)\.?$', r[0].strip())
        if m:
            index[int(m.group(1))] = i
    cited, discharged = [], []
    for r in rows:
        just = re.sub(r'\\[a-z]+\{([^}]*)\}', r'\1', r[2])
        # the numbers after the rule's name: premisses, and in parentheses discharged lines
        rest = just.split(None, 1)[1] if len(just.split(None, 1)) > 1 else ''
        cited.append([index[int(x)] for x in re.findall(r'\b(\d+)\b', rest) if int(x) in index])
        discharged.append({index[int(x)] for p in re.findall(r'\(([^)]*)\)', rest)
                           for x in re.findall(r'\b(\d+)\b', p) if int(x) in index})
    deps = []
    for n in range(len(rows)):
        d = set()
        for k in cited[n]:
            if k < n:
                d.add(k)
                d |= deps[k]
        deps.append(d)
    assumption = [no_just[i] and depth[i] > 0 and not header[i] for i in range(len(rows))]
    scope_end = {i: _scope_end(i, depth, assumption) for i in range(len(rows)) if assumption[i]}
    for i, (a, b) in folds.items():
        scope_end[i] = max(scope_end.get(i, 0), b + 1)
    # a hypothesis of the theorem (no justification, depth 0) is open to the end
    for i in range(len(rows)):
        if no_just[i] and depth[i] == 0 and not header[i]:
            scope_end[i] = len(rows)
    seen = {tuple(range(a, b + 1)) for a, b in folds.values()}
    out = {}
    for n in range(len(rows)):
        if not deps[n]:
            continue
        lo = min(deps[n])
        open_at_n = {i for i, e in scope_end.items() if i < n <= e and i not in discharged[n]}
        hidden = sorted(k for k in range(lo, n) if (k in deps[n] or header[k]) and k not in open_at_n)
        if len(hidden) >= 2 and tuple(hidden) not in seen:
            seen.add(tuple(hidden))
            out[n] = hidden
    return out


def _rule_and_numbers(just):
    "the rule of a justification (its first word, \\texttt{} and \\_ removed), its premisses and its discharged lines"
    j = re.sub(r'\\texttt\{([^}]*)\}', r'\1', just).replace('\\_', '_').strip()
    parts = j.split(None, 1)
    if not parts:
        return '', [], []
    rest = parts[1] if len(parts) > 1 else ''
    discharged = [int(x) for p in re.findall(r'\(([^)]*)\)', rest) for x in re.findall(r'\b(\d+)\b', p)]
    premisses = [int(x) for x in re.findall(r'\b(\d+)\b', re.sub(r'\([^)]*\)', '', rest))]
    return parts[0], premisses, discharged


INTRO_RULES = ('ifI', 'forallI')
ELIM_RULES = ('ifE', 'forallE')


def inductions(rows, is_induction=lambda r: r.startswith('ind_'), intro_rules=INTRO_RULES, elim_rules=ELIM_RULES):
    """The inductions of a proof text that can be collapsed to one line, as a
    book writes them ("by induction on p, (p . q) . r = p . (q . r)"): an
    induction whose motive is generalized over later hypotheses ("if q : y = z
    then ...") is spelled out by Informath with introductions of them in its
    cases (intro_rules, cited by the induction line through each other) and
    eliminations after it (elim_rules, each citing the line before, with the
    hypotheses reassumed). Collapsed, the last of the eliminations stays,
    cited from the lines that prove the cases, and the rest is hidden: the
    introductions, the goal assumptions that they discharge, the induction
    line itself, the eliminations and the reassumed hypotheses.
    Returns {index of the last line: (hidden indices, induction line index,
    indices of the case lines)}; inductions without such scaffolding are left
    out."""
    rows, depth, sentence, no_just, header = _lines(rows)
    index = {}
    for i, r in enumerate(rows):
        m = re.match(r'(\d+)\.?$', r[0].strip())
        if m:
            index[int(m.group(1))] = i
    info = [_rule_and_numbers(r[2]) for r in rows]
    prem = [[index[x] for x in ps if x in index] for _, ps, _ in info]
    disch = [[index[x] for x in ds if x in index] for _, _, ds in info]
    out = {}
    for i in range(len(rows)):
        if no_just[i] or not is_induction(info[i][0]):
            continue
        hidden, cases = set(), []
        for c in prem[i]:
            x = c
            while not no_just[x] and info[x][0] in intro_rules and prem[x]:
                hidden.add(x)
                # the goal assumptions it discharges, not the first line, which
                # has the theorem's hypotheses (-proof-merge-hypos merges into
                # it a case's hypothesis written the same)
                hidden |= {a for a in disch[x] if a > 0}
                # and the line introducing its variable (forallI 2, 4: let v be ...)
                hidden |= {a for a in prem[x][:-1] if a > 0 and no_just[a]}
                x = prem[x][-1]                  # its body: ifI 3 (2), forallI 1, 3
            if x != c:
                if x not in hidden:
                    cases.append(x)
            elif not no_just[c]:
                cases.append(c)          # a case proved without introductions
        # the eliminations after the induction line, and the hypotheses they reassume
        last, cur, pending, k = i, i, [], i + 1
        while k < len(rows):
            if no_just[k] and not header[k]:
                pending.append(k)
            elif info[k][0] in elim_rules and cur in prem[k]:
                hidden |= set(pending)
                hidden.add(cur)
                pending, cur, last = [], k, k
            else:
                break
            k += 1
        if last == i or not hidden:
            continue
        out[last] = (sorted(h for h in hidden if h != last), i, sorted(set(cases) - hidden))
    return out


def case_trees(rows, is_elim=lambda r: r.startswith(('ind_', 'cases_')), skip=()):
    """The nested case analyses and inductions of a proof text that can be
    collapsed to their outermost line, as a book writes "by cases on k": the
    eliminator lines that the outermost one cites through each other, and the
    lines introducing the parts of each case ("let x be ..."), are hidden;
    the lines proving the single cases stay, and so do the induction
    hypotheses (the assumptions an induction discharges). Lines in skip (the
    last lines of inductions() collapses) are left alone.
    Returns {index of the outermost line: (hidden indices, indices of the case
    lines, indices of the eliminator lines)}; a single eliminator, not
    nested, is left out."""
    rows, depth, sentence, no_just, header = _lines(rows)
    index = {}
    for i, r in enumerate(rows):
        m = re.match(r'(\d+)\.?$', r[0].strip())
        if m:
            index[int(m.group(1))] = i
    info = [_rule_and_numbers(r[2]) for r in rows]
    prem = [[index[x] for x in ps if x in index] for _, ps, _ in info]
    disch = [{index[x] for x in ds if x in index} for _, _, ds in info]
    elim = [not no_just[i] and is_elim(info[i][0].partition('@')[0]) for i in range(len(rows))]
    out, member = {}, set()
    for f in range(len(rows) - 1, -1, -1):
        if not elim[f] or f in member or f in skip:
            continue
        tree, todo = {f}, [f]
        while todo:
            e = todo.pop()
            for p in prem[e]:
                if elim[p] and p not in tree and p not in skip:
                    tree.add(p)
                    todo.append(p)
        if len(tree) < 2:
            continue
        ihs = set().union(*[disch[e] for e in tree])
        hidden = {e for e in tree if e != f}
        cases = set()
        for e in tree:
            for p in prem[e]:
                if p in tree:
                    continue
                if no_just[p]:
                    if p not in ihs and depth[p] > 0:
                        hidden.add(p)          # introducing the parts of a case, not the theorem's hypotheses
                else:
                    cases.add(p)
        member |= tree
        out[f] = (sorted(hidden), sorted(cases), sorted(tree))
    return out


LABELS = {
    'Eng': {'fold all': 'fold all', 'unfold all': 'unfold all', 'line': 'line %s', 'lines': 'lines %s-%s',
            'folded': ' (%s folded)', 'derivation': ' (its derivation, %d lines, folded)',
            'toggle': 'fold or unfold %s', 'toggle derivation': 'fold or unfold the derivation of this line',
            'toggle induction': 'show or hide the steps of the induction',
            'induction': ' (%d steps of the induction hidden)',
            'toggle cases': 'show or hide the steps of the case analysis',
            'cases': ' (%d steps hidden)',
            'toggle verification': 'show or hide the verification of this claim',
            'verification': ' (its verification, %d lines, hidden)',
            'toggle part': 'show or hide the rest of this part of the proof',
            'part': ' (%d more lines of this part hidden)'},
    'Fre': {'fold all': 'tout replier', 'unfold all': 'tout déplier', 'line': 'ligne %s', 'lines': 'lignes %s-%s',
            'folded': ' (%s repliées)', 'derivation': ' (sa dérivation, %d lignes, repliée)',
            'toggle': 'replier ou déplier %s', 'toggle derivation': 'replier ou déplier la dérivation de cette ligne',
            'toggle induction': 'montrer ou cacher les étapes de la récurrence',
            'induction': ' (%d étapes de la récurrence cachées)',
            'toggle cases': "montrer ou cacher les étapes de l'analyse de cas",
            'cases': ' (%d étapes cachées)',
            'toggle verification': 'montrer ou cacher la vérification de cette affirmation',
            'verification': ' (sa vérification, %d lignes, cachée)',
            'toggle part': 'montrer ou cacher la suite de cette partie de la preuve',
            'part': ' (%d autres lignes de cette partie cachées)'},
}


def verification_collapses(rows, short_html=''):
    """The collapses of the verifications of claims in a proof text made
    from a tactic proof (Rocq's assert ... { ... }, Lean's have ... := by):
    rows whose fourth element is the step's path (the bullets and braces it
    is in) and fifth its unit ('verify' for a claim). The lines of the
    braced proof that follows a claim, which are in a case of their own
    below the claim's path, are hidden under the claim."""
    out = {}
    for i, r in enumerate(rows):
        if len(r) < 5 or r[4] != 'verify' or i + 1 >= len(rows) or len(rows[i + 1]) < 4:
            continue
        p = list(r[3] or [])
        q = list(rows[i + 1][3] or [])
        if len(q) <= len(p) or q[:len(p)] != p:
            continue
        block = q[:len(p) + 1]
        hidden = []
        for j in range(i + 1, len(rows)):
            pj = list(rows[j][3] or []) if len(rows[j]) > 3 else []
            if pj[:len(block)] != block:
                break
            hidden.append(j)
        if hidden:
            out[i] = (hidden, short_html, 'verification')
    return out


def part_collapses(rows, short_html='', least=2):
    """The collapses of the parts of a proof text made from a tactic proof
    (the cases of Rocq's bullets and braces): rows whose fourth element is
    the step's path. A part is the lines from one that enters a case (its
    path longer than the line before, or another case at the same level)
    to the end of that case; its lines after the first are hidden under it
    if there are at least `least` of them."""
    out = {}
    paths = [list(r[3] or []) if len(r) > 3 else None for r in rows]
    for i, p in enumerate(paths):
        if not p or i == 0 or paths[i - 1] is None:
            continue
        q = paths[i - 1]
        # the case entered: the shortest beginning of the path not shared with
        # the line before (none if the line returns to an enclosing case)
        k = 0
        while k < len(p) and k < len(q) and p[k] == q[k]:
            k += 1
        if k == len(p):
            continue
        key = p[:k + 1]

        def block(key):
            out = []
            for j in range(i + 1, len(rows)):
                if paths[j] is None or paths[j][:len(key)] != key:
                    break
                out.append(j)
            return out
        hidden = block(key)
        # a case divided into cases from its first line on (split; - ...; -
        # split): the part is the first of these
        while len(key) < len(p) and len({tuple(paths[j][len(key):len(key) + 1]) for j in hidden} - {()}) > 1:
            key = p[:len(key) + 1]
            hidden = block(key)
        if len(hidden) >= least:
            out[i] = (hidden, short_html, 'part')
    return out


def proof_table(rows, sentence_html=None, justification_html=None, number_html=None,
                cells=('n', 's', 'j'), table_class='proof', fold_links=True, derivation_folds=True,
                lang='Eng', collapses=None):
    """A proof text as an HTML table with folds, preceded by "fold all ·
    unfold all" if it has subproofs. sentence_html(sentence, row) renders a
    sentence without its indentation, justification_html(j) and
    number_html(n) the other cells (all by default escaped as text). The
    labels are in English, or with lang='Fre' in French (LABELS).

    collapses = {index of a line: (hidden indices, short justification html[,
    'induction' or 'cases'])} (from inductions() and case_trees()): the line is shown with the short justification and
    the hidden lines folded away, under a symbol (⊕, ⊖ when open) that shows
    them and the line's own justification."""
    w = LABELS.get(lang, LABELS['Eng'])
    esc = lambda t, *_: html.escape(t, quote=False)
    sentence_html = sentence_html or esc
    justification_html = justification_html or esc
    number_html = number_html or esc
    rows = [(list(r) + ['', '', ''])[:3] for r in rows]
    folds = subproofs(rows)
    derived = derivations(rows, folds) if derivation_folds else {}
    inside = {}
    for i, (a, b) in folds.items():
        for k in range(a, b + 1):
            inside.setdefault(k, []).append(str(i))
    for n, hidden in derived.items():
        for k in hidden:
            inside.setdefault(k, []).append('d%d' % n)
    collapses = collapses or {}
    for n, (hidden, *_) in collapses.items():
        derived.pop(n, None)
        for k in hidden:
            inside.setdefault(k, []).append('c%d' % n)
    out = ['<div class="foldable">']
    if folds and fold_links:
        out.append('<div class="folds"><a href="#" data-fold="all">%s</a> · '
                   '<a href="#" data-fold="none">%s</a></div>' % (w['fold all'], w['unfold all']))
    out.append('<table class="%s">' % table_class)
    cn, cs, cj = cells
    for k, r in enumerate(rows):
        num, sentence, just = r
        attrs, classes = '', []
        if k in inside:
            attrs += ' data-in="%s"' % ' '.join(inside[k])
            if any(c.startswith('c') for c in inside[k]):
                classes.append('hid')          # inside a collapse, which starts closed
        if k in collapses:
            attrs += ' data-scope="c%d"' % k
            classes += ['coll', 'closed']
        elif k in folds:
            attrs += ' data-scope="%d"' % k
        elif k in derived:
            attrs += ' data-scope="d%d"' % k
            classes.append('deriv')
        if classes:
            attrs += ' class="%s"' % ' '.join(classes)
        depth, sentence = depth_of(sentence)
        style = ' style="--depth:%d"' % depth if depth else ''
        body = sentence_html(sentence, r)
        if k in folds:
            a, b = folds[k]
            lines = (w['line'] % rows[a][0].rstrip('.') if a == b
                     else w['lines'] % (rows[a][0].rstrip('.'), rows[b][0].rstrip('.')))
            folded = w['folded'] % lines
            if a == b and lang == 'Fre':
                folded = folded.replace('repliées', 'repliée')
            body = ('<span class="tog" title="%s"></span>' % (w['toggle'] % lines)
                    + body + '<span class="folded">%s</span>' % folded)
        if k in collapses:
            kind = collapses[k][2] if len(collapses[k]) > 2 else 'induction'
            body = ('<span class="tog" title="%s"></span>' % w['toggle ' + kind]
                    + body + '<span class="folded">%s</span>' % (w[kind] % len(collapses[k][0])))
            jhtml = ('<span class="short">%s</span><span class="long">%s</span>'
                     % (collapses[k][1], justification_html(just)))
        elif k in derived:
            body = ('<span class="tog" title="%s"></span>' % w['toggle derivation']
                    + body + '<span class="folded">%s</span>' % (w['derivation'] % len(derived[k])))
        if k not in collapses:
            jhtml = justification_html(just)
        out.append('<tr%s><td class="%s">%s</td><td class="%s"%s>%s</td><td class="%s">%s</td></tr>'
                   % (attrs, cn, number_html(num), cs, style, body, cj, jhtml))
    out.append('</table></div>')
    return '\n'.join(out)


FOLD_CSS = """
 .folds{font-size:.8em;margin:.2rem 0 0}
 .folds a{color:var(--muted,#888)}
 .tog{cursor:pointer;color:var(--link,#0645ad);user-select:none;display:inline-block;width:1em;margin-left:-1em}
 .tog::before{content:"\\25BE"}
 tr.closed .tog::before{content:"\\25B8"}
 tr.deriv .tog{color:var(--muted,#888)}
 .folded{display:none;color:var(--muted,#888);font-size:.85em}
 tr.closed .folded{display:inline}
 tr.hid{display:none}
 tr.coll .tog::before{content:"\\2296"}
 tr.coll.closed .tog::before{content:"\\2295"}
 tr.coll.closed .long,tr.coll:not(.closed) .short{display:none}
"""

FOLD_JS = """
(function () {
  // a row is hidden if a fold that it is inside (data-in) is closed
  function refresh(table) {
    var closed = {};
    table.querySelectorAll('tr.closed').forEach(function (r) { closed[r.dataset.scope] = true; });
    table.querySelectorAll('tr[data-in]').forEach(function (r) {
      r.classList.toggle('hid', r.dataset.in.split(' ').some(function (s) { return closed[s]; }));
    });
  }
  document.addEventListener('click', function (ev) {
    var t = ev.target;
    if (t.classList && t.classList.contains('tog')) {
      var row = t.closest('tr');
      row.classList.toggle('closed');
      refresh(row.closest('table'));
    } else if (t.dataset && t.dataset.fold && t.closest('.foldable')) {
      ev.preventDefault();
      // "fold all" folds the subproofs, "unfold all" everything
      var table = t.closest('.foldable').querySelector('table');
      var rows = t.dataset.fold === 'all' ? 'tr[data-scope]:not(.deriv):not(.coll)' : 'tr[data-scope]';
      table.querySelectorAll(rows).forEach(function (r) {
        r.classList.toggle('closed', t.dataset.fold === 'all');
      });
      refresh(table);
    }
  });
})();
"""


# ---------------------------------------------------------------- command line

def latex_html(t):
    """A sentence or a justification of RunInformath's LaTeX proof tables as HTML
    for MathJax: the formulas ($...$) kept, \\texttt and \\verb as code."""
    t = html.escape(t, quote=False)
    t = re.sub(r'\\verb#(.*?)#', lambda m: '<code>%s</code>' % m.group(1), t)
    t = re.sub(r'\\texttt\{((?:[^{}]|\{[^{}]*\})*)\}', lambda m: '<code>%s</code>' % m.group(1).replace('\\_', '_'), t)
    t = re.sub(r'\\textbf\{([^{}$]*)\}', r'<b>\1</b>', t)
    t = t.replace('\\begin{itemize}', '<ul>').replace('\\end{itemize}', '</ul>')
    return re.sub(r'\\item\b\s*', '<li>', t)


def collapsed(rows):
    "the collapses of the inductions of a proof (inductions()), with a short justification"
    out = {}
    for last, (hidden, ind, cases) in inductions(rows).items():
        rule = _rule_and_numbers(rows[ind][2])[0].partition('@')[0]
        nums = ', '.join(rows[c][0].rstrip('.') for c in cases)
        out[last] = (hidden, 'by <code>%s</code>%s' % (html.escape(rule), ', ' + nums if nums else ''))
    return out


def main():
    """python3 proof_folds.py --json: on standard input {"proofs": [{"rows": [[n, sentence,
    justification], ...], "lang": "Eng"}, ...]}, the rows as RunInformath's LaTeX tables
    have them; on standard output {"tables": [html, ...], "css": FOLD_CSS, "js": FOLD_JS}.
    Used by RunInformath -to-html-doc."""
    import json, sys
    if sys.argv[1:] != ['--json']:
        sys.exit(main.__doc__)
    data = json.load(sys.stdin)
    tables = []
    for p in data.get('proofs', []):
        rows = [(list(r) + ['', '', ''])[:3] for r in p['rows']]
        try:
            coll = collapsed(rows)
        except Exception:
            coll = {}
        tables.append(proof_table(rows, sentence_html=lambda t, row: latex_html(t),
                                  justification_html=latex_html, lang=p.get('lang', 'Eng'), collapses=coll))
    json.dump({'tables': tables, 'css': FOLD_CSS, 'js': FOLD_JS}, sys.stdout, ensure_ascii=False)


if __name__ == '__main__':
    main()
