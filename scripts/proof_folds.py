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


LABELS = {
    'Eng': {'fold all': 'fold all', 'unfold all': 'unfold all', 'line': 'line %s', 'lines': 'lines %s-%s',
            'folded': ' (%s folded)', 'derivation': ' (its derivation, %d lines, folded)',
            'toggle': 'fold or unfold %s', 'toggle derivation': 'fold or unfold the derivation of this line'},
    'Fre': {'fold all': 'tout replier', 'unfold all': 'tout déplier', 'line': 'ligne %s', 'lines': 'lignes %s-%s',
            'folded': ' (%s repliées)', 'derivation': ' (sa dérivation, %d lignes, repliée)',
            'toggle': 'replier ou déplier %s', 'toggle derivation': 'replier ou déplier la dérivation de cette ligne'},
}


def proof_table(rows, sentence_html=None, justification_html=None, number_html=None,
                cells=('n', 's', 'j'), table_class='proof', fold_links=True, derivation_folds=True,
                lang='Eng'):
    """A proof text as an HTML table with folds, preceded by "fold all ·
    unfold all" if it has subproofs. sentence_html(sentence, row) renders a
    sentence without its indentation, justification_html(j) and
    number_html(n) the other cells (all by default escaped as text). The
    labels are in English, or with lang='Fre' in French (LABELS)."""
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
    out = ['<div class="foldable">']
    if folds and fold_links:
        out.append('<div class="folds"><a href="#" data-fold="all">%s</a> · '
                   '<a href="#" data-fold="none">%s</a></div>' % (w['fold all'], w['unfold all']))
    out.append('<table class="%s">' % table_class)
    cn, cs, cj = cells
    for k, r in enumerate(rows):
        num, sentence, just = r
        attrs = ''
        if k in inside:
            attrs += ' data-in="%s"' % ' '.join(inside[k])
        if k in folds:
            attrs += ' data-scope="%d"' % k
        elif k in derived:
            attrs += ' data-scope="d%d" class="deriv"' % k
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
        elif k in derived:
            body = ('<span class="tog" title="%s"></span>' % w['toggle derivation']
                    + body + '<span class="folded">%s</span>' % (w['derivation'] % len(derived[k])))
        out.append('<tr%s><td class="%s">%s</td><td class="%s"%s>%s</td><td class="%s">%s</td></tr>'
                   % (attrs, cn, number_html(num), cs, style, body, cj, justification_html(just)))
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
      var rows = t.dataset.fold === 'all' ? 'tr[data-scope]:not(.deriv)' : 'tr[data-scope]';
      table.querySelectorAll(rows).forEach(function (r) {
        r.classList.toggle('closed', t.dataset.fold === 'all');
      });
      refresh(table);
    }
  });
})();
"""
