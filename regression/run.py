#!/usr/bin/env python3
"""Regression tests for Informath: run the cases of cases.py and compare.

    python3 regression/run.py                  # all cases
    python3 regression/run.py --tier demo      # what `make demo` runs
    python3 regression/run.py exx-eng sets-latex
    python3 regression/run.py --accept ...     # store the outputs as expected
    python3 regression/run.py --list

For each case, the standard output is compared with expected/<name>.out, and
the checks of the case are made (exit code, LaTeX compiles, no new failure
lines, no new undefined constants). Timings are compared with those stored in
expected/metrics.json. The outputs, diffs and a report go to results/latest/.

A case is
    PASS     output as expected, checks ok
    SKIP     cannot run here, e.g. a language missing from the grammar
    XFAIL    a known failure (see its xfail in cases.py), still failing
    XPASS    a known failure that no longer fails: remove its xfail
    CHANGED  output differs from the expected, checks ok: review the diff, and
             --accept it if the change is intended
    NEW      no expected output yet: --accept to store it
    FAIL     a check failed: nonzero exit, timeout, LaTeX error, a new failure
             line or more undefined constants than expected
and may carry warnings, e.g. SLOW (more than twice the stored time).

The exit status is 0 if every case passed, was skipped or failed as known, 1 otherwise.
"""
import argparse, concurrent.futures as cf, datetime, difflib, glob, json, os, re, shutil
import subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
EXPECTED = os.path.join(HERE, 'expected')
METRICS = os.path.join(EXPECTED, 'metrics.json')
RESULTS = os.path.join(HERE, 'results', 'latest')
sys.path.insert(0, HERE)
from cases import CASES

UNDEFINED = re.compile(r'UNDEFINED|UNRESOLVED|NOTYET')
TIERS = ['demo', 'multi', 'dev', 'check']


# ---------------------------------------------------------------------------
# the binary and the state of the build

def find_binary(explicit):
    """The RunInformath to test: --bin, $INFORMATH_BIN, this checkout's stack
    build, or the one on the path, in this order."""
    if explicit:
        return os.path.abspath(explicit), '--bin'
    if os.environ.get('INFORMATH_BIN'):
        return os.environ['INFORMATH_BIN'], '$INFORMATH_BIN'
    try:
        r = subprocess.run(['stack', 'path', '--local-install-root'], cwd=ROOT,
                           capture_output=True, text=True, timeout=60)
        b = os.path.join(r.stdout.strip(), 'bin', 'RunInformath')
        if r.returncode == 0 and os.path.exists(b):
            return b, 'stack build of this checkout'
    except (OSError, subprocess.TimeoutExpired):
        pass
    b = shutil.which('RunInformath')
    if b:
        return b, 'PATH (may be built from another checkout!)'
    sys.exit('no RunInformath found: build with `stack build` or give --bin')


def build_state(binary):
    """Facts for the report, and warnings about a stale build."""
    def mtime(p):
        return os.path.getmtime(p) if os.path.exists(p) else 0
    def stamp(t):
        return datetime.datetime.fromtimestamp(t).strftime('%Y-%m-%d %H:%M') if t else 'missing'
    git = subprocess.run(['git', 'describe', '--always', '--dirty'], cwd=ROOT,
                         capture_output=True, text=True).stdout.strip()
    pgf = os.path.join(ROOT, 'share', 'InformathEng.pgf')
    facts = [('commit', git), ('binary', binary), ('binary built', stamp(mtime(binary))),
             ('grammar', 'share/InformathEng.pgf, ' + stamp(mtime(pgf))),
             ('full grammar', 'share/InformathFull.pgf, '
              + stamp(mtime(os.path.join(ROOT, 'share', 'InformathFull.pgf'))))]
    warnings = []
    newest_gf = max([mtime(p) for p in glob.glob(os.path.join(ROOT, 'grammars', '*.gf'))] + [0])
    if newest_gf > mtime(pgf):
        warnings.append('some grammars/*.gf is newer than share/InformathEng.pgf: '
                        'run `make english_grammar`')
    # src/Informath.hs is left out: `make english_grammar` rewrites it every time
    newest_hs = max([mtime(p) for p in glob.glob(os.path.join(ROOT, 'src', '*.hs'))
                     + glob.glob(os.path.join(ROOT, 'app', '*.hs'))
                     if not p.endswith('Informath.hs')] + [0])
    if newest_hs > mtime(binary):
        warnings.append('some src/*.hs or app/*.hs is newer than the binary: run `stack build`')
    return facts, warnings


# ---------------------------------------------------------------------------
# running and checking one case

def normalize(text, work):
    """Output with the paths of this run replaced and trailing spaces removed."""
    text = text.replace(work, '{WORK}').replace(ROOT, '{ROOT}')
    return '\n'.join(l.rstrip() for l in text.split('\n')).rstrip('\n') + '\n'


def compile_latex(doc, work, timeout=180):
    """Compile a LaTeX document; returns (ok, errors, pages, first error)."""
    tex = os.path.join(work, 'doc.tex')
    open(tex, 'w').write(doc)
    try:
        subprocess.run(['pdflatex', '-interaction=nonstopmode', 'doc.tex'], cwd=work,
                       capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return False, 1, 0, 'pdflatex timed out'
    log = open(os.path.join(work, 'doc.log'), errors='replace').read()
    errors = [l for l in log.split('\n') if l.startswith('!')]
    m = re.search(r'Output written on doc\.pdf \((\d+) page', log)
    pages = int(m.group(1)) if m else 0
    return (not errors and pages > 0), len(errors), pages, (errors[0] if errors else '')


def run_case(case, binary, expected_metrics):
    name = case['name']
    work = os.path.join(RESULTS, name)
    shutil.rmtree(work, ignore_errors=True)
    os.makedirs(work)
    cmd = case['cmd'].format(RUN=binary, WORK=work, ROOT=ROOT)
    env = dict(os.environ, INFORMATH_ROOT=ROOT)
    timeout = case.get('timeout', 300)
    start = time.time()
    try:
        r = subprocess.run(['bash', '-o', 'pipefail', '-c', cmd], cwd=ROOT, env=env,
                           capture_output=True, text=True, timeout=timeout)
        code, out, err = r.returncode, r.stdout, r.stderr
    except subprocess.TimeoutExpired as e:
        partial = e.stdout or ''
        if isinstance(partial, bytes):
            partial = partial.decode(errors='replace')
        code, out, err = None, partial, 'TIMEOUT'
    seconds = time.time() - start
    out = normalize(out, work)
    open(os.path.join(work, 'stdout.txt'), 'w').write(out)
    open(os.path.join(work, 'stderr.txt'), 'w').write(err or '')

    res = {'name': name, 'tier': case['tier'], 'what': case['what'], 'seconds': round(seconds, 1),
           'failures': [], 'warnings': [], 'notes': [], 'diff': ''}
    lines = [l for l in out.split('\n') if l.strip()]
    res['metrics'] = {'lines': len(lines), 'undefined': len(UNDEFINED.findall(out)),
                      'seconds': round(seconds, 1)}

    # the command itself
    if code is None:
        res['failures'].append('timed out after %d s' % timeout)
    elif code != 0 and not (code == 1 and 'grep' in cmd and not lines):
        tail = (err or '').strip().split('\n')[-3:]
        res['failures'].append('exit code %d: %s' % (code, ' | '.join(tail)[:300]))

    old = expected_metrics.get(name, {})

    # a case that cannot run here, e.g. a language missing from the grammar
    if case.get('skip') and re.search(case['skip'], err or ''):
        res['status'] = 'SKIP'
        res['notes'].append('skipped: ' + case['skip'])
        json.dump(res, open(os.path.join(work, 'result.json'), 'w'), indent=1)
        return res

    # LaTeX: it must compile, with no more errors than stored
    if case.get('latex') and code == 0:
        ok, nerr, pages, first = compile_latex(out, work)
        res['metrics']['latex_pages'] = pages
        res['metrics']['latex_errors'] = nerr
        if pages == 0:
            res['failures'].append('LaTeX: no PDF; %s' % first)
        elif old and nerr > old.get('latex_errors', 0):
            res['failures'].append('LaTeX: %d errors, expected %d; %s'
                                   % (nerr, old.get('latex_errors', 0), first))
        elif old and nerr < old.get('latex_errors', 0):
            res['notes'].append('LaTeX: %d errors, expected %d (improvement)'
                                % (nerr, old.get('latex_errors', 0)))
        elif nerr:
            res['notes'].append('LaTeX: %d errors (as stored)' % nerr if old else
                                'LaTeX: %d errors; %s' % (nerr, first))

    # comparison with the expected output
    golden = os.path.join(EXPECTED, name + '.out')
    if case.get('golden', True):
        if not os.path.exists(golden):
            res['status'] = 'NEW'
        else:
            exp = open(golden).read()
            if exp == out:
                res['status'] = 'PASS'
            else:
                res['status'] = 'CHANGED'
                diff = list(difflib.unified_diff(exp.split('\n'), out.split('\n'),
                                                 'expected/' + name + '.out', 'actual', n=1,
                                                 lineterm=''))
                res['diff'] = '\n'.join(diff)
                open(os.path.join(work, 'diff.txt'), 'w').write(res['diff'] + '\n')
                added = sum(1 for l in diff if l.startswith('+') and not l.startswith('+++'))
                removed = sum(1 for l in diff if l.startswith('-') and not l.startswith('---'))
                res['notes'].append('%d lines added, %d removed' % (added, removed))
                if case.get('fewer'):
                    new = set(out.split('\n')) - set(exp.split('\n')) - {''}
                    gone = set(exp.split('\n')) - set(out.split('\n')) - {''}
                    if new:
                        res['failures'].append('%d new failure lines, e.g. %s'
                                               % (len(new), sorted(new)[0][:120]))
                    if gone:
                        res['notes'].append('%d failure lines gone (improvement)' % len(gone))
    else:
        res['status'] = 'PASS' if name in expected_metrics else 'NEW'

    # metrics against the stored ones
    if old:
        if res['metrics']['undefined'] > old.get('undefined', 0):
            res['failures'].append('undefined constants: %d, expected %d'
                                   % (res['metrics']['undefined'], old.get('undefined', 0)))
        t0 = old.get('seconds', 0)
        if t0 and seconds > 2 * t0 and seconds - t0 > 5:
            res['warnings'].append('SLOW: %.1f s, expected %.1f s' % (seconds, t0))
        p0 = old.get('latex_pages')
        p1 = res['metrics'].get('latex_pages')
        if p0 and p1 is not None and p1 != p0:
            res['notes'].append('LaTeX pages %d, expected %d' % (p1, p0))
    if case.get('xfail'):
        if res['failures']:
            res['status'] = 'XFAIL'
            res['notes'].insert(0, 'known failure: ' + case['xfail'])
            res['failures'] = []
        else:
            res['status'] = 'XPASS'
            res['notes'].insert(0, 'no longer fails: remove its xfail')
    elif res['failures']:
        res['status'] = 'FAIL'
    json.dump(res, open(os.path.join(work, 'result.json'), 'w'), indent=1)
    return res


# ---------------------------------------------------------------------------
# the report

def report(results, facts, warnings, started):
    order = {'FAIL': 0, 'XPASS': 1, 'CHANGED': 2, 'NEW': 3, 'XFAIL': 4, 'SKIP': 5, 'PASS': 6}
    counts = {s: sum(1 for r in results if r['status'] == s) for s in order}
    md = ['# Informath regression report', '',
          'Run %s, %d cases: %s.' % (started, len(results),
                                     ', '.join('%d %s' % (counts[s], s) for s in order if counts[s])),
          '']
    md += ['| | |', '|---|---|'] + ['| %s | %s |' % kv for kv in facts] + ['']
    for w in warnings:
        md.append('**Warning:** %s' % w)
    md += ['', '| status | case | tier | seconds | notes |', '|---|---|---|---:|---|']
    for r in sorted(results, key=lambda r: (order[r['status']], TIERS.index(r['tier']), r['name'])):
        notes = '; '.join(r['failures'] + r['warnings'] + r['notes'])
        md.append('| %s | %s | %s | %.1f | %s |'
                  % (r['status'], r['name'], r['tier'], r['seconds'], notes.replace('|', '\\|')))
    for r in results:
        if r['diff']:
            d = r['diff'].split('\n')
            md += ['', '## %s: %s' % (r['name'], r['what']), '', '```diff']
            md += d[:40] + (['... (%d more lines in results/latest/%s/diff.txt)'
                             % (len(d) - 40, r['name'])] if len(d) > 40 else []) + ['```']
    open(os.path.join(RESULTS, 'report.md'), 'w').write('\n'.join(md) + '\n')
    return counts


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('names', nargs='*', help='cases to run (default: all of the tiers)')
    ap.add_argument('--tier', default=','.join(TIERS), help='comma-separated tiers, default all')
    ap.add_argument('--accept', action='store_true',
                    help='store the outputs and metrics of the cases run as expected')
    ap.add_argument('--jobs', type=int, default=4, help='cases run in parallel (default 4)')
    ap.add_argument('--bin', help='the RunInformath binary to test')
    ap.add_argument('--list', action='store_true', help='list the cases and exit')
    args = ap.parse_args()

    tiers = args.tier.split(',')
    cases = [c for c in CASES if (c['name'] in args.names if args.names else c['tier'] in tiers)]
    if args.names and len(cases) != len(set(args.names)):
        sys.exit('unknown cases: %s' % ', '.join(set(args.names) - {c['name'] for c in CASES}))
    if args.list:
        for c in CASES:
            print('%-26s %-6s %-22s %s' % (c['name'], c['tier'], c.get('make') or '', c['what']))
        return 0

    binary, source = find_binary(args.bin)
    facts, warnings = build_state(binary)
    facts.insert(2, ('binary from', source))
    started = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
    os.makedirs(RESULTS, exist_ok=True)
    expected_metrics = json.load(open(METRICS)) if os.path.exists(METRICS) else {}
    for w in warnings:
        print('WARNING:', w)
    print('testing %s (%s), %d cases' % (binary, source, len(cases)))

    results = []
    with cf.ThreadPoolExecutor(max_workers=args.jobs) as ex:
        futures = {ex.submit(run_case, c, binary, expected_metrics): c for c in cases}
        for f in cf.as_completed(futures):
            r = f.result()
            results.append(r)
            extra = '; '.join(r['failures'] + r['warnings'] + r['notes'])
            print('%-8s %-26s %6.1f s  %s' % (r['status'], r['name'], r['seconds'], extra[:150]))

    counts = report(results, facts, warnings, started)
    print('\n%s -- report in %s' % (', '.join('%d %s' % (n, s) for s, n in counts.items() if n),
                                     os.path.relpath(os.path.join(RESULTS, 'report.md'), os.getcwd())))

    if args.accept:
        os.makedirs(EXPECTED, exist_ok=True)
        accepted = 0
        for r in results:
            if r['status'] == 'SKIP' or (r['status'] == 'FAIL' and any(
                    'timed out' in f or 'exit code' in f for f in r['failures'])):
                print('not accepted, as it did not run through: %s' % r['name'])
                continue
            case = next(c for c in CASES if c['name'] == r['name'])
            if case.get('golden', True):
                shutil.copy(os.path.join(RESULTS, r['name'], 'stdout.txt'),
                            os.path.join(EXPECTED, r['name'] + '.out'))
            expected_metrics[r['name']] = r['metrics']
            accepted += 1
        json.dump(dict(sorted(expected_metrics.items())), open(METRICS, 'w'), indent=1)
        print('accepted %d cases into %s' % (accepted, os.path.relpath(EXPECTED, os.getcwd())))
        return 0
    ok = counts['PASS'] + counts['XFAIL'] + counts['SKIP']
    return 0 if ok == len(results) else 1


if __name__ == '__main__':
    sys.exit(main())
