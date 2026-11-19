#!/usr/bin/env python3
"""What the faithfulness audit compares, for one chapter of the formalization:

    python3 dump.py 7 > /tmp/ch7.txt

For each declaration of coq/Chapter7.v that formalizes a sentence (its
//TAG tag, TAG and the CoNLL-U file of the sentences from config.py): the
sentence and the one before it, the Rocq statement, its English reading by
Informath (index.json), and the comment lines of the Rocq file just above the
declaration (where corrections and edge cases are noted).  Run it in the
project directory.  Declarations without a tag (helpers) are listed by name only.
"""
import json, os, re, sys

import importlib.util
ROCQ = os.getcwd()
_spec = importlib.util.spec_from_file_location('config', os.path.join(ROCQ, 'config.py'))
config = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(config)
TAG = getattr(config, 'TAG', 'TEXT')
CONLLU = getattr(config, 'CONLLU', os.path.join(ROCQ, 'text', 'source.conllu'))


def sentences():
    out, sid = {}, None
    for line in open(CONLLU, encoding='utf-8'):
        if line.startswith('# sent_id = '):
            sid = int(line.split('= ')[1])
        elif line.startswith('# latextext = ') and sid is not None:
            out[sid] = line[len('# latextext = '):].strip()
        elif line.startswith('# text = ') and sid is not None and sid not in out:
            out[sid] = line[len('# text = '):].strip()
    return out


def comments_above(lines, line_no):
    """The comment block ending just above the tag of a declaration."""
    i = line_no - 2
    while i >= 0 and ('//%s:' % TAG) in lines[i]:
        i -= 1
    block = []
    while i >= 0 and (lines[i].strip() == '' or block and not lines[i].strip().startswith(('Theorem', 'Lemma', 'Definition', 'Axiom', 'Qed', 'Record', 'Fixpoint', 'Proof', 'Inductive'))):
        if lines[i].strip() == '' and block and block[-1].strip() == '':
            break
        block.append(lines[i])
        i -= 1
        if len(block) > 25:
            break
    text = ''.join(reversed(block)).strip()
    return text if '(*' in text and ('//%s:' % TAG) not in text else ''


def main():
    n = sys.argv[1]
    mod = 'Chapter%s' % n
    text = sentences()
    lines = open(os.path.join(ROCQ, 'coq', mod + '.v'), encoding='utf-8').readlines()
    decls = [d for d in json.load(open(os.path.join(ROCQ, 'index.json')))['declarations'] if d.get('module') == mod]
    untagged = []
    for d in decls:
        m = re.search(r'//%s:(\d+)' % re.escape(TAG), d.get('comment', ''))
        if not m:
            untagged.append(d['coq'])
            continue
        sid = int(m.group(1))
        print('=' * 78)
        print('%s  [%s, line %s, sentence %d]' % (d['coq'], d.get('kind', ''), d.get('line', '?'), sid))
        print('BEFORE: %s' % text.get(sid - 1, '')[:600])
        print('SENTENCE: %s' % text.get(sid, '')[:1500])
        c = comments_above(lines, int(d.get('line', 1)))
        if c:
            print('COMMENT: %s' % c[:1200])
        print('ROCQ: %s' % d.get('source', '')[:1500])
        print('READING: %s' % d.get('readings', {}).get('Eng', '')[:1500])
    print('=' * 78)
    print('UNTAGGED (%d): %s' % (len(untagged), ' '.join(untagged)))


if __name__ == '__main__':
    main()
