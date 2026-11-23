#!/usr/bin/env python3
"""Post-process GF's Haskell GADT (src/Informath.hs): a lexical function with
a quoted name ('p-adic_Adj') is read with showCId, which keeps the quotes,
and written back with mkCId, which then makes another name, quoted once more
on each round. The quotes are removed in reading (unquoteCId)."""
import sys
path = sys.argv[1]
s = open(path).read()
if 'unquoteCId ::' not in s:
    s = s.replace('(showCId i)', '(unquoteCId i)')
    s += '''

-- | The name of a function as mkCId takes it: a quoted name ('p-adic_Adj')
-- without its quotes (added by scripts/unquote-gadt.py)
unquoteCId :: CId -> String
unquoteCId i = case showCId i of
  '\\'' : s@(_ : _) | last s == '\\'' -> unescape (init s)
  s -> s
 where
  unescape ('\\\\' : c : cs) = c : unescape cs
  unescape (c : cs) = c : unescape cs
  unescape [] = []
'''
    open(path, 'w').write(s)
