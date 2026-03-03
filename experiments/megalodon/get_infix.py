"""
collecting infix expressions from a megalodon file
and converting them to BNFC rules and translation rules
"""

import sys

FIRSTPREC = 5
BINDERPREC = 3

sought_keywords = {'Infix', 'Postfix', 'Prefix', 'Binder', 'Binder+'}

fixes = {line.strip() for line in sys.stdin
         if line.split()
         and line.split()[0] in sought_keywords}

precedences = {int(w) for fix in fixes for w in fix.split() if w.isdigit()}
precedences = {p: i for p, i in
               zip(sorted(list(precedences)), range(FIRSTPREC, len(precedences) + FIRSTPREC + 1))}



opers = []
def infix_rule(s):
    ws = s[:-1].split()  # dropping final .
    keyword = "BinderP" if ws[0] == 'Binder+' else ws[0]
    operator = ws[1].replace('\\', '\\\\')
    if operator in opers:
        print('Warning, repeated operator:', operator)
    opers.append(operator)
    targetprec = (BINDERPREC
                  if keyword.startswith('Binder')
                  else precedences[int(ws[2])])
    direction = ws[3] if ws[3] in ['left', 'right'] else None
    argprec = (targetprec
               if (direction is None or keyword.startswith('P'))
               else targetprec + 1)
    argprec1 = (targetprec
               if direction == 'left'
               else targetprec + 1)
    argprec2 = (targetprec
               if direction == 'right'
               else targetprec + 1)
    definiens = ws[-1]
    definiens_ident = '(Ident "' + definiens + '")'
    rulename = '_'.join(['E', keyword, definiens])

    Exp = lambda p: 'Exp' + str(p)
    rule = [rulename + '.', Exp(targetprec), '::=']
    translation = ['   ', rulename]

    trans = lambda s: '(trans ' + s + ')'

    if keyword == 'Infix':
        rule.extend([Exp(argprec1), '"'+operator+'"', Exp(argprec2)])
        translation.extend(['x', 'y', '->' , "apps", definiens_ident, trans('x'), trans('y')])
    if keyword == 'Prefix':
        rule.extend(['"'+operator+'"', Exp(argprec)])
        translation.extend(['x','->' , "apps", definiens_ident, trans('x')])
    if keyword == 'Postfix':
        rule.extend([Exp(argprec), '"'+operator+'"'])
        translation.extend(['x','->' , "apps", definiens_ident, trans('x')])
    if keyword.startswith('Binder'):
        rule.extend(['"'+operator+'"', "Bind", '"'+ws[2]+'"', Exp(targetprec)])

    rule.append(';')
    
    print(s)
    print(' '.join(rule))
    print(' '.join(translation))

for fix in fixes:
    print(infix_rule(fix))

print('coercions Exp ', max(precedences.values())+1, ';')



