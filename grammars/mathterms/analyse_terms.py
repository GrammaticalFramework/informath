
# usage: python3 analyse_terms.py | grep '\-\- Ger' >DerivedTermsGer.gf

MANUALLY_FILTERED = '../../../old/v2/mathterms/manually-filtered-derived-terms.txt'
OLD_DERIVED_DIR = '../../../old/v2/mathterms/'

LANGS = ['Eng', 'Fre', 'Ger', 'Swe']

def mk_fun(s):
    s = '_'.join(s.split())  # spaces replaced by underscores
    
    if (s[0].isalpha() and
        all(ord(c)<256 and (c.isdigit() or c.isalpha() or c in "_'")
            for c in s)):  # test if legal GF identifier
        return s
    else:
        return "'" + s.replace("'", "\\'") + "'"  # if not, single quotes make it legal
            
def gf_abs_rule(t):
    cat = t[1][0][-2:]
    ids = [t.split('_')[-3] for t in t[1]]
    fun = mk_fun(' '.join([w for w in t[0][:-1] if w != 'None'] + [cat]))
    return (fun, f'fun {fun} : {cat} ; -- {", ".join(ids)}')


def linearizations(lang):
    file = f'{OLD_DERIVED_DIR}DerivedMathTerms{lang}.gf'
    with open(file) as lines:
        lins = {line.split()[1]: ' '.join(line.split()[3:-1])
                  for line in lines if line.split()[0] == 'lin'}
    return lins


# to inspect linearizations
# for lin in linearizations('Fre').items():
#    print(lin)


# find linearizations for a list of funs in the lins dict
def find_linearizations(funs, lins):
    return {fun: lins.get(fun, None) for fun in funs}




# to generate GF
if __name__ == '__main__':
    lins = {lang: linearizations(lang) for lang in LANGS}
    with open(MANUALLY_FILTERED) as file:
        for line in file:
            if line[:2] != '--':
                eline = eval(line)
                print(gf_abs_rule(eline)[1])
                funs = eline[-1]
                for lang in LANGS:                    
                    linset = {lin for lin in set(find_linearizations(funs, lins[lang]).values()) if lin}
                    lin = ' | '.join(linset) if linset else 'variants {}'
                    print('lin', gf_abs_rule(eline)[0], '=', lin, ';', '--', lang)




        

