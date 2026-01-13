# extract definitions and statements from Naprocho-ZF files
# python3 extract_naproche_zf.py ../../naproche-zf/library/set.tex >../tmp/fromsets.tex

import sys

TEXFILES=sys.argv[1].split(',')

def emit(lines):
    s = ' '.join(lines).replace('\\', ' \\')
    print(s)
    return []


for file in TEXFILES:
    with open(file) as tex:
        lines = []
        align = False
        for line in tex:
            line = line.strip()
            if line.startswith('\\begin{align*'):
                align = True
            if line.startswith('\\begin{'):
                form = line[7:line.find('}')]
                if '\\label' in line:
                    label = '_' + line[line.find('\\label') + 7:-1]
                else:
                    label = ''
                lines = [form.capitalize() + label + '.']
            elif line.startswith('\\label'):
                label = '_' + line[line.find('\\label') + 7:-1]
                lines[0] = lines[0][:-1] + label + '.'
            elif line.startswith('\\end{align*'):
                align = False
            elif line.startswith('\\end{'):
                lines = emit(lines)
            else:
                if align:
                    line = line[1:] if line.startswith('&') else line
                    lines[-1] += ' ' + line
                else:
                    lines.append(line)

                    
