# The project for Informath's scripts/rocq (rocq-export.py, rocq-index.py):
# its modules, symbol table and settings.  See scripts/rocq/project.py in
# Informath for all the settings.
import os, re

_here = os.path.dirname(os.path.abspath(__file__))

# coq/Chapter1.v, Chapter2.v, ...: the units of the text, in its order
MODULES = sorted((f[:-2] for f in os.listdir(os.path.join(_here, 'coq'))
                  if re.fullmatch(r'Chapter\d+\.v', f)), key=lambda m: int(m[7:]))

# made by build.sh from dkgf/ChapterN.dkgf
TABLE = 'table.dkgf'

# the word of the tags (* //TAG:<sent_id> <sentence> *), and the source's
# sentences: a CoNLL-U file with "# sent_id =" and "# text =" lines
TAG = 'TEXT'
CONLLU = os.path.join(_here, 'text', 'source.conllu')

# constants whose function argument is a bound variable and a body (the set
# of x in X such that R: setOf X (fun x => R)): name -> its position
BINDER_CONSTS = {}

# records that are sets of objects rather than algebraic structures
# RECORDS_AS_STRUCTURES = False

# record field -> the variable that names it in the structure's definition
FIELD_VARIABLES = {}
