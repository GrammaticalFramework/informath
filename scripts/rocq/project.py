"""The project that the Rocq scripts work for: its directory and its
configuration (config.py there).

The project directory is $ROCQ_INFORMATH_PROJECT, or else the current
directory: the scripts are run in a project's directory, which has
config.py, the symbol table, coq/ (the Rocq files) and out/ (the export).

config.py gives, all optional except MODULES and TABLE (which rocq-index.py
needs; rocq-export.py works without config.py):

    MODULES          the Rocq modules, in coq/<M>.v
    TABLE            the symbol table (.dkgf), relative to the project
    BINDER_CONSTS    constants whose function arguments are bound variables and
                     bodies of a binder notation: name -> position of that
                     argument, or a tuple of positions; eta-expanded
    DROP             name -> positions of arguments the readings leave out
    SHIFT1           name -> position of a level argument n that stands for n+1
    VALUE_FIELDS     record fields whose application to the record alone is
                     the record itself (rv x is x)
    SECTION_VARS     the section variables of the Rocq file (G1, G2)
    PREDICATE_SETS   types defined as T -> Prop that are read as sets: D s is
                     "s is in D" (bmem D s), fun s => P at the type is setof (s => P)
    BINDER_MACROS    the symbol table's binder macros and their arities
    FUNCTION_FORMS   their forms for a function argument (\\CoqAllF)
    ATOMIC           macro name prefixes whose forms need no parentheses
    SIDE_CONDITIONS  propositions (prefixes) left out of proof texts
"""
import importlib.util
import os

DIR = os.path.abspath(os.environ.get('ROCQ_INFORMATH_PROJECT') or os.environ.get('COQ_INFORMATH_PROJECT')
                      or os.getcwd())
SHARED = os.path.dirname(os.path.abspath(__file__))

# without config.py (a single file exported, as RunInformath -from-formalism
# does), all the settings have their defaults
_path = os.path.join(DIR, 'config.py')
if os.path.exists(_path):
    _spec = importlib.util.spec_from_file_location('coq_informath_config', _path)
    config = importlib.util.module_from_spec(_spec)
    _spec.loader.exec_module(config)
else:
    import types
    config = types.SimpleNamespace()


def get(name, default):
    return getattr(config, name, default)


def informath_root():
    """Informath: the checkout these scripts are in (scripts/rocq/)."""
    return os.path.normpath(os.path.join(SHARED, '..', '..'))
