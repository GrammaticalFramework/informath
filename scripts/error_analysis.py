import sys
import json

# python3 scripts/error_analysis.py tmp/M_training_full_predictions_natural_agda.jsonl (all|tuned|gold)



INFILE = sys.argv[1]
MODE = sys.argv[2]

with open(INFILE) as file:
    data = [json.loads(line) for line in file]


keymap = {
    'Eng': 'Engl',
    'Baseline output': 'Base',
    'M_training_full prediction': 'Tune',
    'label': 'Gold',
    'baseline_bleu-4': 'B_BL',
    'Fine-tuned_blue-4': 'T_BL'
    }

data = [{keymap[k]: v for k, v in d.items()} for d in data]

rename = lambda ws, i: ' '.join([ws[0], ws[1] + 'x' + i] + ws[2:])


# printing all data line by line, by stanzas
if MODE == 'all':
  for d in data:
    for k, v in d.items():
        print(k +':', v)
    print()


# printing baseline predicted Agda, renamed thm labels
# use this for generating an Agda file from the baseline predictions
if MODE == 'base':
  i = 0
  for d in data:
    print(rename(d['Base'].split(), str(i)))
    i += 1

# printing predicted Agda, renamed thm labels
# use this for generating an Agda file from the predictions
if MODE == 'tuned':
  i = 0
  for d in data:
    print(rename(d['Tune'].split(), str(i)))
    i += 1

# printing gold standard Agda, renamed thm labels
if MODE == 'gold':
  i = 0
  for d in data:
    print(rename(d['Gold'].split(), str(i)))
    i += 1


    
"""
## all of the predicted Agda theorems parse correctly with 'agda' command, but they have type errors
## common reason: uses of infix operators like ==, +, > are not supported by vanilla Agda but assume some library.
## they are predicted repeatedly, so the base model must have got them from somewhere. Maybe from Lean?

Engl: Thm20a. Every prime number of the form $4k+1$ can be written as the sum of two squares.
Base: 
Tune: postulate thm20a : (p : Nat) -> prime p -> congruent p 1 4 -> exists Nat (\ x -> exists Nat (\ y -> and (square x) (square y) (equal (plus (times x x) (times y y)) p)))
Gold: postulate thm20a : (p : Nat) -> prime p -> (k : Nat) -> Eq p (plus (times 4 k) 1) -> exists Nat (\ x -> exists Nat (\ y -> Eq p (plus (square x) (square y))))
B_BL: 0.0
T_BL: 63.97
# clever use of congruent; the last part would not type-check


Engl: Thm22. There are uncountably many reals.
Base: 
Tune: postulate thm22 : exists Real (\ x -> not (exists Real (\ y -> and (Lt y x) (Lt x y))))
Gold: postulate thm22 : not (denumerable Real)
B_BL: 0.0
T_BL: 5.72
# this says something quite different, although meaningful (would type-check)

Engl: Thm22. $\mathbb{R}$ is uncountable.
Base: 
Tune: postulate thm22 : uncountable Real
Gold: postulate thm22 : not (denumerable Real)
B_BL: 0.0
T_BL: 23.45
# morally correct if you think about the meaning, but the predicate uncountable is not in scope

Engl: Thm52. If $A$ is a finite set, then its powerset has cardinality $2^{\lvert A \rvert}$.
Base: 
Tune: postulate thm52 : (A : Set) -> finite A -> equalset (powerset A) (powerset2 A)
Gold: postulate thm52 : (A : Set) -> finite A -> Eq (cardinality (powerset A)) (pow 2 (cardinality A))
B_BL: 0.0
T_BL: 54.53
# non-existing ("hallucinated" ?) identifier powerset2
# equalset is not the correct predicate for equality of cardinality

Engl: Thm78a. Let $u$ and $v$ be vectors. If $u \perp v$ then $u \cdot v = 0$.
Base: 
Tune: postulate thm78a : (u : Vector) -> (v : Vector) -> orthogonal u v -> dotProduct u v == 0
Gold: postulate thm78a : (u : Vector) -> (v : Vector) -> if (orthogonal u v) (Eq (dotProduct u v) 0)
B_BL: 0.0
T_BL: 60.19
# equivalent formulations: Tune uses a function type, where Gold uses the if operator
# Tune uses the == operator, which is not defined in our Agda library


Engl: Thm91. $\lvert u+v \rvert \leq \lvert u \rvert+\lvert v \rvert$.
Base: 
Tune: postulate thm91 : (u : Vector) -> (v : Vector) -> length (plus u v) <= plus (length u) (length v)
Gold: postulate thm91 : (u : Vector) -> (v : Vector) -> Leq (norm (vectorPlus u v)) (plus (norm u) (norm v))
B_BL: 0.0
T_BL: 55.77
# morally, equivalent
# Tune uses plus (defined for numbers but not for vectors) instead of vectorPlus
# Tune uses the <= operator not defined
"""
