open import BaseConstants -- NORMALIZED

postulate thm01x0 : (m : Nat) -> (n : Nat) -> Gt n 0 -> not (Eq (pow (div m n) 2) 2) -- NORMALIZED
postulate thm01x1 : (m : Nat) -> (n : Nat) -> if (and (Gt n 0) (Eq (pow (div m n) 2) 2)) false -- NORMALIZED
postulate thm01ax2 : (m : Nat) -> (n : Nat) -> not (Eq (pow (div m (plus n 1)) 2) 2) -- NORMALIZED
postulate thm01bx3 : all Rat (\ q -> not (Eq (pow q 2) 2)) -- NORMALIZED
postulate thm01bx4 : all Rat (\ x -> not (Eq (pow x 2) 2)) -- NORMALIZED
postulate thm01bx5 : (q : Rat) -> Neq (pow q 2) 2 -- NORMALIZED
postulate thm01ex6 : irrational (sqrt 2) -- NORMALIZED
postulate thm01ex7 : irrational (sqrt 2) -- NORMALIZED
postulate thm01fx8 : (p : Rat) -> (q : Rat) -> not (Eq (pow p 2) (times 2 (pow q 2))) -- NORMALIZED
postulate thm01fx9 : (p : Rat) -> (q : Rat) -> not (Eq (pow p 2) (times 2 (pow q 2))) -- NORMALIZED
--postulate thm02x10 : (P : Polynomial) -> Gt (degree P) 0 -> exists Complex (\ c -> Eq (P c) 0) -- NORMALIZED
--postulate thm02x11 : (P : Polynomial) -> Neq (degree P) 0 -> exists Complex (\ z -> Eq (P z) 0) -- NORMALIZED
--postulate thm02x12 : (P : Polynomial) -> Neq (degree P) 0 -> exists Complex (\ c -> Eq (P c) 0) -- NORMALIZED
--postulate thm03x13 : countable Rat -- NORMALIZED
--postulate thm03x14 : countable Real -- NORMALIZED
--postulate thm03ax15 : cardinality Real Nat -- NORMALIZED
--postulate thm04x16 : (u : Vector) -> (v : Vector) -> perpendicular u v -> Eq (length (plus u v)) (sqrt (plus (pow (length u) 2) (pow (length v) 2))) -- NORMALIZED
--postulate thm04x17 : (u : Vector) -> (v : Vector) -> orthogonal u v -> Eq (length (plus u v)) (sqrt (plus (pow (length u) 2) (pow (length v) 2))) -- NORMALIZED
--postulate thm07x18 : (p : Nat) -> (q : Nat) -> prime p -> prime q -> Eq (times (legendre p q) (legendre q p)) (pow -1 (times (minus p 1) (minus q 1))) -- NORMALIZED
--postulate thm07x19 : (p : Nat) -> (q : Nat) -> prime p -> prime q -> equal (times (legendre p q) (legendre q p)) (pow -1 (times (minus p 1) (minus q 1))) -- NORMALIZED
--postulate thm09x20 : (r : Real) -> Eq (area (Circle r)) (times pi (pow r 2)) -- NORMALIZED
--postulate thm09x21 : (r : Real) -> Eq (area (Circle r)) (times pi (pow r 2)) -- NORMALIZED
--postulate thm09x22 : (r : Real) -> Eq (areaCircle r) (times pi (pow r 2)) -- NORMALIZED
postulate thm10FermatLittlex23 : (p : Int) -> (a : Int) -> prime p -> exists Int (\ q -> Eq (minus (pow a p) a) (times p q)) -- NORMALIZED
--postulate thm10FermatLittlex24 : (p : Int) -> (a : Int) -> prime p -> divide p (minus (pow a p) a) -- NORMALIZED
postulate thm11x25 : (n : Nat) -> exists Nat (\ p -> and (Geq p n) (prime p)) -- NORMALIZED
---postulate thm11x26 : all Nat (\ n -> not (forall Prime (\ p -> n > p))) -- NORMALIZED
--postulate thm19x28 : (n : Nat) -> exists Nat (\ a -> exists Nat (\ b -> exists Nat (\ c -> exists Nat (\ d -> and (nonneg a) (nonneg b) (nonneg c) (nonneg d))))) (lambda a -> lambda b -> lambda c -> lambda d -> Eq n (plus (plus (pow a 2) (pow b 2)) (plus (pow c 2) (pow d 2)))) -- NORMALIZED
--postulate thm20ax29 : (p : Nat) -> prime p -> congruent p 1 4 -> exists Nat (\ x -> exists Nat (\ y -> and (square x) (square y) (equal (plus (times x x) (times y y)) p))) -- NORMALIZED
postulate thm20ax30 : (p : Nat) -> prime p -> exists Nat (\ k -> Eq p (plus (times 4 k) 1)) -> exists Nat (\ x -> exists Nat (\ y -> and (Eq p (plus (times x x) (times y y))) (Lt 0 y))) -- NORMALIZED
--postulate thm20bx31 : (p : Nat) -> prime p -> congruent p 1 4 -> exists Nat (\ x -> exists Nat (\ y -> and (square x) (square y))) -- NORMALIZED
postulate thm20bx32 : (p : Nat) -> prime p -> congruent p 1 4 -> exists Nat (\ x -> exists Nat (\ y -> and (Eq (plus x y) p) (and (Geq x 0) (Geq y 0)))) -- NORMALIZED
--postulate thm22x33 : uncountable Real -- NORMALIZED
postulate thm22x34 : exists Real (\ x -> not (exists Real (\ y -> and (Lt y x) (Lt x y)))) -- NORMALIZED
--postulate thm22x35 : uncountable Real -- NORMALIZED
--postulate thm51wilsonx36 : (n : Nat) -> iff (prime n) (congruent (factorial (minus n 1)) -1 (mod n)) -- NORMALIZED
--postulate thm51bx37 : (n : Nat) -> iff (prime n) (divisible n (plus (factorial (minus n 1) 1))) -- NORMALIZED
--postulate thm52x38 : (A : Set) -> finite A -> equalset (powerset A) (set (pow (cardinality A) 2)) -- NORMALIZED
--postulate thm52x39 : (A : Set) -> finite A -> equalset (powerset A) (powerset2 A) -- NORMALIZED
--postulate thm58x40 : (n : Nat) -> (k : Nat) -> Leq 0 k -> Leq k n -> binomial n k -- NORMALIZED
--postulate thm58x41 : (A : Set) -> Eq (cardinality A) n -> (k : Nat) -> and (Leq 0 k) (Leq k n -> Eq (cardinality (combinationsFromSet A k)) (binomial n k)) -- NORMALIZED
--postulate thm58x42 : (A : Set) -> finite A -> Eq (cardinality A) n -> Leq 0 k -> Leq k n -> equalset (combinationsFromSet A k) (subsets A k) -- NORMALIZED
--postulate thm78x43 : (u : Real) -> (v : Real) -> Leq (times u v) (times (abs u) (abs v)) -- NORMALIZED
postulate thm78ax45 : (v : Vector) -> (w : Vector) -> orthogonal v w -> Eq (dotProduct v w) 0 -- NORMALIZED
postulate thm78ax46 : (u : Vector) -> (v : Vector) -> orthogonal u v -> Eq (dotProduct u v) 0 -- NORMALIZED
postulate thm78ax47 : (u : Vector) -> (v : Vector) -> orthogonal u v -> Eq (dotProduct u v) 0 -- NORMALIZED
postulate thm78ax48 : (u : Vector) -> (v : Vector) -> orthogonal u v -> Eq (dotProduct u v) 0 -- NORMALIZED
postulate thm78ax49 : (u : Vector) -> (v : Vector) -> orthogonal u v -> Eq (dotProduct u v) 0 -- NORMALIZED
--postulate thm91x50 : (u : Vector) -> (v : Vector) -> Leq (length (plus u v)) (plus (length u) (length v)) -- NORMALIZED
--postulate thm91x51 : (u : Vector) -> (v : Vector) -> Leq (length (plus u v)) (plus (length u) (length v)) -- NORMALIZED
--postulate thm91x52 : (u : Vector) -> (v : Vector) -> Leq (norm (plus u v)) (plus (norm u) (norm v)) -- NORMALIZED
--postulate thm98x53 : (n : Nat) -> exists Nat (\ m -> and (Lt n m) (Lt m (times 2 n))) prime -- NORMALIZED
--postulate thm98x54 : (n : Nat) -> exists Nat (\ p -> and (Lt (plus n 1) p) (Lt p (minus (times 2 n) 1)) (prime p)) -- NORMALIZED
--postulate thm98x55 : (n : Nat) -> exists Nat (\ p -> and (Lt n p) (Lt p (times 2 n))) (prime p) -- NORMALIZED
postulate thm98x56 : (n : Nat) -> exists Nat (\ p -> and (Lt n p) (and (Lt p (times 2 n)) (prime p))) -- NORMALIZED
