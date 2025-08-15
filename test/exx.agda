open import BaseConstants

postulate prop10 : even 0

postulate prop20 : (n : Nat) -> even n -> odd (plus n 1)

postulate prop30 : (n : Nat) -> odd n -> even (plus n 1)

postulate prop40 : all Nat (\ n -> or (even n) (odd n))

postulate prop50 : all Nat (\ n -> not (and (even n) (odd n)))

postulate prop60 : all Nat (\ n -> Eq (plus n 0) n)

postulate prop70 : odd 1

postulate prop80 : even 2

postulate prop90 : (n : Nat) -> if (and (even n) (prime n)) (Eq n 2)

postulate prop100 : (a : Int) -> (b : Int) -> (c : Int) -> Eq (plus (plus a b) c) 0 -> Lt (times (times a b) c) 0

postulate prop110 : (a : Int) -> (c : Int) -> and (odd a) (odd c) -> all Int (\ b -> even (plus (times a b) (times b c)))

postulate prop120 : (a : Int) -> (b : Int) -> iff (even (times a b)) (or (even a) (even b))

postulate prop130 : (x : Int) -> (y : Int) -> if (odd (times x y)) (and (odd x) (odd y))

same_parity : Int -> Int -> Prop

same_parity = \ x -> \ y -> or (and (even x) (even y)) (and (odd x) (odd y))

postulate prop140 : (x : Int) -> (y : Int) -> iff (same_parity x y) (even (plus x y))

postulate prop150 : (n : Nat) -> even (minus (pow n 3) n)

postulate prop160 : (a : Int) -> (b : Int) -> if (same_parity (plus a b) (times a b)) (and (even a) (even b))

