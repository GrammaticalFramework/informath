-- A test of scripts/agda and RunInformath -from-formalism=agda (make from_formalism_tests)
module Example where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

add : ℕ → ℕ → ℕ
add zero n = n
add (succ m) n = succ (add m n)

double : ℕ → ℕ
double n = add n n

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

ap : {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
ap f refl = refl

add-zero : (n : ℕ) → add n zero ≡ n
add-zero zero = refl
add-zero (succ n) = ap succ (add-zero n)
