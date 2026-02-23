module BaseConstants where

--  Prop = Set --- depends on Agda version

  data false : Prop where
  
  data and (A B : Prop) : Prop where
    andI : A -> B -> and A B
    
  data or (A B : Prop) : Prop where
    orIL : A -> or A B
    orIR : B -> or A B
    
  if : Prop -> Prop -> Prop
  if A B = A -> B
  
  all : (A : Set) -> (B : (A -> Prop)) -> Prop
  all A B = (x : A) -> B x
  
  data exists (A : Set) (B : (A -> Prop)) : Prop where
    existI : (a : A) -> B a -> exists A B

  not : Prop -> Prop
  not = \ A -> if A false

  iff : Prop -> Prop -> Prop
  iff = \ A -> \ B -> and (if A B) (if B A)

  data Num : Set where
    Zero : Num
    Succ : Num -> Num

  {-# BUILTIN NATURAL Num #-}

  Dig : Set
  Dig = Num
  
  Nat : Set
  Nat = Num

  Int : Set
  Int = Num

  Rat : Set
  Rat = Num

  Real : Set
  Real = Num
 
  Complex : Set
  Complex = Num

  nd : Dig -> Nat
  nd x = x
  
  postulate nn : Dig -> Nat -> Nat

  postulate Eq : (x : Real) -> (y : Real) -> Prop
  postulate Lt : Real -> Real -> Prop

  postulate Gt : Real -> Real -> Prop
  postulate Neq : Real -> Real -> Prop

  postulate Leq : Real -> Real -> Prop
  postulate Geq : Real -> Real -> Prop

  postulate positive : (x : Real) -> Prop
  postulate negative : Real -> Prop

  postulate plus : (x : Real) -> (y : Real) -> Real
  postulate minus : Real -> Real -> Real
  postulate times : Real -> Real -> Real
  postulate div : Real -> Real -> Real
  postulate pow : Real -> Real -> Real
  postulate gcd : Int -> Int -> Int
  postulate factorial : Nat -> Nat
  postulate sqrt : Num -> Num
  postulate neg : Num -> Num
  
  postulate even : Int -> Prop

  odd : Int -> Prop
  odd = \ n -> not (even n)

  divisible : Int -> Int -> Prop
  divisible = \ n -> \ m -> exists Int (\ k -> Eq n (times k m))

  prime : Nat -> Prop
  prime = \ n -> not (exists Nat (\ m -> and (Lt 1 m) (and (Lt m n) (divisible n m))))

  postulate function : Set -> Set -> Set
  postulate union : Set -> Set -> Set
  postulate intersection : Set -> Set -> Set
  postulate difference : Set -> Set -> Set
  postulate powerset : Set -> Set
  postulate suchthat : (A : Set) -> ((B : A) -> Prop) -> Set
  postulate fst : (A : Set) -> (B : A -> Prop) -> (suchthat A B) -> A

  postulate cartesian : Set -> Set -> Set

  postulate complement : Set -> Set

  postulate emptyset : Set

  postulate universeset : Set

  postulate subset : Set -> Set -> Prop

  postulate subseteq : Set -> Set -> Prop

  postulate superset : Set -> Set -> Prop

  postulate superseteq : Set -> Set -> Prop

  postulate equalset : Set -> Set -> Prop

  postulate element : universeset -> Set -> Prop

  postulate notelement : universeset -> Set -> Prop

  postulate positivePart : Set -> Set

  postulate negativePart : Set -> Set

  postulate finite : Set -> Prop

  infinite : Set -> Prop
  infinite = \ A -> not (finite A)

  congruent : (a : Int) -> (b : Int) -> (m : Nat) -> Prop
  congruent = \ a -> \ b -> \ m -> exists Nat (\ k -> Eq (minus a b) (times k m))

  postulate Enum : Set

  postulate nil : Enum

  postulate cons : Num -> Enum -> Enum

  postulate enumset : Enum -> Set

  rational : Real -> Prop
  rational = \ x -> exists Int (\ p -> exists Int (\ q -> and (Neq q 0) (Eq x (div p q))))

  irrational : Real -> Prop
  irrational = \ x -> not (rational x)

  square : Real -> Real
  square = \ x -> pow x 2

  postulate Polynomial : Set

  postulate degree : (P : Polynomial) -> Nat

  postulate isRoot : (c : Complex) -> (P : Polynomial) -> Prop

  Cardinal : Set
  Cardinal = Num
  
  postulate cardinality : (A : Set) -> Cardinal

  denumerable : (A : Set) -> Prop
  denumerable = \ A -> Eq (cardinality A) (cardinality Nat)

  postulate Vector : Set

  postulate norm : Vector -> Real

  length : (v : Vector) -> Real
  length = \ v -> norm v

  postulate resultant : Vector -> Vector -> Vector

  postulate legendre : Nat -> Nat -> Int

  postulate pi : Real

  postulate Circle : Set

  postulate radius : Circle -> Real

  postulate area : Circle -> Real

  binomial : (n : Nat) -> (k : Nat) -> Nat
  binomial = \ n -> \ k -> div (factorial n) (times (factorial k) (factorial (minus n k)))

  postulate combinations : (n : Nat) -> (k : Nat) -> Nat

  postulate combinationsFromSet : (A : Set) -> (k : Nat) -> Set

  postulate sin : Real -> Real

  cos : Real -> Real
  cos = \ x -> sin (minus (div pi 2) x)

  tan : Real -> Real
  tan = \ x -> div (sin x) (cos x)

  postulate arcsin : Real -> Real

  postulate arccos : Real -> Real

  postulate arctan : Real -> Real

  postulate angleBetween : Vector -> Vector -> Real

  dotProduct : (u : Vector) -> (v : Vector) -> Real
  dotProduct = \ u -> \ v -> times (times (length u) (length v)) (cos (angleBetween u v))

  orthogonal : (u : Vector) -> (v : Vector) -> Prop
  orthogonal = \ u -> \ v -> Eq (angleBetween u v) (div pi 2)

  perpendicular : (u : Vector) -> (v : Vector) -> Prop
  perpendicular = \ u -> \ v -> orthogonal u v

  vectorPlus : (u : Vector) -> (v : Vector) -> Vector
  vectorPlus = \ u -> \ v -> resultant u v
