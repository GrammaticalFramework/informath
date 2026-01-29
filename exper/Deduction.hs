module Deduction where

-- experiment with Jan von Plato 2017. "From Gentzen to Jaskowski and Back:
-- Algorithmic Translation of Derivations Between the Two Main Systems of Natural Deduction."

data Formula =
    And Formula Formula
  | Or Formula Formula
  | If Formula Formula
  | Not Formula
  | Falsum
  | Atom String
  deriving Show

data Proof =
    AndI Formula Formula Proof Proof
  | AndE1 Formula Formula Proof
  | AndE2 Formula Formula Proof
  | OrI1 Formula Formula Proof
  | OrI2 Formula Formula Proof
  | OrE Formula Formula Formula Proof Proof Proof
  | IfI Formula Formula Proof
  | IfE Formula Formula Proof Proof
  | NotI Formula Proof
  | NotE Formula Proof Proof
  | FalsumE Formula Proof
  | Hypo Formula
  deriving Show


prf :: Formula -> String
prf = pr 0 where
  pr n f = case f of
    And a b -> par 3 n (pr 3 a ++ " \\& " ++ pr 4 b)
    Or a b -> par 2 n (pr 2 a ++ " \\vee " ++ pr 3 b)
    If a b -> par 1 n (pr 2 a ++ " \\supset " ++ pr 2 b)
    Not a -> par 4 n ("\\sim " ++ pr 4 a)
    Falsum -> "\\bot"
    Atom s -> s
  par k n f = if k >= n then f else "(" ++ f ++ ")"

aA = Atom "A"
aB = Atom "B"
aC = Atom "C"

main = do
  putStrLn $ prf (And (Or aA aB) aC)
  putStrLn $ prf (If (And aA aB) (And (Not (Not aA)) (Not (Not aB))))



