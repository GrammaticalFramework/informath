module Main where

import Data.Char
import qualified Data.Map as M

type Binds = [[Int]]
type BindMap = M.Map String Binds

lexx :: String -> [String]
lexx s = case lex s of
  [(tok, [])] -> [tok]
  [(tok, rest)] -> tok : lexx rest
  _ -> []

getBinds :: String -> Binds
getBinds = getb . lexx where
  getb toks = case toks of
    "(" : ts -> case break (== ")") ts of
      (bs, _:tt) -> map readInt bs : getb tt
    i : ts -> [readInt i] : getb ts
    _ -> []

readInt :: String -> Int
readInt s = case s of
  _:_ | all isDigit s -> read s
  _ -> error $ "integer expected, found " ++ s

data Exp =
    EApp String [Exp]
  | EAbs [String] Exp
  deriving (Show, Eq)

prExp exp = case exp of
  EApp f [] -> f
  EApp f xs -> "(" ++ unwords (f : map prExp xs) ++ ")"
  EAbs xs e -> "(\\" ++ unwords (xs ++ ["->", prExp e]) ++ ")"


eVar x = EApp x []

unbind :: BindMap -> Exp -> Exp
unbind bmap exp = case exp of
  EApp f cs -> EApp f (concatMap flatten cs)
 where
  flatten t = case t of
    EAbs xs b -> map eVar xs ++ [unbind bmap b]
    _ -> [unbind bmap t]

rebind :: BindMap -> Exp -> Exp
rebind bmap exp = case exp of
  EApp f cs -> case M.lookup f bmap of
    Just binds -> EApp f (map eAbs (groups binds cs))
    _ -> EApp f (map (rebind bmap) cs)
 where
   groups binds cs = [[cs !! (b-1) | b <- bind] | bind <- binds]
   eAbs ts = case ts of
     [t] -> rebind bmap t
     _ -> EAbs (map getVar (init ts)) (rebind bmap (last ts))
   getVar t = case t of
     EApp x [] -> x
     _ -> error $ "can't get variable from " ++ show t


bindMap = M.fromList [
  ("integral", getBinds "(1 2) 3 4"),
  ("ifi", getBinds "1 2 (3 4)") 
  ]

test exp = do
  let uexp = unbind bindMap exp
  let ruexp = rebind bindMap uexp
  putStrLn $ prExp exp
  putStrLn $ prExp uexp
  putStrLn $ prExp ruexp
  print (ruexp == exp)

main = do
  test $ EApp "integral" [EAbs ["x"] (eVar "77"), eVar "11", eVar "12"]
  test $ EApp "ifi" [(eVar "A"), (eVar "A"), (EAbs ["x"] (eVar "x"))]
  test $ EApp "ifi" [(eVar "A"), EApp "if" [eVar "B", eVar "A"], (EAbs ["x"] (EApp "ifi" [eVar "B", eVar "A", EAbs ["y"] (eVar "x")]))]


mainz = do
  print $ lexx "(1 2) 3 (4 5 6)"
  print $ getBinds "(1 2) 3 (4 5 6)"
  print $ getBinds "(1 2) 3"
  print $ getBinds "1 2 3"



