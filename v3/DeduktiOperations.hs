{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module DeduktiOperations where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import CommonConcepts

import Data.Char
import qualified Data.Map as M

-- frequency map of identifiers in code, excluding bound variables

identsInTypes :: Tree a -> M.Map QIdent Int
identsInTypes t = M.fromListWith (+) [(x, 1) | x <- ids t] where
  ids :: Tree a -> [QIdent]
  ids tree = case tree of
    EIdent qident -> [qident]
    EAbs bind exp -> [x | x <- ids exp, x /= bind2var bind]
    EFun (HVarExp var exp) body -> ids exp ++ [x | x <- ids body, x /= var]
    EFun (HParVarExp var exp) body -> ids  (EFun (HVarExp var exp) body)
    RRule pattbinds patt exp ->
      [x | x <- ids patt ++ ids exp, notElem x (pattbindIdents pattbinds)] ---- types in pattbinds
    JStatic qident typ -> qident : ids typ
    JDef qident typ exp -> qident : ids typ ++ ids exp
    JThm qident typ exp -> qident : ids typ ++ ids exp
    JInj qident typ exp -> qident : ids typ ++ ids exp
    
    _ -> composOpMPlus ids tree


-- consider only typings, for instance when generating natural language
dropDefinitions :: Module -> Module
dropDefinitions (MJmts jmts) = MJmts (concatMap drops jmts) where
  drops :: Jmt -> [Jmt]
  drops jmt = case jmt of
    JDef qident (MTExp typ) _ -> [JStatic qident typ]
    JThm qident (MTExp typ) _ -> [JStatic qident typ]
    JInj qident (MTExp typ) _ -> [JStatic qident typ]
    JStatic _ _ -> [jmt]
    _ -> []

-- collect types of idents
identTypes :: Module -> M.Map QIdent Exp
identTypes (MJmts jmts) = M.fromList (concatMap idtyp jmts) where
  idtyp :: Jmt -> [(QIdent, Exp)]
  idtyp jmt = case jmt of
    JDef qident (MTExp typ) _ -> [(qident, typ)]
    JThm qident (MTExp typ) _ -> [(qident, typ)]
    JInj qident (MTExp typ) _ -> [(qident, typ)]
    JStatic qident typ -> [(qident, typ)]
    _ -> []


-- for a coercion application, only leave its last argument
ignoreCoercions :: [QIdent] -> Tree a -> Tree a
ignoreCoercions cs t = case t of
  EApp _ _ -> case splitApp t of
    (EIdent f, xs@(_:_)) | elem f cs -> ignoreCoercions cs (last xs)
    (f, xs) -> foldl EApp (ignoreCoercions cs f) (map (ignoreCoercions cs) xs)
  _ -> composOp (ignoreCoercions cs) t

-- typically, ignore explicit type arguments to form a polymorphic expression 
ignoreFirstArguments ::[(QIdent, Int)] -> Tree a -> Tree a
ignoreFirstArguments cns t = case t of
  EApp _ _ -> case splitApp t of
    (EIdent f, xs@(_:_)) -> case lookup f cns of
      Just n -> foldl EApp (EIdent f) (map (ignoreFirstArguments cns) (drop n xs))
      _ -> foldl EApp (EIdent f) (map (ignoreFirstArguments cns) xs)
    (f, xs) -> foldl EApp (ignoreFirstArguments cns f) (map (ignoreFirstArguments cns) xs)
  _ -> composOp (ignoreFirstArguments cns) t


alphaConvert :: M.Map String String -> Tree a -> Tree a
alphaConvert convs t = case t of
  QIdent a -> maybe t QIdent $ M.lookup a convs
  _ -> composOp (alphaConvert convs) t

peano2int :: Tree a -> Tree a
peano2int t = case t of
  EApp f x -> case splitApp t of
    (g, xs) -> case countSucc t of
      (0, _) -> foldl EApp g (map peano2int xs)
      (n, EIdent z) | z == identZero -> int2exp n
      (1, exp) -> EApp (EApp (EIdent identPlus) (peano2int exp)) (int2exp 1)
      (n, exp) -> EApp (EApp (EIdent identPlus) (peano2int exp)) (int2exp n)
  _ -> composOp peano2int t
 where
   countSucc :: Exp -> (Int, Exp)
   countSucc exp = case exp of
     EApp (EIdent f) x | f == identSucc -> case countSucc x of
       (n, y) -> (n + 1, y)
       _ -> (0, exp)
     _ -> (0, exp)

-- deciding the kind of a new constant
guessCat :: QIdent -> Exp -> String
guessCat ident@(QIdent c) typ =
  let
    (hypos, val) = splitType typ
    arity = length hypos
  in case lookupConstant c of
    Just (cat, _) -> cat
    _ -> case splitApp val of
      (f, _) | f == typeProp -> case arity of
        0 -> "Name" --- not really
        1 -> "Adj"
        2 -> "Rel"
        _ -> "Fun"
      (f, _) | elem f [typeSet, typeType] -> case arity of
        0 -> "Kind"
        _ -> "Fun"
      (EIdent f, _) | f == identElem -> case arity of
        0 -> "Name"
        _ -> "Fun"
      (EIdent f, _) | f == identProof -> "Label"
      _ -> "Unknown"

-- to begin with, to decide how to render a hypo
catExp :: Exp -> String
catExp e = case e of
  EApp _ _ -> case splitApp e of
    (EIdent f@(QIdent c), _) -> case lookupConstant c of
      Just (k, _) | elem k ["Adj", "Rel", "Compar"] -> "Prop"
      _ | elem f [identConj, identDisj, identImpl,
                  identEquiv, identPi, identSigma, identNeg] -> "Prop"
      _ -> "Kind"
  _ -> "Kind"


splitType :: Exp -> ([Hypo], Exp)
splitType exp = case exp of
  EFun hypo body -> case splitType body of
    ([], _) -> ([hypo], body)        
    (hypos, rest) -> (hypo:hypos, rest)
  _ -> ([], exp)

addVarsToHypos :: [Hypo] -> [Hypo]
addVarsToHypos = adds vars where
  adds :: [QIdent] -> [Hypo] -> [Hypo]
  adds vs hypos = case hypos of
    HExp exp : hh -> HVarExp (head vs) exp : adds (tail vs) hh
    hypo@(HVarExp var _) : hh -> hypo : adds (filter (/= var) vs) hh
    hypo@(HParVarExp var _) : hh -> hypo : adds (filter (/= var) vs) hh
    _ -> []
  vars = [QIdent s |
           s <- ["x", "y", "z", "u", "v", "w"] ++ ["X"  ++ show i | i <- [1..11]]]
	 --- finite list so that filter works

-- strip abstraction when function type arguments are moved to hypos, as in Lean
stripAbs :: [Hypo] -> Exp -> Exp
stripAbs hypos exp = case (hypos, exp) of
  (h:hs, EAbs _ body) -> stripAbs hs body
  _ -> exp

splitApp :: Exp -> (Exp, [Exp])
splitApp exp = case exp of
  EApp fun arg -> case splitApp fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (exp, [])

splitAbs :: Exp -> ([Bind], Exp)
splitAbs exp = case exp of
  EAbs bind body -> case splitAbs body of
    ([], _) -> ([bind], body)        
    (binds, rest) -> (bind:binds, rest)
  _ -> ([], exp)

splitPatt :: Patt -> (Patt, [Patt])
splitPatt patt = case patt of
  PApp fun arg -> case splitPatt fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (patt, [])

splitIdent :: QIdent -> Exp -> [Exp]
splitIdent conn exp = case splitApp exp of
  (EIdent fun, [a, b]) | fun == conn -> case splitIdent conn a of
    [] -> [a, b]
    cs -> cs ++ [b]
  _ -> []

isWildIdent :: QIdent -> Bool
isWildIdent (QIdent s) = all (=='_') s

getNumber :: Exp -> [Exp] -> Maybe String
getNumber fun args =
  case (fun, args) of
    (EIdent (QIdent n), [x]) | n == nd -> getDigit x
    (EIdent (QIdent n), [x, y]) | n == nn -> do
      d <- getDigit x
      n <- uncurry getNumber (splitApp y) 
      return (d ++ n)
    _ -> Nothing
 where
   getDigit :: Exp -> Maybe String
   getDigit x = case x of
     EIdent (QIdent [d]) | elem d "0123456789" -> return [d]
     _ -> Nothing

int2exp :: Int -> Exp
int2exp = cc . show
  where
    cc s = case s of
      [d] -> EApp (EIdent (QIdent nd)) (EIdent (QIdent s))
      d:ds -> EApp (EApp (EIdent (QIdent nn)) (EIdent (QIdent [d]))) (cc ds)
      
unresolvedIndexIdent :: Int -> QIdent
unresolvedIndexIdent i = QIdent ("UNRESOLVED_INDEX_" ++ show i)


-- used in quantified propositions
bind2var :: Bind -> QIdent
bind2var bind = case bind of
  BVar v -> v
  BTyped v _ -> v

pattbindIdents :: [Pattbind] -> [QIdent]
pattbindIdents = concatMap bident where
  bident :: Pattbind -> [QIdent]
  bident pattbind = case pattbind of
    PBVar x -> [x]
    PBTyped x _ -> [x]
    _ -> []

-- strip the qualifier part of an ident
stripQualifiers :: Tree a -> Tree a
stripQualifiers t = case t of
  QIdent c -> QIdent (stripQ c)
  _ -> composOp stripQualifiers t
 where
   stripQ c = case break (=='.') c of
     (_, _:x) -> x
     _ -> c

