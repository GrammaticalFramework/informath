{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module DeduktiOperations where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import Dedukti.ParDedukti (myLexer)
import Dedukti.LexDedukti (Token(..), prToken)
import CommonConcepts
import ConstantData

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


applyConstantData :: ConstantData -> Tree a -> Tree a
applyConstantData cd = appConst []
 where
   appConst :: [QIdent] -> Tree a -> Tree a
   appConst bs t = case t of
    QIdent _ | elem t bs -> t
    c@(QIdent _) -> fst $ lookid c
    BVar _ -> t
    BTyped x exp -> BTyped x (appConst bs exp)
    HVarExp x exp -> HVarExp x (appConst bs exp)
    HParVarExp x exp -> HParVarExp x (appConst bs exp)
    EAbs b exp -> EAbs (appConst bs b) (appConst (bind2var b : bs) exp)
    EFun h exp -> EFun (appConst bs h) (appConst (hypo2vars h ++ bs) exp)
    EApp _ _ -> case splitApp t of
      (EIdent c, xs) | elem c bs -> foldl EApp (EIdent c) (map (appConst bs) xs)
      (EIdent c, xs) -> case lookid c of
        (ident, com) -> foldl EApp (EIdent ident) (map (appConst bs) (applyCombination com xs))
      (f, xs) -> foldl EApp (appConst bs f) (map (appConst bs) xs)
    _ -> composOp (appConst bs) t

   lookid :: QIdent -> (QIdent, Combination)
   lookid f@(QIdent c) = case M.lookup c cd of
     Just (BASE cat fun) -> (gfAnnotate cat fun f, ComALL)
     Just (ALIAS _ dkid com) -> (applyConstantData cd (QIdent dkid), com)
     Just (NEW _ cat fun com) -> (gfAnnotate cat fun f, com)
     Just (CONV other) -> (QIdent other, ComALL)
     _ -> (f, ComALL)

   gfAnnotate :: GFCat -> GFFun -> QIdent -> QIdent
   gfAnnotate cat fun ident@(QIdent c) = QIdent (c ++ "&" ++ cat ++ "&" ++ fun)

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


eliminateLocalDefinitions :: Tree a -> Tree a
eliminateLocalDefinitions = elim [] where
  elim :: [(QIdent, Exp)] -> Tree a -> Tree a
  elim defs t = case t of
    EIdent x -> maybe t id (lookup x defs)
    EFun (HLetExp x d) e -> elim ((x, elim defs d):defs) e
    EFun (HLetTyped x _ d) e -> elim defs (EFun (HLetExp x d) e)
    _ -> composOp (elim defs) t


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
       (0, _) -> (0, exp)
       (n, y) -> (n + 1, y)
     _ -> (0, exp)

enum2list :: Exp -> Maybe [Exp]
enum2list t = case t of
  EApp (EApp (EIdent (QIdent "cons")) x) xs -> do
    exps <- enum2list xs
    return (x : exps)
  EIdent (QIdent "nil") -> return []
  _ -> Nothing

list2enum :: [Exp] -> Exp
list2enum xs = case xs of
  x:xx -> EApp (EApp (EIdent (QIdent "cons")) x) (list2enum xx)
  _ -> EIdent (QIdent "nil")

-- deciding the kind of a new constant
guessCat :: QIdent -> Exp -> String
guessCat ident@(QIdent c) typ =
  let
    (hypos, val) = splitType typ
    arity = length hypos
  in case lookupConstant c of
    Just (cat, _) -> cat
    _ -> case splitApp val of
      (EIdent f, _) | f == identProp -> case arity of
        0 -> "Name" --- not really
        1 -> "Adj"
        2 -> "Reladj"
        _ -> "Fun"
      (EIdent f, _) | elem f [identSet, identType] -> case arity of
        0 -> "Noun"
        _ -> "Fun"
      (EIdent f, _) | f == identElem -> case arity of
        0 -> "Name"
        _ -> "Fun"
      (EIdent f, _) | f == identProof -> "Label"
      _ -> "UnresolvedConstant_" ++ c --- error ("Unresolved constant " ++ c)


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

-- make hypo-bound vars consistent with defining abstraction, adding vars if not given
addVarsToHypos :: Maybe Exp -> [Hypo] -> [Hypo]
addVarsToHypos mexp = adds vars where
  adds :: [QIdent] -> [Hypo] -> [Hypo]
  adds vs hypos = case hypos of
    HExp exp : hh -> HVarExp (head vs) exp : adds (tail vs) hh
    HVarExp _ exp  : hh ->  HVarExp (head vs) exp : adds (tail vs) hh
    HParVarExp _ exp : hh ->  HParVarExp (head vs) exp : adds (tail vs) hh
    _ -> []
  vars = case mexp of
    Just exp -> absIdents exp ++ newvars
    _ -> newvars
  newvars = [QIdent s |
             s <- ["x", "y", "z", "u", "v", "w"] ++ ["X"  ++ show i | i <- [1..11]]]
	 --- finite list so that filter works

-- strip abstraction when function type arguments are moved to hypos, as in Lean
stripAbs :: [Hypo] -> Exp -> Exp
stripAbs hypos exp = case (hypos, exp) of
  (h:hs, EAbs _ body) -> stripAbs hs body
  _ -> exp

-- get a list of idents from an abstraction expression
absIdents :: Exp -> [QIdent]
absIdents = map bind2ident . fst . splitAbs

bind2ident :: Bind -> QIdent
bind2ident bind = case bind of
  BVar var -> var
  BTyped var _ -> var

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
    (EIdent (QIdent n), [x]) | n == nd -> getDigit x -- (nd 6)
    (EIdent (QIdent n), [x, y]) | n == nn -> do  -- (nn 6 (nd 7))
      d <- getDigit x
      n <- uncurry getNumber (splitApp y)
      return (d ++ n)
    (EIdent (QIdent n), []) -> getDigit fun -- bare 6
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

hypo2vars :: Hypo -> [QIdent]
hypo2vars hypo = case hypo of
  HVarExp v _ -> [v]
  HParVarExp v _ -> [v]
  HLetExp v _ -> [v]
  HLetTyped v _ _ -> [v]
  HExp v -> []

hypo2type :: Hypo -> Maybe Exp
hypo2type hypo = case hypo of
  HVarExp v e -> Just e
  HParVarExp v e -> Just e
  HLetExp v _ -> Nothing
  HLetTyped v e _ -> Just e
  HExp e -> Just e

pattbindIdents :: [Pattbind] -> [QIdent]
pattbindIdents = concatMap bident where
  bident :: Pattbind -> [QIdent]
  bident pattbind = case pattbind of
    PBVar x -> [x]
    PBTyped x _ -> [x]

-- strip the qualifier part of an ident
stripQualifiers :: Tree a -> Tree a
stripQualifiers t = case t of
  QIdent c -> QIdent (stripQ c)
  _ -> composOp stripQualifiers t
 where
   stripQ c = case break (=='.') c of
     (_, _:x) -> x
     _ -> c

deduktiTokens :: String -> [String]
deduktiTokens = map prToken . myLexer