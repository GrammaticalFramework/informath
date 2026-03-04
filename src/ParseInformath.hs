module ParseInformath where

import Environment
import CommonConcepts (verbalCats)
import PGF
import Data.Char(isAlpha, isAlphaNum)
import Data.List(sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set

main_pgf = "grammars/Informath.pgf"
max_number = 199 -- number of trees considered with checkVariables
max_number_taken = 3 -- number of trees considered for semantics

-- these are the functions to be exported to other modules

parseJmt :: Env -> Type -> String -> (Maybe [Expr], String)
parseJmt env cat s =
  let
     gr = grammar env
     eng = fromLang env
     notspur =  if isFlag "-include-unreachable" env then (const True) else (not . isSpurious env)
  in
  case (fst (parse_ gr eng cat (Just 4) s)) of  --- Just 4 is default in PGF.parse
    ParseOk ps -> 
         let trees = sortOn treeDepth
	       [t | t <- take max_number ps, notspur t, checkVariables env t]
         in
	 if not (null trees)
            then (Just (take max_number_taken trees), "# SUCCESS " ++ show (length trees))
            else (Just [], "# FAILURE VARCHECK")
    ParseFailed pos -> 
         (Nothing, "# FAILURE AT " ++ show pos)
    ParseIncomplete -> 
         (Nothing, "# FAILURE INCOMPLETE")

---------------

-- spurious tree: containing lexical functions not mapped to/from Dedukti
isSpurious :: Env -> Expr -> Bool
isSpurious env expr = case unApp expr of
  Just (f, []) -> not (reachable f)
  Just (_, xs) -> any (isSpurious env) xs
  _ -> False
 where
   reachable f = case functionType pgf f of
     Just ty -> case unType ty of
       (_, c, _) | Set.member (showCId c) verbalCats -> Set.member f (reachableFunctions env)
       _ -> True
     _ -> True
   pgf = grammar env

-- quick hack to get the effect of a callback: check that variables are a(a|d|_|'|\)*
-- and don't in particular overshadow digits

checkVariables :: Env -> Expr -> Bool
checkVariables env expr = case unApp expr of
  Just (f, [x]) | showCId f == "StrIdent" -> case showExpr [] x of
    c -> trac env "IDENT? " (isIdent (tracs env c (init (tail c))))
  Just (f, [x]) | showCId f == "StringMacro" -> case showExpr [] x of
    c -> trac env "MACRO? " (isMacro (tracs env c (init (tail c))))
  Just (_, args) -> all (checkVariables env) args
  _ -> True
 where
  isIdent s@(c:cs) = isAlpha c && all isAlphaNum cs
  isMacro s = case s of
    '\\':'\\':cs -> {- isFlag "-parseusermacros" env && -} all isAlpha cs
    _ -> False


unindexGFTree :: Env -> [String] -> Expr -> Expr
unindexGFTree env termindex expr = case unind expr of
  t:_ -> tracs env ("FOUND " ++ showExpr [] t) t
  _ -> expr
 where
  pgf = grammar env
  lang = fromLang env
  unind expr = case unApp expr of
    Just (f, [x]) -> case unInt x of
      Just i -> case showCId f of
        "IndexedTermExp" -> parsed "Exp" (look i)
        "IndexedFormulaProp" -> parsed "Prop" (look i)
        "IndexedLetFormulaHypo" -> do
	   formula <- parsed "Formula" (filter (/='$') (look i))
	   return $ mkApp (mkCId "LetFormulaHypo") [formula]
        "IndexedDeclarationArgKind" -> do
	   declaration <- parsed "Declaration" (filter (/='$') (look i))
	   return $ mkApp (mkCId "DeclarationArgKind") [declaration]
        _ -> return expr
      _ -> do
        ux <- unind x
        return $ mkApp f [ux]
    Just (f, xs) -> do
       uxs <- mapM unind xs
       return $ mkApp f uxs
    _ -> return expr

  look i = termindex !! i

  mkTyp c = mkType [] (mkCId c) []

  parsed c s = case parseJmt env (mkTyp c) s of
      (Just (t:ts), _) -> return (tracs env ("PARSED " ++ showExpr [] t) t) ---- todo: ambiguity if ts
      _ -> []

treeLength :: Expr -> Int
treeLength t = case unApp t of
  Just (f, ts@(_:_)) -> 1 + sum (map treeLength ts)
  _ -> 1

treeDepth :: Expr -> Int
treeDepth t = case unApp t of
  Just (f, ts@(_:_)) -> 1 + maximum (map treeDepth ts)
  _ -> 1

