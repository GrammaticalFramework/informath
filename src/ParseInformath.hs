module ParseInformath where

import Environment
import CommonConcepts (verbalCats)
import PGF
import Data.Char (isAlpha, isAlphaNum)
import Data.List (sortOn)
import qualified Data.Set as Set

max_number = 1999 -- number of trees considered with checkVariables
max_number_taken = 19 -- number of trees considered for semantics
max_number_unindexed = 50 -- number of combinations of parsed $...$ parts considered

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
         let trees = sortOn treeDepth [t | t <- take max_number ps, notspur t, checkVariables env t]
         in
         if not (null trees)
         then (Just (take max_number_taken trees), "# SUCCESS " ++ show (length trees))
         else (Just [], "# FAILURE SPURIOUS OR VARCHECK")
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
       (_, c, _) | Set.member (showCId c) verbalCats -> Set.member f (reachableFunctions env) || ("noLabel" == showCId f)
       _ -> True
     _ -> True
   pgf = grammar env

-- quick hack to get the effect of a callback: check that variables are a(a|d|_|'|\)*
-- and don't in particular overshadow digits

checkVariables :: Env -> Expr -> Bool
checkVariables env expr = case unApp expr of
  Just (f, [x]) | showCId f == "StrIdent" -> case showExpr [] x of
    c -> trac env "IDENT? " (isIdent (tracs env c (init (tail c))))
  -- a user macro: \name, and not a symbol of the grammar itself such as \times
  Just (f, [x]) | showCId f == "StringMacro" -> case unStr x of
    Just m -> trac env "MACRO? " (isMacroName m && null (lookupMorpho (morpho env) m))
    _ -> False
  Just (_, args) -> all (checkVariables env) args
  _ -> True
 where
  isIdent (c:cs) = isAlpha c && all isAlphaNum cs
  isMacroName s = case s of
    '\\':cs@(_:_) -> all isAlpha cs
    _ -> False


-- all the trees obtained by parsing the indexed $...$ parts, in all combinations
-- (e.g. $a \times b$ as both times and cartesian); the tree itself if none parses
unindexGFTrees :: Env -> [String] -> Expr -> [Expr]
unindexGFTrees env termindex expr = case take max_number_unindexed (unind expr) of
  [] -> [expr]
  ts -> [tracs env ("FOUND " ++ showExpr [] t) t | t <- ts]
 where
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
      (Just ts, _) -> [tracs env ("PARSED " ++ showExpr [] t) t | t <- ts]
      _ -> []

-- the first of them, as before
unindexGFTree :: Env -> [String] -> Expr -> Expr
unindexGFTree env termindex = head . unindexGFTrees env termindex

treeLength :: Expr -> Int
treeLength t = case unApp t of
  Just (_, ts@(_:_)) -> 1 + sum (map treeLength ts)
  _ -> 1

treeDepth :: Expr -> Int
treeDepth t = case unApp t of
  Just (_, ts@(_:_)) -> 1 + maximum (map treeDepth ts)
  _ -> 1

