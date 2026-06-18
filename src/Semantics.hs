module Semantics where

import Informath
import PGF hiding (Tree)

import qualified Data.Map as M

type SemDefs = M.Map CId ([Expr] -> Expr)

appSemDefs :: Gf a => SemDefs -> a -> a
appSemDefs defs = fg . applySemDefs defs . gf


applySemDefs :: SemDefs -> Expr -> Expr
applySemDefs defs exp = case unApp exp of
  Just (fun, args) ->
    let args' = map (applySemDefs defs) args in case M.lookup fun defs of
          Just fun' -> fun' args'
          _ -> mkApp fun args'
  _ -> exp


mkSemDef :: Expr -> Expr -> (CId, [Expr] -> Expr)
mkSemDef a b = case unApp a of
  Just (fun, args) -> case mapM getVar args of
    Just xs -> (fun, mkFun xs b)
    _ -> error ("not a valid function definition " ++ showExpr [] a)
  _ -> error ("not a valid function definition " ++ showExpr [] a)
 where
  getVar x = case unApp x of
    Just (y, []) -> return y
    _ -> Nothing
  mkFun xs exp = \vars -> subst [(x, vars !! i) | (x, i) <- zip xs [0..]] exp
  subst vs exp = case unApp exp of
    Just (x, []) -> case lookup x vs of
      Just e -> e
      _ -> exp
    Just (f, args) -> mkApp f (map (subst vs) args)
    _ -> exp


readSemDef :: String -> (CId, [Expr] -> Expr)
readSemDef s = case break (=='=') s of
  (a, _:b) -> case (readExpr a, readExpr b) of
    (Just a', Just b') -> mkSemDef a' b'
    _ -> error ("cannot read semantic definition " ++ s)
  _ -> error ("cannot parse semantic definition " ++ s)


-- the opposite direction: NLG defs, returning lists of variants

type NLGDefs = M.Map CId [[Expr] -> Expr]


appNLGDefs :: Gf a => NLGDefs -> a -> [a]
appNLGDefs defs = map fg . applyNLGDefs defs . gf


applyNLGDefs :: NLGDefs -> Expr -> [Expr]
applyNLGDefs defs exp = case unApp exp of
  Just (fun, args) ->
    let argss' = sequence (map (applyNLGDefs defs) args) in case M.lookup fun defs of
          Just funs' -> [fun' args' | fun' <- funs', args' <- argss']
          _ -> [mkApp fun args' | args' <- argss']
  _ -> [exp]

