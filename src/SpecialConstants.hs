{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module SpecialConstants where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import DeduktiOperations
import Informath

import PGF
import qualified Data.Map as M

-- special constants that don't belong to lexical categories

---- TODO: read constant table from dkgf
---- TODO: apply to all conversions, not just Dedukti2MathCore


specialDeduktiConstants :: M.Map QIdent CId
specialDeduktiConstants = M.fromList [
  (QIdent "sigma", mkCId "SigmaExp"),
  (QIdent "series", mkCId "SeriesExp"),
  (QIdent "integral", mkCId "IntegralExp")
  ]

type DkTree a = Dedukti.AbsDedukti.Tree a


lambdaFlatten :: Exp -> (Exp, [Either Bind Exp])
lambdaFlatten exp = case splitApp exp of
  (f@(EIdent _), xs) -> (f, concatMap flatten xs)
  _ -> (exp, [])
 where
   flatten :: Exp -> [Either Bind Exp] 
   flatten exp = case splitAbs exp of
     (xs, body) -> map Left xs ++ [Right body]

convertArg ::  (Bind -> Expr) -> (Exp -> Expr) -> Either Bind Exp -> Expr
convertArg bident egexp arg = case arg of
  Left bind -> bident bind
  Right exp -> egexp exp


specialDedukti2Informath :: (Bind -> Expr) -> (Exp -> Expr) -> Exp -> Maybe GExp
specialDedukti2Informath bident egexp exp = case lambdaFlatten exp of
  (EIdent fun, args) -> case M.lookup fun specialDeduktiConstants of
    Just gffun -> return $ fg $ mkApp gffun (map (convertArg bident egexp) args)
    _ -> Nothing
  _ -> Nothing


{-
specialDedukti2InformathOld :: (Bind -> GIdent) -> (Exp -> GExp) -> Exp -> Maybe GExp
specialDedukti2InformathOld bident egexp exp = case exp of
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent (QIdent "sigma") | length args == 3 ->
        let [m, n, EAbs b f] = args
        in return $ GSigmaExp (bident b) (egexp m) (egexp n) (egexp f)  
      EIdent (QIdent "series") | length args == 2 ->
        let [m, EAbs b f] = args
        in return $ GSeriesExp (bident b) (egexp m) (egexp f)  
      EIdent (QIdent "integral") | length args == 3 ->
        let [m, n, EAbs b f] = args
        in return $ GIntegralExp (bident b) (egexp m) (egexp n) (egexp f)  
      EIdent (QIdent "enumset") | length args == 1 -> case enum2list (head args) of
        Just exps@(_:_) -> return $ GEnumSetExp (gExps (map egexp exps))
	Just [] -> return $ GNameExp (LexName "emptyset_Name")
	_ -> return $ GAppExp (egexp fun) (gExps (map egexp args))
      _ -> Nothing
  _ -> Nothing

-}


--- this should be on a more general level

gExps :: [GExp] -> GExps
gExps exps = case exps of
  [exp] -> GOneExps exp
  _ -> GManyExps (GListExp exps)
