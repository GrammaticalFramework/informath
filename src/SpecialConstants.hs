{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module SpecialConstants where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import DeduktiOperations
import CommonConcepts
import Informath
import BuildConstantTable (mainCats)

import PGF
import qualified Data.Map as M
import qualified Data.Set as S

-- special constants that don't belong to lexical categories

---- TODO: apply to all conversions, not just Dedukti2MathCore

lambdaFlatten :: Exp -> (Exp, [Either Bind Exp])
lambdaFlatten exp = case splitApp exp of
  (f@(EIdent _), xs) -> (f, concatMap flatten xs)
  _ -> (exp, [])
 where
   flatten :: Exp -> [Either Bind Exp] 
   flatten exp = case splitAbs exp of
     (xs, body) -> map Left xs ++ [Right body]

data CallBacks =  CallBacks {
  callBind :: Bind -> Expr,
  callIdent :: Exp -> Expr,
  callExp  :: Exp -> Expr,
  callKind  :: Exp -> Expr,
  callProp  :: Exp -> Expr,
  callProof :: Exp -> Expr
  }

convertArg ::  CallBacks -> (String, Either Bind Exp) -> Expr
convertArg callbacks arg = case arg of
  (_, Left bind) -> callBind callbacks bind
  ("Exp", Right exp) -> callExp callbacks exp
  ("Kind", Right exp) -> callKind callbacks exp
  ("Prop", Right exp) -> callProp callbacks exp
  ("Proof", Right exp) -> callProof callbacks exp
  ("Ident", Right exp) -> callIdent callbacks exp
  (_, Right exp) -> callExp callbacks exp ---- ??

specialDedukti2Informath :: CallBacks -> Exp -> Maybe Expr
specialDedukti2Informath callbacks exp = case lambdaFlatten exp of
  (EIdent (QIdent s), args) -> case lookupConstantFull s of
    Just (cat, fun, argcats) | S.member (mkCId cat) mainCats ->
      return $ mkApp (mkCId fun) (map (convertArg callbacks) (zip argcats args))
    _ -> Nothing
  _ -> Nothing

--- this should be on a more general level

gExps :: [GExp] -> GExps
gExps exps = case exps of
  [exp] -> GOneExps exp
  _ -> GManyExps (GListExp exps)
