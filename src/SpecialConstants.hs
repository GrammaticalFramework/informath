module SpecialConstants where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import DeduktiOperations
import Informath

-- special constants that don't belong to lexical categories

specialDeduktiConstants :: [QIdent]
specialDeduktiConstants = map QIdent [
  "sigma", "series", "integral", "enumSet"
  ]

specialDedukti2Informath :: (Bind -> GIdent) -> (Exp -> GExp) -> Exp -> Maybe GExp
specialDedukti2Informath bident egexp exp = case exp of
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



--- this should be on a more general level

gExps :: [GExp] -> GExps
gExps exps = case exps of
  [exp] -> GOneExps exp
  _ -> GManyExps (GListExp exps)
