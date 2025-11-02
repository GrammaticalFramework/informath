module ParseInformath where

import PGF
import Data.Char(isAlpha)
import qualified Data.Map

main_pgf = "grammars/Informath.pgf"
max_number = 19999 -- number of trees considered with checkVariables
max_number_taken = 3 -- number of trees considered for semantics

-- quick stand-alone test: runghc ParseInformath.hs <test/gflean-data.txt

-- this is the function to be exported to other modules

parseJmt :: PGF -> Language -> Type -> String -> (Maybe [Expr], String)
parseJmt gr eng cat s =
  case fst (parse_ gr eng cat (Just 4) s) of  --- Just 4 is default in PGF.parse
    ParseOk ps -> 
         let trees = [t | t <- take max_number ps, checkVariables t]
         in
	 if not (null trees)
            then (Just (take max_number_taken trees), "# SUCCESS " ++ show (length trees))
            else (Just [], "# FAILURE VARCHECK")
    ParseFailed pos -> 
         (Nothing, "# FAILURE AT " ++ show pos)
    ParseIncomplete -> 
         (Nothing, "# FAILURE INCOMPLETE")


-- quick hack to get the effect of a callback: check that variables consist of one letter
-- and that numbers don't overshadow Dedukti digits

checkVariables :: Expr -> Bool
checkVariables expr = case unApp expr of
  -- eliminate parenthesis versions only used in MathCore, to prevent spurious ambiguity
  Just (f, _) | elem (showCId f) ["AndProp", "OrProp", "IfProp", "IffProp"] -> False
  Just (f, [x]) | showCId f == "StrIdent" -> case showExpr [] x of
    [_,c,_] | isAlpha c -> notElem c "CNQRZ"
    _ -> False
  Just (_, args) -> all checkVariables args
  _ -> True


unindexGFTree :: PGF -> Language -> [String] -> Expr -> Expr
unindexGFTree pgf lang termindex expr = maybe expr id (unind  expr) where
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
  parsed c s = do
    cat <- readType c
    let (mts, msg) = parseJmt pgf lang cat s
    case mts of
      Just (t:ts) -> return t ---- todo: ambiguity if ts
      _ -> Nothing
