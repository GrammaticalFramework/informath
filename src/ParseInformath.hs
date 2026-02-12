module ParseInformath where

import PGF
import Data.Char(isAlpha, isAlphaNum)
import qualified Data.Map

main_pgf = "grammars/Informath.pgf"
max_number = 19999 -- number of trees considered with checkVariables
max_number_taken = 3 -- number of trees considered for semantics

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


-- quick hack to get the effect of a callback: check that variables are a(a|d|_|'|\)*
-- and don't in particular overshadow digits

checkVariables :: Expr -> Bool
checkVariables expr = case unApp expr of
  Just (f, [x]) | showCId f == "StrIdent" -> case showExpr [] x of
    c | isIdent (init (tail c)) -> True --- notElem c "CNQRZ"
    _ -> False
  Just (_, args) -> all checkVariables args
  _ -> True
 where
  isIdent s@(c:cs) = isAlpha c && all (\x -> isAlphaNum x || elem x "\\_'") cs


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
