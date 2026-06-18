module Semantics where

import Informath
import PGF hiding (Tree)

import qualified Data.Map as M
import Data.List (permutations)

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


-------------------
-- Cooper storage
------------------

analyseT :: Tree a -> STM QEnv (Tree a)
analyseT tree = case tree of
  GQuantExp quant -> do
      env <- get
      put (next quant env)
      return (current env)
  _ -> composOpM analyseT tree


analysedT :: Tree a -> (Tree a, QEnv)
analysedT t = runSTM (analyseT t) initQEnv

storageResults :: GProp -> [GProp]
storageResults = results . analysedT where

  results :: (GProp, QEnv) -> [GProp] 
  results (prop, (_, quants)) = [foldr (\q p -> q p) prop prefs | prefs <- prefixes quants]

  prefixes :: [GQuant] -> [[GProp -> GProp]]
  prefixes quants = permutations (map mkPrefix (zip quants [0..]))

  mkPrefix :: (GQuant, Int) -> (GProp -> GProp)
  mkPrefix (quant, i) = case quant of
    GEveryKindQuant kind -> GCoreAllProp kind (newIdent i)
    GAllKindQuant kind -> GCoreAllProp kind (newIdent i)
    GSomeKindQuant kind -> GCoreExistProp kind (newIdent i)
    GIndefKindQuant kind -> GCoreExistProp kind (newIdent i)
    _ -> id ---- TODO: other quantifier prefixes


inSituResults :: Tree a -> [Tree a]
inSituResults t = case t of
  GAdj2Prop _ _ _ -> storageResults t
  GAdj3Prop _ _ _ _ -> storageResults t
  GVerb2Prop _ _ _ -> storageResults t
  GNoun2Prop _ _ _ -> storageResults t
  GAdv2Prop _ _ _ -> storageResults t
  GAxiomJmt label hypos prop -> [GAxiomJmt label hypos prop' | prop' <- inSituResults prop]
  ---- TODO more cases in inSituResults
  _ -> composOpM inSituResults t


-- STM vibe coded with Claude Code from original non-monadic in informath-experiments

type QEnv = (Int, [GQuant])
initQEnv = (0, [])

current (i, s) = GTermExp (GIdentTerm (newIdent i))
next s (i, ss) = (i + 1, s:ss)

newIdent i = GStrIdent (GString ("_x_" ++ show i))

-- A state monad polymorphic in the state s: a function s -> (a, s).
newtype STM s a = STM { runSTM :: s -> (a, s) }

instance Functor (STM s) where
  fmap f (STM g) = STM $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (STM s) where
  pure a = STM $ \s -> (a, s)
  STM f <*> STM g = STM $ \s ->
    let (h, s')  = f s
        (a, s'') = g s'
    in (h a, s'')

instance Monad (STM s) where
  return = pure
  STM g >>= f = STM $ \s -> let (a, s') = g s in runSTM (f a) s'

get :: STM s s
get = STM $ \s -> (s, s)

put :: s -> STM s ()
put s = STM $ \_ -> ((), s)

evalSTM :: STM s a -> s -> a
evalSTM m s = fst (runSTM m s)

execSTM :: STM s a -> s -> s
execSTM m s = snd (runSTM m s)


