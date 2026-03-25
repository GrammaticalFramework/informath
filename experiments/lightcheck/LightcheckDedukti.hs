{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------
-- Lightweight type checker and metavariable resolver for Dedukti
-- based on GF TC module from 2005/10/02 20:50:19
-- modified from Thierry Coquand's type checking algorithm
-- to return conatraints instead of False
-----------------------------------------------------------------

module Main where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti

import Dedukti.ParDedukti
import Dedukti.LexDedukti
import qualified Dedukti.ErrM as DE

---- import DeduktiOperations

import Control.Monad
import qualified Data.Map as M

import System.Environment (getArgs)

main = do
  xx <- getArgs -- filenames
  mo <- readDeduktiModule xx
  case checkModule mo of
    Left s -> putStrLn s
    Right (th, cs) -> mapM_ putStrLn [printConstraints c | c@(_:_, _) <- cs]




type Err a = Either String a

bad = Left
ok = Right

errIn m e = case e of
  Left s -> Left (m ++ s)
  _ -> e

-- values used in TC type checking

data Val =
    VGen Int
  | VApp Val Val
  | VType
  | VClos Env Exp
    deriving (Eq, Show)


prVal v = case v of
  VApp f a -> "(" ++ prVal f ++ " " ++ prVal a ++ ")"
  VClos [] exp -> printTree exp
  VClos env exp -> printTree exp ++ "{" ++ show env ++ "}"
  _ -> "(" ++ show v ++ ")"
  

type Constraint = (Val, Val)

printConstraints (msg, cs) = unwords (msg : ":" : [prVal v ++ " <> " ++ prVal w ++ " ; " | (v, w) <- cs])


type Binds = [(QIdent, Val)]
type Constraints = [Constraint]
type MetaSubst = [(Int, Val)]

type Theory = M.Map QIdent (Val, Val) -- type, value
type Context = [(QIdent, Val)] -- types
type Env = [(QIdent, Val)] -- values

emptyTheory = M.fromList [(QIdent "Type", (VClos [] typ, VClos [] typ))]
 where typ = EIdent (QIdent "Type")

lookupIdentType :: TCEnv -> QIdent -> Err Val
lookupIdentType tcenv c = do 
  case lookup c (context tcenv) of
    Just ty -> return ty
    _ -> case M.lookup c (theory tcenv) of
      Just (ty, _) -> return ty
      _ -> bad ("unknown type of identifier: " ++ show c)
  
lookupIdentValue :: TCEnv -> QIdent -> Err Val
lookupIdentValue tcenv c = do 
  case lookup c (environment tcenv) of
    Just v -> return v
    _ -> case M.lookup c (theory tcenv) of
      Just (_, v) -> return v
      _ -> bad ("unknown value of identifier: " ++ show c ++ " in " ++ show (environment tcenv))

data TCEnv = TCEnv {
  nextval :: Int,
  environment :: Env,
  context :: Context,
  theory  :: Theory
  }

updateEnv :: QIdent -> Val -> Val -> TCEnv -> TCEnv
updateEnv c v ty tcenv =
  tcenv{environment = (c, v):(environment tcenv), context = (c, ty):(context tcenv)}

updateNextval :: TCEnv -> TCEnv
updateNextval tcenv = tcenv{nextval = 1 + nextval tcenv}

emptyTCEnv :: TCEnv
emptyTCEnv = TCEnv 0 [] [] M.empty

whnf :: TCEnv -> Val -> Err Val
whnf tcenv v = case v of
  VApp u w -> do
    u' <- whnf tcenv u
    w' <- whnf tcenv w
    app tcenv u' w'
  VClos env e -> eval tcenv e
  _ -> return v

app :: TCEnv -> Val -> Val -> Err Val
app tcenv u v = case u of
  VClos env (EAbs bind e) ->
    eval tcenv{environment = (bind2var bind, v) : env} e
  _ -> return $ VApp u v


eval :: TCEnv -> Exp -> Err Val
eval tcenv e = case e of
  EIdent (QIdent "Type") -> return VType
  EIdent x -> lookupIdentValue tcenv x
  EApp f a -> do 
    f' <- eval tcenv f
    a' <- eval tcenv a
    app tcenv f' a'
{-
  ELet x typ_ df body -> do
    v <- eval tcenv df
    eval (updateEnv x v tcenv) body
-}
  _ -> return $ VClos (environment tcenv) e

-- invariant: constraints are in whnf
eqVal :: TCEnv -> Val -> Val -> Err [(Val, Val)]
eqVal tcenv u1 u2 = errIn ("eqVal: " ++ prVal u1 ++ " -- " ++ prVal u2) $ do
  w1 <- whnf tcenv u1
  w2 <- whnf tcenv u2
  case (w1, w2) of
    (VApp f1 a1, VApp f2 a2) -> do
      cs1 <- eqVal tcenv f1 f2
      cs2 <- eqVal tcenv a1 a2
      return (cs1 ++ cs2)
    (VClos env1 (EAbs bind1 e1), VClos env2 (EAbs bind2 e2)) -> do
      let v = VGen (nextval tcenv)
      let x1 = bind2var bind1
      let x2 = bind2var bind2
      let tcenv' = updateNextval tcenv
      eqVal tcenv' (VClos ((x1, v):env1) e1) (VClos ((x2, v):env2) e2)
    (VClos env1 (EFun h1 b1), VClos env2 (EFun h2 b2)) -> do
      let v = VGen (nextval tcenv)
      let x1 = hypo2var h1
      let a1 = hypo2type h1
      let x2 = hypo2var h2
      let a2 = hypo2type h2
      cs1 <- eqVal tcenv (VClos env1 a1) (VClos env2 a2)
      let tcenv' = updateNextval tcenv
      cs2 <- eqVal tcenv' (VClos ((x1, v):env1) b1) (VClos ((x2, v):env2) b2)
      return (cs1 ++ cs2)
    _ -> return [(w1, w2) | w1 /= w2]

{- bug:
./LightcheckDedukti ../../share/BaseConstants.dk

def divisible : Elem Int -> Elem Int -> Prop := n => m => exists Int (k => Eq n (times k m)) .:
  eqVal: Elem A{[(QIdent "m",VGen 1),(QIdent "n",VGen 0)]} -- Elem Num{[(QIdent "k",VGen 2),(QIdent "m",VGen 1),(QIdent "n",VGen 0)]}
  unknown value of identifier: QIdent "A" in [(QIdent "k",VGen 2),(QIdent "m",VGen 1),(QIdent "n",VGen 0)] :
-}

checkType :: TCEnv -> Exp -> Err (Exp, [(Val, Val)])
checkType tcenv e = checkExp tcenv e VType

checkExp :: TCEnv -> Exp -> Val -> Err (Exp, [(Val, Val)])
checkExp tcenv e ty = do
  case e of
  
    EAbs x_ n -> do
      typ <- whnf tcenv ty
      case typ of
        VClos env (EFun ya b) -> do
          let x = bind2var x_
          let y = hypo2var ya
          let a = hypo2type ya
          let v = VGen (nextval tcenv)
          let tcenv' = updateEnv x v (VClos env a) (updateNextval tcenv)
          (n', cs) <- checkExp tcenv' n (VClos ((y, v) : env) b)
          return (EAbs (BVar x) n', cs)
        _ -> bad ("function type expected for" ++ show e ++ " found " ++ show typ)

{-
    Let (x, (mb_typ, e1)) e2 -> do
      (val,e1,cs1) <- case mb_typ of
                        Just typ -> do (_,cs1) <- checkType th tenv typ
                                       val <- eval rho typ
                                       (e1,cs2) <- checkExp th tenv e1 val
                                       return (val,e1,cs1++cs2)
                        Nothing  -> do (e1,val,cs) <- inferExp th tenv e1
                                       return (val,e1,cs)
      (e2,cs2) <- checkExp th (k,rho,(x,val):gamma) e2 typ
      return (ALet (x,(val,e1)) e2, cs1++cs2)
-}

    EFun xa b -> do
      typ <- whnf tcenv ty
      case typ of
        VType -> do
          let x = hypo2var xa
          let a = hypo2type xa
          let v = VGen (nextval tcenv)
          (a', csa) <- checkType tcenv a
          let tcenv' = updateEnv x v (VClos (environment tcenv) a') (updateNextval tcenv)
          (b', csb) <- checkType tcenv' b
          return (EFun (HVarExp x a') b', csa ++ csb)
        _ -> bad ("Type expected")
    _ -> do
     (e', w, cs1) <- inferExp tcenv e
     cs2 <- eqVal tcenv w ty
     return (e', cs1 ++ cs2)

inferExp :: TCEnv -> Exp -> Err (Exp, Val, [(Val,Val)])
inferExp tcenv e = case e of
   EIdent c -> do
     ty <- lookupIdentType tcenv c
     return (e, ty, [])
   EApp e1 e2 -> do
    (f', w, csf) <- inferExp tcenv e1
    typ <- whnf tcenv w
    case typ of
      VClos env (EFun xa b) -> do
        let x = hypo2var xa
        let a = hypo2type xa
        (a', csa) <- checkExp tcenv e2 (VClos env a)
        let b' = VClos ((x, VClos (environment tcenv) e2) : env) b
        return $ (EApp f' a', b', csf ++ csa)
      _ -> bad ("function type expected for " ++ printTree e1 ++ " found " ++ prVal typ)
   _ -> bad ("cannot infer type of " ++ show e)


checkJmt :: TCEnv -> Jmt -> Err ((QIdent, (Val, Val)), [(Val, Val)]) -- type, value, constrs
checkJmt tcenv jmt = case jmt of
  JDef c mtyp mexp -> case mtyp of
    MTExp typ -> do
      (typ', cst) <- checkType tcenv typ
      case mexp of
        MEExp exp -> do
          (exp', cse) <- checkExp tcenv exp (VClos [] typ')
          return ((c, (VClos [] typ', VClos [] exp')), cst ++ cse)
        _ -> return ((c, (VClos [] typ', VClos [] (EIdent c))), cst)
    _ -> case mexp of
      MEExp exp -> do
        (exp', typv, cse) <- inferExp tcenv exp
        return ((c, (typv, VClos [] exp')), cse)
  JStatic c typ -> checkJmt tcenv (JDef c (MTExp typ) MENone)
  JThm c typ exp -> checkJmt tcenv (JDef c typ exp)
  JInj c typ exp -> checkJmt tcenv (JDef c typ exp)
  _ -> return ((QIdent "NONE", (VGen 0, VGen 0)), []) --- no checking of rules


checkModule :: Module -> Err (Theory, [(String, [(Val, Val)])])
checkModule (MJmts jmts) = checkJmts emptyTheory jmts where
  checkJmts th jmts = case jmts of
    jmt : js -> case checkJmt emptyTCEnv{theory = th} jmt of
       Right ((c, tyval), cs) -> do 
         let msg = if null cs then "" else (printTree jmt)
         let th' = M.insert c tyval th
         (th'', css) <- checkJmts th' js
         return (th'', (msg, cs) : css)
       Left s -> do
         let msg = printTree jmt ++ ": " ++ s
         (th'', css) <- checkJmts th js
         return (th'', (msg, []) : css)
    _ -> return (th, [])


-------- to be imported

bind2var :: Bind -> QIdent
bind2var bind = case bind of
  BVar v -> v
  BTyped v _ -> v

hypo2var :: Hypo -> QIdent
hypo2var hypo = case hypo of
  HVarExp x _ -> x
  HParVarExp x _ -> x
  HExp _ -> QIdent "_h" --- should not matter

hypo2type :: Hypo -> Exp
hypo2type hypo = case hypo of
  HVarExp _ t -> t
  HParVarExp _ t -> t
  HExp t -> t

readDeduktiModule :: [FilePath] -> IO Module
readDeduktiModule files = mapM readFile files >>= return . parseDeduktiModule . unlines

-- | To parse a Dedukti file into its AST.
parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  DE.Bad e -> error ("parse error: " ++ e)
  DE.Ok mo -> mo

