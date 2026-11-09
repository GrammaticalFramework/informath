{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module AnnotateDedukti where

import BuildConstantTable
import Dedukti.AbsDedukti
import DeduktiOperations
import DeduktiTheoryAPI
import CommonConcepts (lookupConstant, symbolicCats, verbalCats, mainCats)
import Environment

import PGF hiding (Hypo)

import Data.List (intersperse, sortOn)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Monoid (Sum(..))
import Control.Monad.Writer (WriterT(..), tell, censor, lift)
import qualified Data.Map as M
import qualified Data.Set as S


type DkTree a = Dedukti.AbsDedukti.Tree a

-- annotate Dk idents with cats and funs ; used only internally
-- a tree with no annotation at all, as a verbal constant is in an argument
-- of a symbolic one (\FinsetProd with the disjoint union of s and t), is
-- annotated again with such constants shown as themselves, \mathrm{c}(x)
annotateDkIdents :: Env -> DkTree a -> [DkTree a]
annotateDkIdents env t0 = case annotateDkIdentsFb False env t of
  [] -> annotateDkIdentsFb True env t
  ts -> ts
 where
  t = hideArgs (symbolTable env) t0

-- | The arguments that readings do not show: the coercions read as the
-- argument they coerce (#COERCION), unless that makes the two sides of an
-- equation or equivalence the same, as in ↑(m - n) = ↑m - ↑n, whose content
-- is the coercion; and the implicit arguments of constants (#HIDE)
hideArgs :: SymbolTable -> DkTree a -> DkTree a
hideArgs st = hide . uncoerce
 where
  hides = hideTable st
  coes = coercionTable st

  hide :: forall a. DkTree a -> DkTree a
  hide t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent c, args) | Just is <- M.lookup c hides ->
        foldl EApp (EIdent c) [hide a | (i, a) <- zip [1..] args, notElem i is]
      (f, args) -> foldl EApp (hide f) (map hide args)
    _ -> runIdentity (composOpM (Identity . hide) t)

  uncoerce :: forall a. DkTree a -> DkTree a
  uncoerce t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent c, args)
        | Just k <- M.lookup c coes, length args >= k ->
            foldl EApp (uncoerce (args !! (k-1))) (map uncoerce (drop k args))
        | isEquation c, length args >= 2 ->
            let (rest, [a, b]) = splitAt (length args - 2) args
                (a', b') = (uncoerce a, uncoerce b)
            in if hide a' == hide b' && hide a /= hide b
               then foldl EApp (EIdent c) (map uncoerce rest ++ [a, b])
               else foldl EApp (EIdent c) (map uncoerce rest ++ [a', b'])
      (f, args) -> foldl EApp (uncoerce f) (map uncoerce args)
    _ -> runIdentity (composOpM (Identity . uncoerce) t)

  isEquation :: QIdent -> Bool
  isEquation (QIdent c) = elem c ["Eq", "Eqt", "iff", "Equ"]

annotateDkIdentsFb :: Bool -> Env -> DkTree a -> [DkTree a]
annotateDkIdentsFb fallback env =
  if force_symbolic || symbolic_latex
  then annot []
  else checkSymbolics . annot []
  

 where
  table = constantTable (symbolTable env)
  drops = dropTable (symbolTable env)
  msyns = ifvar $ argValueMaybeInt "-synonyms" (flags env)
  msymbs = ifvar $ argValueMaybeInt "-symbolics" (flags env)
  ifvar mi = if (isFlag "-variations" env) || (isFlag "-more-variants" env) then mi else Just 1
  force_symbolic = isFlag "-force-symbolic" env
  symbolic_latex = isFlag "-to-symbolic-latex" env

  -- don't annotate bound variables: they override constants
  annot :: forall a. [QIdent] -> DkTree a -> [DkTree a]
  annot bounds = map fst . runWriterT . annotV annotId bounds

  -- each alternative comes with its verbality, the number of verbal constants
  -- in it, counted as it is built, as verbality would count them
  -- ai gives the alternatives of a constant: annotId, or symbolicLatexId's
  -- in an argument of a symbolic head that has no nonverbal alternative
  annotV :: forall a. (QIdent -> [(QIdent, Profile)]) -> [QIdent] -> DkTree a -> Annots (DkTree a)
  annotV ai bounds t = WriterT (spread (runWriterT (annotV0 ai bounds t)))

  annotV0 :: forall a. (QIdent -> [(QIdent, Profile)]) -> [QIdent] -> DkTree a -> Annots (DkTree a)
  annotV0 ai bounds t = case t of
    EApp fun arg -> case splitApp t of
      (EIdent c, args) | notElem c bounds -> do
        (f, p) <- lift (ai c)
        case p of
          -- HOAS profiles take arguments apart, so they are annotated first,
          -- and the result is counted afresh, as some arguments may be dropped
          HoasProfile _ -> do
            aargs <- censor (const 0) (mapM (annotV ai bounds) args)
            let r = appProfile p (foldl EApp (EIdent f) aargs)
            tell (Sum (verbality r))
            return r
          -- otherwise only the arguments the profile keeps are annotated,
          -- and under a symbolic head only with nonverbal alternatives
          _ -> case splitApp (appProfile p (foldl EApp (EIdent f) args)) of
            (hd, kept) -> do
              tell (verbalIdent f)
              aargs <- mapM (prune ai bounds f) kept
              return (foldl EApp hd aargs)
      _ -> EApp <$> annotV ai bounds fun <*> annotV ai bounds arg
    EIdent c -> do
      c2 <- annotV ai bounds c
      tell (verbalIdent c2)
      return (EIdent c2)
    -- a pattern in a rewrite rule shows the arguments an expression shows
    PApp _ _ -> case splitPatt t of
      (PVar c, args) | notElem c bounds -> do
        (f, p) <- lift (ai c)
        aargs <- mapM (annotV ai bounds) args
        return (appPattProfile p (foldl PApp (PVar f) aargs))
      _ -> composOpM (annotV ai bounds) t
    QIdent _ | notElem t bounds -> lift (map fst (ai t))
    EAbs b exp -> EAbs <$> censor (const 0) (annotV ai bounds b)
                       <*> annotV ai (bind2ident b : bounds) exp
    EFun h exp -> EFun <$> annotV ai bounds h <*> annotV ai (hypo2topvars h ++ bounds) exp
    BVar _ -> return t
    BTyped v ty -> BTyped v <$> annotV ai bounds ty
    HVarExp v ty -> HVarExp v <$> annotV ai bounds ty
    HParVarExp v ty -> HParVarExp v <$> annotV ai bounds ty
    HLetExp v ty -> HLetExp v <$> annotV ai bounds ty
    HLetTyped v ty exp -> HLetTyped v <$> annotV ai bounds ty <*> annotV ai bounds exp
    -- the name of a judgement counts as verbality counts it
    JStatic _ _ -> named
    JDef _ _ _ -> named
    JThm _ _ _ -> named
    JInj _ _ _ -> named
    _ -> composOpM (annotV ai bounds) t
   where
     named = do
       r <- composOpM (annotV ai bounds) t
       tell (verbalIdent (jmtName r))
       return r
     jmtName :: DkTree b -> QIdent
     jmtName j = case j of
       JStatic c _ -> c
       JDef c _ _ -> c
       JThm c _ _ -> c
       JInj c _ _ -> c
       _ -> QIdent ""

  -- the alternatives of an argument of f that checkSymbolics would accept;
  -- in the fallback, if it has none, as a constant in it has only verbal
  -- entries, the ones of symbolic LaTeX, where such a constant is shown as
  -- itself, so that one verbal constant does not take the whole formula with it
  prune :: (QIdent -> [(QIdent, Profile)]) -> [QIdent] -> QIdent -> Exp -> Annots Exp
  prune ai bounds (QIdent f) a
    | force_symbolic || symbolic_latex = us
    | otherwise = case lookupConstant f of
        Just (cat, _) | S.member cat symbolicCats ->
          case filter ((== 0) . snd) (runWriterT us) of
            [] | fallback -> WriterT (filter ((== 0) . snd) (runWriterT (annotV rawId bounds a)))
            vs -> WriterT vs
        _ -> us
   where
     us = annotV ai bounds a

  rawId c = case M.lookup c table of
    Just entry -> symbolicLatexId c entry
    _ -> [(c, NoProfile)]

  verbalIdent :: QIdent -> Sum Int
  verbalIdent (QIdent k) = case lookupConstant k of
    Just (kat, _) | S.member kat verbalCats -> 1
    _ -> 0

  -- the number of verbal constants in an annotated tree
  verbality :: DkTree a -> Int
  verbality u = length [k | QIdent k <- identsInTree u,
                            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats]

  -- the alternatives of a subtree multiply up the tree, so a subtree keeps
  -- at most maxAnnots of them (0 = all): the most symbolic, the most verbal
  -- and ones spread evenly between them; -variations keeps all by default
  maxAnnots = case argValueMaybeInt "-max-annotations" (flags env) of
    Just k -> k
    _ | isFlag "-variations" env || isFlag "-more-variants" env -> 0
    _ -> defaultMaxAnnotations

  spread :: [(DkTree a, Sum Int)] -> [(DkTree a, Sum Int)]
  spread us
    | maxAnnots <= 0 || null (drop maxAnnots us) = us
    | maxAnnots == 1 = take 1 (sortOn snd us)
    | otherwise =
        let vs = sortOn snd us
            n = length vs
            picks = S.fromList [(i * (n-1)) `div` (maxAnnots-1) | i <- [0 .. maxAnnots-1]]
        in [u | (j, u) <- zip [0..] vs, S.member j picks]

  tkSyns = maybe id take msyns
  tkSymbs = maybe id take msymbs

  annotId c = case M.lookup c table of
    Just entry | symbolic_latex -> symbolicLatexId c entry
    Just entry -> [annotIdent c (maybe 0 id (M.lookup c drops)) fpt |
                       let symbs = tkSymbs (symbolics entry),
                       fpt <- symbs ++
		              if not (null symbs) && force_symbolic
			      then []
			      else tkSyns (primary entry : synonyms entry)
                       ]
    _ -> [(c, NoProfile)]

  -- in symbolic LaTeX, a constant is shown with its symbolic notation, or
  -- else with its core function (such as and, forall); failing both, it is
  -- left as it is, to be shown as \mathrm{c}, with all of its arguments but
  -- the ones #DROP hides: the verbal entries may leave out arguments that
  -- their wording implies, which a formula must show
  symbolicLatexId c entry = case tkSymbs (symbolics entry) of
    symbs@(_:_) -> [annotIdent c ndrop fpt | fpt <- symbs]
    _ -> case primary entry of
      fpt@(_, typ) | S.member (showCId (valCat typ)) mainCats -> [annotIdent c ndrop fpt]
      _ -> [(c, maybe NoProfile DropProfile (M.lookup c drops))]
   where
     ndrop = maybe 0 id (M.lookup c drops)

  checkSymbolics :: [DkTree a] -> [DkTree a]
  checkSymbolics ts = [t | t <- ts, not (badSymb t)]

  -- bad symbolics are subtrees with symbolic root and at least one verbal subtree
  badSymb :: DkTree a -> Bool
  badSymb t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent (QIdent c), ts) -> case lookupConstant c of
        Just (cat, _) | S.member cat symbolicCats ->
          not (null
           [u | u <- ts, not (null [k | QIdent k <- identsInTree u,
            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats])])
        _  -> any badSymb ts
      (f, ts) -> any badSymb (f : ts)
    EAbs b exp -> badSymb b || badSymb exp
    EFun h exp -> badSymb h || badSymb exp
    BTyped _ ty -> badSymb ty
    HVarExp _ ty -> badSymb ty
    HParVarExp _ ty -> badSymb ty
    HLetExp _ ty -> badSymb ty
    HLetTyped _ ty exp -> badSymb ty || badSymb exp
    JDef _ mt me -> badSymb mt || badSymb me
    JStatic _ ty -> badSymb ty
    JThm _ mt me -> badSymb mt || badSymb me
    JInj _ mt me -> badSymb mt || badSymb me
    JRules rules -> any badSymb rules
    RRule pattbinds patt exp -> any badSymb pattbinds || badSymb patt || badSymb exp
    MTExp exp -> badSymb exp
    MEExp exp -> badSymb exp
    ---- TODO patt
    _ -> False


  badSymbolics :: DkTree a -> [DkTree a]
  badSymbolics t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent (QIdent c), ts) -> case lookupConstant c of
        Just (cat, _) | S.member cat symbolicCats ->
          [u | u <- ts, not (null [k | QIdent k <- identsInTree u,
            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats])]
        _  -> concatMap badSymbolics ts
      (f, ts) -> concatMap badSymbolics (f : ts)
    _ -> composOpM badSymbolics t

-- alternative annotations, each with its verbality
type Annots a = WriterT (Sum Int) [] a

-- the default of -max-annotations
defaultMaxAnnotations :: Int
defaultMaxAnnotations = 256

annotIdent :: QIdent -> Int -> (FunProfile, Type) -> (QIdent, Profile)
annotIdent (QIdent s) d ((f, p), t) =
  (QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t) ++ [showProfile p], p)
    where
      dk c = showCId c
      dkp f = showGFTree f

harmonizeJmt :: Jmt -> Jmt
harmonizeJmt jmt = case jmt of
  JDef ident (MTExp typ) meexp@(MEExp exp) ->
    let
       (hypos, kind) = splitType typ
       ahypos = map proofFunHypo (unifyVars meexp hypos)
       hvars = concatMap hypo2vars ahypos
       dexp = etaAppExp exp hvars
    in JDef ident (MTExp (foldr EFun kind ahypos)) (MEExp dexp)
  JDef ident (MTExp typ) MENone ->
    let
       (hypos, kind) = splitType typ
       ahypos = map proofFunHypo (unifyVars MENone hypos)
    in JDef ident (MTExp (foldr EFun kind ahypos)) MENone
  JStatic ident typ -> harmonizeJmt (JDef ident (MTExp typ) MENone)
  JThm ident mtyp mexp -> harmonizeJmt (JDef ident mtyp mexp)
  JInj ident mtyp mexp -> harmonizeJmt (JDef ident mtyp mexp)
  _ -> jmt

-- A hypothesis that is a function into proofs, such as
--   d : (A : Elem C) -> (B : Elem C) -> Proof P -> Proof Q -> Proof R
-- (a derived rule taken as a premiss) is read as a proof of the proposition
-- it amounts to,
--   d : Proof (forall C (A => forall C (B => if (and P Q) R))),
-- so that it comes out as "assume that for all A and B in C, if P and Q,
-- then R" and not as a function to "elements of Proof applied to R".
-- This is done before annotation, so that the logical constants introduced
-- get their symbol table entries like the ones written in the source.
proofFunHypo :: Hypo -> Hypo
proofFunHypo h = case h of
  HVarExp x ty | Just p <- proofFun2prop ty -> HVarExp x (proofOf p)
  HParVarExp x ty | Just p <- proofFun2prop ty -> HParVarExp x (proofOf p)
  HExp ty | Just p <- proofFun2prop ty -> HExp (proofOf p)
  _ -> h
 where
  proofOf = EApp (EIdent identProof)

-- the proposition proved by a function type into proofs; Nothing for other
-- types, including a plain Proof P
proofFun2prop :: Exp -> Maybe Exp
proofFun2prop e = case e of
  EFun _ _ -> body e
  _ -> Nothing
 where
  body :: Exp -> Maybe Exp
  body t = case t of
    EApp (EIdent f) p | f == identProof -> Just p
    EFun h b -> do
      q <- body b
      case h of
        HVarExp x ty -> bound x ty q
        HParVarExp x ty -> bound x ty q
        HExp ty -> premiss ty q
        _ -> Nothing
    _ -> Nothing
  bound :: QIdent -> Exp -> Exp -> Maybe Exp
  bound x ty q = case ty of
    EApp (EIdent f) a | f == identElem -> Just (propPi a (EAbs (BVar x) q))
    _ -> premiss ty q
  -- consecutive premisses P, Q of R make one, "if P and Q, then R", which
  -- is equivalent to "if P, then if Q, then R" and closer to how texts say it
  premiss :: Exp -> Exp -> Maybe Exp
  premiss ty q = case ty of
    EApp (EIdent f) p | f == identProof -> Just $ case q of
      EApp (EApp (EIdent i) p2) r | i == identImpl -> propImp (propAnd p p2) r
      _ -> propImp p q
    _ -> Nothing


etaAppExp :: Exp -> [QIdent] -> Exp
etaAppExp fun args = foldr EAbs (foldl EApp (subst (zip bindvars (map EIdent args)) [] body) rest) ebinds
 where
   (binds, body) = splitAbs fun
   restargs = drop (length binds) args
   ebinds = binds ++ [BVar x | x <- restargs] ---- possible that x overshadow binds ? 
   bindvars = map bind2var ebinds
   rest = map EIdent restargs
   

subst :: [(QIdent, Exp)] -> [QIdent] -> Exp -> Exp
subst gamma bs e = case e of
  EIdent x {- | notElem x bs -} -> case lookup x gamma of
    Just v -> v
    _ -> e
  EApp f a -> case subst gamma bs f of
    EAbs bind body -> subst [(bind2ident bind, subst gamma bs a)] bs body -- beta-reduce redex from substituted higher-order arg
    f' -> EApp f' (subst gamma bs a)
  -- eta-contract x => B x when B is replaced by an abstraction y => b: the
  -- result y => b keeps the variable of the abstraction, where beta-reducing
  -- would rename it to x (forallI's x in "for all x" instead of the
  -- statement's own variable)
  EAbs b (EApp (EIdent g) (EIdent y))
    | y == bind2ident b, g /= y, Just v@(EAbs _ _) <- lookup g gamma -> v
  -- a bound variable that occurs free in what is substituted is renamed:
  -- existsE's (x : Elem A) -> Proof (B x) -> ..., with B := y => pair x y,
  -- must not become pair x x
  EAbs (BTyped x ty) a | Just x' <- captured x a ->
    EAbs (BTyped x' (subst gamma bs ty)) (subst ((x, EIdent x') : gamma) (x' : bs) a)
  EAbs (BVar x) a | Just x' <- captured x a ->
    EAbs (BVar x') (subst ((x, EIdent x') : gamma) (x' : bs) a)
  EFun (HVarExp x ty) a | Just x' <- captured x a ->
    EFun (HVarExp x' (subst gamma bs ty)) (subst ((x, EIdent x') : gamma) (x' : bs) a)
  EFun (HParVarExp x ty) a | Just x' <- captured x a ->
    EFun (HParVarExp x' (subst gamma bs ty)) (subst ((x, EIdent x') : gamma) (x' : bs) a)
  EAbs (BTyped x ty) a -> EAbs (BTyped x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EAbs b a -> EAbs b (subst gamma (bind2ident b : bs) a)
  EFun (HVarExp x ty) a -> EFun (HVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun (HParVarExp x ty) a -> EFun (HParVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun h a -> EFun h (subst gamma (hypo2vars h ++ bs) a)
  _ -> e
 where
  -- a fresh name for x, if x is free in the value of a variable free in the body
  captured x body
    | x `elem` concat [identsInTree v | (k, v) <- gamma, k /= x, k `elem` free] =
        Just (head [y | i <- [1 :: Int ..], let y = suffixed i, notElem y avoid])
    | otherwise = Nothing
   where
    free = identsInTree body
    avoid = free ++ bs ++ map fst gamma ++ concatMap (identsInTree . snd) gamma
    suffixed i = case x of QIdent s -> QIdent (s ++ show i)

-- add vars to hypos from abstraction or new vars if not yet given
unifyVars :: MExp -> [Hypo] -> [Hypo]
unifyVars mexp hypos = adds (zip vars hypos) where

  hypovars :: [Maybe QIdent]
  hypovars = map getVar hypos

  getVar :: Hypo -> Maybe QIdent
  getVar hypo = case hypo of
    HExp _ -> Nothing
    HVarExp x _  -> Just x
    HParVarExp x _ -> Just x
    HLetExp x _ -> Just x
    HLetTyped x _ _ -> Just x

  absvars :: [QIdent]
  absvars = case mexp of
    MEExp exp -> absIdents exp
    _ -> []

  allvars = absvars ++ [x | Just x <- hypovars]

  genvars = [QIdent s | s <- ["x", "y", "z", "u", "v", "w"] ++ ["X"  ++ show i | i <- [1..]]]

  vars = absvars ++ filter (\x -> notElem x allvars) genvars 
  
  adds :: [(QIdent, Hypo)] -> [Hypo]
  adds vshypos = case vshypos of
      (v, HExp exp) : vshs -> HVarExp v exp : adds vshs
      (_, h) : vshs -> h : adds vshs
      _ -> []


