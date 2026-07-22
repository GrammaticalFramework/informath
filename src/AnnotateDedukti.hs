{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module AnnotateDedukti where

import BuildConstantTable
import Dedukti.AbsDedukti
import DeduktiOperations
import CommonConcepts (lookupConstant, symbolicCats, verbalCats)

import PGF hiding (Hypo)

import Data.List (partition, sortOn, sort, groupBy, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S


type DkTree a = Dedukti.AbsDedukti.Tree a

-- annotate Dk idents with cats and funs ; used only internally
annotateDkIdents :: Maybe Int -> Maybe Int -> ConstantTable -> DropTable -> DkTree a -> [DkTree a]
annotateDkIdents msyns msymbs table drops =
                               checkSymbolics .
                               annot []
                ---- . ignoreFirstArguments drops -- done with profile
 where
  -- don't annotate bound variables: they override constants
  annot :: forall a. [QIdent] -> DkTree a -> [DkTree a]
  annot bounds t = case t of
    EApp fun arg -> case splitApp t of
      (EIdent c, args) | notElem c bounds -> [appProfile p (foldl EApp (EIdent f) aargs) | (f, p) <- annotId c, aargs <- sequence (map (annot bounds) args)]
      _ -> [EApp afun aarg | afun <- annot bounds fun, aarg <- annot bounds arg]
    QIdent _ | notElem t bounds -> map fst (annotId t)
    EAbs b exp -> [EAbs b2 exp2 | b2 <- annot bounds b, exp2 <- annot (bind2ident b : bounds) exp]
    EFun h exp -> [EFun h2 exp2 | h2 <- annot bounds h, exp2 <- annot (hypo2topvars h ++ bounds) exp]    
    BVar _ -> [t]
    BTyped v ty -> [BTyped v ty2 | ty2 <- annot bounds ty]
    HVarExp v ty -> [HVarExp v ty2 | ty2 <- annot bounds ty]
    HParVarExp v ty -> [HParVarExp v ty2 | ty2 <- annot bounds ty]
    HLetExp v ty -> [HLetExp v ty2 | ty2 <- annot bounds ty]
    HLetTyped v ty exp -> [HLetTyped v ty2 exp2 | ty2 <- annot bounds ty, exp2 <- annot bounds exp]
    _ -> composOpM (annot bounds) t

  tkSyns = maybe id take msyns
  tkSymbs = maybe id take msymbs

  annotId c = case M.lookup c table of
    Just entry -> [annotIdent c (maybe 0 id (M.lookup c drops)) fpt |
                       fpt <- tkSyns  (primary entry : synonyms entry) ++
                                tkSymbs (symbolics entry)]
    _ -> [(c, NoProfile)]

  checkSymbolics :: [DkTree a] -> [DkTree a]
  checkSymbolics ts = [t | t <- ts, not (badSymb t)] ---- null (badSymbolics t)]

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

annotIdent :: QIdent -> Int -> (FunProfile, Type) -> (QIdent, Profile)
annotIdent (QIdent s) d ((f, p), t) =
  (QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t) ++ [show d], p)
    where
      dk c = showCId c
      dkp f = showGFTree f

harmonizeJmt :: Jmt -> Jmt
harmonizeJmt jmt = case jmt of
  JDef ident (MTExp typ) meexp@(MEExp exp) ->
    let
       (hypos, kind) = splitType typ
       ahypos = unifyVars meexp hypos
       hvars = concatMap hypo2vars ahypos
       dexp = etaAppExp exp hvars
    in JDef ident (MTExp (foldr EFun kind ahypos)) (MEExp dexp)
  JThm ident mtyp mexp -> harmonizeJmt (JDef ident mtyp mexp)
  JInj ident mtyp mexp -> harmonizeJmt (JDef ident mtyp mexp)
  _ -> jmt


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
  EAbs (BTyped x ty) a -> EAbs (BTyped x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EAbs b a -> EAbs b (subst gamma (bind2ident b : bs) a)
  EFun (HVarExp x ty) a -> EFun (HVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun (HParVarExp x ty) a -> EFun (HParVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun h a -> EFun h (subst gamma (hypo2vars h ++ bs) a)
  _ -> e

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


