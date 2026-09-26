{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- Semantics of the constructions in grammars/GodementSyntax.gf: mapping them to MathCore.
-- Used in Informath2MathCore.semantics: godementExpand first splits resp. and
-- extracts namings, then godementSem is applied to each resulting Jmt.
-- godementSem eliminates all GodementSyntax functions, producing trees that
-- the existing sem/addParenth already handle (AndProp, AllProp, ExistProp, IfProp, ...).

module GodementSemantics (
  godementExpand,
  godementSem,
  kindPred,
  predProp,
  numDetPrefix
  ) where

import Informath

------------------------------------------------------------
-- 1. expansions that change the number of judgements

-- a Jmt with resp. becomes two Jmts; namings (CalledExp, DenotedExp) inside
-- a statement become separate definitions with the same hypotheses
godementExpand :: GJmt -> [GJmt]
godementExpand jmt
  | wellFormedDecls jmt = concatMap extractNamings (respResults jmt)
  | otherwise = []

-- DeclArgKind etc. overgenerate in parsing, as their formula argument is any indexed Prop;
-- only membership formulas "x, y in S" are accepted
wellFormedDecls :: Tree a -> Bool
wellFormedDecls t = case t of
  GDeclArgKind _ decl -> isDecl decl
  GDeclSuchThatKind kind decl prop -> isDecl decl && wellFormedDecls kind && wellFormedDecls prop
  GOfTheFormKind kind e decl -> isDecl decl && wellFormedDecls kind && wellFormedDecls e
  _ -> composOpFold True (&&) wellFormedDecls t
 where
   isDecl decl = case declVars decl of
     (_ : _, _) -> True
     _ -> False

respResults :: GJmt -> [GJmt]
respResults t
  | hasResp t = [respProject True t, respProject False t]
  | otherwise = [t]

hasResp :: Tree a -> Bool
hasResp t = case t of
  GRespProp _ _ -> True
  GRespAdj _ _ -> True
  _ -> composOpFold False (||) hasResp t

respProject :: Bool -> Tree a -> Tree a
respProject first t = case t of
  GRespProp a b -> respProject first (if first then a else b)
  GRespAdj a b -> respProject first (if first then a else b)
  _ -> composOp (respProject first) t

-- "the set f^{-1}(e), ..., is a subgroup of G, called the kernel of f, and denoted by Ker(f)"
-- ==> the theorem itself, plus
--     "the kernel of f is defined as f^{-1}(e)" and "Ker(f) is defined as the kernel of f"
extractNamings :: GJmt -> [GJmt]
extractNamings jmt = case jmt of
  GThmJmt label hypos prop proof ->
    let (defs, prop') = namings prop
    in GThmJmt label hypos prop' proof : [mkDef hypos d | d <- defs]
  GAxiomJmt label hypos prop ->
    let (defs, prop') = namings prop
    in GAxiomJmt label hypos prop' : [mkDef hypos d | d <- defs]
  _ -> [jmt]
 where
   mkDef hypos (definiendum, definiens) =
     GDefExpJmt (LexLabel "noLabel") hypos definiendum unspecifiedKind definiens

-- collect (definiendum, definiens) pairs; the tuple monad works as a Writer
namings :: Tree a -> ([(GExp, GExp)], Tree a)
namings t = case t of
  GCalledExp x name -> do
    x' <- namings x
    ([(name, x')], x')
  GDenotedExp x notation -> do
    x' <- namings x
    ([(notation, x')], x')
  _ -> composOpM namings t

------------------------------------------------------------
-- 2. the main translation, one Jmt to one Jmt

godementSem :: Tree a -> Tree a
godementSem t = case t of

-- definitions

  -- "a subgroup N of a group G is said to be normal if P"
  -- ==> "let G be a group, let N be a subgroup of G. Then N is normal if P"
  GDefAdjJmt label hypos x _ adj prop ->
    let (hs, x') = liftIndef x
    in GDefPropJmt label (addHypos hypos hs) (GAdjProp (godementSem adj) x') (godementSem prop)
  GDefNounJmt label hypos x _ kind prop ->
    let (hs, x') = liftIndef x
    in GDefPropJmt label (addHypos hypos hs) (kindPred (godementSem kind) x') (godementSem prop)
  GDefAdjCJmt label hypos xs _ adj prop ->
    let (hs, xs') = liftIndef xs
    in GDefPropJmt label (addHypos hypos hs) (GAdjCCollProp adj xs') (godementSem prop)
  -- "a law of composition on X is by definition any mapping of X x X into X"
  GDefAnyKindJmt label hypos kind _ df ->
    let (hs, kind') = liftIndef kind
    in GDefKindJmt label (addHypos hypos hs) (godementSem kind') (godementSem df)
  -- "p is called the characteristic of K" : the complement is the new name
  -- "the rank of f is defined to be the dimension of Im(f)" : the subject is the new name
  GNamingJmt label hypos x dv y
    | namesComplement dv -> GDefExpJmt label hypos (godementSem y) unspecifiedKind (godementSem x)
    | otherwise          -> GDefExpJmt label hypos (godementSem x) unspecifiedKind (godementSem y)
  GNamingKindJmt label hypos k dv k2
    | namesComplement dv -> GDefKindJmt label hypos (godementSem k2) (godementSem k)
    | otherwise          -> GDefKindJmt label hypos (godementSem k) (godementSem k2)
  GWeDefineExpJmt label hypos x y ->
    GDefExpJmt label hypos (godementSem x) unspecifiedKind (godementSem y)
  GWeSayJmt label hypos p q ->
    GDefPropJmt label hypos (godementSem p) (godementSem q)
  ---- notations should rather go to the symbol table
  GDenoteJmt label hypos notation x ->
    GDefExpJmt label hypos notation unspecifiedKind (godementSem x)
  GIsDenotedJmt label hypos x notation ->
    GDefExpJmt label hypos notation unspecifiedKind (godementSem x)

-- theorems: Russell's treatment of "the unique", witnesses of "namely"
  GAxiomJmt label hypos prop -> GAxiomJmt label (godementSem hypos) (topProp prop)
  GThmJmt label hypos prop proof -> GThmJmt label (godementSem hypos) (topProp prop) proof

-- propositions

  GKindProp x kind -> kindPred (godementSem kind) (godementSem x)
  GNotKindProp x kind -> GCoreNotProp (kindPred (godementSem kind) (godementSem x))
  GHaveProp x det noun -> haveProp (godementSem x) det noun

  -- "for an element x of E to be reflexible it is necessary and sufficient that P"
  GNecSuffProp cond (GQuantExp (GIndefIdentKindQuant y kind)) pred prop ->
    GAllProp (GListArgKind [GIdentArgKind (godementSem kind) y])
      (godementSem (GNecSuffProp cond (identExp y) pred prop))
  GNecSuffProp cond x pred prop ->
    let a = predProp pred (godementSem x)
        b = godementSem prop
    in case cond of
      Gnecessary_Condition  -> GIfProp a b
      Gsufficient_Condition -> GIfProp b a
      _                                    -> GIffProp a b

  -- "the following conditions are equivalent: a) P b) Q c) R"
  GEquivalentProp enum ->
    let ps = map godementSem (enumProps enum)
    in GAndProp (GListProp [GIffProp p q | (p, q) <- zip ps (tail ps)])

  -- "there exists a unique K x such that P(x)"
  -- ==> "there exists a K x such that P(x) and for all K z, if P(z) then z = x"
  GExistUniqueProp argkind prop ->
    let (x, kind) = argKindVar t argkind
        p = godementSem prop
    in GExistProp (GListArgKind [GIdentArgKind kind x]) (uniquely t x kind p)

  -- "if x is reflexible, then so is y"
  GIfSoIsProp x pred y ->
    GIfProp (predProp pred (godementSem x)) (predProp pred (godementSem y))

  GWheneverProp a b -> GIfProp (godementSem b) (godementSem a)
  GGivenProp argkinds prop -> GAllProp (godementSem argkinds) (godementSem prop)

-- kinds: everything becomes SuchThatKind

  GAdjKind adj kind ->
    let x = freshIdent t
    in GSuchThatKind (godementSem kind) x (GAdjProp (godementSem adj) (identExp x))
  GPostAdj2Kind kind adj2 y ->
    let x = freshIdent t
    in GSuchThatKind (godementSem kind) x (GAdj2Prop adj2 (identExp x) (godementSem y))
  GRelKind kind pred ->
    let x = freshIdent t
    in GSuchThatKind (godementSem kind) x (predProp pred (identExp x))
  GWithPropertyKind kind x prop ->
    GSuchThatKind (godementSem kind) x (godementSem prop)
  -- "let H be a subset of G with the following properties: a) P ; b) Q"
  -- ==> "let H be a subset of G. Assume P. Assume Q."
  GListHypo hypos -> GListHypo (map godementSem (concatMap splitProperties hypos))
  -- elsewhere, the properties are about the variable of an ArgKind, not visible here ----
  GPropertiesKind kind enum ->
    GSuchThatKind (godementSem kind) (freshIdent t) (GAndProp (GListProp (map godementSem (enumProps enum))))
  -- "element of G of the form xz, where z in H"
  GOfTheFormKind kind e decl ->
    let x = freshIdent t
    in GSuchThatKind (godementSem kind) x
         (GExistProp (GListArgKind [declArgKind decl])
            (GAdj2Prop (LexAdj2 "Eq_Adj2") (identExp x) (godementSem e)))
  -- "element x in G such that P"; the noun is redundant
  GDeclSuchThatKind _ decl prop -> case declVars decl of
    ([x], set) -> GSuchThatKind (GExpKind (GTermExp set)) x (godementSem prop)
    _ -> t ---- several variables: not a Kind
  GDeclArgKind _ decl -> declArgKind decl
  GSynonymKind k _ -> godementSem k

-- adjectives
  GSynonymAdj a _ -> godementSem a    -- the synonym goes to the symbol table as a | variant

-- quantifiers: "a" and "no" as in MathExtensions, numerals via numDetPrefix in Cooper storage
  GNumDetKindQuant Ga_NumDet kind -> GIndefKindQuant (godementSem kind)
  GNumDetKindQuant Gno_NumDet kind -> GNoKindQuant (godementSem kind)

-- expressions
  GNumberOfExp kind -> GFunExp (LexFun "cardinality_Fun") (GKindExp (godementSem kind))
  GSetOfAllExp kind -> GKindExp (godementSem kind)
  GEndowedExp x y -> GFun2Exp (LexFun2 "structure_Fun2") (godementSem x) (godementSem y) ---- formalization dependent
  GCalledExp x _ -> godementSem x     -- if not already extracted
  GDenotedExp x _ -> godementSem x

  _ -> composOp godementSem t


------------------------------------------------------------
-- 3. predication with Kinds, Preds, and "have"

-- "x is a K" as a proposition
kindPred :: GKind -> GExp -> GProp
kindPred kind x = case kind of
  GNounKind noun -> GNoun1Prop (GNounNoun1 noun) x
  GDepKind dep y -> GNoun2Prop (dep2noun2 dep) x y
  GDep2Kind dep y z -> GNoun3Prop (dep22noun3 dep) x y z
  GExpKind set -> GNoun2Prop (LexNoun2 "element_Noun2") x set
  GSuchThatKind k y p -> GAndProp (GListProp [kindPred k x, substExp y x p])
  GAdjKind adj k -> GAndProp (GListProp [kindPred k x, GAdjProp adj x])
  GPostAdj2Kind k adj2 y -> GAndProp (GListProp [kindPred k x, GAdj2Prop adj2 x y])
  GRelKind k pred -> GAndProp (GListProp [kindPred k x, predProp pred x])
  GWithPropertyKind k y p -> kindPred (GSuchThatKind k y p) x
  GPropertiesKind k enum -> GAndProp (GListProp (kindPred k x : enumProps enum))
  GSynonymKind k _ -> kindPred k x
  _ -> kindPred (godementSem kind) x  ---- loops if godementSem does not reduce the kind

-- the relational noun of a Dep: subgroup_Dep ~ subgroup_Noun2 by naming convention,
-- NounPrepDep ~ NounPrepNoun2 structurally
-- this presupposes that the symbol table has both, mapped to the same constant
dep2noun2 :: GDep -> GNoun2
dep2noun2 dep = case dep of
  LexDep s -> LexNoun2 (renameCat "Dep" "Noun2" s)
  GNounPrepDep noun prep -> GNounPrepNoun2 noun prep

dep22noun3 :: GDep2 -> GNoun3
dep22noun3 dep = case dep of
  LexDep2 s -> LexNoun3 (renameCat "Dep2" "Noun3" s)
  GNounPrepDep2 noun prep1 prep2 -> GNounPrepsNoun3 noun prep1 prep2

renameCat :: String -> String -> String -> String
renameCat old new s = take (length s - length old) s ++ new

-- "x VP"
predProp :: GPred -> GExp -> GProp
predProp pred x = case pred of
  GAdjPred adj -> GAdjProp adj x
  GAdj2Pred adj y -> GAdj2Prop adj x y
  GVerbPred verb -> GVerbProp verb x
  GVerb2Pred verb y -> GVerb2Prop verb x y
  GNoun2Pred noun y -> GNoun2Prop noun x y
  GKindPred kind -> kindPred kind x
  GHavePred det noun -> haveProp x det noun

-- "x has at least two subgroups" ==> the cardinality of the set of subgroups of x is >= 2
-- the noun is understood as relational, "subgroup of x"
haveProp :: GExp -> GNumDet -> GNoun -> GProp
haveProp x det noun =
  let kind = GDepKind (GNounPrepDep noun (LexPrep "of_Prep")) x
      card = GFunExp (LexFun "cardinality_Fun") (GKindExp kind)
  in case det of
    Ga_NumDet -> GExistKindProp kind
    Gno_NumDet -> GCoreNotProp (GExistKindProp kind)
    GAtLeastNumDet c -> GAdj2Prop (LexAdj2 "Geq_Adj2") card (cardExp c)
    GAtMostNumDet c  -> GAdj2Prop (LexAdj2 "Leq_Adj2") card (cardExp c)
    GExactlyNumDet c -> GAdj2Prop (LexAdj2 "Eq_Adj2")  card (cardExp c)

cardExp :: GCard -> GExp
cardExp c = case c of
  GExpCard e -> e
  Gone_Card -> GTermExp (GNumberTerm (GInt 1))
  Gtwo_Card -> GTermExp (GNumberTerm (GInt 2))
  Gthree_Card -> GTermExp (GNumberTerm (GInt 3))

------------------------------------------------------------
-- 4. in situ constructions resolved at the top of a Prop

-- "the unique K x such that P" in Q  ==>  there exists a K x such that P, uniquely, and Q(x)
-- "a K, namely e" in Q               ==>  e is a K and Q(e)
topProp :: GProp -> GProp
topProp prop =
  let (witnesses, prop1) = liftNamely (godementSem prop)
      (uniques, prop2) = liftUnique prop1
      withUniques = foldr
        (\ (x, kind, p) q -> GExistProp (GListArgKind [GIdentArgKind kind x])
                               (GAndProp (GListProp [uniquely prop x kind p, q])))
        prop2 uniques
  in case witnesses of
    [] -> withUniques
    _ -> GAndProp (GListProp (witnesses ++ [withUniques]))

liftNamely :: Tree a -> ([GProp], Tree a)
liftNamely t = case t of
  GNamelyExp (GQuantExp (GIndefKindQuant kind)) e -> ([kindPred kind e], e)
  GNamelyExp _ e -> ([], e)
  _ -> composOpM liftNamely t

liftUnique :: Tree a -> ([(GIdent, GKind, GProp)], Tree a)
liftUnique t = case t of
  GTheUniqueExp argkind prop ->
    let (x, kind) = argKindVar t argkind
    in ([(x, kind, prop)], identExp x)
  _ -> composOpM liftUnique t

-- P(x) and for all K z, if P(z) then z = x
uniquely :: Tree a -> GIdent -> GKind -> GProp -> GProp
uniquely context x kind p =
  let z = freshIdent context
  in GAndProp (GListProp [
       p,
       GAllProp (GListArgKind [GIdentArgKind kind z])
         (GIfProp (substIdent x z p) (GAdj2Prop (LexAdj2 "Eq_Adj2") (identExp z) (identExp x)))
       ])

------------------------------------------------------------
-- 5. indefinite subjects of definitions as hypotheses

-- "a subgroup N of a group G" ==> ([let G be a group, let N be a subgroup of G], N)
liftIndef :: Tree a -> ([GHypo], Tree a)
liftIndef t = case t of
  GQuantExp (GIndefIdentKindQuant x kind) -> do
    kind' <- liftIndef kind
    ([GVarHypo x (godementSem kind')], identExp x)
  -- "a group G" parsed with an apposition noun
  GQuantExp (GIndefKindQuant (GNounKind (GApposIdentNoun noun x))) ->
    ([GVarHypo x (GNounKind noun)], identExp x)
  GQuantExp (GNumDetKindQuant Ga_NumDet (GNounKind (GApposIdentNoun noun x))) ->
    ([GVarHypo x (GNounKind noun)], identExp x)
  _ -> composOpM liftIndef t

splitProperties :: GHypo -> [GHypo]
splitProperties hypo = case hypo of
  GVarHypo x (GPropertiesKind kind enum) -> GVarHypo x kind : map GPropHypo (enumProps enum)
  GVarsHypo xs (GPropertiesKind kind enum) -> GVarsHypo xs kind : map GPropHypo (enumProps enum)
  _ -> [hypo]

addHypos :: GListHypo -> [GHypo] -> GListHypo
addHypos (GListHypo hs) new = GListHypo (map godementSem hs ++ new)

------------------------------------------------------------
-- 6. auxiliaries

unspecifiedKind :: GKind
unspecifiedKind = GNounKind (LexNoun "object_Noun")

namesComplement :: GDefVerb -> Bool
namesComplement dv = case dv of
  Gcalled_DefVerb -> True
  _ -> False

enumProps :: GPropEnum -> [GProp]
enumProps enum = case enum of
  GBasePropEnum a b -> [a, b]
  GConsPropEnum a bs -> a : enumProps bs

identExp :: GIdent -> GExp
identExp x = GTermExp (GIdentTerm x)

argKindVar :: Tree a -> GArgKind -> (GIdent, GKind)
argKindVar context argkind = case argkind of
  GIdentArgKind kind x -> (x, godementSem kind)
  GIdentsArgKind kind (GListIdent [x]) -> (x, godementSem kind)
  GKindArgKind kind -> (freshIdent context, godementSem kind)
  GDeclArgKind kind decl -> case declVars decl of
    ([x], set) -> (x, GExpKind (GTermExp set))
    _ -> (freshIdent context, godementSem kind)  ---- not a declaration of one variable
  _ -> (freshIdent context, unspecifiedKind)     ---- TODO other ArgKinds

-- the variables and the set of a formula "x, y in S"
declVars :: GProp -> ([GIdent], GTerm)
declVars decl = case decl of
  GFormulaProp (GElemFormula (GListTerm terms) set) -> ([x | GIdentTerm x <- terms], set)
  _ -> ([], GIdentTerm (GStrIdent (GString "UNKNOWN")))

declArgKind :: GProp -> GArgKind
declArgKind decl = case declVars decl of
  (xs, set) -> GIdentsArgKind (GExpKind (GTermExp set)) (GListIdent xs)

identsIn :: Tree a -> [String]
identsIn t = case t of
  GStrIdent (GString s) -> [s]
  _ -> composOpFold [] (++) identsIn t

freshIdent :: Tree a -> GIdent
freshIdent t = GStrIdent (GString (head [x | i <- [0 :: Int ..], let x = "_g" ++ show i, notElem x used]))
  where used = identsIn t

-- rename variable x to z
substIdent :: GIdent -> GIdent -> Tree a -> Tree a
substIdent x z t = case t of
  GStrIdent s | GStrIdent s == x -> z
  _ -> composOp (substIdent x z) t

-- substitute an expression for variable x; inside formulas, via TextualTerm
substExp :: GIdent -> GExp -> Tree a -> Tree a
substExp x e t = case e of
  GTermExp (GIdentTerm z) -> substIdent x z t
  _ -> case t of
    GTermExp (GIdentTerm y) | y == x -> e
    GIdentTerm y | y == x -> GTextualTerm e
    _ -> composOp (substExp x e) t

-- quantifier prefix for "at least two Ks", used in Cooper storage in Semantics
numDetPrefix :: GNumDet -> GKind -> GIdent -> GProp -> GProp
numDetPrefix det kind x prop =
  let card = GFunExp (LexFun "cardinality_Fun") (GKindExp (GSuchThatKind kind x prop))
  in case det of
    Ga_NumDet  -> GCoreExistProp kind x prop
    Gno_NumDet -> GCoreNotProp (GCoreExistProp kind x prop)
    GAtLeastNumDet c -> GAdj2Prop (LexAdj2 "Geq_Adj2") card (cardExp c)
    GAtMostNumDet c  -> GAdj2Prop (LexAdj2 "Leq_Adj2") card (cardExp c)
    GExactlyNumDet c -> GAdj2Prop (LexAdj2 "Eq_Adj2")  card (cardExp c)
