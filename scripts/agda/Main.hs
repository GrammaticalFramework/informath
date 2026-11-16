{-# LANGUAGE OverloadedStrings #-}
-- | agda-export: an Agda backend that writes the definitions of each module
--   in Agda's internal syntax as JSON, for agdaterm.py and agda-export.py to
--   translate into Informath's Dedukti.
--
--   > agda-export --agda-export --agda-export-dir=out File.agda
--
--   writes out/<Module>.json: one object per definition, with its name, its
--   kind, its source line, its type, and depending on the kind its clauses
--   (telescope, patterns, body), its constructors, or its fields.  Terms are
--   the type checker's (every implicit argument there, binder names kept),
--   in the forms
--
--   > ["var", name, elims]      ["def", qname, elims]     ["con", qname, elims]
--   > ["lam", hiding, name, body]                         ["pi", hiding, name, dom, cod]
--   > ["sort", level]           ["lit", text]             ["level"]   ["other", text]
--
--   with elims [["app", hiding, term] | ["proj", qname]] and hiding "v"
--   (visible), "h" (hidden, {x}) or "i" (instance, {{x}}).
module Main (main) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Agda.Utils.GetOpt (OptDescr(..), ArgDescr(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.DeepSeq (NFData(..))

import Agda.Main (runAgda)
import Agda.Compiler.Backend
import Agda.Syntax.Internal
import Agda.Syntax.Common
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Position (rStart, posLine)
import Agda.Syntax.Abstract.Name (qnameName, nameBindingSite)
import Agda.Syntax.TopLevelModuleName (TopLevelModuleName)

data Opts = Opts { optEnabled :: Bool, optDir :: FilePath }

instance NFData Opts where
  rnf (Opts e d) = rnf e `seq` rnf d

main :: IO ()
main = runAgda [Backend backend]

backend :: Backend' Opts Opts () () J.Value
backend = Backend'
  { backendName = "agda-export"
  , backendVersion = Nothing
  , options = Opts False "out"
  , commandLineFlags =
      [ Option [] ["agda-export"] (NoArg $ \o -> pure o { optEnabled = True })
          "write the definitions of each module as JSON (Agda's internal syntax)"
      , Option [] ["agda-export-dir"] (ReqArg (\d o -> pure o { optDir = d }) "DIR")
          "the directory of the JSON files (default out)"
      ]
  , isEnabled = optEnabled
  , preCompile = pure
  , postCompile = \_ _ _ -> pure ()
  , preModule = \_ _ _ _ -> pure (Recompile ())
  , postModule = writeModule
  , compileDef = \_ _ _ d -> definition d
  , scopeCheckingSuffices = False
  , mayEraseType = \_ -> pure False
  , backendInteractTop = Nothing
  , backendInteractHole = Nothing
  }

writeModule :: Opts -> () -> IsMain -> TopLevelModuleName -> [J.Value] -> TCM ()
writeModule opts _ _ m defs = liftIO $ do
  createDirectoryIfMissing True (optDir opts)
  let file = optDir opts </> (prettyShow m ++ ".json")
  BL.writeFile file (BL.unlines (map J.encode defs))

-- * Definitions

definition :: Definition -> TCM J.Value
definition d = pure $ J.object $
  [ "name" J..= prettyShow q
  , "line" J..= (maybe 0 (fromIntegral . posLine) (rStart (nameBindingSite (qnameName q))) :: Int)
  , "type" J..= term [] (unEl (defType d))
  ] ++ case theDef d of
    Function{ funClauses = cs, funWith = w, funExtLam = ext } ->
      [ "kind" J..= ("function" :: Text)
      , "clauses" J..= map clause cs
      , "with" J..= fmap prettyShow w
      , "extlam" J..= maybe False (const True) ext ]
    Datatype{ dataPars = np, dataCons = cs } ->
      [ "kind" J..= ("data" :: Text), "pars" J..= np, "constructors" J..= map prettyShow cs ]
    Record{ recPars = np, recConHead = ch, recFields = fs } ->
      [ "kind" J..= ("record" :: Text), "pars" J..= np
      , "constructor" J..= prettyShow (conName ch), "fields" J..= map (prettyShow . unDom) fs ]
    Constructor{ conPars = np, conData = dt } ->
      [ "kind" J..= ("constructor" :: Text), "pars" J..= np, "data" J..= prettyShow dt ]
    Axiom{} -> [ "kind" J..= ("postulate" :: Text) ]
    Primitive{} -> [ "kind" J..= ("primitive" :: Text) ]
    _ -> [ "kind" J..= ("other" :: Text) ]
  where q = defName d

clause :: Clause -> J.Value
clause c = J.object
  [ "tel" J..= [ J.toJSON [J.toJSON (T.pack x), term ctx' (unEl t)]
               | (x, t, ctx') <- telescope (telToList (clauseTel c)) ]
  , "pats" J..= map (\p -> J.toJSON [hid (getHiding p), pattern ctx (namedArg p)]) (namedClausePats c)
  , "body" J..= fmap (term ctx) (clauseBody c)
  ]
  where
    names = [ x | Dom{ unDom = (x, _) } <- telToList (clauseTel c) ]
    ctx = reverse names
    telescope ds = go [] ds
      where go _ [] = []
            go acc (Dom{ unDom = (x, t) } : rest) = (x, t, acc) : go (x : acc) rest

-- * Terms (ctx: the names of the variables in scope, the innermost first)

term :: [String] -> Term -> J.Value
term ctx t = case t of
  Var i es -> arr ["var", s (at i), elims es]
  Def f es -> arr ["def", s (prettyShow f), elims es]
  Con c _ es -> arr ["con", s (prettyShow (conName c)), elims es]
  Lam ai b -> arr ["lam", hid (getHiding ai), s (absName b), term (under b) (unAbs b)]
  Pi a b -> arr ["pi", hid (getHiding a), s (if isAbs b then absName b else "_"), term ctx (unEl (unDom a)),
                 term (under b) (unEl (unAbs b))]
  Sort so -> arr ["sort", s (prettyShow so)]
  Lit l -> arr ["lit", s (prettyShow l)]
  Level _ -> arr ["level"]
  DontCare u -> term ctx u
  _ -> arr ["other", s (prettyShow t)]
  where
    at i = if i < length ctx then ctx !! i else "_v" ++ show i
    -- a binder (Abs) extends the context, a non-dependent one (NoAbs) does not
    under :: Abs a -> [String]
    under b = if isAbs b then absName b : ctx else ctx
    isAbs :: Abs a -> Bool
    isAbs b = case b of
      Abs{} -> True
      NoAbs{} -> False
    elims es = J.toJSON (map elim es)
    elim e = case e of
      Apply a -> arr ["app", hid (getHiding a), term ctx (unArg a)]
      Proj _ f -> arr ["proj", s (prettyShow f)]
      IApply _ _ r -> arr ["app", "v", term ctx r]

pattern :: [String] -> DeBruijnPattern -> J.Value
pattern ctx p = case p of
  VarP _ x -> arr ["var", s (patVar x)]
  DotP _ u -> arr ["dot", term ctx u]
  ConP c _ ps -> arr ["con", s (prettyShow (conName c)),
                      J.toJSON [ J.toJSON [hid (getHiding a), pattern ctx (namedArg a)] | a <- ps ]]
  LitP _ l -> arr ["lit", s (prettyShow l)]
  ProjP _ f -> arr ["proj", s (prettyShow f)]
  IApplyP _ _ _ x -> arr ["var", s (patVar x)]
  DefP _ f ps -> arr ["def", s (prettyShow f),
                      J.toJSON [ J.toJSON [hid (getHiding a), pattern ctx (namedArg a)] | a <- ps ]]

-- the name of a pattern variable: its telescope's (the pattern's may be _)
  where
    patVar x = let i = dbPatVarIndex x in if i < length ctx then ctx !! i else dbPatVarName x

hid :: Hiding -> J.Value
hid h = case h of
  NotHidden -> "v"
  Hidden -> "h"
  Instance _ -> "i"

arr :: [J.Value] -> J.Value
arr = J.toJSON

s :: String -> J.Value
s = J.toJSON

_unused :: Map.Map () ()
_unused = Map.empty
