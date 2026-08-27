{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.Text as Text
import Felix2Informath (translateBlocks)
import qualified Felix.Workspace as Felix
import Informath
import Paths_informath (getDataFileName)
import System.Directory (withCurrentDirectory)
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  arguments <- getArgs
  unless (null arguments)
    (fail "usage: felix-translation")
  fixture <- getDataFileName "test/felix-statements.tex"
  let fixtureDirectory = takeDirectory fixture
      fixtureProjectDirectory = takeDirectory fixtureDirectory
  parsed <- withNaprapocheLibrary fixtureDirectory
    (withCurrentDirectory fixtureProjectDirectory
      (Felix.parseWorkspace fixture))
  blocks <- either
    (fail . Text.unpack . Felix.renderAuthorityFreeParseError)
    pure
    parsed
  judgements <- either fail pure (translateBlocks blocks)
  checkAxiomRegression judgements

withNaprapocheLibrary :: FilePath -> IO a -> IO a
withNaprapocheLibrary library =
  bracket replace restore . const
 where
  replace = do
    previous <- lookupEnv "NAPROCHE_LIB"
    setEnv "NAPROCHE_LIB" library
    pure previous
  restore previous = case previous of
    Nothing -> unsetEnv "NAPROCHE_LIB"
    Just value -> setEnv "NAPROCHE_LIB" value

checkAxiomRegression :: [GJmt] -> IO ()
checkAxiomRegression judgements = case judgements of
  [ GAxiomJmt labelForall (GListHypo forallHypos)
      (GCoreAllProp forallKindX forallX
        (GCoreAllProp forallKindY forallY
          (GCoreAndProp equalityX equalityY)))
    , GAxiomJmt labelExists (GListHypo existsHypos)
      (GCoreExistProp existsKindX existsX
        (GCoreExistProp existsKindY existsY equalityXY))
    ] -> do
      assert "the universal axiom has no hypotheses" (null forallHypos)
      assert "the existential axiom has no hypotheses" (null existsHypos)
      assertLabel "felix_informath_forall" labelForall
      assertLabel "felix_informath_exists" labelExists
      mapM_ assertSetKind [forallKindX, forallKindY, existsKindX, existsKindY]
      assertIdent "x" forallX
      assertIdent "y" forallY
      assertIdent "x" existsX
      assertIdent "y" existsY
      assertEquality "x" "x" equalityX
      assertEquality "y" "y" equalityY
      assertEquality "x" "y" equalityXY
  _ -> fail "the Felix axiom fixture did not retain its two ordered axiom trees"

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)

-- Generated Tree equality does not compare the lengths of list fields.
gfTreeEqual :: Gf a => a -> a -> Bool
gfTreeEqual expected actual = gf expected == gf actual

assertGfEqual :: Gf a => String -> a -> a -> IO ()
assertGfEqual message expected actual =
  assert message (gfTreeEqual expected actual)

assertLabel :: String -> GLabel -> IO ()
assertLabel expected label = case label of
  GIdentLabel identifier -> assertIdent expected identifier
  _ -> fail ("expected an identifier label for " ++ expected)

assertIdent :: String -> GIdent -> IO ()
assertIdent expected (GStrIdent (GString actual)) =
  assert ("expected identifier " ++ expected ++ ", got " ++ actual)
    (actual == expected)

assertSetKind :: GKind -> IO ()
assertSetKind kind = case kind of
  GNounKind (LexNoun name) ->
    assert ("expected set_Noun, got " ++ name) (name == "set_Noun")
  _ -> fail "expected a noun kind for a quantified set variable"

assertEquality :: String -> String -> GProp -> IO ()
assertEquality expectedLeft expectedRight proposition = case proposition of
  GAdj2Prop (LexAdj2 relation) left right -> do
    assert ("expected Eq_Adj2, got " ++ relation) (relation == "Eq_Adj2")
    assertVariableExpression expectedLeft left
    assertVariableExpression expectedRight right
  _ -> fail "expected a binary adjective equality proposition"

assertVariableExpression :: String -> GExp -> IO ()
assertVariableExpression expected expression = case expression of
  GTermExp (GIdentTerm identifier) -> assertIdent expected identifier
  _ -> fail ("expected the variable expression " ++ expected)
