module Main (main) where

import qualified Data.Text as Text
import Felix2Informath
  ( Translation (..)
  , renderTranslationSummary
  , translateBlocks
  )
import qualified Felix.Workspace as Felix
import Informath (Gf (gf))
import PGF (showExpr)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs
  source <- case arguments of
    [file] -> pure file
    _ -> die "usage: felix2informath <file>"
  parsed <- Felix.parseWorkspace source
  blocks <- either
    (die . Text.unpack . Felix.renderAuthorityFreeParseError)
    pure
    parsed
  translation <- either die pure (translateBlocks blocks)
  mapM_ (putStrLn . showExpr [] . gf)
    (translatedJudgements translation)
  hPutStrLn stderr
    (renderTranslationSummary (translationSummary translation))
