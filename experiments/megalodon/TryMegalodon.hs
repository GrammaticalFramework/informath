-- File modified the BNF Converter (bnfc 2.9.6.1).

-- | Program to test parser line by line

module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, lines, map, unwords, putStr
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import AbsMegalodon   ()
import LexMegalodon   ( Token, mkPosToken )
import ParMegalodon   ( pDoc, myLexer )
import PrintMegalodon ( Print, printTree )
import SkelMegalodon  ()

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= mapM_ (run v p) . lines

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStr "Parse Failed..."
      putStr "Tokens: "
      putStr $ unwords $ map (showPosToken . mkPosToken) ts
      putStrLn (" " ++ err)
    Right tree -> do
      putStr "Parse Successful! "
      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrLn $ "[Abstract Syntax]: " ++ show tree
  putStrLn $ "[Linearized tree]: " ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pDoc
    "-s":fs    -> mapM_ (runFile 0 pDoc) fs
    fs         -> mapM_ (runFile 2 pDoc) fs

