-- File modified the BNF Converter (bnfc 2.9.6.1).

-- | Program to test parser line by line

module Main where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Char (isSpace)

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
runFile v p f = putStrLn f >> readFile f >>= \f -> mapM_ (run v p) (zip [1..] (jments f))

jments :: String -> [String]
jments = filter (not . null) . map (unwords . words) . split '.'

--- Data.List.Split cannot be found...
split :: Char -> String -> [String]
split c cs = case break (==c) cs of
  ([], []) -> []
  (s,  []) -> [s]
  (s, c:s2) -> (s ++ [c]) : split c s2

run :: (Print a, Show a) => Verbosity -> ParseFun a -> (Int, String) -> IO ()
run v p (n, s) =
  case p ts of
    Left err -> do
      putStr (show n ++ ": FAILURE: " ++ s)
--      putStr " Tokens: "
--      putStr $ unwords $ map (showPosToken . mkPosToken) ts
      putStrLn (" " ++ err)
    Right tree -> do
      putStr (show n ++ ": SUCCESS: ")
      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
--  putStrLn $ "[Abstract Syntax]: " ++ show tree
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
    []         -> getContents >>= \s -> run 2 pDoc (0, s)
    "-s":fs    -> mapM_ (runFile 0 pDoc) fs
    fs         -> mapM_ (runFile 2 pDoc) fs

