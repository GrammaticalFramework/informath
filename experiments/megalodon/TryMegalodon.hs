-- File modified the BNF Converter (bnfc 2.9.6.1).

-- | Program to test parser line by line

module Main where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Char (isSpace)

import AbsMegalodon 
import LexMegalodon   ( Token, mkPosToken )
import ParMegalodon   ( pDoc, myLexer )
import PrintMegalodon ( Print, printTree )
import SkelMegalodon  ()

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Doc -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= \f -> mapM_ (run v p) (zip [1..] (jments f))

jments :: String -> [String]
jments = filter (not . null) . map (unwords . words) . split '.'

--- Data.List.Split cannot be found...
split :: Char -> String -> [String]
split c cs = case break (==c) cs of
  ([], []) -> []
  (s,  []) -> [s]
  (s, c:s2) -> (s ++ [c]) : split c s2

run :: Verbosity -> ParseFun Doc -> (Int, String) -> IO ()
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

showTree :: Int -> Doc -> IO ()
showTree v tree = do
  let ptree = printTree tree
  let pttree = printTree (transDoc tree)
  putStrLn ptree
  if pttree /= ptree
    then putStrLn ("TRANS: " ++ printTree pttree)
    else return ()

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

transDoc :: Doc -> Doc
transDoc = trans where
  trans :: Tree a -> Tree a
  trans t = case t of
---    E_Infix_M x y -> apps (Ident "M") (trans x) (trans y)
    E_Infix_neq x y -> apps (Ident "neq") (trans x) (trans y)
    E_Infix_iff x y -> apps (Ident "iff") (trans x) (trans y)
    E_Infix_div_SNo x y -> apps (Ident "div_SNo") (trans x) (trans y)
    E_Infix_setprod x y -> apps (Ident "setprod") (trans x) (trans y)
    E_Infix_eq x y -> apps (Ident "eq") (trans x) (trans y)
    E_Infix_or x y -> apps (Ident "or") (trans x) (trans y)
    E_Infix_setsum x y -> apps (Ident "setsum") (trans x) (trans y)
    E_Infix_SNoLe x y -> apps (Ident "SNoLe") (trans x) (trans y)
---    E_Infix_add_SNo x y -> apps (Ident "add_SNo") (trans x) (trans y)
---    E_Infix_exp_SNo_nat x y -> apps (Ident "exp_SNo_nat") (trans x) (trans y)
    E_Infix_binunion x y -> apps (Ident "binunion") (trans x) (trans y)
    E_Infix_setexp x y -> apps (Ident "setexp") (trans x) (trans y)
    E_Infix_mul_nat x y -> apps (Ident "mul_nat") (trans x) (trans y)
    E_Infix_add_nat x y -> apps (Ident "add_nat") (trans x) (trans y)
    E_Prefix_not x -> app (Ident "not") (trans x)
    E_Infix_and x y -> apps (Ident "and") (trans x) (trans y)
    E_Postfix_tag x -> app (Ident "tag") (trans x)
    E_Infix_nIn x y -> apps (Ident "nIn") (trans x) (trans y)
---    E_Infix_mul_SNo x y -> apps (Ident "mul_SNo") (trans x) (trans y)
    E_Infix_exp_nat x y -> apps (Ident "exp_nat") (trans x) (trans y)
    E_Infix_binintersect x y -> apps (Ident "binintersect") (trans x) (trans y)
    E_Prefix_minus_SNo x -> app (Ident "minus_SNo") (trans x)
    E_Infix_setminus x y -> apps (Ident "setminus") (trans x) (trans y)
    E_Infix_SNoLt x y -> apps (Ident "SNoLt") (trans x) (trans y)

    _ -> composOp trans t
    
apps ident x y = foldl EApp (EIdent ident) [x, y]
app ident x = EApp (EIdent ident) x

