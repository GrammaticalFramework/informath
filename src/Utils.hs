module Utils where

import Data.Char
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortOn)

setnub :: Ord a => [a] -> [a]
setnub = S.toList . S.fromList

-- replacing INDEXEDTERM with the $ expression given in Env
unindexString :: [String] -> String -> String
unindexString tindex = unwords . findterms . words
  where
    findterms ws = case ws of
      "\\INDEXEDTERM{" : n : ('}':cs) : ww -> (tindex !! (read n) ++ cs) : findterms ww
      w : ww -> w : findterms ww
      _ -> ws

-- for LaTeX, Agda, etc
snake2camel :: String -> String
snake2camel = concat . capit . words . uncamel where
  uncamel = map (\c -> if c == '_' then ' ' else c)
  capit (w:ws) = w : [toUpper c : cs | (c:cs) <- ws]

frequencyTable :: Ord a => [a] -> [(a, Int)]
frequencyTable xs = sortOn (\ (_, i) -> -i) $ M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]

commaSepInts :: String -> [Int]
commaSepInts s =
  let ws = commaSep s
  in if all (all isDigit) ws then map read ws else error ("expected digits found " ++ s)

fileSuffix = reverse . takeWhile (/= '.') . reverse

commaSep s = words (map (\c -> if c==',' then ' ' else c) s)

toLatexDoc :: [String] -> [String]
toLatexDoc ss = latexPreamble ++ ss ++ [latexEndDoc]

latexEndDoc = "\\end{document}"

latexPreamble = [
  "\\documentclass{article}",
  "\\usepackage{amsfonts}",
  "\\usepackage{amssymb}",
  "\\usepackage{amsmath}",
  "\\setlength\\parindent{0pt}",
  "\\setlength\\parskip{8pt}",
  "\\begin{document}",
  "\\newcommand{\\meets}{\\mathrel{\\supset\\!\\!\\!\\subset}}",
  "\\newcommand{\\notmeets}{\\mathrel{\\not\\meets}}"
  ]

mkJSONObject :: [String] -> String
mkJSONObject fields = "{" ++ concat (intersperse ", " fields) ++ "}"

mkJSONField :: String -> String -> String
mkJSONField key value = show key ++ ": " ++ value

mkJSONListField :: String -> [String] -> String
mkJSONListField key values =
  show key ++ ": " ++ "[" ++ concat (intersperse ", " values) ++ "]"

stringJSON = quote . escape where
  quote s = "\"" ++ s ++ "\""
  escape s = case s of
    c:cs | elem c "\"\\" -> '\\':c:escape cs
    '\n':cs -> '\\':'n':escape cs
    c:cs -> c:escape cs
    _ -> s
