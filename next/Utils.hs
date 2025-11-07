module Utils where

import Data.Char
import qualified Data.Set as S

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

