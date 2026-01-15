module Utils where

import Data.Char
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortOn)
import Text.JSON

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

showFreqs :: [(String, Int)] -> [String]
showFreqs = map (\ (c, n) -> c ++ "\t" ++ show n)

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
  "\\batchmode",
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

mkJSONObject :: [(String, JSValue)] -> JSValue
mkJSONObject fields = makeObj fields

mkJSONField :: String -> JSValue -> (String, JSValue)
mkJSONField key value = (key, value)

mkJSONListField :: String -> [JSValue] -> (String, JSValue)
mkJSONListField key values = mkJSONField key (JSArray values)

stringJSON :: String -> JSValue
stringJSON s = JSString (toJSString s)

encodeJSON :: JSON a => a -> String
encodeJSON = encode

