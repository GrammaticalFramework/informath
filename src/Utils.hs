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
      "\\INDEXEDTERM" : ('{' : cs) : ww -> (tindex !! (read (init cs))) : findterms ww
      "\\INDEXEDTERM" : "{" : cs : "}" : ww -> (tindex !! (read (init cs))) : findterms ww --- different unlexing
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

toLatexDoc :: [String] -> [String] -> [String]
toLatexDoc ms ss = latexPreamble ++ ms ++ ss ++ [latexEndDoc]

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

transInEnv :: String -> ([String] -> String) -> [String] -> [String]
transInEnv env trans = chop where

  chop ss = case break ((== "\\begin{" ++ env ++ "}") . strip) ss of
    (ls, []) -> ls
    (ls, rest) -> ls ++ case break ((== "\\end{" ++ env ++ "}") . strip) rest of
      (ds, line : rest) -> trans (ds ++ [line]) : chop rest
      (ds, []) -> ds


-- like Python strip()
strip :: String -> String
strip = unwords . words


-- like Python split();  Data.List.Split cannot be found...
split :: Char -> String -> [String]
split c cs = case break (==c) cs of
  ([], []) -> []
  (s,  []) -> [strip s]
  (s, _:s2) -> strip s : split c s2
 where
  strip = unwords . words


-- split with c outside a given lim env, such as $..$
-- split with c outside a given lim env, such as $..$
splitOutside lim c str = filter (not . null) (gather segments)
  where
    s = dropWhile isSpace str
    startlim = if (take 1 s == [lim]) then 1 else 0
    segments = filter (not . null) (split lim s)
    gather segs = concatMap handle (zip segs [startlim ..])
    handle (seg, i) = if (even i) then split c seg else [lim : seg ++ [lim]]

-- Python-like dict values from line by line from e.g. symbol tables
dictValues :: String -> [String]
dictValues = map (drop 1 . dropWhile (/= ':')) . lines



