module Utils where

import Data.Char
import Data.List (sortOn, isPrefixOf)
import qualified Data.Set as S
import qualified Data.Map as M
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

fileSuffix :: [Char] -> [Char]
fileSuffix = reverse . takeWhile (/= '.') . reverse

commaSep :: [Char] -> [String]
commaSep s = words (map (\c -> if c==',' then ' ' else c) s)

toLatexDoc :: [String] -> [String] -> [String]
toLatexDoc ms ss = latexPreamble ++ ms ++ ss ++ [latexEndDoc]

latexEndDoc :: String
latexEndDoc = "\\end{document}"

latexPreamble :: [String]
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


-- for generating valid LaTeX
-- the names of Greek letters are shown as the letters (lambda -> \\lambda),
-- and trailing digits as a subscript (P0 -> P_{0}, lambda1 -> \\lambda_{1}),
-- as mathematicians write them; other names of several letters are \\mathrm{}
mkLatexMathIdent :: String -> String
mkLatexMathIdent s = case s of
    '\\':_ -> s
    [_] -> s
    _ | Just g <- greekOrLetter base, not (null digits) -> g ++ "_{" ++ digits ++ "}"
    _ | elem s greekLetters -> "\\" ++ s
    _ -> "\\mathrm{" ++ escapeUnderscores s ++ "}"
  where
    (base, digits) = splitDigits s
    greekOrLetter b = case b of
      [c] | isAlpha c -> Just [c]
      _ | elem b greekLetters -> Just ("\\" ++ b)
      _ -> Nothing

splitDigits :: String -> (String, String)
splitDigits s = let (ds, rb) = span isDigit (reverse s) in (reverse rb, reverse ds)

greekLetters :: [String]
greekLetters = [
  "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
  "lambda", "mu", "nu", "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega",
  "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "Phi", "Psi", "Omega"
  ]

escapeUnderscores :: String -> String
escapeUnderscores = concatMap (\c -> if c=='_' then "\\_" else [c])

-- for converting back to Dedukti
unLatexMathIdent :: String -> String
unLatexMathIdent s = case s of
  _ | isPrefixOf "\\mathrm{" s -> unescapeUnderscores (drop 7 (init s))
  _ | (b, '_':'{':rest) <- break (=='_') s, not (null rest), last rest == '}',
      all isDigit (init rest) -> unLatexMathIdent b ++ init rest
  '\\':g | elem g greekLetters -> g
  _ -> unescapeUnderscores s

unescapeUnderscores :: String -> String
unescapeUnderscores s = case s of
  '\\':'_':cs -> '_':unescapeUnderscores cs
  c:cs -> c:unescapeUnderscores cs
  _ -> s


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
splitOutside :: Char -> Char -> [Char] -> [[Char]]
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



