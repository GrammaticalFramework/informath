module Lexing (lextex, unlextex, indexTex) where

import Data.Char (isSpace,toUpper,toLower)
import Data.List (intersperse)


lextex :: String -> String
lextex = unwords . lexMixed (const False)


-- from GF.Text.Lexing in gf-core
unlextex :: String -> String
unlextex = dropWhile isSpace . unlexMixed (const False) . words


-- in a (tokenized) string, replace $ s $ with \INDEXEDTERM{ i } and tindex !! i = $ s $
indexTex :: String -> (String, [String])
indexTex = massage . ind [] [] . map unwords . chop . words where

  massage (ss, tss) = (unwords (reverse ss), map unwords (reverse tss))

  ind :: [[String]] -> [String] -> [String] -> ([String], [[String]])
  ind ts ss cs = case cs of
    d : ws : e : rest | isDollar d && isDollar e && length ws == 1 ->
      ind ts (e : ws : d : ss) rest
    d : ws : e : rest | isDollar d && isDollar e ->
      let
        trm = [d, ws, e]
        ix = indexedTerm (length ts)
      in ind (trm : ts) (ix : ss) rest
    t : rest -> ind ts (t : ss) rest
    _ -> (ss, ts)

  indexedTerm i = "\\INDEXEDTERM{ " ++ show i ++ " }"

  isDollar s = elem s ["$", "$$"]

  chop :: [String] -> [[String]]
  chop ts = case break (flip elem ["$", "$$"]) ts of
    (s1@(_:_), d:s2) -> s1 : [d] : chop s2
    ([],  d:s2) -> [d] : chop s2
    (_, []) -> [ts]


-- here is the code copied from GF.Text.Lexing, should probably be pruned

-- * Mixed lexing

-- | LaTeX style lexer, with "math" environment using Code between $...$
lexMixed :: (String -> Bool) -> String -> [String]
lexMixed good = concat . alternate False [] where
  alternate env t s = case s of
    '$':'$':cs -> lex env (reverse t) : ["$$"] : alternate (not env) [] cs
    '$':cs -> lex env (reverse t) : ["$"] : alternate (not env) [] cs
    '\\':c:cs | elem c "()[]" -> lex env (reverse t) : [['\\',c]] : alternate (not env) [] cs
    c:cs -> alternate env (c:t) cs
    _ -> [lex env (reverse t)]
  lex env = if env then lexLatexCode else lexText good

unlexMixed :: (String -> Bool) -> [String] -> String
unlexMixed good = fixesAfterDollars . capitInit . concat . alternate False . bindTok where
  alternate env s = case s of
    _:_ -> case break (flip elem ["$","$$", "\\[","\\]","\\(","\\)"]) s of
      (t,[])  -> unlex env t : []
      (t,c:m) -> unlex env t : sep env c m : alternate (not env) m
    _ -> []
  unlex env = if env then unlexCode else (uncapitInit good . unlexText)
  sep env c m = case (m,env) of
    ([p]:_,True) | isPunct p -> c   -- closing $ glued to next punct 
    (_,  True) -> c ++ " "   -- closing $ otherwise separated by space from what follows
    _ -> " " ++ c   -- put space before opening $

  fixesAfterDollars :: String -> String --- ugly hack instead of trying to find the root of a problem
  fixesAfterDollars s = case s of
    '$':'$':'.':cs -> '.':'$':'$': fixesAfterDollars cs 
    '$':'$':',':cs -> ',':'$':'$': fixesAfterDollars cs 
    '$':'.':' ':c:cs -> '$':'.':' ': toUpper c : fixesAfterDollars cs
    c:cs -> c : fixesAfterDollars cs
    _ -> s


-- * Additional lexing uitilties

-- | Capitalize first letter
capitInit s = case s of
  c:cs -> toUpper c : cs
  _ -> s

-- | Uncapitalize first letter
uncapitInit good s = 
  case s of
    c:cs | not (good s) -> toLower c : cs
    _                   -> s

-- | Unquote each string wrapped in double quotes
unquote = map unq where 
  unq s = case s of
    '"':cs@(_:_) | last cs == '"' -> init cs
    _ -> s

isPunct = flip elem ".?!,:;"
isMajorPunct = flip elem ".?!"
isMinorPunct = flip elem ",:;"
isParen = flip elem "()[]{}"
isClosing = flip elem ")]}"

unlexCode :: [String] -> String
unlexCode s = case s of
  w:[] -> w
  [c]:cs | isParen c -> [c] ++ unlexCode cs
  w:cs@([c]:_) | isClosing c -> w ++ unlexCode cs
  w:ws -> w ++ " " ++ unlexCode ws
  _ -> []


-- * Text lexing
-- | Text lexing with standard word capitalization of the first word of every sentence
lexText :: (String -> Bool) -> String -> [String]
lexText good = lexText' (uncapitInit good)

-- | Text lexing with custom treatment of the first word of every sentence.
lexText' :: (String->String) -> String -> [String]
lexText' uncap1 = uncap . lext where
  lext s = case s of
    c:cs | isMajorPunct c -> [c] : uncap (lext cs)
    c:cs | isMinorPunct c -> [c] : lext cs
    c:cs | isSpace c ->       lext cs
    _:_ -> let (w,cs) = break (\x -> isSpace x || isPunct x) s in w : lext cs
    _ -> [s]
  uncap s = case s of
    w:ws -> uncap1 w:ws
    _ -> s

unlexText :: [String] -> String
unlexText = capitInit . unlext where
  unlext s = case s of
    w:[] -> w
    w:[c]:[] | isPunct c -> w ++ [c]
    w:[c]:cs | isMajorPunct c -> w ++ [c] ++ " " ++ capitInit (unlext cs)
    w:[c]:cs | isMinorPunct c -> w ++ [c] ++ " " ++ unlext cs
    w:ws -> w ++ " " ++ unlext ws
    _ -> []

-- | Bind tokens separated by Prelude.BIND, i.e. &+
bindTok :: [String] -> [String]
bindTok ws = case ws of
               w1:"&+":w2:ws -> bindTok ((w1++w2):ws)
               "&+":ws       -> bindTok ws
               "&|":(c:cs):ws-> bindTok ((toUpper c:cs) : ws)
               "&|":ws       -> bindTok ws
               w:ws          -> w:bindTok ws
               []            -> []

-- * Code lexing

-- | Haskell lexer, usable for much code
lexCode :: String -> [String]
lexCode ss = case lex ss of
  [(w@(_:_),ws)] -> w : lexCode ws
  _ -> []
  
-- | LaTeX lexer in the math mode: \ should not be separated from the next word

lexLatexCode :: String -> [String]
lexLatexCode = restoreBackslash . lexCode where --- quick hack: postprocess Haskell's lex
  restoreBackslash ws = case ws of
    "\\":w:ww -> ("\\" ++ w) : restoreBackslash ww
    w:ww -> w:restoreBackslash ww
    _ -> ws

