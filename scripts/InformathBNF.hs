{- | A simplified BNF grammar of Informath's English, for documentation.

    stack runghc scripts/InformathBNF.hs [share/InformathEng.pgf] > doc/informath-bnf.md

For each function of the abstract syntax, a rule of its value category is
made from its English linearization: the function is applied to small trees
of its argument categories, linearized with brackets, and each argument's
bracket is replaced by its category, so that the rule shows the words of the
function and the categories of its arguments, as in

    Jmt ::= Label "." Hypos "Then" "the" Kind "form" "a" Kind "."

The grammar is simplified: each rule shows one form (the default values of
the parameters: singular, positive, ...), a discontinuous argument is shown
where it first occurs, and an argument that the example linearizes as
nothing is not shown. A list category ListC is written [C]. The categories
are presented top-down, from Jmt through the categories that its rules use,
and the lexical items of a category are shown by a few examples.

The example trees are the smallest ones of their categories whose own rules
show all their arguments. A tree of a category that cannot be linearized on
its own (a lexical category such as Adj) is linearized in the context of a
rule that uses it, the first one that works, and its part of the output is
found by its brackets. The Haskell runtime is used, as the C runtime of the
Python bindings aborts on such trees, where the Haskell one raises an
exception that can be caught.
-}

import PGF
import Control.Exception (SomeException, evaluate, try)
import Data.List (intercalate, nub, sortOn, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)

type Sig = ([CId], CId)   -- argument categories, value category

main :: IO ()
main = do
  args <- getArgs
  let file = case args of { f : _ -> f ; _ -> "share/InformathEng.pgf" }
  pgf <- readPGF file
  let lang = mkCId "InformathEng"
      sigs = M.fromList [(f, s) | f <- functions pgf, Just t <- [functionType pgf f],
                                  let (hs, c, _) = unType t,
                                  let s = ([cat | (_, _, ht) <- hs, let (_, cat, _) = unType ht], c)]
      cats = categories pgf

  -- the smallest tree of each category; then, for each category, the smallest
  -- tree whose rule shows all of its arguments, so that no argument of a rule
  -- is lost because its example is empty
  let fillers0 = fixFillers sigs
  rules0 <- rulesWith pgf lang sigs fillers0
  let shown f syms = length [s | s <- syms, take 1 s /= "\""]
                       >= maybe 0 (length . filter (not . isList) . fst) (M.lookup f sigs)
                     && not (null syms)
      better = M.fromListWith (\new old -> if fst new < fst old then new else old)
                 [(c, (length (showExpr [] t), t))
                 | (c, (f, syms)) <- rules0, shown f syms,
                   Just (as, _) <- [M.lookup f sigs], Just es <- [mapM (`M.lookup` fillers0) as],
                   let t = mkApp f es]
      fillers = M.union (M.map snd better) fillers0
  rules <- rulesWith pgf lang sigs fillers

  let byCat = M.fromListWith (flip (++)) [(c, [r]) | (c, r) <- rules]
      nullary f = maybe False (null . fst) (M.lookup f sigs)
      order = topDown (mkCId "Jmt") byCat cats
  putStr (document order byCat nullary sigs)

-- | The rules of all functions, with these example trees.
rulesWith :: PGF -> Language -> M.Map CId Sig -> M.Map CId Tree -> IO [(CId, (CId, [String]))]
rulesWith pgf lang sigs fillers = do
  -- the categories whose trees can be linearized on their own
  standalone <- fmap S.fromList $ fmap concat $ mapM (\(c, t) -> do
                    ok <- linearizes pgf lang t
                    return [c | ok]) (M.toList fillers)
  -- for the others, a context: a function that takes it, in a context of its own
  contexts <- fixContexts pgf lang sigs fillers standalone
  let templateOf f = do
        (as, c) <- M.lookup f sigs
        es <- mapM (`M.lookup` fillers) as
        ctxs <- M.lookup c contexts
        return (c, [ctx (mkApp f es) | ctx <- take 8 ctxs])
  fmap concat $ mapM (\f -> case templateOf f of
      Nothing -> return []
      Just (c, ts) -> firstRule f c ts) (M.keys sigs)
 where
  -- the rule from the first context that gives one
  firstRule _ _ [] = return []
  firstRule f c (t : ts) = do
    r <- try (evaluate (forceList (template f c (bracketedLinearize pgf lang t))))
    case r :: Either SomeException (Maybe [String]) of
      Right (Just syms) -> return [(c, (f, syms))]
      _ -> firstRule f c ts

forceList :: Maybe [String] -> Maybe [String]
forceList m = case m of
  Just xs | sum (map length xs) >= 0 -> m
  _ -> m

linearizes :: PGF -> Language -> Tree -> IO Bool
linearizes pgf lang t = do
  r <- try (evaluate (length (linearize pgf lang t)))
  return $ case r :: Either SomeException Int of
    Right n -> n > 0
    Left _ -> False

-- | The smallest tree of each category, literals given.
fixFillers :: M.Map CId Sig -> M.Map CId Tree
fixFillers sigs = go lits (M.map (const 1) lits)
  where
    lits = M.fromList [(mkCId "Int", mkInt 7), (mkCId "String", mkStr "s"), (mkCId "Float", mkFloat 1.5)]
    go fill size =
      let cands = [(c, (sz, mkApp f [fill M.! a | a <- as]))
                  | (f, (as, c)) <- M.toList sigs, all (`M.member` fill) as,
                    let sz = 1 + sum [size M.! a | a <- as],
                    maybe True (> sz) (M.lookup c size)]
          best = M.fromListWith (\new old -> if fst new < fst old then new else old) cands
      in if M.null best then fill
         else go (M.union (M.map snd best) fill) (M.union (M.map fst best) size)

-- | A context for each category: the identity for the ones that linearize on
-- their own, else a function that takes it, with its context, that
-- linearizes (HyposAssumption does not).
fixContexts :: PGF -> Language -> M.Map CId Sig -> M.Map CId Tree -> S.Set CId -> IO (M.Map CId [Tree -> Tree])
fixContexts pgf lang sigs fillers standalone = go (M.fromSet (const [id]) standalone)
  where
    go ctxs = do
      let cands = [(a, \e -> ctx (mkApp f [if j == i then e else fillers M.! b | (j, b) <- zip [0 :: Int ..] as]))
                  | (f, (as, c)) <- sortOn (length . fst . snd) (M.toList sigs),
                    Just (ctx : _) <- [M.lookup c ctxs], all (`M.member` fillers) as,
                    (i, a) <- zip [0 ..] as, not (M.member a ctxs), M.member a fillers]
      -- all the contexts that work, as one may fail with another tree
      new <- foldl (\acc (a, k) -> do
                m <- acc
                ok <- showsArg a (k (fillers M.! a))
                return (if ok then M.insertWith (flip (++)) a [k] m else m)) (return M.empty) cands
      if M.null new then return ctxs else go (M.union ctxs new)
    showsArg _ t = do
      r <- try (evaluate (length (concatMap showBracketedString (bracketedLinearize pgf lang t))))
      return $ case r :: Either SomeException Int of
        Right _ -> True
        Left _ -> False

-- | The rule of f: the tokens and argument categories of its bracket.
template :: CId -> CId -> [BracketedString] -> Maybe [String]
template f c bss = case concatMap find bss of
    [] -> Nothing
    (fid0, _) : _ -> Just (rule (concat [ch | (fid, ch) <- concatMap find bss, fid == fid0]))
  where
    -- all the parts of the node of f: a discontinuous one ("an instance ...
    -- of K") has several
    find b = case b of
      Bracket bc fid _ _ bf _ ch | bf == f && bc == c -> [(fid, ch)]
      Bracket _ _ _ _ _ _ ch -> concatMap find ch
      Leaf _ -> []
    rule ch = articles (go S.empty ch)
    go _ [] = []
    go seen (x : xs) = case x of
      Leaf "&+" -> go seen xs
      Leaf t -> show t : go seen xs
      -- a discontinuous argument where it first occurs; but a list in a
      -- conjunction has its last element after the conjunction word:
      -- [Exp] "and" Exp
      Bracket bc fid _ _ _ _ _
        | S.member fid seen -> [elemName bc | isList bc] ++ go seen xs
        | otherwise -> catName bc : go (S.insert fid seen) xs
    -- the article of the example word: "a" before a category
    articles syms = case syms of
      "\"an\"" : next : rest | take 1 next /= "\"" -> "\"a\"" : articles (next : rest)
      s : rest -> s : articles rest
      [] -> []

isList :: CId -> Bool
isList c = let s = showCId c in "List" `isPrefixOf` s && length s > 4

elemName :: CId -> String
elemName c = drop 4 (showCId c)

catName :: CId -> String
catName c = if isList c then "[" ++ elemName c ++ "]" else showCId c

-- | The categories from Jmt down, in the order in which the rules use them;
-- then the others.
topDown :: CId -> M.Map CId [(CId, [String])] -> [CId] -> [CId]
topDown start byCat cats = go [start] [] ++ [c | c <- cats, M.member c byCat, c `notElem` reached]
  where
    reached = go [start] []
    names = M.fromList [(catName c, c) | c <- cats]
    go [] seen = reverse seen
    go (c : queue) seen
      | c `elem` seen = go queue seen
      | otherwise =
          let used = nub [u | (_, syms) <- M.findWithDefault [] c byCat, s <- syms,
                              Just u <- [M.lookup s names], u `notElem` seen, u /= c]
          in go (queue ++ used) (c : seen)

-- | The document: a section per category, its rules in a code block.
document :: [CId] -> M.Map CId [(CId, [String])] -> (CId -> Bool) -> M.Map CId Sig -> String
document order byCat nullary sigs = unlines $
  [ "# A BNF grammar of Informath's English"
  , ""
  , "Made by `scripts/InformathBNF.hs` from `share/InformathEng.pgf` (do not edit by hand:"
  , "run `stack runghc scripts/InformathBNF.hs > doc/informath-bnf.md`)."
  , ""
  , "This is a simplified grammar. Each rule is made from the English linearization of"
  , "one function of Informath's abstract syntax (named in the comment after it), with"
  , "its arguments shown by their categories. Each rule shows one form, with the default"
  , "values of the parameters (number, case, mood, ...); an argument that has several"
  , "parts in different places (a discontinuous constituent) is shown where it first"
  , "occurs, and an argument that may be empty may be missing. `[C]` is a list of `C`;"
  , "in a conjunction (`[Exp] \"and\" Exp`), its last element comes after the"
  , "conjunction word. `(...)?` is optional, `(...)*` repeated, and `\"\"` empty."
  , "Strings in quotes are words or symbols; `Ident`, `Int` and `String` are"
  , "identifiers and literals. The categories are given top-down, from the judgements"
  , "(`Jmt`) to the words; for a lexical category, a few of its words are shown."
  , ""
  , "Categories: " ++ intercalate ", " ["[" ++ catName c ++ "](#" ++ anchor (heading c) ++ ")" | c <- order]
  , ""
  ] ++ concatMap section order
  where
    -- a list's heading names it, as its anchor would be the same as its element's
    heading c = if isList c then catName c ++ " (list of " ++ elemName c ++ ")" else catName c
    -- GitHub's anchors: lower case, spaces as hyphens, other punctuation left out
    anchor h = [if ch == ' ' then '-' else toLowerC ch | ch <- h, ch == ' ' || ch == '-' || ch == '_' || isAlnumC ch]
    isAlnumC ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')
    toLowerC ch = if ch >= 'A' && ch <= 'Z' then toEnum (fromEnum ch + 32) else ch
    section c =
      let rs = M.findWithDefault [] c byCat
          (lexical, syntactic) = (filter (nullary . fst) rs, filter (not . nullary . fst) rs)
          lhs = catName c
          pad = replicate (length lhs + 1) ' '
          -- a list of a conjunction (BaseExp : Exp -> Exp -> ListExp), schematically
          conjList = isList c && or [length as == 2 | (f, (as, c')) <- M.toList sigs, c' == c,
                                                      "Base" `isPrefixOf` showCId f]
          -- the same right-hand side from several functions: one rule
          grouped = if conjList then [(elemName c ++ " (\",\" " ++ elemName c ++ ")*", [])]
                    else groupRules syntactic
          ruleLines = [ (if k == 0 then lhs ++ " ::= " else pad ++ "  | ") ++ rhs
                        ++ (if null fs then "" else "   -- " ++ intercalate ", " (map showCId fs))
                      | (k, (rhs, fs)) <- zip [0 :: Int ..] grouped ]
          words' = nub [unwords syms | (_, syms) <- lexical, not (null syms)]
          examples = take 5 (filter (`elem` words') (M.findWithDefault [] (showCId c) preferred)
                             ++ sample 5 (filter plain words'))
          plain w = showCId c `elem` ["ProperName", "Const"] || take 2 w == take 2 (map toLowerC w)
          -- an empty list (BaseHypo), which has no bracket of its own
          emptyBase = isList c && or [null as | (f, (as, c')) <- M.toList sigs, c' == c,
                                                "Base" `isPrefixOf` showCId f]
          lexLine = [ (if null ruleLines then lhs ++ " ::= " else pad ++ "  | ")
                      ++ intercalate " | " (nub examples)
                      ++ (if length lexical > length (nub examples)
                          then " | ...   -- " ++ show (length lexical) ++ " words" else "")
                    | not (null examples), not conjList ]
                    ++ [ pad ++ "  | \"\"" | emptyBase, not conjList ]
          literal = M.findWithDefault [] (showCId c) literals
      in [ "## " ++ heading c, "", "```" ] ++ ruleLines ++ lexLine ++ literal ++ [ "```", "" ]
    groupRules rs = [ (optional rhs, [f | (f, s) <- rs, unwords' s == rhs]) | rhs <- nub [unwords' s | (_, s) <- rs] ]
    unwords' s = if null s then "\"\"" else unwords s
    -- hypotheses are followed by "then", both only if there are any
    optional rhs = replace "[Hypo] \"then\"" "([Hypo] \"then\")?" rhs
    literals = M.fromList
      [ ("Int", ["Int ::= \"0\" | \"1\" | \"2\" | ...   -- integer literals"])
      , ("String", ["String ::= ...   -- string literals: identifiers, macro names, titles"]) ]

-- | Examples of the lexical categories, where the first ones in the grammar
-- would be rare words.
preferred :: M.Map String [String]
preferred = M.fromList
  [ ("Noun", ["\"set\"", "\"number\"", "\"group\"", "\"function\"", "\"sequence\""])
  , ("Adj", ["\"even\"", "\"prime\"", "\"finite\"", "\"continuous\"", "\"injective\""])
  , ("Verb", ["\"converge\"", "\"vanish\"", "\"exist\""])
  , ("Verb2", ["\"divide\"", "\"contain\"", "\"meet\""])
  , ("Adj2", ["\"equal\" \"to\"", "\"less\" \"than\"", "\"divisible\" \"by\""])
  , ("Fun", ["\"absolute\" \"value\"", "\"degree\"", "\"square\""])
  , ("Prep", ["\"of\"", "\"on\"", "\"in\"", "\"by\"", "\"from\""]) ]

replace :: String -> String -> String -> String
replace old new str = case str of
  [] -> []
  _ | old `isPrefixOf` str -> new ++ replace old new (drop (length old) str)
  ch : rest -> ch : replace old new rest

-- | n examples spread over the list.
sample :: Int -> [a] -> [a]
sample n xs
  | length xs <= n = xs
  | otherwise = [xs !! (i * length xs `div` n) | i <- [0 .. n - 1]]
