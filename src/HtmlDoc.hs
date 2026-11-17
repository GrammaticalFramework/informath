-- | HTML documents of informalizations (RunInformath -to-html-doc): for each
-- declaration its source (with -from-formalism), its Dedukti, its readings in
-- the languages of -to-lang, its formula, and the proof text of a theorem, with
-- folds (scripts/proof_folds.py), in the style of the index pages of
-- mathlib-informath, Edukera and HoTT.

module HtmlDoc (
  DocEntry(..),
  htmlPage,
  latexTableRows,
  languageName,
  escapeHtml
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

-- | One declaration of the document.
data DocEntry = DocEntry {
  deName     :: String,
  deKind     :: String,
  deDoc      :: String,                -- ^ a docstring, if any
  deSource   :: Maybe (String, String), -- ^ the formalism and the source text
  deDedukti  :: String,
  deReadings :: [(String, String)],    -- ^ language code, reading (text with LaTeX formulas)
  deFormula  :: String,
  deProofs   :: [(String, String)]     -- ^ language code, proof table as HTML
  }

languageName :: String -> String
languageName c = maybe c id (lookup c [
  ("Eng", "English"), ("Fre", "French"), ("Ger", "German"), ("Swe", "Swedish"),
  ("Cze", "Czech"), ("Fin", "Finnish"), ("Pol", "Polish"), ("Latex", "LaTeX")])

escapeHtml :: String -> String
escapeHtml = concatMap esc where
  esc c = case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    _ -> [c]

-- | A reading as HTML for MathJax: its formulas kept, its lists as HTML lists.
readingHtml :: String -> String
readingHtml = items . replace "\\end{itemize}" "</ul>" . replace "\\begin{itemize}" "<ul>" . bold . code . escapeHtml
 where
  -- a label or a name verbatim: \texttt{\detokenize{x}} or \texttt{x}
  code s = case s of
    _ | "\\texttt{\\detokenize{" `isPrefixOf` s, (b, '}' : '}' : rest) <- break (== '}') (drop 20 s) ->
          "<code>" ++ b ++ "</code>" ++ code rest
    _ | "\\texttt{" `isPrefixOf` s, (b, '}' : rest) <- break (== '}') (drop 8 s) ->
          "<code>" ++ replace "\\_" "_" b ++ "</code>" ++ code rest
    c : cs -> c : code cs
    [] -> []
  items s = case s of
    _ | "\\item" `isPrefixOf` s -> "<li>" ++ items (dropWhile isSpace (drop 5 s))
    c : cs -> c : items cs
    [] -> []
  bold s = case s of
    _ | "\\textbf{" `isPrefixOf` s, (b, '}' : rest) <- break (== '}') (drop 8 s) -> "<b>" ++ b ++ "</b>" ++ bold rest
    c : cs -> c : bold cs
    [] -> []

replace :: String -> String -> String -> String
replace old new s = case s of
  _ | old `isPrefixOf` s -> new ++ replace old new (drop (length old) s)
  c : cs -> c : replace old new cs
  [] -> []

-- | The tables of a LaTeX proof text of RunInformath -proof-text or -tactic-proof
-- (one for each language), each as rows (number, sentence, justification).
latexTableRows :: String -> [[[String]]]
latexTableRows s = case breakOn "\\begin{tabular}" s of
  Just rest ->
    let body = drop 1 (dropWhile (/= '\n') rest)   -- the column specification
        (table, after) = maybe (body, "") id (splitOn' "\\end{tabular}" body)
    in [row r | r <- splitRows table, isRow r] : latexTableRows after
  Nothing -> []
 where
  breakOn pat str = case str of
    _ | pat `isPrefixOf` str -> Just (drop (length pat) str)
    _ : cs -> breakOn pat cs
    [] -> Nothing
  splitOn' pat str = go "" str where
    go acc t = case t of
      _ | pat `isPrefixOf` t -> Just (reverse acc, drop (length pat) t)
      c : cs -> go (c : acc) cs
      [] -> Nothing
  -- rows end with \\ and a newline
  splitRows t = case splitOn' "\\\\\n" t of
    Just (r, rest) -> r : splitRows rest
    Nothing -> [t | any (not . isSpace) t]
  isRow r = case dropWhile isSpace r of
    c : _ -> isDigit c && '&' `elem` r
    [] -> False
  row r =
    let (n, rest) = break (== '&') (dropWhile isSpace r)
        rest' = drop 1 rest
        -- the justification after the last &, the sentence before it
        (j, sentence) = let (rj, rs) = break (== '&') (reverse rest') in (reverse rj, reverse (drop 1 rs))
    in [trim n, trim sentence, trim j]
  trim = trimEnd . dropWhile isSpace
  trimEnd = reverse . dropWhile isSpace . reverse

style :: String
style = unlines [
  " body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:0 auto;max-width:1100px;padding:1.5rem;line-height:1.5;color:#222;background:#fff}",
  " header{border-bottom:1px solid #ddd;margin-bottom:1rem}",
  " .entry{border:1px solid #e3e6ee;border-radius:6px;padding:.6rem .9rem;margin:0 0 1rem}",
  " .name{font-family:monospace;font-weight:700}",
  " .k{font-size:.75em;margin-left:.6rem;padding:0 .4rem;border-radius:3px;background:#eef2fa;color:#345}",
  " .doc{color:#555;font-size:.92em;margin:.3rem 0}",
  " .row{display:grid;grid-template-columns:7.5rem 1fr;gap:.6rem;margin:.35rem 0}",
  " .lab{color:#666;font-size:.85em;padding-top:.15rem}",
  " pre{margin:0;white-space:pre-wrap;font-size:.85em;background:#f7f8fb;padding:.35rem .5rem;border-radius:4px}",
  " summary{cursor:pointer;color:#0645ad;font-size:.85em}",
  " table.proof{border-collapse:collapse}",
  " table.proof td{padding:.1rem .5rem;vertical-align:top}",
  " table.proof td.n{color:#888;text-align:right}",
  " table.proof td.s{padding-left:calc(.5rem + 1.2rem * var(--depth,0));border-left:0}",
  " table.proof td.j{color:#666;font-size:.85em;white-space:nowrap}",
  " .missing{color:#999}"
  ]

-- | The page: title, MathJax macros (as a JSON object), the CSS and JavaScript
-- of the folds, and the entries.
htmlPage :: String -> String -> String -> String -> String -> [DocEntry] -> String
htmlPage title intro macrosJson foldCss foldJs entries = unlines [
  "<!DOCTYPE html>",
  "<html lang=\"en\"><head><meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>" ++ escapeHtml title ++ "</title>",
  "<script>",
  "window.MathJax = {",
  "  tex: { inlineMath: [['$','$']], displayMath: [['$$','$$']], macros: " ++ macrosJson ++ " },",
  "  options: { skipHtmlTags: ['script','noscript','style','textarea','pre','code'] }",
  "};",
  "</script>",
  "<script async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>",
  "<style>" ++ style ++ foldCss ++ "</style></head><body>",
  "<header><h1>" ++ escapeHtml title ++ "</h1><p>" ++ intro ++ "</p></header>",
  concatMap entryHtml entries,
  "<script>" ++ foldJs ++ "</script>",
  "</body></html>"
  ]

entryHtml :: DocEntry -> String
entryHtml e = concat [
  "<div class=\"entry\" id=\"", escapeHtml (deName e), "\">",
  "<span class=\"name\">", escapeHtml (deName e), "</span>",
  "<span class=\"k\">", escapeHtml (deKind e), "</span>",
  if null (deDoc e) then "" else "<div class=\"doc\">" ++ escapeHtml (deDoc e) ++ "</div>",
  concat [row (formalismLabel f) ("<pre>" ++ escapeHtml src ++ "</pre>") | Just (f, src) <- [deSource e]],
  row "Dedukti" (concat ["<details><summary>", lineCount (length (lines (deDedukti e))), "</summary><pre>",
                        escapeHtml (deDedukti e), "</pre></details>"]),
  concat [row (languageName l) (text r) | (l, r) <- deReadings e],
  row "Logic" (text (deFormula e)),
  concat [row (if l == "Eng" then "Proof" else "Proof, " ++ languageName l) p | (l, p) <- deProofs e],
  "</div>\n"
  ]
 where
  row lab content = concat ["<div class=\"row\"><div class=\"lab\">", lab, "</div><div>", content, "</div></div>"]
  text t = if all isSpace t then "<div class=\"missing\">(none)</div>" else "<div>" ++ readingHtml t ++ "</div>"
  lineCount n = show n ++ (if n == 1 then " line" else " lines")
  formalismLabel f = maybe f id (lookup f [("agda", "Agda"), ("lean", "Lean"), ("rocq", "Rocq")])
