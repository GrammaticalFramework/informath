module Environment4 where

import Dedukti.AbsDedukti
import PGF
import BuildConstantTable

import Data.List (intersperse, isPrefixOf)
import Data.Char (isDigit)

type Flag = String

data Env = Env {
  flags :: [Flag],
  grammar :: PGF,
  baseConstantModule :: Module,
  constantTable :: ConstantTable,
  langs :: [Language],
  toLang :: Language,
  fromLang :: Language,
  nbestDedukti :: Maybe Int,
  nbestNLG :: Maybe Int,
  scoreWeights :: [Int]
  }

-------------------------------------------
-- low level auxiliaries

informathPrefix = "Informath"
english = "Eng"

relevantLanguages gr args = [
  lang |
    code <- commaSep (argValue "-languages" english args),
    let Just lang = readLanguage (informathPrefix ++ code),
    elem lang (PGF.languages gr)
  ]

argValue flag df args = case [f | f <- args, isPrefixOf flag (tail f)] of
  f:_ -> drop (length flag + 2) f   -- -<flag>=<value>
  _ -> df
  
argValueMaybeInt flag args = case argValue flag "nothing" args of
  v | all isDigit v -> Just (read v :: Int)
  _ -> Nothing

isFlag flag env = elem flag (flags env)

ifArg flag args msg = if elem flag args then putStrLn msg else return ()

inputFileArgs args = case [arg | arg <- args, head arg /= '-'] of
  [arg] -> Just (arg, fileSuffix arg)
  _ -> Nothing

fileSuffix = reverse . takeWhile (/= '.') . reverse

commaSep s = words (map (\c -> if c==',' then ' ' else c) s)

commaSepInts :: String -> [Int]
commaSepInts s =
  let ws = commaSep s
  in if all (all isDigit) ws then map read ws else error ("expected digits found " ++ s)

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
