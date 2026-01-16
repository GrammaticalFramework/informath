--module InformathServer where
module Main where

import Environment
import InformathAPI

import Network.HTTP
import Network.URI

-- https://github.com/krangelov/http-slim
-- https://hackage.haskell.org/package/network-uri-2.6.4.2/docs/Network-URI.html
--  1076  cabal v1-build
--  1077  ./dist/build/Koe/Koe
--  http://localhost:8080/?dk=Even

main = do
  xx <- getArgs
  informathServer xx

informathServer args = do
  env <- readEnv args 
  putStrLn "http://localhost:8080/?dk= or ?tex="
  simpleServer (Just 8080) Nothing (informathResponse env)

myresponse req@(Request uri method headers body) =
  outputText (unlines ["HELLLO", uriScheme uri, uriPath uri, processQuery (uriQuery uri)])

informathResponse env s = case (break (=='=') s) of
  (_:"dk", _:exp) -> 
     let mmo = parseDeduktiModuleErrorFree s
         results = maybe [] (processDeduktiModule env) mo
     in unlines (printResults env (concatMap (printGenResult env) results))
  (_:"tex", _:exp) -> 
      let results = processLatex env s
      in unlines (printResults env (concatMap (printParseResult env) results))
  _ -> "usage: ?tex=<text> | ?dk=<dedukti>"
