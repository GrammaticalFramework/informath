module Main where

import Network.HTTP
import Network.URI

--  1076  cabal v1-build
--  1077  ./dist/build/Koe/Koe
--  http://localhost:8080/?dk=Even

main = do
--  rsp <- simpleHTTP (getRequest "http://www.haskell.org/")
--  putStr (rspBody rsp)
  simpleServer (Just 8080) Nothing myresponse

--myresponse req@(Request uri@(URI scheme auth path query fragment) method headers body) = outputText body
myresponse req@(Request uri method headers body) =
  outputText (unlines ["HELLLO", uriScheme uri, uriPath uri, processQuery (uriQuery uri)])


-- URI: https://hackage.haskell.org/package/network-uri-2.6.4.2/docs/Network-URI.html

processQuery s = case (break (=='=') s) of
  (_:"dk", _:exp) -> "(" ++ unEscapeString exp ++ ")"
  (_:"num", _:exp) -> show (cube (read ( unEscapeString exp ) ))
  (_, _:v) -> reverse (unEscapeString v)

cube :: Int -> Int
cube n = n*n*n

