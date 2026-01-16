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
  case method of
    GET -> case uriQuery uri of
      s@(_:_) -> outputHTML (unlines (startpage ["GETT", uriScheme uri, uriPath uri,  "<br>", processQuery (uriQuery uri)]))
      _ -> outputHTML (unlines (startpage []))
    POST -> outputHTML (unlines (startpage ["POSTT", uriScheme uri, uriPath uri, body, "<br>", processQuery body]))


-- URI: https://hackage.haskell.org/package/network-uri-2.6.4.2/docs/Network-URI.html

processQuery s = case (break (=='=') (dropWhile (=='?') s)) of
  ("dk", _:exp) -> "(" ++ unEscapeString exp ++ ")"
  ("num", _:exp) -> show (cube (read ( unEscapeString exp ) ))
  (_, _:v) -> reverse (unEscapeString v)
  _ -> unwords (replicate 5 s)

cube :: Int -> Int
cube n = n*n*n

startpage x = [
  "<html>",
  "<body>",
  "START",
  "<form method=\"post\" action=\"/\">",
    "<label for=\"num\">Number:</label>",
    "<input type=\"text\" name=\"num\" /><br />",
    "<input type=\"submit\" />",
  "</form>"] ++ x ++ [
  "</body>",
  "</html>"
  ]
