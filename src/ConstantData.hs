module ConstantData where

import qualified Data.Map as M

main = interact (show . getConstantData) -- to test


-- storing data about constants in a separate file, to be read by RunInformath
-- each line is: constant, info items separated by spaces

type ConstantData = M.Map DkId ConstantInfo

type GFCat = String
type GFFun = String
type DkId = String
type Prover = String

data ConstantInfo = ConstantInfo {
  gffun :: GFFun,
  gfcat :: GFCat,
  symbolic :: [(GFFun, GFCat)],
  synonyms :: [(GFFun, GFCat)],
  conversions :: [(Prover, DkId)]
  }
  deriving Show

getConstantData :: String -> ConstantData
getConstantData = foldr addConstantData M.empty . map words . reverse . lines

addConstantData :: [String] -> ConstantData -> ConstantData
addConstantData ws cdata = case ws of
  "#GF" : dkid : gffun : gfcat : _ ->
    M.insert dkid (ConstantInfo gffun gfcat [] [] []) cdata
  "#SYMBOLIC" : dkid : gffun : gfcat : _ ->
    M.adjust (\info -> info{symbolic = (gffun, gfcat):(symbolic info)}) dkid cdata
  "#SYNONYM" : dkid : gffun : gfcat : _ ->
    M.adjust (\info -> info{synonyms = (gffun, gfcat):(synonyms info)}) dkid cdata
  _ -> cdata
