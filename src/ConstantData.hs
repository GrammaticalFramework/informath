module ConstantData where

import qualified Data.Map as M

-- storing data about constants in a separate file, to be read by RunInformath
-- each line is: constant, info items separated by spaces
-- the Project label is used so that all data can be stored globally and filtered if requested

type ConstantData = M.Map DkId ConstantInfo

type GFCat = String
type GFFun = String
type DkId = String
type Project = String

data Combination =
    ComALL
  | ComNONE
  | ComONLY [Int] -- which arguments to keep
 deriving Show
 
data ConstantInfo =
    BASE GFCat GFFun                      -- constant in BaseConstants.dk
  | ALIAS Project DkId Combination        -- alias for a BASE constant in Project
  | NEW Project GFCat GFFun Combination   -- new constant in Project
  | COERCION Project                      -- coercion, to be peeled away with all args except last
 deriving Show

string2constantData mproj =
  M.fromList .
  filter (isFor mproj) .
  map mkConstantInfo .
  filter (not . null) .
  map words .
  lines

isFor mproj info = case mproj of
  Just proj -> case info of
    (_, ALIAS p _ _) -> p == proj
    (_, NEW p _ _ _) -> p == proj
    (_, COERCION p) -> p == proj
    _ -> True
  _ -> True

mkConstantInfo words = case words of
  c : "BASE" : cat : fun : _ -> (c, BASE cat fun)
  c : "ALIAS" : proj : dkid : ws -> (c, ALIAS proj dkid (mkCombination ws))
  c : "NEW" : proj : cat : fun : ws -> (c, NEW proj cat fun (mkCombination ws))
  c : "COERCION" : proj : _ -> (c, COERCION proj)
  _ -> error $ "ill-formed constant data: " ++ unwords words

mkCombination ws = case ws of
  [] -> ComALL
  ["NONE"] -> ComNONE
  _ -> ComONLY (map read ws)

applyCombination com xs = case com of
  ComALL -> xs
  ComNONE -> []
  ComONLY ks -> [xs !! k | k <- ks]

