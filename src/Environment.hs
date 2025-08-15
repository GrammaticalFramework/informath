module Environment where

import ConstantData
import PGF
import qualified Data.Map as M
import Data.List (isPrefixOf)

data Env = Env {
 flags :: [String],
 constantData :: ConstantData,
 lookBackData :: M.Map String String,  -- from GFFun to DkId ---- and to more info?
 specialConversions :: [String],
 convToAgdaData :: ConstantData,
 convToRocqData :: ConstantData,
 convToLeanData :: ConstantData,
 cpgf :: PGF,
 lang :: Language,
 morpho :: Morpho,
 nbest :: Maybe Int,
 termindex :: [String] -- list of terms replaced by \INDEXEDTERM{ i }
 }

ifFlag x env = elem x (flags env)

ifv env act = if (ifFlag "-v" env) then act else return ()

flagValue flag dfault ff = case [f | f <- ff, isPrefixOf flag (tail f)] of
  f:_ -> drop (length flag + 2) f   -- -<flag>=<value>
  _ -> dfault
