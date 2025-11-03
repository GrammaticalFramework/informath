module Utils where

import qualified Data.Set as S

setnub :: Ord a => [a] -> [a]
setnub = S.toList . S.fromList

