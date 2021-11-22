
module AOC.Common.Numeric (
    PascalTable(..)
  , buildPascalTable
  , binom
  , pascals
  ) where

import qualified Data.Vector.Unboxed as VU
import           Data.List (scanl')

pascals :: [[Int]]
pascals = repeat 1 : map (tail . scanl' (+) 0) pascals

data PascalTable = PascalTable 
    { ptWidth :: !Int
    , ptDepth :: !Int
    , ptTable :: !(VU.Vector Int)
    }
  deriving Show

buildPascalTable
    :: Int            -- ^ num diagonals
    -> Int            -- ^ depth of diagonal
    -> PascalTable
buildPascalTable n d = PascalTable
    { ptWidth = n
    , ptDepth = d
    , ptTable = VU.fromList . concat $
        take d <$> take n pascals
    }

binom
    :: PascalTable
    -> Int          -- ^ diagonal
    -> Int          -- ^ index
    -> Int
binom PascalTable{..} i j = ptTable VU.! (i * ptDepth + j)


