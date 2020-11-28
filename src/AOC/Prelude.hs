-- |
-- Module      : AOC.Prelude
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Custom Prelude while developing challenges.  Ideally, once challenges
-- are completed, an import to this module would be replaced with explicit
-- ones for future readers.
--


module AOC.Prelude (
    module P
  ) where

import           AOC.Common                as P
import           AOC.Solver                as P
import           AOC.Util                  as P
import           Control.Applicative       as P
import           Control.DeepSeq           as P
import           Control.Lens              as P hiding (uncons)
import           Control.Monad             as P
import           Control.Monad.Except      as P
import           Control.Monad.State       as P
import           Data.Bifunctor            as P
import           Data.Char                 as P
import           Data.Containers.ListUtils as P
import           Data.Either               as P
import           Data.Finite               as P (Finite, packFinite, getFinite, modulo, finites)
import           Data.Foldable             as P
import           Data.Function             as P
import           Data.Functor              as P
import           Data.IntMap               as P (IntMap)
import           Data.IntMap.NonEmpty      as P (NEIntMap)
import           Data.IntSet               as P (IntSet)
import           Data.IntSet.NonEmpty      as P (NEIntSet)
import           Data.Kind                 as P
import           Data.List                 as P
import           Data.List.NonEmpty        as P (NonEmpty(..), nonEmpty)
import           Data.List.Split           as P
import           Data.Map                  as P (Map)
import           Data.Map.NonEmpty         as P (NEMap)
import           Data.Maybe                as P
import           Data.Ord                  as P
import           Data.Semigroup            as P
import           Data.Set                  as P (Set)
import           Data.Set.NonEmpty         as P (NESet)
import           Data.Text                 as P (Text)
import           Data.Text.Encoding        as P (encodeUtf8, decodeUtf8)
import           Data.Time                 as P hiding (Day)
import           Data.Traversable          as P
import           Data.Tuple                as P
import           Data.Void                 as P
import           Debug.Trace               as P
import           GHC.Generics              as P (Generic)
import           Numeric.Natural           as P
import           Text.Printf               as P
import           Text.Read                 as P (readMaybe)
