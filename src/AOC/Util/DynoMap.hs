module AOC.Util.DynoMap (
    DynoMap(..)
  , lookupDyno
  , lookupDynoWith
  ) where

import           Control.Monad
import           Data.Dynamic
import           Data.Map      (Map)
import           Data.Maybe
import qualified Data.Map      as M

newtype DynoMap = Dyno { runDyno :: Map String Dynamic }
  deriving newtype (Semigroup, Monoid)

-- | Lookup the value at a given key in a 'Dyno'.
--
-- > lookupDyno "hello"
lookupDyno
    :: forall a. Typeable a
    => String
    -> DynoMap
    -> Maybe a
lookupDyno sym = fromDynamic
             <=< M.lookup sym
               . runDyno

-- | Like 'lookupDyno', but with a default value to be returned if the key
-- is not found or has the wrong type.
lookupDynoWith
    :: forall a. (Typeable a)
    => String
    -> a
    -> DynoMap
    -> a
lookupDynoWith sym def = fromMaybe def . lookupDyno sym
