-- | More efficient data fetching using deduplication, batching, and caching.
module Control.Applicative.Fetch
  ( Fetch
  , fetch
  , runFetch

  , class Resource
  , resource
  ) where

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set as Set
import Partial.Unsafe (unsafePartial)
import Prelude

--------------------------------------------------------------------------------

-- | A computation that fetches data for some set of keys. Fetch computations
-- | can be combined using `(<$>)` and `(<*>)`.
data Fetch k r a =
  Fetch (Set k) (Map k r -> a)

derive instance functorFetch :: Functor (Fetch k r)

instance applyFetch :: (Ord k) => Apply (Fetch k r) where
  apply (Fetch fks ff) (Fetch xks xf) =
    Fetch (Set.union fks xks) (ff <*> xf)

instance applicativeFetch :: (Ord k) => Applicative (Fetch k r) where
  pure = Fetch Set.empty <<< const

-- | A computation that fetches data for some key.
fetch :: ∀ k r. Ord k => k -> Fetch k r r
fetch k = Fetch (Set.singleton k) (unsafePartial fromJust <<< Map.lookup k)

-- | Perform a fetch computation with some resource in some context.
runFetch :: ∀ k r f a. Resource k r f => Functor f => Fetch k r a -> f a
runFetch (Fetch ks f) = f <$> resource ks

--------------------------------------------------------------------------------

-- | A resource implements the fetching of data.
-- |
-- | Instances must satisfy the following laws in addition to the `Ord` laws:
-- |
-- | - Key preservation: every key in the input must appear in the output.
class Ord k <= Resource k r f | k r -> f where
  resource :: Set k -> f (Map k r)
