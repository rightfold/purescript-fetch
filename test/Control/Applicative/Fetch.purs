module Test.Control.Applicative.Fetch
  ( spec
  ) where

import Control.Applicative.Fetch (class Resource, Fetch, Memoize(..), fetch, fetchAla, runFetch)
import Control.Monad.RWS.Trans (evalRWST)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

--------------------------------------------------------------------------------

spec :: ∀ r. Spec r Unit
spec = describe "Control.Applicative.Fetch" do
  voidSpec
  trivialSpec
  batchSpec
  memoizeSpec

--------------------------------------------------------------------------------

newtype VoidKey = VoidKey Void
newtype VoidResource = VoidResource Void

derive newtype instance eqVoidKey :: Eq VoidKey
derive newtype instance ordVoidKey :: Ord VoidKey

instance resourceVoid :: Applicative f => Resource VoidKey VoidResource f where
  resource = const (pure Map.empty)

voidSpec :: ∀ r. Spec r Unit
voidSpec = it "Void" do
  result <- runFetch (pure unit :: Fetch VoidKey VoidResource Unit)
  result `shouldEqual` unit

--------------------------------------------------------------------------------

newtype TrivialKey = TrivialKey Int
newtype TrivialResource = TrivialResource Int

derive newtype instance eqTrivialKey :: Eq TrivialKey
derive newtype instance ordTrivialKey :: Ord TrivialKey

derive newtype instance eqTrivialResource :: Eq TrivialResource
instance showTrivialResource :: Show TrivialResource where
  show (TrivialResource k) = "(TrivialResource " <> show k <> ")"

instance resourceTrivial :: Applicative f =>
  Resource TrivialKey TrivialResource f where
  resource = pure <<< foldl (\m k -> Map.insert k (go k) m) Map.empty
    where go (TrivialKey k) = TrivialResource (k * 2)

trivialSpec :: ∀ r. Spec r Unit
trivialSpec = it "Trivial" do
  result <- runFetch $ (/\) <$> fetch (TrivialKey 1) <*> fetch (TrivialKey 2)
  result `shouldEqual` (TrivialResource 2 /\ TrivialResource 4)

--------------------------------------------------------------------------------

newtype BatchKey = BatchKey Int
newtype BatchResource = BatchResource Unit

derive newtype instance eqBatchKey :: Eq BatchKey
derive newtype instance ordBatchKey :: Ord BatchKey
instance showBatchKey :: Show BatchKey where
  show (BatchKey k) = "(BatchKey " <> show k <> ")"

derive newtype instance eqBatchResource :: Eq BatchResource
instance showBatchResource :: Show BatchResource where
  show (BatchResource k) = "(BatchResource " <> show k <> ")"

instance resourceBatch :: Monad f =>
  Resource BatchKey BatchResource (WriterT (Set BatchKey) f) where
  resource ks = do
    Writer.tell ks
    pure $ foldl (\m k -> Map.insert k (BatchResource unit) m) Map.empty ks

batchSpec :: ∀ r. Spec r Unit
batchSpec = it "Batch" do
  result <- runWriterT <<< runFetch $
    (/\) <$> fetch (BatchKey 1) <*> fetch (BatchKey 2)
  fst result `shouldEqual` (BatchResource unit /\ BatchResource unit)
  snd result `shouldEqual` Set.fromFoldable [BatchKey 1, BatchKey 2]

--------------------------------------------------------------------------------

newtype MemoizeKey = MemoizeKey Int
newtype MemoizeResource = MemoizeResource Int

derive newtype instance eqMemoizeKey :: Eq MemoizeKey
derive newtype instance ordMemoizeKey :: Ord MemoizeKey
instance showMemoizeKey :: Show MemoizeKey where
  show (MemoizeKey k) = "(MemoizeKey " <> show k <> ")"

derive newtype instance eqMemoizeResource :: Eq MemoizeResource
instance showMemoizeResource :: Show MemoizeResource where
  show (MemoizeResource k) = "(MemoizeResource " <> show k <> ")"

instance resourceMemoize :: (MonadWriter (Set MemoizeKey) f) =>
  Resource MemoizeKey MemoizeResource f where
  resource ks = do
    Writer.tell ks
    pure $ foldl (\m k -> Map.insert k (go k) m) Map.empty ks
    where go (MemoizeKey k) = MemoizeResource (k * 2)

memoizeSpec :: ∀ r. Spec r Unit
memoizeSpec = it "Memoize" do
  result <- evalRWST <@> unit <@> Map.empty $ do
    a <- runFetch $ (/\) <$> fetch' (MemoizeKey 1) <*> fetch' (MemoizeKey 2)
    b <- runFetch $ (/\) <$> fetch' (MemoizeKey 2) <*> fetch' (MemoizeKey 3)
    pure $ a /\ b
  fst result `shouldEqual`
    ( (MemoizeResource 2 /\ MemoizeResource 4) /\
      (MemoizeResource 4 /\ MemoizeResource 6) )
  snd result `shouldEqual`
    Set.fromFoldable [MemoizeKey 1, MemoizeKey 2, MemoizeKey 3]
  where fetch' = fetchAla Memoize
