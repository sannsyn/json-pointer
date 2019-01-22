module JSONPointer.Model
(
  JSONPointer,
  run,
  atIndexOrKey,
)
where

import JSONPointer.Prelude hiding (or)
import qualified Data.Attoparsec.Text
import qualified Data.Text


-- |
-- A model of JSONPointer
-- represented in terms of a monoid.
newtype JSONPointer =
  JSONPointer (forall m. Monoid m => (Maybe Int -> Text -> m) -> m)

instance Semigroup JSONPointer where
  (<>) = mappend

instance Monoid JSONPointer where
  {-# INLINE mempty #-}
  mempty =
    JSONPointer $ const mempty
  {-# INLINE mappend #-}
  mappend (JSONPointer fn1) (JSONPointer fn2) =
    JSONPointer $ \handler -> fn1 handler <> fn2 handler

instance Show JSONPointer where
  showsPrec _ (JSONPointer impl) =
    appEndo $
    impl (\_ text -> Endo (showString "/" . showString (Data.Text.unpack text)))

-- |
-- Given a JSON Pointer specification and a function,
-- which interprets a possible index or a textual key into a monoid,
-- results in such a monoid.
{-# INLINE run #-}
run :: Monoid m => JSONPointer -> (Maybe Int -> Text -> m) -> m
run (JSONPointer fn) =
  fn

-- |
-- Constructs JSON Pointer from a possible array index and a textual key.
{-# INLINE atIndexOrKey #-}
atIndexOrKey :: Maybe Int -> Text -> JSONPointer
atIndexOrKey index key =
  JSONPointer $ \handler -> handler index key
