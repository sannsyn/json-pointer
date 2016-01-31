module JSONPointer.Model
where

import JSONPointer.Prelude hiding (or)
import qualified JSONQuery
import qualified Data.Attoparsec.Text


-- |
-- A model of JSONPointer
-- represented in terms of a JSONQuery spec.
newtype JSONPointer =
  JSONPointer (forall a. JSONQuery.Value a -> JSONQuery.Value a)

instance Monoid JSONPointer where
  {-# INLINE mempty #-}
  mempty =
    JSONPointer id
  {-# INLINE mappend #-}
  mappend (JSONPointer fn1) (JSONPointer fn2) =
    JSONPointer $ fn1 . fn2

-- |
-- Given a JSON Pointer and a value parser to apply
-- at the location pointed to,
-- produces a parser of the context value.
{-# INLINE run #-}
run :: JSONPointer -> JSONQuery.Value a -> JSONQuery.Value a
run (JSONPointer fn) =
  fn

or :: JSONPointer -> JSONPointer -> JSONPointer
or (JSONPointer a) (JSONPointer b) =
  JSONPointer $ \c -> a c <|> b c

atIndexOrKey :: Text -> JSONPointer
atIndexOrKey indexOrKey =
  parsingIndex indexOrKey (atKey indexOrKey) (\n -> or (atIndex n) (atKey indexOrKey))
  where
    parsingIndex :: Text -> a -> (Int -> a) -> a
    parsingIndex input empty pure =
      either (const empty) pure $
      Data.Attoparsec.Text.parseOnly parser input
      where
        parser =
          Data.Attoparsec.Text.decimal <* Data.Attoparsec.Text.endOfInput

atIndex :: Int -> JSONPointer
atIndex index =
  JSONPointer $ JSONQuery.arrayLookups . JSONQuery.atIndex index

atKey :: Text -> JSONPointer
atKey key =
  JSONPointer $ JSONQuery.objectLookups . JSONQuery.atKey key
