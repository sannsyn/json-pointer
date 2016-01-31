module JSONPointer
(
  Model.JSONPointer,
  run,
  parse,
  parser,
)
where

import JSONPointer.Prelude
import qualified JSONQuery
import qualified JSONPointer.Parser as Parser
import qualified JSONPointer.Model as Model


-- |
-- Given a JSON Pointer and a value query to apply
-- at the location pointed to,
-- produces a query of the context value.
{-# INLINE run #-}
run :: Model.JSONPointer -> JSONQuery.Value a -> JSONQuery.Value a
run =
  Model.run

-- |
-- Parses the input text into the pointer.
{-# INLINE parse #-}
parse :: Text -> Either Text Model.JSONPointer
parse input =
  Parser.run Parser.jsonPointer input

-- |
-- A composable Attoparsec parser.
{-# INLINE parser #-}
parser :: Parser.Parser Model.JSONPointer
parser =
  Parser.jsonPointer
