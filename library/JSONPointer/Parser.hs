-- |
-- Attoparsec parser.
module JSONPointer.Parser
(
  Parser,
  run,
  jsonPointer,
)
where

import JSONPointer.Prelude
import Data.Attoparsec.Text
import qualified Data.Text
import qualified JSONPointer.Model as Model


-- |
-- Uses the parser to parse the input text in whole.
run :: Parser a -> Text -> Either Text a
run parser input =
  either (Left . fromString) Right $
  parseOnly (parser <* endOfInput) input

-- |
-- JSON Pointer parser.
jsonPointer :: Parser Model.JSONPointer
jsonPointer =
  foldMany referenceToken

referenceToken :: Parser Model.JSONPointer
referenceToken =
  char '/' *> (keyToModel <$> key)
  where
    key =
      Data.Text.pack <$> referenceTokenChars
    keyToModel !text =
      Model.atIndexOrKey (textToIndexMaybe text) text
    textToIndexMaybe =
      either (const Nothing) Just .
      parseOnly parser
      where
        parser =
          decimal <* endOfInput

-- |
-- Reference token chars as per the definition in the JSON Pointer spec.
referenceTokenChars :: Parser [Char]
referenceTokenChars =
  many $ escapeSequence <|> notChar '/'
  where
    escapeSequence =
      char '~' *> (tilde <|> slash <|> other)
      where
        tilde =
          char '0' $> '~'
        slash =
          char '1' $> '/'
        other =
          fail "Illegal escape sequence"

foldMany :: (Alternative m, Monoid a) => m a -> m a
foldMany consume =
  step <|> end
  where
    step =
      mappend <$> consume <*> foldMany consume
    end =
      pure mempty

-- |
-- Note: this parser does not consume any input.
shouldFail :: (Alternative m, Monad m) => m a -> m ()
shouldFail p =
  join $ (p *> pure empty) <|> pure (pure ())
