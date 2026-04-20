module Telegram.Bot.Extra.CommandParser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isAlpha, isSpace)
import Data.Text (Text)

-- | Parse zero or more space-separated non-whitespace words.
words :: Parser [Text]
words = (sepBy1 word (many1 space)) <|> pure []
  where
    -- \| Parse one maximal non-whitespace fragment.
    word = takeWhile1 (\c -> not (isSpace c))

-- | Parse a Telegram bot mention suffix such as @botname.
botName :: Parser Text
botName = char '@' *> takeWhile1 isAlpha

-- | Parse a slash command with the expected name and ignore any trailing bot mention suffixes.
command :: Text -> Parser Text
command commandName = char '/' *> string commandName <* skipMany botName

-- | Parse a required command argument after at least one separating space.
commandArg :: Parser a -> Parser a
commandArg argParser = skipMany1 space *> argParser

-- | Parse an optional command argument after at least one separating space.
mCommandArg :: Parser a -> Parser (Maybe a)
mCommandArg argParser = skipMany1 space *> (Just <$> argParser) <|> pure Nothing

-- | Parse a named slash command and return the remaining text as a required argument, defaulting to the empty text when absent.
commandWithArg :: Text -> Parser Text
commandWithArg commandName = command commandName *> (commandArg takeText <|> pure "")

-- | Parse a named slash command and return the remaining text as an optional argument.
commandWithMArg :: Text -> Parser (Maybe Text)
commandWithMArg commandName = command commandName *> mCommandArg takeText

-- | Parse an arbitrary slash command and return its trailing argument text when present.
unknownCommand :: Parser Text
unknownCommand = char '/' *> takeWhile1 isAlpha *> skipMany botName *> (skipMany1 space *> takeText <|> pure "")

-- | Parse an arbitrary slash command and preserve any trailing argument text.
unknownCommandWithArgs :: Parser Text
unknownCommandWithArgs = char '/' *> takeWhile1 isAlpha *> skipMany botName *> (skipMany1 space *> takeText <|> pure "")

-- | Run a text parser and fall back to the supplied default action when parsing fails.
parseTextUpdate :: p -> Parser p -> Text -> p
parseTextUpdate defaultAction parsers text = case parseOnly parsers text of
    Left _ -> defaultAction
    Right action -> action
