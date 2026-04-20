{-# LANGUAGE OverloadedStrings #-}

module CommandParserSpec (spec) where

import Prelude hiding (words)

import Data.Attoparsec.Text
import Data.Text (Text)
import Test.Hspec

import Telegram.Bot.Extra.CommandParser

parseFully :: Parser a -> Text -> Either String a
parseFully parser = parseOnly (parser <* endOfInput)

spec :: Spec
spec = describe "CommandParser" $ do
    describe "words" $ do
        it "parses empty input as an empty list" $ do
            parseFully words "" `shouldBe` Right []

        it "splits arguments by one or more spaces" $ do
            parseFully words "alpha  beta   gamma" `shouldBe` Right ["alpha", "beta", "gamma"]

        it "keeps non-space punctuation inside words" $ do
            parseFully words "hello, world!" `shouldBe` Right ["hello,", "world!"]

    describe "botName" $ do
        it "parses telegram bot mention suffix" $ do
            parseFully botName "@myBot" `shouldBe` Right "myBot"

        it "fails without leading at sign" $ do
            parseFully botName "myBot" `shouldSatisfy` isLeft

    describe "command" $ do
        it "parses a matching slash command" $ do
            parseFully (command "start") "/start" `shouldBe` Right "start"

        it "accepts a bot mention suffix" $ do
            parseFully (command "start") "/start@myBot" `shouldBe` Right "start"

        it "fails for a different command name" $ do
            parseFully (command "start") "/stop" `shouldSatisfy` isLeft

    describe "commandArg" $ do
        it "parses a required argument after at least one space" $ do
            parseFully (commandArg takeText) "   hello world" `shouldBe` Right "hello world"

        it "fails when the separating space is absent" $ do
            parseFully (commandArg takeText) "hello" `shouldSatisfy` isLeft

    describe "mCommandArg" $ do
        it "parses an optional argument when present" $ do
            parseFully (mCommandArg takeText) "   hello world" `shouldBe` Right (Just "hello world")

        it "returns Nothing when no argument is present" $ do
            parseFully (mCommandArg takeText) "" `shouldBe` Right Nothing

    describe "commandWithArg" $ do
        it "returns the trailing text argument for a known command" $ do
            parseFully (commandWithArg "echo") "/echo hello telegram" `shouldBe` Right "hello telegram"

        it "returns the empty text when argument is absent" $ do
            parseFully (commandWithArg "echo") "/echo" `shouldBe` Right ""

        it "supports bot mentions before the argument" $ do
            parseFully (commandWithArg "echo") "/echo@myBot payload" `shouldBe` Right "payload"

    describe "commandWithMArg" $ do
        it "returns Just trailing text when argument is present" $ do
            parseFully (commandWithMArg "echo") "/echo hello telegram" `shouldBe` Right (Just "hello telegram")

        it "returns Nothing when argument is absent" $ do
            parseFully (commandWithMArg "echo") "/echo" `shouldBe` Right Nothing

    describe "unknownCommand" $ do
        it "parses an arbitrary slash command without arguments" $ do
            parseFully unknownCommand "/custom" `shouldBe` Right ""

        it "returns the trailing text for an arbitrary command with mention" $ do
            parseFully unknownCommand "/custom@myBot some payload" `shouldBe` Right "some payload"

    describe "unknownCommandWithArgs" $ do
        it "preserves trailing text for an arbitrary command" $ do
            parseFully unknownCommandWithArgs "/custom foo bar" `shouldBe` Right "foo bar"

        it "returns empty text when no arguments follow" $ do
            parseFully unknownCommandWithArgs "/custom" `shouldBe` Right ""

    describe "parseTextUpdate" $ do
        it "returns parsed action on successful parse" $ do
            parseTextUpdate "fallback" (commandWithArg "echo") "/echo hi" `shouldBe` "hi"

        it "returns default action on parse failure" $ do
            parseTextUpdate "fallback" (commandWithArg "echo") "echo hi" `shouldBe` "fallback"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
