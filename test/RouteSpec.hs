{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RouteSpec (spec) where

import Data.Proxy
import Data.Text (pack)
import Servant
import Test.Hspec
import Test.Hspec.Wai

import Telegram.Bot.API (Token (..))
import Telegram.Bot.Extra.BotRoute

type TestApi = ForToken Token :> "info" :> Get '[JSON] String

testApi :: Proxy TestApi
testApi = Proxy

testServer :: Server TestApi
testServer = pure "ok"

testApp :: Token -> Application
testApp token = serveWithContext testApi ctx testServer
  where
    ctx = fromToken token `asTypeOf` (undefined :: ForToken Token) :. EmptyContext

spec :: Spec
spec = describe "BotRoute" $ do
    describe "ForToken route formation" $ do
        with (return $ testApp $ Token $ pack "bot123456:ABC") $ do
            it "forms the route based on Token value from Context" $ do
                get "/bot123456:ABC/info" `shouldRespondWith` 200
                get "/info" `shouldRespondWith` 404
                get "/bot123456/info" `shouldRespondWith` 404

        with (return $ testApp $ Token $ pack "myBotToken") $ do
            it "handles myBotToken correctly" $ do
                get "/myBotToken/info" `shouldRespondWith` 200
                get "/otherToken/info" `shouldRespondWith` 404

        with (return $ testApp $ Token $ pack "otherToken") $ do
            it "handles otherToken correctly" $ do
                get "/otherToken/info" `shouldRespondWith` 200
                get "/myBotToken/info" `shouldRespondWith` 404
