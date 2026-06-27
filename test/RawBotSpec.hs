{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RawBotSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, throwIO)
import Data.Aeson (decode, encode)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Servant
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.Wai

import Telegram.Bot.API (Token (..), Update)
import Telegram.Bot.Extra.BotRoute
    ( botApi
    , fromToken
    , serverWithAction
    )

-- | Minimal valid 'Update' (only @update_id@ is required by Telegram's schema).
mkUpdate :: Int -> Update
mkUpdate n = case decode (LBS.pack ("{\"update_id\":" ++ show n ++ "}")) of
    Just u  -> u
    Nothing -> error "mkUpdate: failed to decode minimal Update"

-- | A no-op 'onActionError' fallback for tests whose action never throws.
ignoreError :: SomeException -> IO ()
ignoreError _ = pure ()

-- | Serve the 'BotApi' with a raw fire-and-forget dispatch of the given action.
rawApp :: (SomeException -> IO ()) -> Token -> (Update -> IO ()) -> Application
rawApp onActionError tk action =
  serveWithContext (botApi @Token) ctx (serverWithAction onActionError action)
  where
    ctx = fromToken @Token tk :. EmptyContext

testToken :: Token
testToken = Token "bot123456:ABC"

-- | One second in microseconds; bounds the 'readMVar' wait so a broken
-- dispatch fails the test instead of hanging it.
oneSecond :: Int
oneSecond = 1000000

spec :: Spec
spec = describe "Raw webhook handler (serverWithAction)" $ do
    with (pure (rawApp ignoreError testToken (\_ -> pure ()))) $ do
        it "answers 200 promptly on a minimal Update (fire-and-forget)" $ do
            request "POST" "/bot123456:ABC"
                [("Content-Type", "application/json")]
                (encode (mkUpdate 1))
                `shouldRespondWith` 200

    box <- runIO newEmptyMVar
    with (pure (rawApp ignoreError testToken (\_ -> putMVar box ()))) $ do
        it "dispatches the Update to the action (side effect observed)" $ do
            request "POST" "/bot123456:ABC"
                [("Content-Type", "application/json")]
                (encode (mkUpdate 2))
                `shouldRespondWith` 200
            result <- liftIO (timeout oneSecond (readMVar box))
            liftIO (result `shouldBe` Just ())

    errBox <- runIO newEmptyMVar
    with (pure (rawApp (\e -> putMVar errBox e) testToken (\_ -> throwIO (userError "webhook boom")))) $ do
        it "routes an action exception to onActionError and still answers 200" $ do
            -- The request still succeeds (the action runs fire-and-forget)...
            request "POST" "/bot123456:ABC"
                [("Content-Type", "application/json")]
                (encode (mkUpdate 3))
                `shouldRespondWith` 200
            -- ...and the exception is routed to the caller-supplied fallback.
            result <- liftIO (timeout oneSecond (readMVar errBox))
            liftIO (result `shouldSatisfy` maybe False (("webhook boom" `isInfixOf`) . show))
