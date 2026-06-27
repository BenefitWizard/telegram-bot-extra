{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Bot.Extra.BotRoute where

import Data.Typeable

import Servant
import Servant.Server.Internal.Router

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text
import GHC.Conc (readTVarIO)
import Servant.Client (ClientEnv)
import Telegram.Bot.API (Token (..), Update)
import Telegram.Bot.Simple (BotApp (..))
import Telegram.Bot.Simple.BotApp.Internal (BotEnv (..), asyncLink, defaultBotEnv, issueAction, processActionsIndefinitely)

fromToken :: Token -> ForToken a
fromToken (Token tk) = TaggedToken tk

newtype ForToken a = TaggedToken Text

instance
  ( HasServer api ctx
  , HasContextEntry ctx (ForToken a)
  ) =>
  HasServer (ForToken a :> api) ctx
  where
  type ServerT (ForToken a :> api) m = ServerT api m

  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

  route _ ctx server = pathRouter tk $ route (Proxy @api) ctx server
   where
    TaggedToken tk = getContextEntry ctx :: ForToken a

type BotApi tokenType = ForToken tokenType :> ReqBody '[JSON] Update :> Post '[JSON] ()

botApi :: Proxy (BotApi tokenType)
botApi = Proxy

makeBotHandler :: (MonadIO m) => ClientEnv -> BotApp state update -> m (Update -> Servant.Handler ())
makeBotHandler clientEnv botApp = do
  botEnv <- liftIO $ defaultBotEnv botApp clientEnv
  void $ liftIO $ processActionsIndefinitely botApp botEnv
  pure $ server botApp botEnv

server :: BotApp model action -> BotEnv model action -> Server (BotApi tokenType)
server BotApp{..} botEnv@BotEnv{..} =
  updateHandler
 where
  updateHandler :: Update -> Handler ()
  updateHandler update = liftIO $ handleUpdate update
  handleUpdate update = liftIO . void . asyncLink $ do
    maction <- botAction update <$> readTVarIO botModelVar
    case maction of
      Nothing -> pure ()
      Just action -> issueAction botEnv (Just update) (Just action)

-- | Build a Servant 'Server' for the 'BotApi' directly from a raw
-- @'Update' -> 'IO' ()@ action, bypassing 'BotApp'/'BotEnv' entirely.
-- Each incoming 'Update' is dispatched fire-and-forget in a linked
-- background thread via 'asyncLink', so the webhook answers @200 OK@
-- without waiting for the action to finish. A per-update action exception
-- is handed to the supplied @onActionError@ fallback (which decides whether
-- to log, metric, or ignore it); it does not fail the request. Spawned
-- action threads run to completion even if the handler thread is later torn
-- down — graceful shutdown is out of scope.
serverWithAction
  :: (SomeException -> IO ())  -- ^ @onActionError@: fallback invoked when the user action throws.
  -> (Update -> IO ())
  -> Server (BotApi tokenType)
serverWithAction onActionError action = updateHandler
  where
    updateHandler :: Update -> Handler ()
    updateHandler update = liftIO . void . asyncLink $ safeAction update
    -- Run the user action, routing any exception to the caller-supplied
    -- fallback (fire-and-forget).
    safeAction u = action u `catch` onActionError

-- | Construct a webhook handler function from a raw @'Update' -> 'IO' ()@
-- action, without a 'BotApp' or 'ClientEnv'. Suitable for users who want to
-- keep full access to @telegram-bot-api@ by capturing whatever they need
-- (e.g. a 'Servant.Client.ClientEnv') inside the 'IO' action. The
-- @onActionError@ fallback is invoked if the action throws; see
-- 'serverWithAction'.
makeRawBotHandler
  :: MonadIO m
  => (SomeException -> IO ())  -- ^ @onActionError@: fallback invoked when the user action throws.
  -> (Update -> IO ())
  -> m (Update -> Servant.Handler ())
makeRawBotHandler onActionError action = pure $ serverWithAction onActionError action
