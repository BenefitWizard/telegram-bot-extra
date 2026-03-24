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
