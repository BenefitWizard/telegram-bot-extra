{-| Polling runner for Telegram bots.

Long-polls @'getUpdates'@ and dispatches each 'Update' to a raw
@'Update' -> 'IO' ()@ action fire-and-forget, advancing the Telegram
offset correctly. Blocks forever; stop by cancelling the thread that
runs it.
-}
module Telegram.Bot.Extra.Polling
  ( -- * Runner
    runPollingBot
    -- * Offset helper (pure, unit-testable)
  , nextOffset
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Servant.Client (ClientEnv, runClientM)
import Telegram.Bot.API.GettingUpdates
  ( GetUpdatesRequest (..)
  , Update (..)
  , UpdateId (..)
  , defGetUpdates
  , getUpdates
  )
import Telegram.Bot.API.MakingRequests (responseResult)
import Telegram.Bot.API.Types (Seconds (..))
import Telegram.Bot.Simple.BotApp.Internal (asyncLink)

-- | The next @getUpdates@ offset for a page of 'Update's:
-- @maximum(updateUpdateId) + 1@, or 'Nothing' if the page is empty.
--
-- Uses 'maximum' (not 'last') so the result is correct regardless of the
-- order Telegram returns updates in. 'UpdateId' is a newtype around 'Int'
-- that is not 'Enum' or 'Num', so we use 'coerce' to lift @(+1)@ over it.
nextOffset :: [Update] -> Maybe UpdateId
nextOffset []      = Nothing
nextOffset updates =
  Just (coerce ((+1) :: Int -> Int) (maximum (map updateUpdateId updates)))

-- | Long-poll Telegram, dispatching every 'Update' to the given action.
--
-- Dispatch is fire-and-forget: each update is handed to its own
-- 'asyncLink'-spawned thread. A per-update action exception is handed to the
-- supplied @onActionError@ fallback (which decides whether to log, metric,
-- or ignore it); the exception does NOT stop the polling loop. On a network
-- error ('runClientM' returns 'Left'), the runner sleeps for @retryDelay@
-- microseconds and retries the same request. Captures the 'ClientEnv' so the
-- user action can call @telegram-bot-api@ directly. Blocks forever; stop by
-- cancelling the thread that runs it.
--
-- [Design note vs @telegram-bot-simple@] @telegram-bot-simple@'s
-- @startPolling@ hardcodes @print@-on-error and a fixed @1s@ delay applied
-- after /every/ iteration. Here both the error fallback and the delay are
-- explicit parameters, and the delay is applied only on a network error:
-- the @getUpdates@ long-poll (25 s) already paces the happy path, so an
-- unconditional inter-iteration delay would only stall delivery.
--
-- /Known limitation:/ in-flight action threads are NOT cancelled when the
-- runner is cancelled — they run to completion. Graceful shutdown is out
-- of scope.
runPollingBot
  :: MonadIO m
  => (SomeException -> IO ())  -- ^ @onActionError@: fallback invoked when the user action throws.
  -> Int                        -- ^ @retryDelay@: microseconds to sleep before retrying on a network error (cf. 'threadDelay').
  -> ClientEnv
  -> (Update -> IO ())
  -> m a
runPollingBot onActionError retryDelay clientEnv action = liftIO (go initialReq)
  where
    initialReq = defGetUpdates { getUpdatesTimeout = Just (Seconds 25) }
    -- Run the user action, routing any exception to the caller-supplied
    -- fallback so a single failing update cannot kill the polling loop
    -- (fire-and-forget).
    safeAction u = action u `catch` onActionError
    go req = do
      eRes <- runClientM (getUpdates req) clientEnv
      case eRes of
        Left _err  -> do
          threadDelay retryDelay
          go req
        Right resp -> do
          let ups = responseResult resp
          mapM_ (void . asyncLink . safeAction) ups
          go req { getUpdatesOffset = maybe (getUpdatesOffset req) Just (nextOffset ups) }
