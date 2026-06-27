{-| Headless (queue-fed) runner for Telegram bots.

Reads 'Update's from a bounded 'TBQueue' (STM) and dispatches each to a raw
@'Update' -> 'IO' ()@ action fire-and-forget via 'asyncLink'. Requires no
network and no 'ClientEnv' — ideal for tests, simulation, and replay of
recorded updates.
-}
module Telegram.Bot.Extra.Headless
  ( -- * Runner
    runHeadlessBot
    -- * Test/replay helper
  , feedUpdates
  ) where

import Control.Concurrent.STM
  ( TBQueue
  , atomically
  , readTBQueue
  , writeTBQueue
  )
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Telegram.Bot.API (Update)
import Telegram.Bot.Simple.BotApp.Internal (asyncLink)

-- | Drain a bounded 'TBQueue' of 'Update's, dispatching each to the given
-- action fire-and-forget via 'asyncLink'. Dispatch is fire-and-forget: a
-- per-update action exception is handed to the supplied @onActionError@
-- fallback (which decides whether to log, metric, or ignore it), so a single
-- failing update does NOT stop the loop. Blocks when the queue is empty
-- (no busy-wait); a bounded queue gives the producer natural backpressure.
-- Blocks forever; stop by cancelling the thread that runs it.
--
-- Note: in-flight action threads spawned via 'asyncLink' are NOT cancelled
-- when the runner is cancelled — they run to completion. Graceful shutdown
-- is out of scope.
runHeadlessBot
  :: MonadIO m
  => (SomeException -> IO ())  -- ^ @onActionError@: fallback invoked when the user action throws.
  -> TBQueue Update
  -> (Update -> IO ())
  -> m a
runHeadlessBot onActionError queue action = liftIO go
  where
    -- Run the user action, routing any exception to the caller-supplied
    -- fallback so a single failing update cannot kill the drain loop
    -- (fire-and-forget).
    safeAction u = action u `catch` onActionError
    go = do
      update <- atomically (readTBQueue queue)
      void (asyncLink (safeAction update))
      go

-- | Write a list of 'Update's into a 'TBQueue', one atomically per item.
-- Useful for tests and replay: feed constructed/recorded updates and let
-- 'runHeadlessBot' drain them.
feedUpdates :: TBQueue Update -> [Update] -> IO ()
feedUpdates queue = mapM_ (\u -> atomically (writeTBQueue queue u))
