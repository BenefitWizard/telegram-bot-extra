module HeadlessSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTBQueueIO
  , newTVarIO
  , readTVar
  )
import Control.Exception (SomeException, throwIO)
import Data.Aeson (decode)
import Data.List (isInfixOf, sort)
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Timeout (timeout)
import Test.Hspec
import Telegram.Bot.API (Update, UpdateId (..), updateUpdateId)
import Telegram.Bot.Extra.Headless (feedUpdates, runHeadlessBot)

-- | Construct a minimal 'Update' carrying only an @update_id@. The Telegram
-- 'Update' record has every other field optional, so a two-key JSON object is
-- enough to round-trip through 'decode' (verified in GHCi).
mkUpdate :: Int -> Update
mkUpdate n = case decode (LBS.pack ("{\"update_id\":" ++ show n ++ "}")) of
  Just u  -> u
  Nothing -> error "mkUpdate: failed to decode minimal Update"

-- | A no-op 'onActionError' fallback for tests whose action never throws.
ignoreActionError :: SomeException -> IO ()
ignoreActionError _ = pure ()

-- | Poll a 'TVar' list until it holds at least @n@ elements, or the time budget
-- (microseconds) is exhausted. Always returns — never blocks forever — so a
-- broken dispatch turns into an assertion failure rather than a hung test.
waitForN :: Int -> Int -> TVar [a] -> IO [a]
waitForN n budget tv
  | budget <= 0 = atomically (readTVar tv)
  | otherwise = do
      xs <- atomically (readTVar tv)
      if length xs >= n
        then pure xs
        else do
          threadDelay step
          waitForN n (budget - step) tv
  where
    step = 5000

spec :: Spec
spec = describe "Headless runner" $ do
    it "dispatches an enqueued update fire-and-forget" $ do
        q   <- newTBQueueIO 16
        box <- newEmptyMVar
        let action = putMVar box
        -- The runner starts against an empty queue and blocks; then we feed.
        withAsync (runHeadlessBot ignoreActionError q action) $ \_ -> do
            feedUpdates q [mkUpdate 7]
            -- Bounded wait: fail rather than hang if dispatch is broken.
            Just received <- timeout 3000000 (readMVar box)
            updateUpdateId received `shouldBe` UpdateId 7

    it "dispatches multiple enqueued updates" $ do
        q         <- newTBQueueIO 16
        collected <- newTVarIO []
        let action u = atomically (modifyTVar' collected (updateUpdateId u :))
        withAsync (runHeadlessBot ignoreActionError q action) $ \_ -> do
            feedUpdates q [mkUpdate 1, mkUpdate 2]
            -- Fire-and-forget dispatch races the action threads; we therefore
            -- poll until both arrive (bounded) and compare as a sorted set.
            ids <- waitForN 2 3000000 collected
            sort ids `shouldBe` [UpdateId 1, UpdateId 2]

    it "does not crash while blocked on an empty queue" $ do
        q   <- newTBQueueIO 16
        box <- newEmptyMVar
        let action = putMVar box
        withAsync (runHeadlessBot ignoreActionError q action) $ \_ -> do
            -- Runner is blocked here on the empty queue. Let it wait.
            threadDelay 50000
            -- If the runner had crashed/exited, this read would time out.
            feedUpdates q [mkUpdate 42]
            Just received <- timeout 3000000 (readMVar box)
            updateUpdateId received `shouldBe` UpdateId 42

    it "survives an action that throws and routes the exception to onActionError" $ do
        q    <- newTBQueueIO 16
        box  <- newEmptyMVar      -- successful dispatch (update 5)
        errs <- newEmptyMVar      -- exception captured by the fallback (update 99)
        -- The action throws for update 99, but must still handle update 5.
        let onActionError = putMVar errs
            action u = case updateUpdateId u of
                UpdateId 99 -> throwIO (userError "boom")
                _           -> putMVar box u
        withAsync (runHeadlessBot onActionError q action) $ \_ -> do
            -- Feed the throwing update. Its exception must be routed to the
            -- caller-supplied onActionError fallback (NOT re-thrown into the
            -- drain loop). Waiting on the captured exception is also the
            -- synchronization point — no arbitrary delay needed.
            feedUpdates q [mkUpdate 99]
            Just caught <- timeout 3000000 (readMVar errs)
            show caught `shouldSatisfy` ("boom" `isInfixOf`)
            -- The loop survived: a subsequent update is still dispatched.
            feedUpdates q [mkUpdate 5]
            Just received <- timeout 3000000 (readMVar box)
            updateUpdateId received `shouldBe` UpdateId 5
