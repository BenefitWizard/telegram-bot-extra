# Changelog for `telegram-bot-extra`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `Telegram.Bot.Extra.BotRoute.serverWithAction` and
  `makeRawBotHandler` — raw webhook entry points that build a Servant
  `Server` (resp. an `Update -> Handler ()` function) directly from a raw
  `Update -> IO ()` action, bypassing `BotApp`/`BotEnv` entirely.
- New module `Telegram.Bot.Extra.Polling` exporting
  `runPollingBot :: MonadIO m => (SomeException -> IO ()) -> Int -> ClientEnv -> (Update -> IO ()) -> m a`
  (long-polls `getUpdates`) and the pure offset-advancement helper
  `nextOffset :: [Update] -> Maybe UpdateId`.
- New module `Telegram.Bot.Extra.Headless` exporting
  `runHeadlessBot :: MonadIO m => (SomeException -> IO ()) -> TBQueue Update -> (Update -> IO ()) -> m a`
  (drains a bounded `TBQueue`, no `ClientEnv`/network) and the test/replay
  helper `feedUpdates :: TBQueue Update -> [Update] -> IO ()`.
- `stm` library dependency (for `TBQueue` in the headless runner).
- All three runners (webhook, polling, headless) now share the same
  fire-and-forget dispatch model via `asyncLink` and the same
  `Update -> IO ()` action type; all block forever and are stopped by
  cancelling the thread that runs them.

### Changed

- All three raw runners now take an explicit
  `onActionError :: SomeException -> IO ()` fallback parameter: when the user
  action throws, this is invoked and the exception is contained, so a single
  failing update never stops the runner (previously the action's exceptions
  were re-thrown into the runner thread via `asyncLink`'s `link`, which could
  silently kill the loop). `runPollingBot` additionally takes a `retryDelay ::
  Int` (microseconds) parameter controlling the sleep before retrying on a
  network error. This is more configurable than `telegram-bot-simple`'s
  `startPolling`, which hardcodes `print`-on-error and a fixed 1 s delay.

## 0.1.0.0 - YYYY-MM-DD
