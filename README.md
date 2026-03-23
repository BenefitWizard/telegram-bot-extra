# telegram-bot-extra

A Haskell library providing Servant-based routing utilities for Telegram bots using [telegram-bot-simple](https://github.com/fizruk/telegram-bot-simple).

## Overview

This library provides a `ForToken` Servant combinator that dynamically creates routes based on Telegram bot tokens. This is particularly useful for webhook-based Telegram bots where the token is embedded in the URL path for security.

## Features

- **Token-based routing**: Automatically creates URL paths based on bot tokens
- **Servant integration**: Seamless integration with Servant's type-level routing
- **telegram-bot-simple compatible**: Works with `BotApp` from telegram-bot-simple

## Usage

### Basic Setup

```haskell
import Servant
import Telegram.Bot.API (Token(..))
import Telegram.Bot.Extra.BotRoute
import Telegram.Bot.Simple (BotApp, getBotToken)

type MyBotApi = BotApi Token

myServer :: BotApp Model Action -> BotEnv Model Action -> Server MyBotApi
myServer = server

-- Create application with token in context
app :: Token -> BotApp Model Action -> BotEnv Model Action -> Application
app token botApp botEnv = serveWithContext botApi ctx (myServer botApp botEnv)
  where
    ctx = fromToken token :. EmptyContext
```

### How It Works

The `ForToken` combinator extracts the token from the Servant context and uses it as the URL path segment. For a token `bot123456:ABC`, requests would be routed to:

```
POST /bot123456:ABC
```

This matches Telegram's recommended webhook URL pattern where the token is part of the path for additional security.

### Integration with BotApp

The `server` function integrates with `telegram-bot-simple`'s `BotApp`:

```haskell
server :: BotApp model action -> BotEnv model action -> Server (BotApi tokenType)
```

It handles incoming `Update` values asynchronously, processing them through your bot's action handlers.

## API Reference

### Types

- `ForToken a` - A Servant combinator that creates a route segment from a token
- `BotApi tokenType` - A type alias for a standard bot webhook API endpoint

### Functions

- `fromToken :: Token -> ForToken a` - Convert a `Token` to `ForToken` for use in context
- `botApi :: Proxy (BotApi tokenType)` - Proxy for the bot API type
- `server :: BotApp model action -> BotEnv model action -> Server (BotApi tokenType)` - Create a Servant server for your bot

## Installation

Add to your `stack.yaml` or `cabal` file:

```yaml
extra-deps:
  - git: https://github.com/your-org/telegram-bot-extra.git
    commit: <commit-hash>
```

## Dependencies

- `base >=4.7 && <5`
- `servant`
- `servant-server`
- `telegram-bot-api`
- `telegram-bot-simple`
- `text`

## Testing

Run tests with:

```bash
stack test
```

## License

BSD-3-Clause
