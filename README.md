# krunker-hs

Haskell client library for the Krunker Developer API.

## Installation

Add to your `build-depends` in your `.cabal` file:

```cabal
build-depends:
  krunker-hs
```

## Usage

### Creating a client

```haskell
import Krunker

main :: IO ()
main = do
  client <- newClient "your-api-key"
  -- use client for requests
```

### Making requests

All API functions return `IO (Either ApiError a)`.

```haskell
-- Get a player profile
result <- getPlayer client "PlayerName"
case result of
  Left err -> print err
  Right player -> print (playerName player, playerLevel player)

-- Get a clan
result <- getClan client "ClanName"

-- Get a match by ID
result <- getMatch client 123456789
```

### Paginated endpoints

Endpoints with pagination take `Maybe Int` for optional parameters. Use `Nothing` for defaults.

```haskell
-- Get first page of player matches (default)
result <- getPlayerMatches client "PlayerName" Nothing Nothing

-- Get page 2
result <- getPlayerMatches client "PlayerName" (Just 2) Nothing

-- Get page 1 of a specific season
result <- getPlayerMatches client "PlayerName" (Just 1) (Just 12)

-- Get clan members, page 3
result <- getClanMembers client "ClanName" (Just 3)

-- Get leaderboard for region 3 (Europe), page 1
result <- getLeaderboard client 3 (Just 1)
```

### Working with responses

Response types use record syntax. Access fields directly:

```haskell
result <- getPlayer client "PlayerName"
case result of
  Left err -> putStrLn $ "Error: " ++ show (errorStatus err)
  Right player -> do
    putStrLn $ "Name: " ++ show (playerName player)
    putStrLn $ "Level: " ++ show (playerLevel player)
    putStrLn $ "K/D: " ++ show (playerKdr player)
    putStrLn $ "Ranked profiles: " ++ show (length (playerRanked player))
```

### Error handling

`ApiError` contains the HTTP status and response body:

```haskell
data ApiError = ApiError
  { errorStatus :: Status
  , errorResponse :: ByteString
  }
```

