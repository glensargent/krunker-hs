{-# LANGUAGE OverloadedStrings #-}

module Krunker.Api where

import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Krunker.Client
import Krunker.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status

data ApiError = ApiError
  { errorStatus :: Status,
    errorResponse :: LBS.ByteString
  }
  deriving (Show)

type QueryParams = [(ByteString, Maybe ByteString)]

makeRequest :: Client -> Text -> QueryParams -> IO (Either ApiError LBS.ByteString)
makeRequest client path params = do
  initRequest <- parseRequest $ unpack $ clientBaseUrl client <> path
  let request = setQueryString params $ initRequest
        { requestHeaders = [("X-Developer-API-Key", encodeUtf8 $ clientApiKey client)]
        }
  response <- httpLbs request $ clientManager client
  if statusIsSuccessful $ responseStatus response
    then pure $ Right $ responseBody response
    else pure $ Left $ ApiError (responseStatus response) (responseBody response)

requestAndDecode :: FromJSON a => Client -> Text -> QueryParams -> IO (Either ApiError a)
requestAndDecode client path params = do
  res <- makeRequest client path params
  pure $ res >>= \body ->
    maybe (Left $ ApiError status400 "Failed to decode") Right (decode body)

optionalParam :: ByteString -> Maybe Int -> Maybe (ByteString, Maybe ByteString)
optionalParam key = fmap (\v -> (key, Just $ BS.pack $ show v))

getPlayer :: Client -> Text -> IO (Either ApiError Player)
getPlayer client name = requestAndDecode client ("/player/" <> name) []

getPlayerInventory :: Client -> Text -> IO (Either ApiError [InventoryItem])
getPlayerInventory client name = requestAndDecode client ("/player/" <> name <> "/inventory") []

getPlayerMatches :: Client -> Text -> Maybe Int -> Maybe Int -> IO (Either ApiError PlayerMatchesResponse)
getPlayerMatches client name mPage mSeason =
  requestAndDecode client ("/player/" <> name <> "/matches") $
    catMaybes [optionalParam "page" mPage, optionalParam "season" mSeason]

getPlayerPosts :: Client -> Text -> Maybe Int -> IO (Either ApiError PostsResponse)
getPlayerPosts client name mPage =
  requestAndDecode client ("/player/" <> name <> "/posts") $
    catMaybes [optionalParam "page" mPage]

getMatch :: Client -> Int -> IO (Either ApiError Match)
getMatch client matchId = requestAndDecode client ("/match/" <> pack (show matchId)) []

getClan :: Client -> Text -> IO (Either ApiError Clan)
getClan client name = requestAndDecode client ("/clan/" <> name) []

getClanMembers :: Client -> Text -> Maybe Int -> IO (Either ApiError ClanMembersResponse)
getClanMembers client name mPage =
  requestAndDecode client ("/clan/" <> name <> "/members") $
    catMaybes [optionalParam "page" mPage]

getLeaderboard :: Client -> Int -> Maybe Int -> Maybe Int -> IO (Either ApiError LeaderboardResponse)
getLeaderboard client region mPage mSeason =
  requestAndDecode client ("/leaderboard/" <> pack (show region)) $
    catMaybes [optionalParam "page" mPage, optionalParam "season" mSeason]

getMap :: Client -> Text -> IO (Either ApiError GameMap)
getMap client name = requestAndDecode client ("/map/" <> name) []

getMapLeaderboard :: Client -> Text -> Maybe Int -> IO (Either ApiError MapLeaderboardResponse)
getMapLeaderboard client name mPage =
  requestAndDecode client ("/map/" <> name <> "/leaderboard") $
    catMaybes [optionalParam "page" mPage]

getMapLeaderboardPlayer :: Client -> Text -> Text -> IO (Either ApiError MapLeaderboardPlayerResponse)
getMapLeaderboardPlayer client mapName playerName =
  requestAndDecode client ("/map/" <> mapName <> "/leaderboard/player/" <> playerName) []

getMods :: Client -> Maybe Int -> IO (Either ApiError ModsResponse)
getMods client mPage =
  requestAndDecode client "/mods" $
    catMaybes [optionalParam "page" mPage]

getMod :: Client -> Text -> IO (Either ApiError Mod)
getMod client name = requestAndDecode client ("/mods/" <> name) []

getMarketSkin :: Client -> Int -> Maybe Int -> Maybe Int -> IO (Either ApiError MarketResponse)
getMarketSkin client skinIndex mPage mDays =
  requestAndDecode client ("/market/skin/" <> pack (show skinIndex)) $
    catMaybes [optionalParam "page" mPage, optionalParam "days" mDays]
