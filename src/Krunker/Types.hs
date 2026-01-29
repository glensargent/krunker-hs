{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Krunker.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data RankedProfile = RankedProfile
  { rankedRegion :: Int,
    rankedMmr :: Int,
    rankedWins :: Int,
    rankedLosses :: Int,
    rankedKills :: Int,
    rankedDeaths :: Int,
    rankedAssists :: Int,
    rankedScore :: Int,
    rankedDamageDone :: Int,
    rankedTimePlayed :: Int
  }
  deriving (Show, Generic)

instance FromJSON RankedProfile where
  parseJSON = withObject "RankedProfile" $ \v ->
    RankedProfile
      <$> v .: "region"
      <*> v .: "mmr"
      <*> v .: "wins"
      <*> v .: "losses"
      <*> v .: "kills"
      <*> v .: "deaths"
      <*> v .: "assists"
      <*> v .: "score"
      <*> v .: "damage_done"
      <*> v .: "time_played"

instance ToJSON RankedProfile

data Player = Player
  { playerName :: Text,
    playerClan :: Text,
    playerVerified :: Bool,
    playerFlag :: Int,
    playerBadges :: [Int],
    playerFollowing :: Int,
    playerFollowers :: Int,
    playerRanked :: [RankedProfile],
    playerKr :: Int,
    playerLevel :: Int,
    playerJunk :: Double,
    playerInventory :: Int,
    playerScore :: Int,
    playerSpk :: Double,
    playerKills :: Int,
    playerDeaths :: Int,
    playerKdr :: Double,
    playerKpg :: Double,
    playerGames :: Int,
    playerWins :: Int,
    playerLosses :: Int,
    playerAssists :: Int,
    playerMelees :: Int,
    playerBeatdowns :: Int,
    playerBullseyes :: Int,
    playerHeadshots :: Int,
    playerLegshots :: Int,
    playerWallbangs :: Int,
    playerShots :: Int,
    playerHits :: Int,
    playerMisses :: Int,
    playerTimePlayed :: Int,
    playerNukes :: Int,
    playerAirdrops :: Int,
    playerAirdropsStolen :: Int,
    playerSlimes :: Int,
    playerJuggernauts :: Int,
    playerJuggernautsKilled :: Int,
    playerWarmachines :: Int,
    playerHackerTagged :: Bool,
    playerCreatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON Player where
  parseJSON = withObject "Player" $ \v ->
    Player
      <$> v .: "player_name"
      <*> v .: "clan"
      <*> v .: "verified"
      <*> v .: "flag"
      <*> v .: "badges"
      <*> v .: "following"
      <*> v .: "followers"
      <*> v .: "ranked"
      <*> v .: "kr"
      <*> v .: "level"
      <*> v .: "junk"
      <*> v .: "inventory"
      <*> v .: "score"
      <*> v .: "spk"
      <*> v .: "kills"
      <*> v .: "deaths"
      <*> v .: "kdr"
      <*> v .: "kpg"
      <*> v .: "games"
      <*> v .: "wins"
      <*> v .: "losses"
      <*> v .: "assists"
      <*> v .: "melees"
      <*> v .: "beatdowns"
      <*> v .: "bullseyes"
      <*> v .: "headshots"
      <*> v .: "legshots"
      <*> v .: "wallbangs"
      <*> v .: "shots"
      <*> v .: "hits"
      <*> v .: "misses"
      <*> v .: "time_played"
      <*> v .: "nukes"
      <*> v .: "airdrops"
      <*> v .: "airdrops_stolen"
      <*> v .: "slimes"
      <*> v .: "juggernauts"
      <*> v .: "juggernauts_killed"
      <*> v .: "warmachines"
      <*> v .: "hacker_tagged"
      <*> v .: "created_at"

instance ToJSON Player

data InventoryItem = InventoryItem
  { inventorySkinIndex :: Int,
    inventoryCount :: Int
  }
  deriving (Show, Generic)

instance FromJSON InventoryItem where
  parseJSON = withObject "InventoryItem" $ \v ->
    InventoryItem
      <$> v .: "skin_index"
      <*> v .: "count"

instance ToJSON InventoryItem

data PlayerMatch = PlayerMatch
  { pmMatchId :: Int,
    pmDate :: Text,
    pmMap :: Int,
    pmDuration :: Int,
    pmSeason :: Int,
    pmRegion :: Int,
    pmKills :: Int,
    pmDeaths :: Int,
    pmAssists :: Int,
    pmScore :: Int,
    pmDamageDone :: Int,
    pmHeadshots :: Int,
    pmAccuracy :: Int,
    pmObjectiveScore :: Int,
    pmKr :: Int,
    pmVictory :: Int,
    pmRoundsWon :: Int,
    pmTeam :: Int,
    pmPlayTime :: Int,
    pmMmr :: Int
  }
  deriving (Show, Generic)

instance FromJSON PlayerMatch where
  parseJSON = withObject "PlayerMatch" $ \v ->
    PlayerMatch
      <$> v .: "match_id"
      <*> v .: "date"
      <*> v .: "map"
      <*> v .: "duration"
      <*> v .: "season"
      <*> v .: "region"
      <*> v .: "kills"
      <*> v .: "deaths"
      <*> v .: "assists"
      <*> v .: "score"
      <*> v .: "damage_done"
      <*> v .: "headshots"
      <*> v .: "accuracy"
      <*> v .: "objective_score"
      <*> v .: "kr"
      <*> v .: "victory"
      <*> v .: "rounds_won"
      <*> v .: "team"
      <*> v .: "play_time"
      <*> v .: "mmr"

instance ToJSON PlayerMatch

data PlayerMatchesResponse = PlayerMatchesResponse
  { pmrPage :: Int,
    pmrPerPage :: Int,
    pmrMatches :: [PlayerMatch]
  }
  deriving (Show, Generic)

instance FromJSON PlayerMatchesResponse where
  parseJSON = withObject "PlayerMatchesResponse" $ \v ->
    PlayerMatchesResponse
      <$> v .: "page"
      <*> v .: "per_page"
      <*> v .: "matches"

instance ToJSON PlayerMatchesResponse

data Post = Post
  { postDate :: Text,
    postText :: Text,
    postVotes :: Int,
    postCommentCount :: Int
  }
  deriving (Show, Generic)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \v ->
    Post
      <$> v .: "date"
      <*> v .: "text"
      <*> v .: "votes"
      <*> v .: "comment_count"

instance ToJSON Post

data PostsResponse = PostsResponse
  { postsPage :: Int,
    postsPerPage :: Int,
    postsPosts :: [Post]
  }
  deriving (Show, Generic)

instance FromJSON PostsResponse where
  parseJSON = withObject "PostsResponse" $ \v ->
    PostsResponse
      <$> v .: "page"
      <*> v .: "per_page"
      <*> v .: "posts"

instance ToJSON PostsResponse

data MatchParticipant = MatchParticipant
  { mpPlayerName :: Text,
    mpKills :: Int,
    mpDeaths :: Int,
    mpAssists :: Int,
    mpScore :: Int,
    mpDamageDone :: Int,
    mpHeadshots :: Int,
    mpAccuracy :: Int,
    mpObjectiveScore :: Int,
    mpVictory :: Int,
    mpRoundsWon :: Int,
    mpTeam :: Int,
    mpPlayTime :: Int
  }
  deriving (Show, Generic)

instance FromJSON MatchParticipant where
  parseJSON = withObject "MatchParticipant" $ \v ->
    MatchParticipant
      <$> v .: "player_name"
      <*> v .: "kills"
      <*> v .: "deaths"
      <*> v .: "assists"
      <*> v .: "score"
      <*> v .: "damage_done"
      <*> v .: "headshots"
      <*> v .: "accuracy"
      <*> v .: "objective_score"
      <*> v .: "victory"
      <*> v .: "rounds_won"
      <*> v .: "team"
      <*> v .: "play_time"

instance ToJSON MatchParticipant

data Match = Match
  { matchId :: Int,
    matchDate :: Text,
    matchMap :: Int,
    matchDuration :: Int,
    matchSeason :: Int,
    matchRegion :: Int,
    matchParticipants :: [MatchParticipant]
  }
  deriving (Show, Generic)

instance FromJSON Match where
  parseJSON = withObject "Match" $ \v ->
    Match
      <$> v .: "match_id"
      <*> v .: "date"
      <*> v .: "map"
      <*> v .: "duration"
      <*> v .: "season"
      <*> v .: "region"
      <*> v .: "participants"

instance ToJSON Match

data Clan = Clan
  { clanName :: Text,
    clanOwnerName :: Text,
    clanScore :: Int,
    clanRank :: Int,
    clanMemberCount :: Int,
    clanCreatedAt :: Text,
    clanDiscord :: Text
  }
  deriving (Show, Generic)

instance FromJSON Clan where
  parseJSON = withObject "Clan" $ \v ->
    Clan
      <$> v .: "name"
      <*> v .: "owner_name"
      <*> v .: "score"
      <*> v .: "rank"
      <*> v .: "member_count"
      <*> v .: "created_at"
      <*> v .: "discord"

instance ToJSON Clan

data ClanMember = ClanMember
  { cmPlayerName :: Text,
    cmRole :: Int
  }
  deriving (Show, Generic)

instance FromJSON ClanMember where
  parseJSON = withObject "ClanMember" $ \v ->
    ClanMember
      <$> v .: "player_name"
      <*> v .: "role"

instance ToJSON ClanMember

data ClanMembersResponse = ClanMembersResponse
  { cmrPage :: Int,
    cmrPerPage :: Int,
    cmrMembers :: [ClanMember]
  }
  deriving (Show, Generic)

instance FromJSON ClanMembersResponse where
  parseJSON = withObject "ClanMembersResponse" $ \v ->
    ClanMembersResponse
      <$> v .: "page"
      <*> v .: "per_page"
      <*> v .: "members"

instance ToJSON ClanMembersResponse

data LeaderboardEntry = LeaderboardEntry
  { lePosition :: Int,
    lePlayerName :: Text,
    leMmr :: Int,
    leWins :: Int,
    leLosses :: Int,
    leKills :: Int,
    leDeaths :: Int,
    leAssists :: Int,
    leScore :: Int,
    leDamageDone :: Int
  }
  deriving (Show, Generic)

instance FromJSON LeaderboardEntry where
  parseJSON = withObject "LeaderboardEntry" $ \v ->
    LeaderboardEntry
      <$> v .: "position"
      <*> v .: "player_name"
      <*> v .: "mmr"
      <*> v .: "wins"
      <*> v .: "losses"
      <*> v .: "kills"
      <*> v .: "deaths"
      <*> v .: "assists"
      <*> v .: "score"
      <*> v .: "damage_done"

instance ToJSON LeaderboardEntry

data LeaderboardResponse = LeaderboardResponse
  { lrPage :: Int,
    lrPerPage :: Int,
    lrSeason :: Int,
    lrRegion :: Int,
    lrEntries :: [LeaderboardEntry]
  }
  deriving (Show, Generic)

instance FromJSON LeaderboardResponse where
  parseJSON = withObject "LeaderboardResponse" $ \v ->
    LeaderboardResponse
      <$> v .: "page"
      <*> v .: "per_page"
      <*> v .: "season"
      <*> v .: "region"
      <*> v .: "entries"

instance ToJSON LeaderboardResponse

data GameMap = GameMap
  { gmMapId :: Int,
    gmName :: Text,
    gmDescription :: Text,
    gmCreatorName :: Text,
    gmVotes :: Int,
    gmGameplays :: Int,
    gmPlaytime :: Int,
    gmCategory :: Int,
    gmCreatedAt :: Text,
    gmUpdatedAt :: Text,
    gmLeaderboardType :: Text,
    gmLeaderboardOrder :: Int
  }
  deriving (Show, Generic)

instance FromJSON GameMap where
  parseJSON = withObject "GameMap" $ \v ->
    GameMap
      <$> v .: "map_id"
      <*> v .: "name"
      <*> v .: "description"
      <*> v .: "creator_name"
      <*> v .: "votes"
      <*> v .: "gameplays"
      <*> v .: "playtime"
      <*> v .: "category"
      <*> v .: "created_at"
      <*> v .: "updated_at"
      <*> v .: "leaderboard_type"
      <*> v .: "leaderboard_order"

instance ToJSON GameMap

data MapLeaderboardEntry = MapLeaderboardEntry
  { mlePosition :: Int,
    mlePlayerName :: Text,
    mleValue :: Int,
    mleDate :: Text
  }
  deriving (Show, Generic)

instance FromJSON MapLeaderboardEntry where
  parseJSON = withObject "MapLeaderboardEntry" $ \v ->
    MapLeaderboardEntry
      <$> v .: "position"
      <*> v .: "player_name"
      <*> v .: "value"
      <*> v .: "date"

instance ToJSON MapLeaderboardEntry

data MapLeaderboardResponse = MapLeaderboardResponse
  { mlrPage :: Int,
    mlrPerPage :: Int,
    mlrMapName :: Text,
    mlrLeaderboardType :: Text,
    mlrLeaderboardOrder :: Int,
    mlrEntries :: [MapLeaderboardEntry]
  }
  deriving (Show, Generic)

instance FromJSON MapLeaderboardResponse where
  parseJSON = withObject "MapLeaderboardResponse" $ \v ->
    MapLeaderboardResponse
      <$> v .: "page"
      <*> v .: "per_page"
      <*> v .: "map_name"
      <*> v .: "leaderboard_type"
      <*> v .: "leaderboard_order"
      <*> v .: "entries"

instance ToJSON MapLeaderboardResponse

data MapLeaderboardPlayerResponse = MapLeaderboardPlayerResponse
  { mlprMapName :: Text,
    mlprLeaderboardType :: Text,
    mlprLeaderboardOrder :: Int,
    mlprPlayerName :: Text,
    mlprPosition :: Int,
    mlprValue :: Int,
    mlprDate :: Text
  }
  deriving (Show, Generic)

instance FromJSON MapLeaderboardPlayerResponse where
  parseJSON = withObject "MapLeaderboardPlayerResponse" $ \v ->
    MapLeaderboardPlayerResponse
      <$> v .: "map_name"
      <*> v .: "leaderboard_type"
      <*> v .: "leaderboard_order"
      <*> v .: "player_name"
      <*> v .: "position"
      <*> v .: "value"
      <*> v .: "date"

instance ToJSON MapLeaderboardPlayerResponse

data Mod = Mod
  { modId :: Int,
    modName :: Text,
    modDescription :: Text,
    modCreatorName :: Text,
    modVotes :: Int,
    modFeatured :: Bool,
    modVersion :: Int,
    modCreatedAt :: Text,
    modUpdatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON Mod where
  parseJSON = withObject "Mod" $ \v ->
    Mod
      <$> v .: "mod_id"
      <*> v .: "name"
      <*> v .: "description"
      <*> v .: "creator_name"
      <*> v .: "votes"
      <*> v .: "featured"
      <*> v .: "version"
      <*> v .: "created_at"
      <*> v .: "updated_at"

instance ToJSON Mod

data ModsResponse = ModsResponse
  { modsPage :: Int,
    modsPerPage :: Int,
    modsMods :: [Mod]
  }
  deriving (Show, Generic)

instance FromJSON ModsResponse where
  parseJSON = withObject "ModsResponse" $ \v ->
    ModsResponse
      <$> v .: "page"
      <*> v .: "per_page"
      <*> v .: "mods"

instance ToJSON ModsResponse

data MarketListing = MarketListing
  { mlPrice :: Int,
    mlSellerName :: Text,
    mlListedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON MarketListing where
  parseJSON = withObject "MarketListing" $ \v ->
    MarketListing
      <$> v .: "price"
      <*> v .: "seller_name"
      <*> v .: "listed_at"

instance ToJSON MarketListing

data MarketOwner = MarketOwner
  { moPlayerName :: Text,
    moCount :: Int
  }
  deriving (Show, Generic)

instance FromJSON MarketOwner where
  parseJSON = withObject "MarketOwner" $ \v ->
    MarketOwner
      <$> v .: "player_name"
      <*> v .: "count"

instance ToJSON MarketOwner

data PriceHistory = PriceHistory
  { phDate :: Text,
    phAveragePrice :: Double,
    phSales :: Int
  }
  deriving (Show, Generic)

instance FromJSON PriceHistory where
  parseJSON = withObject "PriceHistory" $ \v ->
    PriceHistory
      <$> v .: "date"
      <*> v .: "average_price"
      <*> v .: "sales"

instance ToJSON PriceHistory

data MarketResponse = MarketResponse
  { mrSkinIndex :: Int,
    mrTotalListings :: Int,
    mrLowestPrice :: Int,
    mrAveragePrice :: Double,
    mrTotalCirculating :: Int,
    mrListings :: [MarketListing],
    mrOwners :: [MarketOwner],
    mrPriceHistory :: [PriceHistory]
  }
  deriving (Show, Generic)

instance FromJSON MarketResponse where
  parseJSON = withObject "MarketResponse" $ \v ->
    MarketResponse
      <$> v .: "skin_index"
      <*> v .: "total_listings"
      <*> v .: "lowest_price"
      <*> v .: "average_price"
      <*> v .: "total_circulating"
      <*> v .: "listings"
      <*> v .: "owners"
      <*> v .: "price_history"

instance ToJSON MarketResponse
