{-# LANGUAGE OverloadedStrings #-}

module Krunker.Client where

import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data Client = Client
  { clientBaseUrl :: Text,
    clientManager :: Manager,
    clientApiKey :: Text
  }

defaultBaseUrl :: Text
defaultBaseUrl = "https://gapi.svc.krunker.io/api"

newClient :: Text -> IO Client
newClient apiKey = do
  manager <- newManager tlsManagerSettings
  pure $ Client defaultBaseUrl manager apiKey
