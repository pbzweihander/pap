module Lib (
  main,
) where

import Control.Concurrent (newChan)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.Functor ((<&>))
import Data.Text (pack)
import Db (newDb)
import Handler
import Network.HTTP.Req (useURI)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Text.URI (mkURI)
import WaiAppStatic.Storage.Embedded (embeddedSettings)

frontend :: [(FilePath, ByteString)]
frontend = $(embedDir "../frontend/dist")

app :: Context scheme -> Application
app ctx req respond =
  case pathInfo req of
    ["api", "event"] -> case requestMethod req of
      "GET" -> getEvent ctx req respond
      _ -> respond $ responseLBS status405 [] ""
    ["api", "room"] -> case requestMethod req of
      "GET" -> getRoom ctx req respond
      "POST" -> postRoom ctx req respond
      "PUT" -> putRoom ctx req respond
      "DELETE" -> deleteRoom ctx req respond
      _ -> respond $ responseLBS status405 [] ""
    ["api", "rooms"] -> case requestMethod req of
      "GET" -> getRooms ctx req respond
      _ -> respond $ responseLBS status405 [] ""
    _ -> (staticApp $ embeddedSettings frontend) req respond

main :: IO ()
main = do
  db <- newDb
  chan <- newChan
  rawSlackWebhookUrl <- getEnv "SLACK_WEBHOOK_URL" <&> pack
  slackWebhookUrl <- mkURI rawSlackWebhookUrl <&> useURI
  case slackWebhookUrl of
    Just (Left (url, _)) -> do
      run 3000 $ app $ Context{db = db, chan = chan, slackWebhookUrl = url}
    Just (Right (url, _)) -> do
      run 3000 $ app $ Context{db = db, chan = chan, slackWebhookUrl = url}
    _ -> error "invalid SLACK_WEBHOOK_URL environment variable"
