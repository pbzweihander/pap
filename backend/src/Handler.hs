module Handler (
  Context (..),
  getEvent,
  getRooms,
  getRoom,
  postRoom,
  putRoom,
  deleteRoom,
)
where

import Control.Concurrent.Chan
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Db
import GHC.Generics (Generic)
import qualified Network.HTTP.Req as R
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource

data Context scheme = Context
  { db :: Db
  , chan :: Chan ServerEvent
  , slackWebhookUrl :: R.Url scheme
  }

pushEvent :: Chan ServerEvent -> IO ()
pushEvent chan = do
  time <- getPOSIXTime
  writeChan chan (ServerEvent Nothing Nothing [BSB.string8 . show $ time])

getEvent :: Context scheme -> Application
getEvent ctx req respond = do
  eventSourceAppChan ctx.chan req respond

getRooms :: Context scheme -> Application
getRooms ctx _ respond = do
  rooms <- listRooms ctx.db
  respond $ responseLBS status200 [("content-type", "application/json")] $ encode rooms

newtype GetRoomReq = GetRoomReq {user :: User}
  deriving (Generic)

instance FromJSON GetRoomReq

getRoom :: Context scheme -> Application
getRoom ctx rawReq respond = do
  case L.find (\(k, _) -> k == "user") $ queryString rawReq of
    Just (_, Just user) -> do
      room <- getJoinedRoom ctx.db $ decodeUtf8 user
      respond $ responseLBS status200 [("content-type", "application/json")] $ encode room
    _ -> respond $ responseLBS status422 [] ""

data PostRoomReq = PostRoomReq {name :: RoomName, user :: User, notice :: T.Text}
  deriving (Generic)

instance FromJSON PostRoomReq

postRoom :: Context scheme -> Application
postRoom ctx rawReq respond = do
  rawReqBody <- strictRequestBody rawReq
  let maybeReq = decode rawReqBody :: Maybe PostRoomReq
  case maybeReq of
    Just req -> do
      if T.length req.user <= 30 && T.length req.name <= 30
        then do
          createRoom ctx.db req.name req.user req.notice
          res <- respond $ responseLBS status200 [] ""
          pushEvent ctx.chan
          _ <- R.runReq R.defaultHttpConfig $ do
            R.req
              R.POST
              ctx.slackWebhookUrl
              ( R.ReqBodyBs $
                  BS.toStrict $
                    BSB.toLazyByteString $
                      BSB.byteString "{\"text\":\""
                        <> BSB.byteString (encodeUtf8 req.user)
                        <> "님이 "
                        <> BSB.byteString (encodeUtf8 req.name)
                        <> " 방을 만들었어요.\",\"blocks\":[{\"type\":\"header\",\"text\":{\"type\":\"plain_text\",\"text\":\""
                        <> BSB.byteString (encodeUtf8 req.name)
                        <> "\",\"emoji\":false}},{\"type\":\"section\",\"text\":{\"type\":\"plain_text\",\"text\":\""
                        <> BSB.byteString (encodeUtf8 req.notice)
                        <> "\",\"emoji\":false}},{\"type\":\"context\",\"elements\":[{\"type\":\"plain_text\",\"text\":\""
                        <> BSB.byteString (encodeUtf8 req.user)
                        <> "\",\"emoji\":false}]},{\"type\":\"divider\"}]}"
              )
              R.ignoreResponse
              (R.header "content-type" "application/json")
          pure res
        else
          respond $ responseLBS status400 [] ""
    Nothing -> respond $ responseLBS status422 [] ""

data PutRoomReq = PutRoomReq {name :: RoomName, user :: User}
  deriving (Generic)

instance FromJSON PutRoomReq

putRoom :: Context scheme -> Application
putRoom ctx rawReq respond = do
  rawReqBody <- strictRequestBody rawReq
  let maybeReq = decode rawReqBody :: Maybe PutRoomReq
  case maybeReq of
    Just req -> do
      if T.length req.user <= 30
        then do
          joinRoom ctx.db req.name req.user
          resp <- respond $ responseLBS status200 [] ""
          pushEvent ctx.chan
          pure resp
        else
          respond $ responseLBS status400 [] ""
    Nothing -> respond $ responseLBS status422 [] ""

newtype DeleteRoomReq = DeleteRoomReq {user :: User}
  deriving (Generic)

instance FromJSON DeleteRoomReq

deleteRoom :: Context scheme -> Application
deleteRoom ctx rawReq respond = do
  rawReqBody <- strictRequestBody rawReq
  let maybeReq = decode rawReqBody :: Maybe DeleteRoomReq
  case maybeReq of
    Just req -> do
      leaveRoom ctx.db req.user
      resp <- respond $ responseLBS status200 [] ""
      pushEvent ctx.chan
      pure resp
    Nothing -> respond $ responseLBS status422 [] ""
