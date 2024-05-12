module Handler (
  Context (..),
  getEvent,
  getRooms,
  getRoom,
  postRoom,
  putRoom,
  deleteRoom,
) where

import Control.Concurrent.Chan
import Data.Aeson
import Data.ByteString.Builder (string8)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Db
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource

data Context = Context {db :: Db, chan :: Chan ServerEvent}

pushEvent :: Chan ServerEvent -> IO ()
pushEvent chan = do
  time <- getPOSIXTime
  writeChan chan (ServerEvent Nothing Nothing [string8 . show $ time])

getEvent :: Context -> Application
getEvent ctx req respond = do
  eventSourceAppChan ctx.chan req respond

getRooms :: Context -> Application
getRooms ctx _ respond = do
  rooms <- listRooms ctx.db
  respond $ responseLBS status200 [("content-type", "application/json")] $ encode rooms

newtype GetRoomReq = GetRoomReq {user :: User}
  deriving (Generic)

instance FromJSON GetRoomReq

getRoom :: Context -> Application
getRoom ctx rawReq respond = do
  case L.find (\(k, _) -> k == "user") $ queryString rawReq of
    Just (_, Just user) -> do
      room <- getJoinedRoom ctx.db $ decodeUtf8 user
      respond $ responseLBS status200 [("content-type", "application/json")] $ encode room
    _ -> respond $ responseLBS status422 [] ""

data PostRoomReq = PostRoomReq {name :: RoomName, user :: User, notice :: T.Text}
  deriving (Generic)

instance FromJSON PostRoomReq

postRoom :: Context -> Application
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
          pure res
        else
          respond $ responseLBS status400 [] ""
    Nothing -> respond $ responseLBS status422 [] ""

data PutRoomReq = PutRoomReq {name :: RoomName, user :: User}
  deriving (Generic)

instance FromJSON PutRoomReq

putRoom :: Context -> Application
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

deleteRoom :: Context -> Application
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
