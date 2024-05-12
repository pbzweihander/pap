module Db (
  RoomName,
  User,
  Room,
  Db,
  newDb,
  createRoom,
  joinRoom,
  getJoinedRoom,
  leaveRoom,
  listRooms,
) where

import Control.Concurrent.STM
import Control.Exception
import Data.Aeson (ToJSON)
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import Data.Text (Text)
import GHC.Generics (Generic)

type RoomName = Text
type User = Text

data Room = Room
  { name :: RoomName
  , owner :: User
  , notice :: Text
  , participants :: [User]
  }
  deriving (Generic)

instance ToJSON Room

type DbInner = H.BasicHashTable RoomName Room
type Db = TMVar DbInner

newDb :: IO Db
newDb = H.new >>= newTMVarIO

createRoom :: Db -> RoomName -> User -> Text -> IO ()
createRoom db name owner notice = do
  bracket
    (atomically $ takeTMVar db)
    (atomically . putTMVar db)
    ( \dbInner ->
        H.mutate
          dbInner
          name
          ( \case
              Just room -> (Just room, ())
              Nothing -> (Just Room{name = name, owner = owner, notice = notice, participants = [owner]}, ())
          )
    )

leaveFromAllRoom :: DbInner -> User -> IO ()
leaveFromAllRoom db user = do
  H.mapM_
    ( \(name, room) -> do
        let p = L.delete user room.participants
        H.insert db name room{participants = p}
    )
    db

cleanUpRooms :: DbInner -> IO ()
cleanUpRooms db = do
  H.mapM_
    ( \(name, room) -> do
        let isEmpty = L.null room.participants
        if isEmpty
          then do
            H.delete db name
          else do
            pure ()
    )
    db

joinRoom :: Db -> RoomName -> User -> IO ()
joinRoom db name user = do
  bracket
    (atomically $ takeTMVar db)
    (atomically . putTMVar db)
    ( \dbInner -> do
        leaveFromAllRoom
          dbInner
          user
        H.mutate
          dbInner
          name
          ( \case
              Just room -> (Just room{participants = room.participants ++ [user]}, ())
              Nothing -> (Nothing, ())
          )
        cleanUpRooms dbInner
    )

getJoinedRoom :: Db -> User -> IO (Maybe Room)
getJoinedRoom db user = do
  bracket
    (atomically $ takeTMVar db)
    (atomically . putTMVar db)
    ( \dbInner -> do
        l <- H.toList dbInner
        pure $ L.find (\room -> user `elem` room.participants) $ map snd l
    )

leaveRoom :: Db -> User -> IO ()
leaveRoom db user = do
  bracket
    (atomically $ takeTMVar db)
    (atomically . putTMVar db)
    ( \dbInner -> do
        leaveFromAllRoom dbInner user
        cleanUpRooms dbInner
    )

listRooms :: Db -> IO [Room]
listRooms db = do
  dbInner <- atomically $ readTMVar db
  l <- H.toList dbInner
  pure $ map snd l
