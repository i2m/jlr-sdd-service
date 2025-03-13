{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Storage.Users where

import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)


type UserId = Integer
type Username = Maybe T.Text

data User = User
  { userId :: UserId
  , userName :: Username
  , freeUntil :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance ToJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User userId userName freeUntil) = toRow (userId, userName, freeUntil)

findOrInsertUser :: Connection -> UserId -> Username -> IO User
findOrInsertUser conn userId userName =
  findUserById conn userId >>= \case
    Just user -> pure user
    Nothing -> do
      let user = User userId userName Nothing
      insertUser conn user
      return user

insertUser :: Connection -> User -> IO ()
insertUser conn = execute conn "INSERT INTO users (user_id, user_name, free_until) VALUES (?, ?, ?)"

findUserById :: Connection -> Integer -> IO (Maybe User)
findUserById conn userId = listToMaybe <$> query conn "SELECT * FROM users WHERE user_id = ?" (Only userId)
