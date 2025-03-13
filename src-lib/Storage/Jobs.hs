{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Jobs where

import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Data.UUID.V1 (nextUUID)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField

type JobId = UUID

type SimlpeManufacturer = T.Text

type SimlpeLockedFunction = T.Text

type Seed = T.Text

type Password = T.Text

data Job = Job
  { jobId :: JobId,
    manufacturer :: SimlpeManufacturer,
    lockedFunction :: SimlpeLockedFunction,
    seed :: Seed,
    -- a one-time password generated from the seed
    -- doesn't need to be stored as salted and hashed
    password :: Password
  }
  deriving (Show)

instance FromField UUID where
  fromField f@(Field (SQLText t) _) =
    case UUID.fromText t of
      Just uuid -> Ok uuid
      Nothing -> returnError ConversionFailed f "couldn't parse UUID field"
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance ToField UUID where
  toField = SQLText . UUID.toText
  {-# INLINE toField #-}

instance FromRow Job where
  fromRow = Job <$> field <*> field <*> field <*> field <*> field

instance ToRow Job where
  toRow (Job jobId manufacturer lockedFunction seed password) = toRow (jobId, manufacturer, lockedFunction, seed, password)

insertJob :: Connection -> SimlpeManufacturer -> SimlpeLockedFunction -> Seed -> Password -> IO JobId
insertJob conn man lf seed pwd = do
  Just uuid <- nextUUID
  execute conn "INSERT INTO jobs (job_id, manufacturer, locked_function, seed, password) VALUES (?, ?, ?, ?, ?)" (Job uuid man lf seed pwd)
  pure uuid

findJobById :: Connection -> JobId -> IO (Maybe Job)
findJobById conn jobId = listToMaybe <$> query conn "SELECT * FROM jobs WHERE job_id = ?" (Only jobId)
