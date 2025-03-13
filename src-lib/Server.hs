{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import AccessCodes (generatePassword, seedToVin)
import Control.Exception (throwIO)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Data.Aeson.Types (ToJSON)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.SQLite.Simple (open)
import GHC.Generics (Generic)
import LockedFunctions
  ( LockedFunctions (Functions),
    Manufacturers (Jaguar, LandRover),
  )
import Servant
  ( Application,
    Capture,
    Get,
    JSON,
    PostCreated,
    ServerError (errBody),
    err404,
    err500,
    (:<|>) (..),
    (:>),
  )
import Servant.Server (Server, serve)
import Storage.Jobs (Job (password), JobId, Password, findJobById, insertJob)
import Storage.JobsPayments (PreCheckoutQueryId, TelegramPaymentChargeId, failedPaymentJob, preCheckoutJob, refundPaymentJob, succeedPaymentJob)
import Storage.Users (User, UserId, Username, findOrInsertUser)

type Seed = T.Text

type Vin = T.Text

data ServerStatus = Ok | Down
  deriving (Show, Generic)

instance ToJSON ServerStatus

type GenPassApi =
  Get '[JSON] ServerStatus
    :<|> "user" :> Capture "userId" UserId :> Capture "userName" Username :> PostCreated '[JSON] User
    :<|> "checkout_job" :> Capture "userId" UserId :> Capture "jobId" JobId :> Capture "preCheckoutQueryId" PreCheckoutQueryId :> PostCreated '[JSON] JobId
    :<|> "failed_job" :> Capture "userId" UserId :> Capture "jobId" JobId :> Capture "preCheckoutQueryId" PreCheckoutQueryId :> PostCreated '[JSON] JobId
    :<|> "succeed_job" :> Capture "userId" UserId :> Capture "jobId" JobId :> Capture "telegramPaymentChargeId" TelegramPaymentChargeId :> PostCreated '[JSON] Password
    :<|> "refund_job" :> Capture "userId" UserId :> Capture "jobId" JobId :> Capture "telegramPaymentChargeId" TelegramPaymentChargeId :> PostCreated '[JSON] Password
    :<|> "get_vin" :> Capture "seed" Seed :> Get '[JSON] Vin
    :<|> "gen_pass"
      :> ( "jg" :> Capture "func" (Functions 'Jaguar) :> Capture "seed" Seed :> PostCreated '[JSON] JobId
             :<|> "lr" :> Capture "func" (Functions 'LandRover) :> Capture "seed" Seed :> PostCreated '[JSON] JobId
         )

api :: Proxy GenPassApi
api = Proxy

apiImpl :: FilePath -> Server GenPassApi
apiImpl dbFile = status :<|> createUser :<|> checkoutJob :<|> failedJob :<|> succeedJob :<|> refundJob :<|> getVin :<|> genPass Jaguar :<|> genPass LandRover
  where
    connIO = open dbFile

    status = pure Ok

    createUser userId userName = liftIO $ do
      conn <- connIO
      findOrInsertUser conn userId userName

    getVin seed =
      case seedToVin seed of
        Left err -> throwError $ err500 {errBody = encodeUtf8 . fromStrict $ err}
        Right vin -> pure vin

    genPass mnfc func seed =
      case generatePassword func seed of
        Left err -> throwError $ err500 {errBody = encodeUtf8 . fromStrict $ err}
        Right pwd -> liftIO $ do
          conn <- connIO
          insertJob conn (T.pack $ show mnfc) (T.pack $ show func) seed pwd

    checkoutJob userId jobId queryId = liftIO $ do
      conn <- connIO
      mJob <- findJobById conn jobId
      case mJob of
        Just _ -> do
          preCheckoutJob conn userId jobId queryId
          pure jobId
        Nothing -> throwIO $ err404 {errBody = encodeUtf8 . fromStrict $ "Job Not Found"}

    failedJob userId jobId queryId = liftIO $ do
      conn <- connIO
      mJob <- findJobById conn jobId
      case mJob of
        Just _ -> do
          failedPaymentJob conn userId jobId queryId
          pure jobId
        Nothing -> throwIO $ err404 {errBody = encodeUtf8 . fromStrict $ "Job Not Found"}

    succeedJob userId jobId telPayId = liftIO $ do
      conn <- connIO
      mJob <- findJobById conn jobId
      case mJob of
        Just job -> do
          succeedPaymentJob conn userId jobId telPayId
          pure $ password job
        Nothing -> throwIO $ err404 {errBody = encodeUtf8 . fromStrict $ "Job Not Found"}

    refundJob userId jobId telPayId = liftIO $ do
      conn <- connIO
      mJob <- findJobById conn jobId
      case mJob of
        Just job -> do
          refundPaymentJob conn userId jobId telPayId
          pure $ password job
        Nothing -> throwIO $ err404 {errBody = encodeUtf8 . fromStrict $ "Job Not Found"}

app :: FilePath -> Application
app dbFile = serve api $ apiImpl dbFile
