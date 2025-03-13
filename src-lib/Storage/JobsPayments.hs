{-# LANGUAGE OverloadedStrings #-}

module Storage.JobsPayments where

import Data.Text qualified as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Text.Read

import Storage.Jobs (JobId)
import Storage.Users (UserId)

type PreCheckoutQueryId = T.Text
type TelegramPaymentChargeId = T.Text

data JobsPaymentsStatus = Pending | Succeeded | Failed | Refunded deriving (Show, Read)

instance FromField JobsPaymentsStatus where
  fromField f@(Field (SQLText t) _) =
    case readEither $ T.unpack t of
      Right s -> Ok s
      Left e -> returnError ConversionFailed f ("couldn't parse JobsPaymentsStatus field: " ++ e)
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance ToField JobsPaymentsStatus where
  toField = SQLText . T.pack . show

data JobsPayments = JobsPayments
  { userId :: UserId
  , jobId :: JobId
  , preCheckoutQueryId :: Maybe PreCheckoutQueryId
  , telegramPaymentChargeId :: Maybe TelegramPaymentChargeId
  , status :: JobsPaymentsStatus
  , datetime :: UTCTime
  }
  deriving (Show)

instance FromRow JobsPayments where
  fromRow = JobsPayments <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow JobsPayments where
  toRow (JobsPayments userId jobId preCheckoutQueryId telegramPaymentChargeId status datetime) = toRow (userId, jobId, preCheckoutQueryId, telegramPaymentChargeId, status, datetime)

preCheckoutJob :: Connection -> UserId -> JobId -> PreCheckoutQueryId -> IO ()
preCheckoutJob conn userId jobId queryId = do
  now <- getCurrentTime
  execute conn "INSERT INTO jobs_payments VALUES (?, ?, ?, ?, ?, ?)" (JobsPayments userId jobId (Just queryId) Nothing Pending now)

succeedPaymentJob :: Connection -> UserId -> JobId -> TelegramPaymentChargeId -> IO ()
succeedPaymentJob conn userId jobId paymentId = do
  now <- getCurrentTime
  execute conn "INSERT INTO jobs_payments VALUES (?, ?, ?, ?, ?, ?)" (JobsPayments userId jobId Nothing (Just paymentId) Succeeded now)

refundPaymentJob :: Connection -> UserId -> JobId -> TelegramPaymentChargeId -> IO ()
refundPaymentJob conn userId jobId paymentId = do
  now <- getCurrentTime
  execute conn "INSERT INTO jobs_payments VALUES (?, ?, ?, ?, ?, ?)" (JobsPayments userId jobId Nothing (Just paymentId) Refunded now)


failedPaymentJob :: Connection -> UserId -> JobId -> PreCheckoutQueryId -> IO ()
failedPaymentJob conn userId jobId queryId = do
  now <- getCurrentTime
  execute conn "INSERT INTO jobs_payments VALUES (?, ?, ?, ?, ?, ?)" (JobsPayments userId jobId (Just queryId) Nothing Failed now)
