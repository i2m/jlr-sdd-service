{-# LANGUAGE OverloadedStrings #-}

module Storage.Init where

import Database.SQLite.Simple

initDatabase :: FilePath -> IO ()
initDatabase file = withConnection file $ \conn ->
  execute_ conn "PRAGMA journal_mode = WAL"
    >> execute_
      conn
      "CREATE TABLE IF NOT EXISTS users (\
      \ user_id UNSIGNED BIG INT NOT NULL,\
      \ user_name TEXT,\
      \ free_until TIMESTAMP\
      \ )"
    >> execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS users_user_id_uniq ON users (user_id)"
    >> execute_
      conn
      "CREATE TABLE IF NOT EXISTS packs_payments (\
      \ user_id UNSIGNED BIG INT NOT NULL,\
      \ pack_type TEXT CHECK ( pack_type IN ('unlim_day','unlim_week', 'unlim_month') ) NOT NULL,\
      \ pre_checkout_query_id TEXT,\
      \ telegram_payment_charge_id TEXT,\
      \ status TEXT CHECK ( status IN ('pending', 'succeeded', 'failed', 'refunded') ) NOT NULL,\
      \ datetime TIMESTAMP NOT NULL\
      \ )"
    >> execute_ conn "CREATE INDEX IF NOT EXISTS packs_payments_user_id_pack_id_status ON packs_payments (user_id, pack_type, status)"
    >> execute_ conn "CREATE INDEX IF NOT EXISTS packs_payments_pre_checkout_query_id ON packs_payments (pre_checkout_query_id)"
    >> execute_
      conn
      "CREATE TABLE IF NOT EXISTS jobs (\
      \ job_id TEXT NOT NULL,\
      \ manufacturer TEXT NOT NULL,\
      \ locked_function TEXT NOT NULL,\
      \ seed TEXT NOT NULL,\
      \ password TEXT NOT NULL\
      \ )"
    >> execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS jobs_job_id_uniq ON jobs (job_id)"
    >> execute_
      conn
      "CREATE TABLE IF NOT EXISTS jobs_payments (\
      \ user_id UNSIGNED BIG INT NOT NULL,\
      \ job_id TEXT NOT NULL,\
      \ pre_checkout_query_id TEXT,\
      \ telegram_payment_charge_id TEXT,\
      \ status TEXT CHECK ( status IN ('Pending', 'Succeeded', 'Failed', 'Refunded') ) NOT NULL,\
      \ datetime TIMESTAMP NOT NULL\
      \ )"
    >> execute_ conn "CREATE INDEX IF NOT EXISTS jobs_payments_user_id_job_id_status ON jobs_payments (user_id, job_id, status)"
    >> execute_ conn "CREATE INDEX IF NOT EXISTS jobs_payments_pre_checkout_query_id ON jobs_payments (pre_checkout_query_id)"
