module Main where

import Network.Wai.Handler.Warp (run)
import Server (app)
import Storage (initDatabase)

main :: IO ()
main = do
  let dbFile = "database.sqlite"
  initDatabase dbFile
  run 3000 $ app dbFile
