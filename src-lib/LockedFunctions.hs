{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module LockedFunctions where


import qualified Data.Text as T
import Servant ( FromHttpApiData(..) )


data Manufacturers = Jaguar
                   | LandRover
                   deriving (Show)


class LockedFunctions (mf :: Manufacturers) where
  data Functions mf
  funcId :: Functions mf -> Int


instance LockedFunctions LandRover where
  data Functions 'LandRover = CffEditorLr
                            | L316OdoApp
                            | TaiwanVehicleUpdate
                            | SoftwareDownloadLr
                            | L322OdoApp
                            | L322RecoverKeys
                            | L322EraseKeys
                            | Option8
    deriving (Show)

  funcId :: Functions 'LandRover -> Int
  funcId CffEditorLr = 1
  funcId L316OdoApp = 2
  funcId TaiwanVehicleUpdate = 4
  funcId SoftwareDownloadLr = 8
  funcId L322OdoApp = 16
  funcId L322RecoverKeys = 32
  funcId L322EraseKeys = 64
  funcId Option8 = 128


instance LockedFunctions Jaguar where
  data Functions 'Jaguar = VinBypass
                         | VidBlockEditor
                         | X150OdoApp
                         | CffEditorJag
                         | X250OdoApp
                         | SoftwareDownloadJag
                         | X351OdoApp
                         | X351RecoverKeys
    deriving (Show)

  funcId :: Functions 'Jaguar -> Int
  funcId VinBypass = 1
  funcId VidBlockEditor = 2
  funcId X150OdoApp = 4
  funcId CffEditorJag = 8
  funcId X250OdoApp = 16
  funcId SoftwareDownloadJag = 32
  funcId X351OdoApp = 64
  funcId X351RecoverKeys = 128


instance FromHttpApiData (Functions 'LandRover) where
  parseUrlPiece :: T.Text -> Either T.Text (Functions 'LandRover)
  parseUrlPiece "ccf_edit" = Right CffEditorLr
  parseUrlPiece "l316_odo" = Right L316OdoApp
  parseUrlPiece "thai" = Right TaiwanVehicleUpdate
  parseUrlPiece "swdl" = Right SoftwareDownloadLr
  parseUrlPiece "l322_odo" = Right L322OdoApp
  parseUrlPiece "l322_recover_keys" = Right L322RecoverKeys
  parseUrlPiece "l322_erase_keys" = Right L322EraseKeys
  parseUrlPiece "opt8" = Right Option8
  parseUrlPiece _ = Left "Unknown function"


instance FromHttpApiData (Functions 'Jaguar) where
  parseUrlPiece :: T.Text -> Either T.Text (Functions 'Jaguar)
  parseUrlPiece "vin_bypass" = Right VinBypass
  parseUrlPiece "block_editor" = Right VidBlockEditor
  parseUrlPiece "x150_odo" = Right X150OdoApp
  parseUrlPiece "ccf_edit" = Right CffEditorJag
  parseUrlPiece "x250_odo" = Right X250OdoApp
  parseUrlPiece "swdl" = Right SoftwareDownloadJag
  parseUrlPiece "x351_odo" = Right X351OdoApp
  parseUrlPiece "x351_recover_keys" = Right X351RecoverKeys
  parseUrlPiece _ = Left "Unknown function"
