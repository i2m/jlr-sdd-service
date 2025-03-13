{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AccessCodes where

import Data.Text qualified as T
import LockedFunctions (LockedFunctions (Functions, funcId))
import Text.Printf (printf)

seedToCodes :: T.Text -> Either T.Text [Int]
seedToCodes seed
  | T.length seed < 10 = Left "Seed length is less than 10 chars"
  | T.length seed > 10 = Left "Seed length is more than 10 chars"
  | otherwise = maybe (Left "Unknown chars in seed") Right codes
  where
    findCode char = T.findIndex (== char) $ T.pack seedChars
    codes = traverse findCode (T.unpack seed)

seedToVin :: T.Text -> Either T.Text T.Text
seedToVin seed = do
  codes <- seedToCodes seed
  return $ T.pack $ (vinChars !!) . (codes !!) <$> vinCodesPositionInSeed

generateKey :: T.Text -> Either T.Text T.Text
generateKey seed = do
  codes <- seedToCodes seed
  return $ T.pack $ (passwordChars !!) . (codes !!) <$> seedCodesShuffle

generateFuncKey :: (LockedFunctions mf) => Functions mf -> Either T.Text T.Text
generateFuncKey lf = do
  codes <- maybe (Left "Unknown chars in locked function id") Right (traverse findVinChar lfIdChrs)
  return $ T.pack $ (passwordChars !!) <$> codes
  where
    lfIdChrs = printf "%02x" (funcId lf)
    findVinChar char = T.findIndex (== char) $ T.pack vinChars

generatePassword :: (LockedFunctions mf) => Functions mf -> T.Text -> Either T.Text T.Text
generatePassword func seed = T.concat <$> sequence [key, fkey, fkey]
  where
    key = generateKey seed
    fkey = generateFuncKey func

passwordChars :: [Char]
passwordChars =
  [ 'Y',
    '1',
    'D',
    'B',
    '3',
    'S',
    'V',
    'U',
    '4',
    'E',
    '5',
    'K',
    '8',
    'J',
    'O',
    '6',
    'X',
    'N',
    'G',
    'Z',
    'T',
    'H',
    '7',
    'I',
    'M',
    '0',
    'C',
    'P',
    'L',
    'F',
    'A',
    'Q',
    'W',
    '2',
    'R',
    '9'
  ]

seedCodesShuffle :: [Int]
seedCodesShuffle = [5, 3, 0, 6, 9, 1, 7, 8, 2, 4]

vinCodesPositionInSeed :: [Int]
vinCodesPositionInSeed = [0, 9, 2, 4, 6, 8]

seedChars :: [Char]
seedChars =
  [ 'Q',
    'W',
    '2',
    'R',
    '9',
    'Y',
    '1',
    'D',
    'E',
    '3',
    'S',
    'V',
    'U',
    '4',
    'O',
    '5',
    'K',
    'T',
    'J',
    'B',
    '6',
    'X',
    'N',
    'I',
    'Z',
    '0',
    'G',
    '7',
    'H',
    'M',
    '8',
    'C',
    'P',
    'L',
    'F',
    'A'
  ]

vinChars :: [Char]
vinChars =
  [ 'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9'
  ]
