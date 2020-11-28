{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Lz4
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Bits                      ( shiftL )
import qualified Data.ByteString.Char8         as BC
import           Data.Char                      ( ord )
import qualified Data.Text                     as T
import           System.Environment
import           Data.Text.Lens

readLz4 :: FilePath -> IO BC.ByteString
readLz4 f = do
  contents <- BC.readFile f
  let bsc = BC.drop 8 contents
      sz  = lEBsToInt $ BC.take 4 bsc
  return $ decompressBlockSz (BC.drop 4 bsc) sz


readJsonLz4 :: FilePath -> IO Value
readJsonLz4 f = do
  bs <- readLz4 f
  return $ bs ^?! _JSON

-- https://stackoverflow.com/questions/26519579/how-do-i-convert-4-bytes-of-a-bytestring-to-an-int
-- 1.  Get the first 4 bytes of a ByteString
-- 2.  Calculate the amount to shift by
-- 3.  Fold over the ByteString, summing the result of shifting the current byte by the current offset
lEBsToInt :: BC.ByteString -> Int -- littleEndianByteStringToInt
lEBsToInt bs =
  let bsI = BC.take 4 bs
  in                                                                                                                                                                                            -- 1.
      fst $ BC.foldl
        (\(s, i) b ->
          let shiftBy = i * 8
          in                                                                                                                                                                                            -- 2.
              (s + (ord b `shiftL` shiftBy), i + 1)
        )
        (0, 0)
        bsI

getCurrentTab :: Value -> (String, String)
getCurrentTab v =
  let iwindow = v ^?! key "selectedWindow" . _Integer . to fromIntegral - 1
      window  = v ^?! key "windows" . nth iwindow
      itab    = window ^?! key "selected" . _Integer . to fromIntegral - 1
      tab     = window ^?! key "tabs" . nth itab
      ientry  = tab ^?! key "index" . _Integer . to fromIntegral - 1
  in  tab ^?! key "entries" . nth ientry . to
        (\o ->
          ( o ^?! key "title" . _String . unpacked
          , o ^?! key "url" . _String . unpacked
          )
        )

extractCurrentTab :: FilePath -> IO (String, String)
extractCurrentTab recoveryJson = do
  value <- readJsonLz4 recoveryJson
  return $ getCurrentTab value

currentFirefoxTab :: FilePath -> IO ()
currentFirefoxTab recoveryJson = do
  (title, url) <- extractCurrentTab recoveryJson
  putStrLn title
  putStrLn url


main :: IO ()
main = do
  [recoveryJson] <- getArgs
  currentFirefoxTab recoveryJson

-- https://unix.stackexchange.com/questions/385023/firefox-reading-out-urls-of-opened-tabs-from-the-command-line export opentabs=$(find ~/.mozilla/firefox*/*.default/sessionstore-backups/recovery.jsonlz4);
-- path to firefox's recovery.jsonlz4:
--   macos ~/Library/Application Support/Firefox/Profiles/*.default-*/sessionstore-backups/recovery.jsonlz4"
--   linux ~/.mozilla/firefox*/*.default/sessionstore-backups/recovery.jsonlz4
