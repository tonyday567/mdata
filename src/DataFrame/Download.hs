{-# LANGUAGE OverloadedStrings #-}

-- | Download datasets from URLs
module DataFrame.Download
  ( downloadToCache,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Directory
  ( createDirectoryIfMissing,
    copyFile,
  )
import Network.HTTP.Simple
  ( httpLBS,
    parseRequest,
    getResponseBody,
  )
import Control.Exception (try, SomeException)
import DataFrame.Cache (getCacheDir, cachePath)

-- | Download dataset from URL and save to cache
-- Supports file:// and http(s):// URLs
downloadToCache :: Text -> Text -> IO (Either String FilePath)
downloadToCache url name = do
  cacheDirectory <- getCacheDir
  createDirectoryIfMissing True cacheDirectory

  dest <- cachePath name

  if T.isPrefixOf "file://" url
    then do
      -- Handle file:// URLs (local files)
      let filePath = T.unpack (T.drop 7 url)
      result <- try (copyFile filePath dest) :: IO (Either SomeException ())
      case result of
        Left err -> pure (Left $ "Failed to copy file: " <> show err)
        Right _ -> pure (Right dest)
    else if T.isPrefixOf "http://" url || T.isPrefixOf "https://" url
      then do
        -- Handle HTTP(S) URLs
        result <- try (downloadUrl (T.unpack url) dest) :: IO (Either SomeException ())
        case result of
          Left err -> pure (Left $ "Failed to download: " <> show err)
          Right _ -> pure (Right dest)
      else
        pure (Left $ "Unsupported URL scheme: " <> T.unpack url)

-- | Download from HTTP(S) URL and save to file
downloadUrl :: String -> FilePath -> IO ()
downloadUrl url dest = do
  req <- parseRequest url
  response <- httpLBS req
  BL.writeFile dest (getResponseBody response)
