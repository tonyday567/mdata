{-# LANGUAGE OverloadedStrings #-}

-- | Cache management for downloaded datasets
module DataFrame.Cache
  ( cachePath,
    getCacheDir,
    isCached,
    cacheSize,
    evictToSize,
    clearCache,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
import Control.Monad (when)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeFile,
    getFileSize,
    getHomeDirectory,
  )
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)

-- | Get cache directory absolute path (~/.cache/dataframe-load/)
getCacheDir :: IO FilePath
getCacheDir = do
  homeDir <- getHomeDirectory
  pure (homeDir </> ".cache" </> "dataframe-load")

-- | Get cache path for a dataset (absolute)
cachePath :: Text -> IO FilePath
cachePath name = do
  dir <- getCacheDir
  pure (dir </> T.unpack name <> ".csv")

-- | Check if dataset is cached
isCached :: Text -> IO Bool
isCached name = do
  path <- cachePath name
  doesFileExist path

-- | Get total cache size in bytes
cacheSize :: IO Int64
cacheSize = do
  cacheDir <- getCacheDir
  exists <- doesDirectoryExist cacheDir
  if exists
    then do
      files <- listDirectory cacheDir
      sizes <- mapM (\f -> fromIntegral <$> getFileSize (cacheDir </> f)) files `catch` \(_ :: SomeException) -> pure []
      pure (sum sizes)
    else pure 0

-- | Evict oldest files until cache is under size limit
-- For now, just remove all cached files if over limit
evictToSize :: Int64 -> IO ()
evictToSize maxSize = do
  currentSize <- cacheSize
  when (currentSize > maxSize) $ do
    putStrLn $ "Cache size " <> show currentSize <> " exceeds limit " <> show maxSize <> ". Clearing cache."
    clearCache

-- | Clear entire cache
clearCache :: IO ()
clearCache = do
  cacheDir <- getCacheDir
  exists <- doesDirectoryExist cacheDir
  if exists
    then do
      files <- listDirectory cacheDir
      mapM_ (removeFile . (cacheDir </>)) files `catch` \(_ :: SomeException) -> pure ()
    else pure ()
