{-# LANGUAGE OverloadedStrings #-}

-- | dataframe-load: Educational chart explorer for data visualization
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (forever, void)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Generics
import Options.Applicative
import System.FilePath (takeExtension, (</>))
import System.FSNotify
import System.Directory (createDirectoryIfMissing)
import Data.ByteString qualified as BS
import Prelude

-- New modules
import Chart
import DataFrame qualified as D
import DataFrame.Chart
import DataFrame.Load
import Prettychart

data Run = RunLoad | RunWatch | RunDemo | RunPush | RunCounter deriving (Eq, Show)

data AppConfig = AppConfig
  { appPort :: Int,
    appRun :: Run,
    appPushSeconds :: Int,
    appChartsPerSecond :: Int,
    appAnimStart :: Int,
    appAnimStep :: Int,
    appDataset :: String,
    appWatchDir :: FilePath
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig 9160 RunLoad 3 50 1 100 "s5e11" "/tmp/watch/"

parseRun :: Parser Run
parseRun =
  flag' RunLoad (long "load" <> help "load and serve dataset (default)")
    <|> flag' RunDemo (long "demo" <> help "run interactive server demo")
    <|> flag' RunPush (long "push" <> help "run chart push test")
    <|> flag' RunCounter (long "counter" <> help "run simple counter test")
    <|> flag' RunWatch (long "watch" <> help "watch SVG files")
    <|> pure RunLoad

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> option auto (value (appPort def) <> long "port" <> help "server port")
    <*> parseRun
    <*> option auto (value (appPushSeconds def) <> long "push-seconds" <> help "push duration")
    <*> option auto (value (appChartsPerSecond def) <> long "charts-per-second" <> help "fps")
    <*> option auto (value (appAnimStart def) <> long "anim-start" <> help "animation start")
    <*> option auto (value (appAnimStep def) <> long "anim-step" <> help "animation step")
    <*> option str (value (appDataset def) <> long "dataset" <> help "dataset name")
    <*> option str (value (appWatchDir def) <> long "watchdir" <> help "watch directory")

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "dataframe-load: chart exploration")

-- | File watcher for SVG updates
demoWatcher :: FilePath -> Int -> IO ()
demoWatcher watchdir port = do
  let cfg = ChartServerConfig port Nothing
  (send, _) <- startChartServerHyperbole cfg
  _ <- async $ withManager $ \mgr -> do
    _ <- watchDir mgr watchdir (isJust . svgEvent) (void . onSvgChange send)
    forever $ threadDelay 1000000
  forever $ threadDelay 1000000

svgEvent :: Event -> Maybe FilePath
svgEvent (Added fp _ IsFile) = if takeExtension fp == ".svg" then Just fp else Nothing
svgEvent (Modified fp _ IsFile) = if takeExtension fp == ".svg" then Just fp else Nothing
svgEvent _ = Nothing

onSvgChange :: (Text -> IO Bool) -> Event -> IO Bool
onSvgChange act e = maybe (pure False) (\fp -> TIO.readFile fp >>= act) (svgEvent e)

-- | Animation generators
multiPlotAnimation :: D.DataFrame -> Int -> Int -> Int -> ChartOptions
multiPlotAnimation df start step i = multiPlot (D.take (step * i + start) df)

-- | Load CSV data from dataset
getDF :: String -> IO D.DataFrame
getDF dataset = case dataset of
  "penguins" -> D.readCsv "penguins.csv"
  _ -> D.readCsv "other/s5e11/test.csv"

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  case appRun o of
    RunLoad -> loadServer (appPort o)
    RunWatch -> demoWatcher (appWatchDir o) (appPort o)
    RunPush -> do
      -- New architecture: server handles dataset loading via routes
      -- User selects dataset from browser dropdown
      putStrLn "Starting in multi-dataset mode..."
      putStrLn "Select a dataset from the browser dropdown"
      let cfg = PushConfig (appPort o) (appPushSeconds o) (appChartsPerSecond o)
      -- Use dummy animation function (not used in new /dataset/:name routes)
      pushServer counterChart cfg
    RunCounter -> do
      let cfg = PushConfig (appPort o) (appPushSeconds o) (appChartsPerSecond o)
      pushServer counterChart cfg
    RunDemo -> do
      let watchdir = appWatchDir o
      let port = appPort o
      -- Generate welcome chart from default dataframe
      df <- getDF (appDataset o)
      let chart = multiPlot df
      let svg = encodeChartOptions chart
      createDirectoryIfMissing True watchdir
      BS.writeFile (watchdir </> "welcome-chart.svg") svg
      putStrLn $ "✓ Welcome chart saved to " <> (watchdir </> "welcome-chart.svg")
      putStrLn $ "✓ FileWatch server running on port " <> show port
      putStrLn "Toss SVGs into /tmp/watch/ and watch them appear!"
      -- Start file watcher and keep running
      demoWatcher watchdir port
