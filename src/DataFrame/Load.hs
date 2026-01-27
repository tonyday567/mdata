{-# LANGUAGE OverloadedStrings #-}

module DataFrame.Load
  ( pushServer,
    loadServer,
    PushConfig (..),
  )
where

import Chart (ChartOptions, encodeChartOptions)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void, forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)
import Network.HTTP.Types (ok200, status500)
import Network.Wai (Application, Response, ResponseReceived, pathInfo, responseLBS, responseStream, queryString)
import Network.Wai.Handler.Warp (run)
import System.Exit (exitSuccess)
import Prelude
import DataFrame qualified as D
import DataFrame.Datasets (datasetNames, lookupUrl, localDatasets)
import DataFrame.Cache (cachePath, isCached)
import DataFrame.Download (downloadToCache)
import DataFrame.Chart (multiPlot)

-- | Push server configuration
data PushConfig = PushConfig
  { pcPort :: Int,
    pcSeconds :: Int,
    pcChartsPerSecond :: Int
  }

-- | Start server that streams chart frames via SSE
pushServer :: (Int -> ChartOptions) -> PushConfig -> IO ()
pushServer anim cfg = do
  let port = pcPort cfg
  let seconds = pcSeconds cfg
  let chartsPerSec = pcChartsPerSecond cfg
  let numFrames = seconds * chartsPerSec
  let frameDelay = round (1000000.0 / fromIntegral chartsPerSec :: Double)

  putStrLn $ "Starting chart server on port " <> show port
  putStrLn $ "Configuration: " <> show seconds <> "s, " <> show chartsPerSec <> " charts/sec (" <> show numFrames <> " total frames)"
  putStrLn $ "Open browser to http://localhost:" <> show port

  let app :: Application
      app request respond = do
        case pathInfo request of
          [] -> serveIndexHtml respond
          ["sse"] -> do
            -- Check for dataset query parameter
            let qParams = queryString request
            case lookup "dataset" qParams of
              Nothing -> streamFrames respond (encodeChartOptions . anim) numFrames frameDelay
              Just (Just datasetName) -> handleDatasetLoad (decodeUtf8 datasetName) respond numFrames frameDelay
              Just Nothing -> streamFrames respond (encodeChartOptions . anim) numFrames frameDelay
          ["kill"] -> do
            rr <- respond (responseLBS ok200 [] "")
            void $ forkIO (threadDelay 100000 >> exitSuccess)
            pure rr
          _ -> serveIndexHtml respond

  putStrLn "Server started, press ctrl-c to stop"
  run port app

-- | Serve HTML with dataset dropdown
serveIndexHtml :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveIndexHtml respond = do
  -- Get remote datasets
  let remoteOptions = T.intercalate "\n"
        [ "<option value='" <> name <> "'>" <> name <> "</option>"
        | name <- datasetNames ]

  -- Get local datasets from cache
  localNames <- localDatasets
  let localOptions = T.intercalate "\n"
        [ "<option value='" <> name <> "'>" <> name <> " (local)</option>"
        | name <- localNames ]

  let html = BL.fromStrict $ encodeUtf8 $
        "<!DOCTYPE html><html><head><title>Dataset Chart Explorer</title><style>" <>
        "body{font-family:monospace; margin:20px} " <>
        ".selector{background:#f0f0f0;padding:15px;margin:10px 0;border-radius:5px} " <>
        ".stats{background:#eee;padding:10px;margin:10px 0} " <>
        ".warning{color:red} " <>
        "select, button, input{padding:5px;margin:5px} " <>
        "</style></head><body>" <>
        "<h1>Dataset Chart Explorer</h1>" <>
        "<div class='selector'>" <>
        "  <h3>Select Dataset</h3>" <>
        "  <select id='dataset-dropdown'>" <>
        "    <option value=''>-- Choose a dataset --</option>" <>
        (if not (T.null remoteOptions) then "<optgroup label='Remote Datasets'>" <> remoteOptions <> "</optgroup>" else "") <>
        (if not (T.null localOptions) then "<optgroup label='Local Datasets'>" <> localOptions <> "</optgroup>" else "") <>
        "  </select>" <>
        "  <button onclick='loadDataset()'>Load</button>" <>
        "  <button onclick=\"fetch('/kill')\">Kill Server</button>" <>
        "</div>" <>
        "<div id='status' style='padding:10px;'></div>" <>
        "<div class='stats'>" <>
        "  <div id='count'>Frames received: 0</div>" <>
        "  <div id='rate'>FPS: 0</div>" <>
        "  <div id='gaps' class='warning'></div>" <>
        "  <div id='latency'>Avg latency: 0ms</div>" <>
        "  <div id='buffered'>Buffered: 0 frames</div>" <>
        "</div>" <>
        "<div id='svg'></div>" <>
        "<script>" <>
        "let es = null;" <>
        "function loadDataset() {" <>
        "  const name = document.getElementById('dataset-dropdown').value;" <>
        "  if (name) { " <>
        "    if (es) es.close(); " <>
        "    document.getElementById('status').innerText = 'Loading...'; " <>
        "    document.getElementById('count').innerText = 'Frames received: 0'; " <>
        "    document.getElementById('rate').innerText = 'FPS: 0'; " <>
        "    document.getElementById('gaps').innerText = ''; " <>
        "    es = new EventSource('/sse?dataset=' + name); " <>
        "    es.onmessage = handleMessage; " <>
        "    es.onerror = () => { document.getElementById('status').innerText = 'Connection error'; }; " <>
        "  }" <>
        "}" <>
        "let count = 0, lastTime = Date.now(), frameRate = 0, lastFrameNum = -1;" <>
        "let frameTimes = [], gaps = 0;" <>
        "function handleMessage(e) { " <>
        "  const data = e.data;" <>
        "  if (data.startsWith('status:')) { " <>
        "    document.getElementById('status').innerText = data.substring(7); " <>
        "  } else { " <>
        "    count++; " <>
        "    const now = Date.now(); " <>
        "    const elapsed = now - lastTime; " <>
        "    frameTimes.push(elapsed); " <>
        "    if(frameTimes.length > 50) frameTimes.shift(); " <>
        "    const avgTime = frameTimes.reduce((a,b)=>a+b,0)/frameTimes.length; " <>
        "    frameRate = (1000/avgTime).toFixed(1); " <>
        "    lastTime = now; " <>
        "    if(lastFrameNum >= 0 && count !== lastFrameNum + 1) { gaps++; } " <>
        "    lastFrameNum = count; " <>
        "    document.getElementById('count').innerText = 'Frames received: ' + count; " <>
        "    document.getElementById('rate').innerText = 'FPS: ' + frameRate; " <>
        "    document.getElementById('gaps').innerText = gaps > 0 ? '⚠ Frame gaps detected: ' + gaps : ''; " <>
        "    document.getElementById('latency').innerText = 'Avg latency: ' + avgTime.toFixed(1) + 'ms'; " <>
        "    document.getElementById('buffered').innerText = 'Buffered: ' + frameTimes.length + ' frames'; " <>
        "    document.getElementById('svg').innerHTML = data; " <>
        "  } " <>
        "}" <>
        "</script>" <>
        "</body></html>"
  respond $ responseLBS ok200
    [("Content-Type", "text/html; charset=utf-8")]
    html

-- | Handle dataset loading and streaming
handleDatasetLoad :: Text -> (Response -> IO ResponseReceived) -> Int -> Int -> IO ResponseReceived
handleDatasetLoad name respond numFrames frameDelay = do
  -- Check if it's a local dataset first
  localNames <- localDatasets
  let isLocal = name `elem` localNames

  if isLocal
    then do
      putStrLn $ "Loading local dataset: " <> T.unpack name
      loadDatasetDirect name respond numFrames frameDelay
    else
      case lookupUrl name of
        Nothing -> serveError ("Unknown dataset: " <> name) respond
        Just url -> do
          putStrLn $ "Loading dataset: " <> T.unpack name <> " from " <> T.unpack url
          loadDatasetWithDownload name url respond numFrames frameDelay

-- | Load dataset that's already in cache (local saved dataframe)
loadDatasetDirect :: Text -> (Response -> IO ResponseReceived) -> Int -> Int -> IO ResponseReceived
loadDatasetDirect name respond numFrames frameDelay = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  respond $ responseStream ok200 headers $ \write flush -> do
    sendStatusMessage write flush "Loading local dataframe..."
    path <- cachePath name
    df <- D.readCsv path
    putStrLn $ "DataFrame loaded: " <> show (D.nRows df) <> " rows"

    -- Stream chart frames
    sendStatusMessage write flush "Generating charts..."
    let step = max 1 (D.nRows df `div` numFrames)
    forM_ [0 .. numFrames - 1] $ \i -> do
      let frameData = D.take (step * (i + 1)) df
      let chart = multiPlot frameData
      let svg = encodeChartOptions chart
      let svgOneLine = BS.filter (/= (10 :: Word8)) svg
      let frameBuilder = BB.string8 "data: " <> BB.byteString svgOneLine <> BB.string8 "\n\n"
      write frameBuilder
      flush
      threadDelay frameDelay

    sendStatusMessage write flush "Complete!"
    putStrLn "Chart stream complete, holding connection..."
    forever $ do
      let keepAlive = BB.string8 ": keep-alive\n\n"
      write keepAlive
      flush
      threadDelay 30000000

-- | Load dataset from remote URL with download
loadDatasetWithDownload :: Text -> Text -> (Response -> IO ResponseReceived) -> Int -> Int -> IO ResponseReceived
loadDatasetWithDownload name url respond numFrames frameDelay = do

      let headers =
            [ ("Content-Type", "text/event-stream"),
              ("Cache-Control", "no-cache"),
              ("Connection", "keep-alive"),
              ("X-Accel-Buffering", "no")
            ]

      respond $ responseStream ok200 headers $ \write flush -> do
        -- Check if cached
        cached <- isCached name
        if cached
          then do
            putStrLn $ "Using cached dataset: " <> T.unpack name
            sendStatusMessage write flush "Loading from cache..."
          else do
            sendStatusMessage write flush "Downloading dataset..."
            result <- downloadToCache url name
            case result of
              Left err -> do
                putStrLn $ "Download failed: " <> err
                sendErrorMessage write flush (T.pack err)
                exitSuccess
              Right _ -> sendStatusMessage write flush "Download complete!"

        -- Load DataFrame from cache
        putStrLn $ "Reading CSV from " <> T.unpack name
        path <- cachePath name
        df <- D.readCsv path
        putStrLn $ "DataFrame loaded: " <> show (D.nRows df) <> " rows"

        -- Stream chart frames
        sendStatusMessage write flush "Generating charts..."
        let step = max 1 (D.nRows df `div` numFrames)
        forM_ [0 .. numFrames - 1] $ \i -> do
          -- Generate chart for current frame (showing subset of data)
          let frameData = D.take (step * (i + 1)) df
          let chart = multiPlot frameData
          let svg = encodeChartOptions chart
          let svgOneLine = BS.filter (/= (10 :: Word8)) svg
          let frameBuilder = BB.string8 "data: " <> BB.byteString svgOneLine <> BB.string8 "\n\n"
          write frameBuilder
          flush
          threadDelay frameDelay

        sendStatusMessage write flush "Complete!"
        putStrLn "Chart stream complete, holding connection..."
        forever $ do
          let keepAlive = BB.string8 ": keep-alive\n\n"
          write keepAlive
          flush
          threadDelay 30000000

-- | Serve error page via SSE
serveError :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serveError err respond = do
  let html = BL.fromStrict $ encodeUtf8 $
        "<!DOCTYPE html><html><head><title>Error</title></head><body>" <>
        "<h1>Error</h1><p>" <> err <> "</p>" <>
        "<p><a href='/'>Back to home</a></p>" <>
        "</body></html>"
  respond $ responseLBS status500 [("Content-Type", "text/html; charset=utf-8")] html

-- | Send status message via SSE (prefixed with "status:")
sendStatusMessage :: (BB.Builder -> IO ()) -> IO () -> Text -> IO ()
sendStatusMessage write flush msg = do
  let frame = BB.string8 "data: status:" <> BB.byteString (encodeUtf8 msg) <> BB.string8 "\n\n"
  write frame
  flush
  putStrLn $ "Status: " <> T.unpack msg

-- | Send error message via SSE
sendErrorMessage :: (BB.Builder -> IO ()) -> IO () -> Text -> IO ()
sendErrorMessage write flush msg = do
  sendStatusMessage write flush ("ERROR: " <> msg)

-- | Load and serve a single chart with dataset dropdown
loadServer :: Int -> IO ()
loadServer port = do
  let app :: Application
      app request respond = do
        case pathInfo request of
          [] -> serveLoadIndexHtml respond
          ["sse"] -> do
            let qParams = queryString request
            case lookup "dataset" qParams of
              Nothing -> streamEmptyChart respond
              Just (Just datasetName) -> handleLoadDataset (decodeUtf8 datasetName) respond
              Just Nothing -> streamEmptyChart respond
          _ -> serveLoadIndexHtml respond

  putStrLn $ "Starting chart server on port " <> show port
  putStrLn $ "Open browser to http://localhost:" <> show port
  putStrLn "Server started, press ctrl-c to stop"
  run port app

-- | Serve HTML with dataset dropdown
serveLoadIndexHtml :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveLoadIndexHtml respond = do
  -- Get remote datasets
  let remoteOptions = T.intercalate "\n"
        [ "<option value='" <> name <> "'>" <> name <> "</option>"
        | name <- datasetNames ]

  -- Get local datasets from cache
  localNames <- localDatasets
  let localOptions = T.intercalate "\n"
        [ "<option value='" <> name <> "'>" <> name <> " (local)</option>"
        | name <- localNames ]

  let html = BL.fromStrict $ encodeUtf8 $
        "<!DOCTYPE html><html><head><title>Dataset Chart</title><style>" <>
        "body{font-family:monospace; margin:20px} " <>
        ".selector{background:#f0f0f0;padding:15px;margin:10px 0;border-radius:5px} " <>
        "</style></head><body>" <>
        "<h1>Dataset Chart</h1>" <>
        "<div class='selector'>" <>
        "  <h3>Select Dataset</h3>" <>
        "  <select id='dataset-dropdown'>" <>
        "    <option value=''>-- Choose a dataset --</option>" <>
        (if not (T.null remoteOptions) then "<optgroup label='Remote Datasets'>" <> remoteOptions <> "</optgroup>" else "") <>
        (if not (T.null localOptions) then "<optgroup label='Local Datasets'>" <> localOptions <> "</optgroup>" else "") <>
        "  </select>" <>
        "  <button onclick='loadDataset()'>Load</button>" <>
        "</div>" <>
        "<div id='status' style='padding:10px;'></div>" <>
        "<div id='svg'></div>" <>
        "<script>" <>
        "let es = null;" <>
        "function loadDataset() {" <>
        "  const name = document.getElementById('dataset-dropdown').value;" <>
        "  if (name) { " <>
        "    if (es) es.close(); " <>
        "    document.getElementById('status').innerText = 'Loading...'; " <>
        "    es = new EventSource('/sse?dataset=' + name); " <>
        "    es.onmessage = (e) => { " <>
        "      if (e.data.startsWith('status:')) { " <>
        "        document.getElementById('status').innerText = e.data.substring(7); " <>
        "      } else { " <>
        "        document.getElementById('svg').innerHTML = e.data; " <>
        "      } " <>
        "    }; " <>
        "    es.onerror = (e) => { document.getElementById('status').innerText = 'Connection error'; }; " <>
        "  }" <>
        "}" <>
        "</script>" <>
        "</body></html>"
  respond $ responseLBS ok200
    [("Content-Type", "text/html; charset=utf-8")]
    html

-- | Handle dataset selection and load
handleLoadDataset :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLoadDataset name respond = do
  localNames <- localDatasets
  let isLocal = name `elem` localNames

  if isLocal
    then loadDatasetDirectChart name respond
    else
      case lookupUrl name of
        Nothing -> serveError ("Unknown dataset: " <> name) respond
        Just url -> loadDatasetWithDownloadChart name url respond

-- | Load local dataset and stream single chart
loadDatasetDirectChart :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
loadDatasetDirectChart name respond = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  respond $ responseStream ok200 headers $ \write flush -> do
    sendStatusMessage write flush "Loading local dataframe..."
    path <- cachePath name
    df <- D.readCsv path
    putStrLn $ "DataFrame loaded: " <> show (D.nRows df) <> " rows"

    sendStatusMessage write flush "Generating chart..."
    let chart = multiPlot df
    let svg = encodeChartOptions chart
    let svgOneLine = BS.filter (/= (10 :: Word8)) svg
    let frameBuilder = BB.string8 "data: " <> BB.byteString svgOneLine <> BB.string8 "\n\n"
    write frameBuilder
    flush

    sendStatusMessage write flush "Complete!"
    putStrLn "Chart sent, holding connection..."
    forever $ do
      let keepAlive = BB.string8 ": keep-alive\n\n"
      write keepAlive
      flush
      threadDelay 30000000

-- | Load dataset from remote URL with download and stream single chart
loadDatasetWithDownloadChart :: Text -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
loadDatasetWithDownloadChart name url respond = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  respond $ responseStream ok200 headers $ \write flush -> do
    -- Check if cached
    cached <- isCached name
    if cached
      then do
        putStrLn $ "Using cached dataset: " <> T.unpack name
        sendStatusMessage write flush "Loading from cache..."
      else do
        sendStatusMessage write flush "Downloading dataset..."
        result <- downloadToCache url name
        case result of
          Left err -> do
            putStrLn $ "Download failed: " <> err
            sendErrorMessage write flush (T.pack err)
            exitSuccess
          Right _ -> sendStatusMessage write flush "Download complete!"

    -- Load DataFrame from cache
    putStrLn $ "Reading CSV from " <> T.unpack name
    path <- cachePath name
    df <- D.readCsv path
    putStrLn $ "DataFrame loaded: " <> show (D.nRows df) <> " rows"

    sendStatusMessage write flush "Generating chart..."
    let chart = multiPlot df
    let svg = encodeChartOptions chart
    let svgOneLine = BS.filter (/= (10 :: Word8)) svg
    let frameBuilder = BB.string8 "data: " <> BB.byteString svgOneLine <> BB.string8 "\n\n"
    write frameBuilder
    flush

    sendStatusMessage write flush "Complete!"
    putStrLn "Chart sent, holding connection..."
    forever $ do
      let keepAlive = BB.string8 ": keep-alive\n\n"
      write keepAlive
      flush
      threadDelay 30000000

-- | Stream empty chart (no dataset selected)
streamEmptyChart :: (Response -> IO ResponseReceived) -> IO ResponseReceived
streamEmptyChart respond = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  let streamBody write flush = do
        let msg = BB.string8 "data: status:Select a dataset\n\n"
        write msg
        flush
        forever $ do
          let keepAlive = BB.string8 ": keep-alive\n\n"
          write keepAlive
          flush
          threadDelay 30000000

  respond $ responseStream ok200 headers streamBody

-- | Stream frames via SSE
streamFrames :: (Response -> IO ResponseReceived) -> (Int -> BS.ByteString) -> Int -> Int -> IO ResponseReceived
streamFrames respond anim numFrames frameDelay = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  let streamBody write flush = do
        putStrLn "Client connected, waiting 3 seconds before push..."
        threadDelay 3000000
        putStrLn "Starting frame push..."

        forM_ [0 .. numFrames - 1] $ \i -> do
          let svg = anim i
          let svgOneLine = BS.filter (/= (10 :: Word8)) svg
          let frameBuilder = BB.string8 "data: " <> BB.byteString svgOneLine <> BB.string8 "\n\n"
          write frameBuilder
          flush
          threadDelay frameDelay

        putStrLn $ "✓ Push complete (" <> show numFrames <> " frames sent)"
        putStrLn "Stream holding open with keep-alive..."
        forever $ do
          let keepAlive = BB.string8 ": keep-alive\n\n"
          write keepAlive
          flush
          threadDelay 30000000

  respond $ responseStream ok200 headers streamBody
