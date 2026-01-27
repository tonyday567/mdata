{-# LANGUAGE OverloadedStrings #-}

-- | Generate example charts from popular datasets
module Main where

import Chart (encodeChartOptions)
import Data.ByteString qualified as BS
import DataFrame.Chart (multiPlot)
import DataFrame.Plot (getDF)
import System.FilePath ((</>))

main :: IO ()
main = do
  let datasets = ["s5e11", "adult", "cars"]
  let outDir = "other"

  mapM_ (\ds -> generateChart ds outDir) datasets
  putStrLn "✓ Example charts generated in other/"

generateChart :: String -> FilePath -> IO ()
generateChart name dir = do
  putStrLn $ "Generating chart for " <> name <> "..."
  df <- getDF name
  let chart = multiPlot df
  let svg = encodeChartOptions chart
  let outFile = dir </> (name <> "-chart.svg")
  BS.writeFile outFile svg
  putStrLn $ "  ✓ " <> outFile
