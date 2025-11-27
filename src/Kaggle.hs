{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}



module Kaggle where

import DataFrame
import Chart
import Prelude
import Prettychart
import DataFrame as D
import DataFrame.Internal.Statistics
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Exts
import GHC.Generics
import Optics.Core

go :: IO ()
go = putStrLn "go"


data BoxPlotOptions =
  BoxPlotOptions {
    q2Style :: Style,
    q3Style :: Style,
    q1Style :: Style,
    q4Style :: Style
  } deriving (Generic, Show, Eq)

defaultBoxPlotOptions = BoxPlotOptions defaultRectStyle defaultRectStyle defaultLineStyle defaultLineStyle

boxPlot :: BoxPlotOptions -> VU.Vector Double -> ChartOptions
boxPlot o v = c
  where
    qs = VU.toList $ quantiles' (VU.fromList [0,1,2,3,4]) 4 v
    l1 = LineChart (view #q1Style o) [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l4 = LineChart (view #q4Style o) [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r2 = RectChart (view #q2Style o)  [Rect 0 1 (qs !! 1) (qs !! 2)]
    r3 = RectChart (view #q3Style o) [Rect 0 1 (qs !! 2) (qs !! 3)]
    c = (mempty :: ChartOptions) &
      set (#markupOptions % #chartAspect) (FixedAspect 0.25) &
      set #hudOptions defaultHudOptions &
      over (#hudOptions % #axes) (Prelude.drop 1) &
      set #chartTree (named "boxplot" [l1,r2,r3,l4])
