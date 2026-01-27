{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module DataFrame.Chart where

import Prettyprinter
import Chart
import Data.List qualified as List
import Data.List (sortOn)
import Data.Maybe
import Data.Text qualified as T
import Data.Text (Text)
import Data.Map.Strict qualified as Map
import Data.Typeable (typeRep, typeOf, Typeable)
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import DataFrame qualified as D
import DataFrame.Functions qualified as F
import DataFrame.Internal.Statistics qualified as D
import DataFrame.Internal.Expression qualified as D
import Flow
import GHC.Exts
import GHC.Generics
import NumHask qualified as N
import NumHask.Space qualified as N
import Optics.Core hiding ((<|), (|>))
import Prettychart
import Prelude as P
import Data.List (foldl')
import Data.Ord (Down (..))

addTitle :: Place -> Double -> T.Text -> HudOptions -> HudOptions
addTitle p s t = over #titles (<> [Priority 8 (defaultTitleOptions t & set (#style % #size) s & set #place p)])

-- * box plot
data BoxPlotOptions
    = BoxPlotOptions
    { q2Style :: Style
    , q3Style :: Style
    , q1Style :: Style
    , q4Style :: Style
    }
    deriving (Generic, Show, Eq)

defaultBoxPlotOptions = BoxPlotOptions defaultRectStyle defaultRectStyle defaultLineStyle defaultLineStyle

{- | Create a box-n-whiskers plot.

>>> boxPlot defaultBoxPlotOptions [1..1000]
-}
boxPlot :: BoxPlotOptions -> [Double] -> ChartOptions
boxPlot o v = c
  where
    qs = VU.toList $ D.quantiles' (VU.fromList [0, 1, 2, 3, 4]) 4 (VU.fromList v)

    l1 = LineChart (view #q1Style o) [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l4 = LineChart (view #q4Style o) [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r2 = RectChart (view #q2Style o) [Rect 0 1 (qs !! 1) (qs !! 2)]
    r3 = RectChart (view #q3Style o) [Rect 0 1 (qs !! 2) (qs !! 3)]
    c =
        (mempty :: ChartOptions)
            & set (#markupOptions % #chartAspect) (FixedAspect 0.25)
            & set #hudOptions defaultHudOptions
            & over (#hudOptions % #axes) (P.drop 1)
            & set #chartTree (named "boxplot" [l1, r2, r3, l4])

data StackedBarOptions
    = StackedBarOptions
    { itemStyles :: [(Style, Style)]
    , maxStacks :: Int
    }
    deriving (Generic, Show, Eq)

defaultStackedBarOptions = StackedBarOptions (fmap (\c -> (defaultRectStyle |> set #borderSize zero |> set #color c |> set (#color % opac') 0.2, defaultTextStyle |> set #size 0.06 |> set #color c |> set (#color % opac') 0.6 |> over (#color % lightness') (* 0.7))) (palette <$> [0 .. 19])) 20

-- | Create a stacked bar plot
--
-- >>> writeChartOptions "other/stacked-bar.svg" $ stackedBar defaultStackedBarOptions "test" (zip  ["person","woman","man","camera","tv"] [20,23.1,31,16,10])
--
stackedBar :: StackedBarOptions -> T.Text -> [(T.Text, Double)] -> ChartOptions
stackedBar o t xs = co
  where
    ls = fmap fst xs
    vs' = fmap snd xs
    vs = (/(sum vs')) <$> vs'
    bd = BarData (fmap pure vs) [t] ls
    bc = barChart (defaultBarOptions |> set #displayValues False |> set #barStacked Stacked |> set #barRectStyles (view #itemStyles o |> fmap fst)) bd
    acc0 = List.scanl' (+) 0 vs
    mids = zipWith (\a0 a1 -> (a0 + a1) / 2) acc0 (List.drop 1 acc0)
    ct = zipWith (\s (t, a) -> TextChart s [(t, Point zero (0.5 - a))]) (view #itemStyles o |> fmap snd) (zip ls mids)
    co =
      bc
        |> set (#hudOptions % #axes % each % #item % #axisBar) Nothing
        |> set (#hudOptions % #axes % each % #item % #ticks % #glyphTick) Nothing
        |> set (#hudOptions % #axes % each % #item % #ticks % #textTick % _Just % #style % #size) 0.08
        |> set (#hudOptions % #axes % each % #item % #adjustments) (Just $ Adjustments 0.08 0.16 0.2 True)
        |> set (#markupOptions % #chartAspect) (FixedAspect 0.4)
        |> set (#hudOptions % #legends) mempty
        |> over #chartTree (<> named "labels" ct)

-- | create a chart of the database columns
--
-- >>> writeChartOptions "Test.svg" =<< multiPlot <$> getDF
--
multiPlot :: D.DataFrame -> ChartOptions
multiPlot df = horiCO AlignRight 0.05 (frameless <$> cs) |> over #chartTree (\x -> x <> frameChart clear 0.1 x)
  where
    colTypes = df |> D.describeColumns |> D.columnAsList (F.col @Text "Type")
    labels = df |> D.describeColumns |> D.columnAsList (F.col @Text "Column Name")
    cs = zipWith (makeChartByType df) labels colTypes
    frameless c = c |> set (#hudOptions % #frames) []

makeChartByType :: D.DataFrame -> Text -> Text -> ChartOptions
makeChartByType df colname t = case t of
  "Double" -> boxPlotForColumn df colname
  "Int" -> boxPlotForColumnInt df colname
  "Text" -> stackedBarForColumn df colname
  "Bool" -> stackedBarForColumn df colname
  "Maybe Double" -> boxPlotForColumnMaybe df colname
  "Maybe Int" -> boxPlotForColumnIntMaybe df colname
  "Maybe Text" -> stackedBarForColumnMaybe df colname
  "Maybe Bool" -> stackedBarForColumnMaybe df colname
  _ -> mempty

boxPlotForColumn :: D.DataFrame -> Text -> ChartOptions
boxPlotForColumn df colName = boxPlot defaultBoxPlotOptions vals |> ho
  where
    vals = D.columnAsList @Double (F.col colName) df
    ho = over #hudOptions (addTitle PlaceBottom 0.03 colName)

boxPlotForColumnMaybe :: D.DataFrame -> Text -> ChartOptions
boxPlotForColumnMaybe df colName = boxPlot defaultBoxPlotOptions vals |> ho
  where
    vals = catMaybes (D.columnAsList @(Maybe Double) (F.col colName) df)
    ho = over #hudOptions (addTitle PlaceBottom 0.03 colName)

boxPlotForColumnInt :: D.DataFrame -> Text -> ChartOptions
boxPlotForColumnInt df colName = boxPlot defaultBoxPlotOptions (fromIntegral <$> vals) |> ho
  where
    vals = D.columnAsList @Int (F.col colName) df
    ho = over #hudOptions (addTitle PlaceBottom 0.03 colName)

boxPlotForColumnIntMaybe :: D.DataFrame -> Text -> ChartOptions
boxPlotForColumnIntMaybe df colName = boxPlot defaultBoxPlotOptions (fromIntegral <$> vals) |> ho
  where
    vals = catMaybes (D.columnAsList @(Maybe Int) (F.col colName) df)
    ho = over #hudOptions (addTitle PlaceBottom 0.03 colName)

stackedBarForColumn :: D.DataFrame -> Text -> ChartOptions
stackedBarForColumn df colName =
  let vals = D.columnAsList @Text (F.col colName) df
      -- Count occurrences using Map
      counts = foldl' (\m v -> Map.insertWith (+) v (1::Int) m) Map.empty vals
      -- Sort by count descending
      sorted = sortOn (Down . snd) (Map.toList counts)
      -- Take top 10 categories
      top10 = take 10 sorted
      -- Convert to [(Text, Double)] format
      chartData = [(fst pair, fromIntegral (snd pair)) | pair <- top10]
  in (stackedBar defaultStackedBarOptions colName chartData)

stackedBarForColumnMaybe :: D.DataFrame -> Text -> ChartOptions
stackedBarForColumnMaybe df colName =
  let vals = catMaybes (D.columnAsList @(Maybe Text) (F.col colName) df)
      -- Count occurrences using Map
      counts = foldl' (\m v -> Map.insertWith (+) v (1::Int) m) Map.empty vals
      -- Sort by count descending
      sorted = sortOn (Down . snd) (Map.toList counts)
      -- Take top 10 categories
      top10 = take 10 sorted
      -- Convert to [(Text, Double)] format
      chartData = [(fst pair, fromIntegral (snd pair)) | pair <- top10]
  in (stackedBar defaultStackedBarOptions colName chartData)

-- | Counter chart generator: x -> ChartOptions
counterChart :: Int -> ChartOptions
counterChart x = mempty |> set #chartTree (named (T.pack "counter")
      (pure $ TextChart defaultTextStyle [(T.pack (show x), zero)]))
