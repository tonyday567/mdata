{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Interface layer for dataframe chart-svg.
module DataFrame.Plot where

import Chart
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as T
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import DataFrame qualified as D
import DataFrame.Internal.Statistics qualified as D
import Flow
import GHC.Exts
import GHC.Generics
import NumHask qualified as N
import NumHask.Space qualified as N
import Optics.Core hiding ((<|), (|>))
import Prettychart
import Prelude as P

data HistogramOptions
  = HistogramOptions
  { style :: Style,
    numBins :: Int
  }
  deriving (Generic, Show, Eq)

histogram :: HistogramOptions -> [Double] -> ChartOptions
histogram o xs = ch'
  where
    ch' = (mempty :: ChartOptions) |> set #chartTree (named "histogram" [c]) |> set #hudOptions ho
    ho =
      mempty
        |> set #axes [Priority 5 (defaultXAxisOptions |> set (#ticks % #lineTick) Nothing)]
    c = RectChart (view #style o) rects
    r = N.unsafeSpace1 xs :: (Range Double)
    hcuts = N.gridSensible N.OuterPos False r (view #numBins o)
    h = N.fill hcuts xs
    rects = filter (\(Rect _ _ _ y') -> y' /= 0) $ N.makeRects (N.IncludeOvers (N.width r / fromIntegral (view #numBins o))) h

defaultHistogramOptions :: HistogramOptions
defaultHistogramOptions = HistogramOptions (defaultRectStyle |> set #color (paletteO 2 0.2) |> set #borderColor (paletteO 2 1)) 10


-- * scatter

data ScatterOptions
  = ScatterOptions
  { styles :: [Style],
    maxScatters :: Int,
    maxPoints :: Int
  }
  deriving (Generic, Show, Eq)

defaultScatterOptions = ScatterOptions (P.take 8 $ zipWith (\s c -> defaultGlyphStyle |> set #glyphShape s |> set #color c) (gpalette <$> [0 .. 7]) (palette <$> [0 .. 7])) 8 1000

-- | first vector is the x axis values
scatter :: ScatterOptions -> [Double] -> [Double] -> ChartOptions
scatter o xs ys = co
  where
    c = GlyphChart (head (view #styles o)) (P.take (view #maxPoints o) (zipWith Point xs ys))
    co = mempty |> set #chartTree (named "scatter" [c]) |> set #hudOptions defaultHudOptions

-- | first vector is the x axis values
scatters :: ScatterOptions -> [(T.Text, [Point Double])] -> ChartOptions
scatters o xs = co
  where
    cs = zipWith (\x s -> GlyphChart s (P.take (view #maxPoints o) x)) (snd <$> xs) (view #styles o)
    co = mempty |> set #chartTree (named "scatters" cs) |> set #hudOptions defaultHudOptions

-- * pies

data Secant = Secant {origin :: Point Double, radius :: Double, startAngle :: Double, endAngle :: Double} deriving (Generic)

ra = (+ (-0.25)) .> (* (-2 * pi)) .> ray @(Point Double)

secantPie :: Secant -> [PathData Double]
secantPie (Secant o r a0 a1) = singletonPie o (ArcPosition (o N.+ ra a0) (o N.+ ra a1) (ArcInfo (Point r r) 0 False True))

-- | Convert an [a] into a [Range a], where the input represents intervals on a real line.
--
-- This should be
-- > grid . ungrid == id
ungrid :: (Num a) => [a] -> [Range a]
ungrid xs = zipWith Range acc0 (drop 1 acc0)
  where
    acc0 = List.scanl' (+) 0 xs

-- | SecantOptions
--
-- offsets are magnitude away from the origin versus the unit circle
-- The direction of the offset is equal to the angle formed by the center of the slice.
data SecantOptions = SecantOptions {pathStyle :: Style, offset :: Double, textStyle :: Style, textOffset :: Double} deriving (Generic, Show, Eq)

defaultSecantOptions = SecantOptions (defaultPathStyle |> set #borderSize zero) 0.05 (defaultTextStyle |> set #size 0.05) 0.7

data PieOptions
  = PieOptions
  { secants :: [SecantOptions]
  }
  deriving (Generic, Show, Eq)

defaultPieOptions :: PieOptions
defaultPieOptions = PieOptions (fmap (\c -> defaultSecantOptions |> set (#pathStyle % #color) (paletteO c 0.3) |> set (#pathStyle % #borderColor) (paletteO c 1) |> set (#textStyle % #color) (palette c |> over lightness' (* 0.6))) [0 .. 8])

-- | pie
--
pie :: PieOptions -> [(T.Text, Double)] -> ChartOptions
pie o x = co
  where
    vs = snd <$> x
    ls = fst <$> x
    rs = ungrid vs
    cs = zipWith (\so r -> PathChart (view #pathStyle so) (secantPie (Secant ((view #offset so) N.*| ra (mid r)) one (N.lower r) (N.upper r)))) (view #secants o) rs
    ct = zipWith3 (\so m l -> TextChart (view #textStyle so) [(l, (view #textOffset so) N.*| ra m)]) (view #secants o) (fmap N.mid rs) ls
    co = (mempty :: ChartOptions) & set (#markupOptions % #chartAspect) ChartAspect & set #chartTree ((cs <> ct) |> named "pie")

-- | switch a pie slice to a new Style
switchStyle :: Int -> SecantOptions -> PieOptions -> PieOptions
switchStyle x so o = o |> over (#secants % each % #textStyle % #color % lightness') (min 0.4) |> set (#secants % ix x) so

-- | darken
switchStyles :: [Int] -> SecantOptions -> PieOptions -> PieOptions
switchStyles xs so o = foldl' (\acc x -> switchStyle x so acc) o xs


-- | Adds another title at the top.
setTitle :: T.Text -> ChartOptions -> ChartOptions
setTitle t co = co |> over (#hudOptions % #titles) ((Priority 8 (defaultTitleOptions t |> set #place PlaceTop |> set (#style % #size) 0.08 |> set (#style % #color) (paletteO 2 1))):)

addTitle :: Place -> Double -> T.Text -> HudOptions -> HudOptions
addTitle p s t = over #titles (<> [Priority 8 (defaultTitleOptions t & set (#style % #size) s & set #place p)])


-- * box plot

data BoxPlotOptions
  = BoxPlotOptions
  { q2Style :: Style,
    q3Style :: Style,
    q1Style :: Style,
    q4Style :: Style
  }
  deriving (Generic, Show, Eq)

defaultBoxPlotOptions = BoxPlotOptions defaultRectStyle defaultRectStyle defaultLineStyle defaultLineStyle

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
  { itemStyles :: [(Style, Style)],
    maxStacks :: Int
  }
  deriving (Generic, Show, Eq)

defaultStackedBarOptions = StackedBarOptions (fmap (\c -> (defaultRectStyle |> set #borderSize zero |> set #color c, defaultTextStyle |> set #size 0.05 |> set #color c |> over (#color % lightness') (*0.6))) (palette <$> [0 .. 7])) 8

stackedBar :: StackedBarOptions -> T.Text -> [(T.Text, Double)] -> ChartOptions
stackedBar o t xs = co
  where
    ls = fmap fst xs
    vs = fmap snd xs
    bd = BarData (fmap pure vs) [t] ls
    bc = barChart (defaultBarOptions |> set #displayValues False |> set #barStacked Stacked |> set #barRectStyles (view #itemStyles o |> fmap fst)) bd
    acc0 = List.scanl' (+) 0 vs <> [1]
    mids = zipWith (\a0 a1 -> (a0+a1)/2) acc0 (List.drop 1 acc0)
    ct = zipWith (\s (t,a) -> TextChart s [(t, Point zero (0.5-a))]) (view #itemStyles o |> fmap snd) (zip ls mids)
    co = bc |> set (#markupOptions % #chartAspect) (FixedAspect 0.4) |> set (#hudOptions % #legends) mempty |> over #chartTree (<> named "labels" ct)

-- * weird helpers
arcZero :: [PathData Double] -> Bool
arcZero [] = False
arcZero [_] = False
arcZero (x : y@(ArcP _ p1) : xs) = pointPath x == p1 || arcZero (y : xs)
arcZero (_ : y : xs) = arcZero (y : xs)

noText :: ChartOptions -> ChartOptions
noText co = over (#chartTree % charts') (filter (\c -> view #chartData c |> isTextData |> not)) co
  where
    isTextData (TextData _) = True
    isTextData _ = False
