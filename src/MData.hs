{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | A place to put functions until they get dumped or transferred elsewhere
module MData where

import Data.List qualified as List
import Chart
import Prelude as P
import Prettychart
import DataFrame qualified as D
import DataFrame.Internal.Statistics qualified as D
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Exts
import GHC.Generics
import Optics.Core hiding ((|>),(<|))
import Data.Text qualified as T
import NumHask.Space qualified as N
import NumHask qualified as N
import Flow
import Data.Maybe

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
    qs = VU.toList $ D.quantiles' (VU.fromList [0,1,2,3,4]) 4 v

    l1 = LineChart (view #q1Style o) [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l4 = LineChart (view #q4Style o) [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r2 = RectChart (view #q2Style o)  [Rect 0 1 (qs !! 1) (qs !! 2)]
    r3 = RectChart (view #q3Style o) [Rect 0 1 (qs !! 2) (qs !! 3)]
    c = (mempty :: ChartOptions) &
      set (#markupOptions % #chartAspect) (FixedAspect 0.25) &
      set #hudOptions defaultHudOptions &
      over (#hudOptions % #axes) (P.drop 1) &
      set #chartTree (named "boxplot" [l1,r2,r3,l4])

data ScatterPlotOptions =
  ScatterPlotOptions {
    scatterStyles :: [Style],
    maxScatters :: Int,
    maxPoints :: Int,
    titleX :: TitleOptions,
    titleY :: TitleOptions
  } deriving (Generic, Show, Eq)

defaultScatterPlotOptions = ScatterPlotOptions (P.take 8 $ zipWith (\s c -> defaultGlyphStyle & set #glyphShape s & set #color c) (gpalette <$> [0..7]) (palette <$> [0..7])) 8 1000 (defaultTitleOptions mempty & set (#style % #size) 0.06 & set #place PlaceBottom) (defaultTitleOptions mempty & set (#style % #size) 0.06 & set #buffer 0.1 & set #place PlaceLeft)

-- | first vector is the x axis values
scatterPlot :: ScatterPlotOptions -> (Maybe T.Text, VU.Vector Double) -> (Maybe T.Text, VU.Vector Double) -> ChartOptions
scatterPlot o (t0, v0) (t1, v1) = ch'
  where
    c = GlyphChart (head (view #scatterStyles o)) (P.take (view #maxPoints o) $ zipWith Point (VU.toList v0) (VU.toList v1))
    ch' = (mempty :: ChartOptions) & set #chartTree (named "scatterPlot" [c]) & set #hudOptions ho
    ho = defaultHudOptions & maybe id (\tx -> over #titles ((Priority 8 (view #titleX o & set #text tx)):)) t0 & maybe id (\ty -> over #titles ((Priority 8 (view #titleY o & set #text ty)):)) t1


data Secant = Secant { origin :: Point Double, radius :: Double, startAngle :: Double, endAngle :: Double } deriving (Generic)

ra = (+(-0.25)) .> (*(-2 * pi)) .> ray @(Point Double)

secantPie :: Secant -> [PathData Double]
secantPie (Secant o r a0 a1) = singletonPie o (ArcPosition (o N.+ ra a0) (o N.+ ra a1) (ArcInfo (Point r r) 0 False True))

-- | Convert an [a] into a [Range a], where the input represents intervals on a real line.
--
-- This should be
-- > grid . ungrid == id
--
ungrid :: Num a => [a] -> [Range a]
ungrid xs = zipWith Range acc0 (drop 1 acc0)
  where
    acc0 = List.scanl' (+) 0 xs

-- | SecantOptions
--
-- offsets are magnitude away from the origin versus the unit circle
-- The direction of the offset is equal to the angle formed by the center of the slice.
data SecantOptions = SecantOptions { pathStyle :: Style, offset :: Double, textStyle :: Style, textOffset :: Double } deriving (Generic, Show, Eq)

defaultSecantOptions = SecantOptions (defaultPathStyle |> set #borderSize zero) 0.05 (defaultTextStyle |> set #size 0.05) 0.7

data PiePlotOptions =
  PiePlotOptions {
    secants :: [SecantOptions],
    title :: Maybe TitleOptions
  } deriving (Generic, Show, Eq)

defaultPiePlotOptions :: PiePlotOptions
defaultPiePlotOptions = PiePlotOptions (fmap (\c -> defaultSecantOptions |> set (#pathStyle % #color) (paletteO c 0.3) |> set (#pathStyle % #borderColor) (paletteO c 1) |> set (#pathStyle % #color) (palette c |> over lightness' (*0.6))) [0..8]) (Just (defaultTitleOptions mempty |> set #place PlaceBottom |> set (#style % #size) 0.08 |> set (#style % #color) (paletteO 2 1)))

-- | piePlot
--
-- >>> ls = T.pack <$> D.columnAsList @String "item" df
-- >>> vs = D.columnAsList @Double "prop" df
-- >>> -- piePlot defaultPiePlotOptions (zip ls vs)
piePlot :: PiePlotOptions -> [(T.Text, Double)] -> ChartOptions
piePlot o x = co
  where
    vs = snd <$> x
    ls = fst <$> x
    rs = ungrid (vs <> [one])
    cs = zipWith (\so r -> PathChart (view #pathStyle so) (secantPie (Secant ((view #offset so) N.*| ra (mid r)) one (N.lower r) (N.upper r)))) (view #secants o) rs
    ct = zipWith3 (\so m l -> TextChart (view #textStyle so) [(l, (view #textOffset so) N.*| ra m)]) (view #secants o) (fmap N.mid rs) ls
    co = (mempty :: ChartOptions) & set (#markupOptions % #chartAspect) ChartAspect & set #chartTree ((cs <> ct) |> named "piePlot")

histPlot :: HistPlotOptions -> (Maybe T.Text, VU.Vector Double) -> ChartOptions
histPlot o (t, v) = ch'
  where
    ch' = (mempty :: ChartOptions) & set #chartTree (named "histPlot" [c]) & set #hudOptions ho
    ho =
      mempty |>
      set #axes [ Priority 5 (defaultXAxisOptions |> set (#ticks % #lineTick) Nothing)] |>
      set #titles ((\t' -> Priority 8 (view #title o |> set #text t')) <$> maybeToList t)
    c = RectChart (view #style o) rects
    r = N.unsafeSpace1 $ VU.toList v :: (Range Double)
    hcuts = N.gridSensible N.OuterPos False r (view #grain o)
    h = N.fill hcuts (VU.toList v)
    rects = filter (\(Rect _ _ _ y') -> y' /= 0) $ N.makeRects (N.IncludeOvers (N.width r / fromIntegral (view #grain o))) h

data HistPlotOptions =
  HistPlotOptions {
    style :: Style,
    grain :: Int,
    title :: TitleOptions
  } deriving (Generic, Show, Eq)

defaultHistPlotOptions :: HistPlotOptions
defaultHistPlotOptions = HistPlotOptions (defaultRectStyle |> set #color (paletteO 2 0.2) |> set #borderColor (paletteO 2 1)) 10 (defaultTitleOptions mempty |> set #place PlaceBottom |> set (#style % #size) 0.08 |> set (#style % #color) (paletteO 2 1))

