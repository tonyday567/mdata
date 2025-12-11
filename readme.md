
# mdata

This repo contains:

-   a [dataframe](https://github.com/mchav/dataframe) example taken from a kaggle run.
-   [perf](https://github.com/tonyday567/perf) which is being used to measure performance of common usage patterns.
-   [chart-svg](https://github.com/tonyday567/chart-svg) | dataframe integration and development
-   a live chart build using [prettychart](https://github.com/tonyday567/prettychart)
-   some CI infrastructure to begin to measure integration.
-   some pandoc conversion experiments: from org => markdown => ipynb
    (This is not catered for in nbconvert or jupytext wrt outputs)


# Imports

    :r
    
    :set -XNoImplicitPrelude
    :set -XImportQualifiedPost
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -XTupleSections
    :set -XQuasiQuotes
    
    -- base, text & bytestring encoding (compatability check, also)
    import Prelude as P
    import NumHask.Prelude qualified as N
    import Control.Category ((>>>))
    import Data.Function
    import Data.Maybe
    import Data.Bool
    import Data.List qualified as List
    import Control.Monad
    import Data.Bifunctor
    import Data.ByteString.Char8 qualified as C
    import Data.Text qualified as T
    
    -- prettyprinter (dev help)
    import Prettyprinter
    
    -- common dataframe imports
    import DataFrame qualified as D
    import DataFrame.Functions qualified as F
    import DataFrame.Internal.Expression qualified as D
    import DataFrame.Internal.Statistics qualified as D
    import qualified Data.Vector.Algorithms.Intro as VA
    import qualified Data.Vector.Unboxed as VU
    import qualified Data.Vector.Unboxed.Mutable as VUM
    
    -- common chart-svg imports
    import Chart
    import Prettychart
    import Chart.Examples
    import Optics.Core hiding ((|>),(<|))
    import Control.Lens qualified as Lens
    import Data.Data.Lens qualified as Lens
    
    -- dev helpers
    import Perf
    import Flow
    
    -- functions not yet transferred elsewhere
    import MData
    
    -- example data from https://www.kaggle.com/competitions/playground-series-s5e11
    dfTest <- D.readCsv "data/s5e11/test.csv"

    Configuration is affected by the following files:
    - cabal.project
    Build profile: -w ghc-9.12.2 -O1
    In order, the following will be built (use -v for more details):
     - mdata-0.1.0.0 (interactive) (lib) (first run)
    Preprocessing library for mdata-0.1.0.0...
    GHCi, version 9.12.2: https://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling MData            ( src/MData.hs, interpreted )
    Ok, one module loaded.
    Ok, one module reloaded.


## Live charts

This gives you a browser page and live charting capabilities.

    (display, quit) <- startChartServer (Just "mdata")
    disp x = display $ x & set (#markupOptions % #markupHeight) (Just 250) & set (#hudOptions % #frames % ix 1 % #item % #buffer) 0.1

    Setting phasers to stun... (port 9160) (ctrl-c to gqhuciit>)

<http://localhost:9160/>

testing, testing; one, two, three

    disp unitExample

    True


## dataframe creation


### direct method

    df0 = mempty |> D.insert "item" ["person","woman","man","camera","tv"] |> D.insert "value" [20,23.1,31,16,10]
    v = F.col @Double "value"
    xs = D.columnAsList @Double "value" df0
    xs' = (/ sum xs) <$> xs
    df = D.insert "prop" xs' df0
    df

    -------------------------------------
     item  | value  |        prop
    -------|--------|--------------------
    [Char] | Double |       Double
    -------|--------|--------------------
    person | 20.0   | 0.1998001998001998
    woman  | 23.1   | 0.2307692307692308
    man    | 31.0   | 0.3096903096903097
    camera | 16.0   | 0.15984015984015984
    tv     | 10.0   | 9.99000999000999e-2


### expr method

    df0 = mempty |> D.insert "item" ["person","woman","man","camera","tv"] |> D.insert "value" [20,23.1,31,16,10]
    v = F.col @Double "value"
    prop e = e / F.sum e
    df = D.derive "prop" (prop v) df0
    df

    --------------------------------------
     item  | value  |         prop
    -------|--------|---------------------
    [Char] | Double |        Double
    -------|--------|---------------------
    person | 20.0   | 0.16652789342214822
    woman  | 23.1   | 0.1923397169025812
    man    | 31.0   | 0.2581182348043297
    camera | 16.0   | 0.13322231473771856
    tv     | 10.0   | 8.326394671107411e-2


### F.sum bug?

    df0 = mempty |> D.insert "value" [20,23.1,31,16,10]
    v = F.col @Double "value"
    df = D.derive "sum" (F.sum v) df0
    df

    ---------------
    value  |  sum
    -------|-------
    Double | Double
    -------|-------
    20.0   | 120.1
    23.1   | 120.1
    31.0   | 120.1
    16.0   | 120.1
    10.0   | 120.1


## stacked bar


### version 1: single stacked vertical bar chart

    ls = T.pack <$> D.columnAsList @String "item" df
    vs = D.columnAsList @Double "prop" df
    bd = BarData (fmap pure vs) ["item"] ls
    bd

    BarData {barData = [[0.16652789342214822],[0.1923397169025812],[0.2581182348043297],[0.13322231473771856],[8.326394671107411e-2]], barRowLabels = ["item"], barColumnLabels = ["person","woman","man","camera","tv"]}

    bc = barChart (defaultBarOptions |> set #displayValues False |> set #barStacked Stacked |> set (#barRectStyles % each % #borderSize) 0) bd
    disp bc
    writeChartOptions "other/bar1.svg" bc

![img](other/bar1.svg)


### version 2: skinny

    bc' = Lens.transformOnOf Lens.template Lens.uniplate (over chroma' (*1.5) .> over opac' (*0.6)) bc |> set (#markupOptions % #chartAspect) (FixedAspect 0.4)
    
    disp (bc')
    writeChartOptions "other/bar2.svg" bc'

![img](other/bar2.svg)


### version 3: remove legend and embed labels

    
    acc0 = List.scanl' (+) 0 vs <> [1]
    mids = zipWith (\a0 a1 -> (a0+a1)/2) acc0 (List.drop 1 acc0)
    ct = zipWith (\c (t,a) -> TextChart (defaultTextStyle |> set #size 0.05 |> set #color (palette c |> over lightness' (*0.6))) [(t, Point zero (0.5-a))]) [0..] (zip ls mids)
    
    bc'' = bc' |> set (#hudOptions % #legends) mempty |> over #chartTree (<> named "labels" ct)
    
    disp (bc'')
    writeChartOptions "other/bar3.svg" bc''

![img](other/bar3.svg)


## pie secants

Pie chart convention starts at the y-axis and lays out secant slices clockwise.

\`ra\` maps (0,1) (the proportional pie slice) into a point on a unit circle (by this convetion).

    ra = (+(-0.25)) .> (*(-2 * pi)) .> ray @(Point Double)
    secantPie (Secant o r a0 a1) = singletonPie o (ArcPosition (o N.+ ra a0) (o N.+ ra a1) (ArcInfo (Point r r) 0 False True))

This is a very common scan for a Column.

    acc0 = List.scanl' (+) 0 vs <> [1]
    mids = zipWith (\a0 a1 -> (a0+a1)/2) acc0 (List.drop 1 acc0)
    
    xs = zipWith (\a0 a1 -> secantPie (Secant (0.05 N.*| ra ((a0+a1)/2)) one a0 a1)) acc0 (List.drop 1 acc0)
    
    cs = zipWith (\c x -> PathChart (defaultPathStyle |> set #borderSize 0 |> set #color (paletteO c 0.3)) x) [0..] xs
    
    ct = zipWith (\c (t,a) -> TextChart (defaultTextStyle |> set #size 0.05 |> set #color (palette c & over lightness' (*0.6))) [(t, 0.7 N.*| ra a)]) [0..] (zip ls mids)
    co = (mempty :: ChartOptions) & set (#markupOptions % #chartAspect) ChartAspect & set #chartTree ((cs <> ct) |> unnamed)
    disp co
    writeChartOptions "other/pie.svg" co

![img](other/pie.svg)

![img](other/pie.svg)


# kaggle example


## dataframe check

    D.describeColumns df

    -----------------------------------------------------------------
        Column Name      | # Non-null Values | # Null Values |  Type
    ---------------------|-------------------|---------------|-------
            Text         |        Int        |      Int      |  Text
    ---------------------|-------------------|---------------|-------
    grade_subgrade       | 254569            | 0             | Text
    loan_purpose         | 254569            | 0             | Text
    employment_status    | 254569            | 0             | Text
    education_level      | 254569            | 0             | Text
    marital_status       | 254569            | 0             | Text
    gender               | 254569            | 0             | Text
    interest_rate        | 254569            | 0             | Double
    loan_amount          | 254569            | 0             | Double
    credit_score         | 254569            | 0             | Int
    debt_to_income_ratio | 254569            | 0             | Double
    annual_income        | 254569            | 0             | Double
    id                   | 254569            | 0             | Int

    D.summarize df

    --------------------------------------------------------------------------------------------------------
    Statistic |    id    | annual_income | debt_to_income_ratio | credit_score | loan_amount | interest_rate
    ----------|----------|---------------|----------------------|--------------|-------------|--------------
      Text    |  Double  |    Double     |        Double        |    Double    |   Double    |    Double
    ----------|----------|---------------|----------------------|--------------|-------------|--------------
    Count     | 254569.0 | 254569.0      | 254569.0             | 254569.0     | 254569.0    | 254569.0
    Mean      | 721278.0 | 48233.08      | 0.12                 | 681.04       | 15016.75    | 12.35
    Minimum   | 593994.0 | 6011.77       | 1.0e-2               | 395.0        | 500.05      | 3.2
    25%       | 657636.0 | 27950.3       | 7.0e-2               | 646.0        | 10248.58    | 10.98
    Median    | 721278.0 | 46528.98      | 0.1                  | 683.0        | 15000.22    | 12.37
    75%       | 784920.0 | 61149.44      | 0.16                 | 719.0        | 18831.46    | 13.69
    Max       | 848562.0 | 380653.94     | 0.63                 | 849.0        | 48959.26    | 21.29
    StdDev    | 73487.88 | 26719.66      | 7.0e-2               | 55.62        | 6922.17     | 2.02
    IQR       | 127284.0 | 33199.14      | 8.0e-2               | 73.0         | 8582.88     | 2.71
    Skewness  | 0.0      | 1.72          | 1.42                 | -0.17        | 0.21        | 4.0e-2


# chart dev


## boxPlot example

    c0 = (either (error . show) id) (D.columnAsDoubleVector "interest_rate" df)
    ch = boxPlot defaultBoxPlotOptions c0
    writeChartOptions "other/box1.svg" ch
    disp ch

    True

![img](other/box1.svg)


## scatterPlot example

    True

    c0 = (either (error . show) id) (D.columnAsDoubleVector "interest_rate" df)
    c1 = (either (error . show) id) (D.columnAsDoubleVector "loan_amount" df)
    
    ch = GlyphChart defaultGlyphStyle (Prelude.take 1000 $ zipWith Point (VU.toList c0) (VU.toList c1))
    
    ch' = (mempty :: ChartOptions) & set #chartTree (named "scatterPlot" [ch]) & set #hudOptions defaultHudOptions & set (#hudOptions % #titles) [(Priority 8 (defaultTitleOptions "interest_rate" & set #place PlaceBottom & set (#style % #size) 0.06)),(Priority 8 (defaultTitleOptions "loan_amount" & set #place PlaceLeft & set (#style % #size) 0.06 & set #buffer 0.1))]
    
    writeChartOptions "other/scatter1.svg" ch'
    disp ch'

    True

Using MData.scatterPlot

    v0 = (either (error . show) id) (D.columnAsDoubleVector "interest_rate" df)
    v1 = (either (error . show) id) (D.columnAsDoubleVector "loan_amount" df)
    ch = scatterPlot defaultScatterPlotOptions (Just "interest_rate", v0) (Just "loan_amount", v1)
    
    writeChartOptions "other/scatter1.svg" ch
    disp ch

    True

![img](other/scatter1.svg)


# reference

Comparable python:

<https://www.kaggle.com/code/ravitejagonnabathula/predicting-loan-payback>

notebook best practice:

<https://marimo.io/blog/lessons-learned>

converting to ipynb:

<https://pandoc.org/installing.html>

    pandoc readme.md -o mdata.ipynb

chart-svg api tree

![img](https://hackage-content.haskell.org/package/chart-svg-0.8.2.1/docs/other/ast.svg)


# (deprecated) testing snippets


## file read testing

It&rsquo;s a good chunky first example.

    s <- readFile "other/test.csv"
    length s

    23021430

    rf = readFile "other/test.csv"
    (m,n) <- tickIO (length <$> rf)
    print n
    toSecs m

    23021430
    0.144087667

    (m,df) <- tickIO (D.readCsv "other/test.csv")
    print $ toSecs m
    :t df

    0.944859458
    df :: DataFrame

Example data is  from <https://www.kaggle.com/competitions/playground-series-s5e11>


## get a Column and compute quartiles.

    c = (either (error . show) id) (columnAsDoubleVector "interest_rate" df)
    :t c
    q4s = VU.toList $ quantiles' (VU.fromList [0,1,2,3,4]) 4 c
    :t q4s
    q4s

    c :: VU.Vector Double
    q4s :: [Double]
    [3.2,10.98,12.37,13.69,21.29]


## box plot constructor

A box plot is:

-   (maybe) a vertical tick at the min
-   a LineChart from min to q1
-   a RectChart from q1 to q2
-   a RectChart from q2 to q3
-   a LineChart q3 to max
-   (maybe) a vertical tick at the max

    l1 = LineChart defaultLineStyle [[Point (q4s !! 0) 0.5, Point (q4s !! 1) 0.5]]
    l2 = LineChart defaultLineStyle [[Point (q4s !! 3) 0.5, Point (q4s !! 4) 0.5]]
    r1 = RectChart defaultRectStyle [Rect (q4s !! 1) (q4s !! 2) 0 1]
    r2 = RectChart defaultRectStyle [Rect (q4s !! 2) (q4s !! 3) 0 1]

    c = (mempty :: ChartOptions) & set #hudOptions defaultHudOptions & set #chartTree (unnamed [l1,r1,r2,l2])

    disp c

    True

    writeChartOptions "other/c.svg" c

![img](other/c.svg)


## vertical version

    qs = q4s
    l1 = LineChart defaultLineStyle [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l2 = LineChart defaultLineStyle [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r1 = RectChart defaultRectStyle [Rect 0 1 (qs !! 1) (qs !! 2)]
    r2 = RectChart defaultRectStyle [Rect 0 1 (qs !! 2) (qs !! 3)]

    c = (mempty :: ChartOptions) & set (#markupOptions % #chartAspect) (FixedAspect 0.25) & set #hudOptions defaultHudOptions & over (#hudOptions % #axes) (Prelude.drop 1) & set #chartTree (named "boxplot" [l1,r1,r2,l2])
    disp c

    True


# initial build

    cabal init  --non-interactive mdata -d "base,dataframe,perf,chart-svg,prettychart,vector"

