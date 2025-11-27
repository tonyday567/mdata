
# kaggle

A reproducable build for dataHaskell kaggle.


# build

Built using:

    cabal init  --non-interactive kaggle -d "base,dataframe,perf,chart-svg,prettychart"

Run in ghci using:

    import Prelude
    :set -XImportQualifiedPost
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -XTupleSections
    :set -XQuasiQuotes
    import Control.Category ((>>>))
    import Data.Function
    import Data.Maybe
    import Data.Bool
    import Data.List qualified as List
    import Control.Monad
    import Data.Bifunctor
    import Chart
    import Prettychart
    import Chart.Examples
    import Optics.Core hiding ((|>))
    import Perf
    import Data.ByteString.Char8 qualified as C
    import Data.Text qualified as T
    
    import Prettyprinter
    (display, quit) <- startChartServer (Just "kaggle")
    disp x = display $ x & set (#markupOptions % #markupHeight) (Just 250) & set (#hudOptions % #frames % ix 1 % #item % #buffer) 0.1
    
    import DataFrame as D
    import DataFrame.Internal.Statistics
    import qualified Data.Vector.Algorithms.Intro as VA
    import qualified Data.Vector.Unboxed as VU
    import qualified Data.Vector.Unboxed.Mutable as VUM
    
    import Kaggle

    Configuration is affected by the following files:
    - cabal.project
    Build profile: -w ghc-9.12.2 -O1
    In order, the following will be built (use -v for more details):
     - kaggle-0.1.0.0 (interactive) (lib) (file src/Kaggle.hs changed)
    Preprocessing library for kaggle-0.1.0.0...
    GHCi, version 9.12.2: https://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling Kaggle           ( src/Kaggle.hs, interpreted )
    Ok, one module loaded.
    Setting pghhacsie>r s to stun... (port 9160) (ctrl-c to quit)


### Example chart display

    :t lineExample
    disp lineExample

    lineExample :: ChartOptions
    True

This gives you a browser page and live charting capabilities.


### file read testing

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


# s5e11

<https://www.kaggle.com/competitions/playground-series-s5e11>


## dataframe

    df <- D.readCsv "data/s5e11/test.csv"

    describeColumns df

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


## boxPlot example

    ir = (either (error . show) id) (columnAsDoubleVector "interest_rate" df)
    ch = boxPlot defaultBoxPlotOptions ir
    writeChartOptions "other/box1.svg" ch
    disp ch

    True

![img](other/box1.svg)


## scatterPlot example

    c0 = (either (error . show) id) (columnAsDoubleVector "interest_rate" df)
    c1 = (either (error . show) id) (columnAsDoubleVector "loan_amount" df)
    
    ch = GlyphChart defaultGlyphStyle (Prelude.take 1000 $ zipWith Point (VU.toList c0) (VU.toList c1))
    
    ch' = (mempty :: ChartOptions) & set #chartTree (named "scatterPlot" [ch]) & set #hudOptions defaultHudOptions
    
    writeChartOptions "other/scatter1.svg" ch'
    disp ch'

    True

![img](other/scatter1.svg)


# reference

Comparable python:

<https://www.kaggle.com/code/ravitejagonnabathula/predicting-loan-payback>

notebook best practice:
<https://marimo.io/blog/lessons-learned>

converting to ipynb:
<https://pandoc.org/installing.html>

chart-svg api tree

![img](https://hackage-content.haskell.org/package/chart-svg-0.8.2.1/docs/other/ast.svg)


# boxPlot testing


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

