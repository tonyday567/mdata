# dataframe-load

I use this repo to load new csv files, locally, and some popular favorites for testing.

### loading datasets

```
cabal exec dataframe-load -- --port 9160
```

 ... starts a server at localhost:9160, and provides a dropdown box to select a dataset. On loading the dataframe (into df) it provides a chart (multiPlot df) on the page so you can see what you have (if anything!).

You can drop your own datasets in ~/.cache/dataframe-load/ and it will let you load them next serve.

### filewatch

```
cabal exec dataframe-load -- --demo --port 9160
```

As well as being a --demo, this also starts a filewatch routine, watching /tmp/watch/ and any svg file you throw in there will be rendered. Very handy for adhoc testing or working our where all your derivations have gotten to.
 
### old-school

The actual loading is pretty simple in cabal repl steps:

```
import DataFrame qualified as D
import Chart qualified as C
import DataFrame.Chart qualified as DC

-- find csv data out there (or add to url list here)

-- read from file
df <- D.readCsv "/Users/tonyday567/.cache/titanic.csv"

-- create the multi plot
chart = DC.multiPlot df

-- create an SVG text snippet and write to file.
C.writeChartOptions "other/titanic.svg" chart

```
 
### gallery

**s5e11** — original test, from kaggle.

![s5e11](./other/s05e11.svg)

**penguins**

![penguins](./other/penguins.svg)

**titanic**

![titanic](./other/titanic.svg)

**seoulbikes** 

![seoulbikes](./other/seoulbikes.svg)

From the [Seoul Bike Sharing Demand](https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand) UCI dataset.

## Common Options

- `--port PORT` — Server port (default: 9160)
- `--dataset NAME` — Dataset name for RunLoad (default: s5e11)
- `--watchdir PATH` — Directory to watch for SVG files (default: /tmp/watch/)

## Build

```bash
cabal build
cabal exec dataframe-load
```


