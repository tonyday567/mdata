# dataframe-load

Educational chart explorer for DataFrame visualization. Load datasets and stream them as interactive SVG charts via HTTP.

## Modes

### RunLoad (default)
Download, load, and serve a full dataset chart. Select dataset from browser dropdown.

```bash
cabal exec dataframe-load -- --port 9160
```

- Browser dropdown to select from remote (downloadable) or local datasets
- Automatically downloads if not cached
- Generates full multi-plot chart of the complete dataset
- Streams chart once to the browser
- No animation loop—just the final full chart

### RunPush
Streams animated chart frames over 5 seconds. Select dataset from browser dropdown.

```bash
cabal exec dataframe-load -- --push --port 9160
```

### RunDemo
FileWatch server with welcome chart. Drop SVG files into `/tmp/watch/` and watch them appear.

```bash
cabal exec dataframe-load -- --demo --port 9160
```

### RunWatch
Pure file watcher. Serves SVG files from `/tmp/watch/` as they change.

```bash
cabal exec dataframe-load -- --watch --port 9160
```

### RunCounter
Simple counter animation test.

```bash
cabal exec dataframe-load -- --counter --port 9160
```

## Common Options

- `--port PORT` — Server port (default: 9160)
- `--dataset NAME` — Dataset name for RunLoad (default: s5e11)
- `--watchdir PATH` — Directory to watch for SVG files (default: /tmp/watch/)

## Build

```bash
cabal build
cabal exec dataframe-load
```
