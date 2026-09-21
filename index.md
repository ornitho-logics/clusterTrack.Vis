# clusterTrack.Vis

[![pkgdown](https://github.com/ornitho-logics/clusterTrack.Vis/actions/workflows/pkgdown.yaml/badge.svg?branch=main)](https://github.com/ornitho-logics/clusterTrack.Vis/actions/workflows/pkgdown.yaml)
[](https://github.com/ornitho-logics/clusterTrack.Vis) [![License: GPL
\>=
3](https://img.shields.io/badge/license-GPL%20%3E%3D%203-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[](https://github.com/ornitho-logics/clusterTrack.Vis/commits/main)

`clusterTrack.Vis` is the companion package for
[`clusterTrack`](https://github.com/ornitho-logics/clusterTrack).

It contains visualization tools, workflows, and exploratory examples
built around `clusterTrack` outputs.

Create interactive maps from clustered tracks with
[`map()`](https://ornitho-logics.github.io/clusterTrack.Vis/reference/map.md).
Each map includes:

- track lines and cluster polygons;
- numbered markers for cluster sites;
- popups with cluster and observation details; and
- a time slider for exploring cluster stop times.

Save maps as HTML with
[`save_map()`](https://ornitho-logics.github.io/clusterTrack.Vis/reference/map.md),
and create a browsable gallery of saved maps with
[`site()`](https://ornitho-logics.github.io/clusterTrack.Vis/reference/site.md).

## Installation

``` r

remotes::install_github('ornitho-logics/clusterTrack.Vis')
```

## Interactive maps

``` r

library(clusterTrack)
library(clusterTrack.Vis)

data(mini_ruff)
ctdf = as_ctdf(mini_ruff) |> cluster_track()
map(ctdf)
```

[`map()`](https://ornitho-logics.github.io/clusterTrack.Vis/reference/map.md)
returns a `leaflet` map, so it can be extended with standard `leaflet`
tools before saving it. Export one or more maps to HTML with:

``` r

out_path = "path/to/maps"
map(ctdf) |> save_map(path = out_path)
```

For a collection of exported maps,
[`site()`](https://ornitho-logics.github.io/clusterTrack.Vis/reference/site.md)
copies a Quarto index template into the output directory. Render the
resulting `index.qmd` to create an HTML gallery with thumbnails and
links to the maps:

``` r

site(out_path)
```

See the [map gallery
article](https://ornitho-logics.github.io/clusterTrack.Vis/articles/site.html)
for a complete example.
