# Histogram of observation times by cluster

Plot per-cluster histograms of observation times with cluster start/stop
markers, stacked vertically.

## Usage

``` r
# S3 method for class 'ctdf'
hist(x, binwidth = 3600, ...)
```

## Arguments

- x:

  A `ctdf` object.

- binwidth:

  Bin width in seconds (POSIXct is binned in seconds). Defaults to 1
  hour.

- ...:

  Ignored.

## Value

A `ggplot`.

## Examples

``` r
require(clusterTrack.Vis)
data(pesa56511)
ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#> → Find putative cluster regions.
#>  ⠙ 13 segments processed [2.1s]
#>  ⠹ 15 segments processed [2.2s]
#>  ⠸ 18 segments processed [2.4s]
#>  ⠼ 23 segments processed [2.6s]
#>  ⠴ 29 segments processed [2.8s]
#>  ⠦ 32 segments processed [2.9s]
#>  ⠧ 35 segments processed [3.2s]
#>  ⠇ 37 segments processed [3.3s]
#>  ⠏ 43 segments processed [3.5s]
#>  ⠏ 46 segments processed [3.7s]
#> ! Repairing[1]...
#> → Local clustering.
#> ! Repairing[2]...
#> ! Compute lof scores...
hist(ctdf)
```
