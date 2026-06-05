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
#>  ⠙ 17 segments processed [2.1s]
#>  ⠹ 18 segments processed [2.2s]
#>  ⠸ 23 segments processed [2.4s]
#>  ⠼ 29 segments processed [2.6s]
#>  ⠴ 33 segments processed [2.8s]
#>  ⠦ 36 segments processed [3s]
#>  ⠧ 40 segments processed [3.1s]
#>  ⠇ 45 segments processed [3.4s]
#>  ⠇ 46 segments processed [3.5s]
#> ! Repairing[1]...
#> → Local clustering.
#> ! Repairing[2]...
#> ! Compute lof scores...
hist(ctdf)
```
