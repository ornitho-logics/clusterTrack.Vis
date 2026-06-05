# Extended summary of a ctdf

Extends the clusterTrack::summary(ctdf)

## Usage

``` r
summarise_ctdf(ctdf)
```

## Arguments

- ctdf:

  A `ctdf` object.

## Examples

``` r
data(pesa56511)
ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#> → Find putative cluster regions.
#>  ⠙ 13 segments processed [2s]
#>  ⠹ 17 segments processed [2.2s]
#>  ⠸ 18 segments processed [2.4s]
#>  ⠼ 23 segments processed [2.9s]
#>  ⠴ 28 segments processed [3s]
#>  ⠦ 30 segments processed [3.1s]
#>  ⠧ 34 segments processed [3.4s]
#>  ⠇ 36 segments processed [3.5s]
#>  ⠏ 41 segments processed [3.7s]
#>  ⠋ 46 segments processed [4s]
#> ! Repairing[1]...
#> → Local clustering.
#> ! Repairing[2]...
#> ! Compute lof scores...

summarise_ctdf(ctdf)
#> Key: <cluster>
#>    cluster               start                stop  lof_q95     ids     N
#>      <int>              <POSc>              <POSc>    <num>  <char> <int>
#> 1:       1 2014-06-02 23:25:19 2014-06-04 05:00:22 2.213307    5-75    62
#> 2:       2 2014-06-04 07:39:07 2014-06-04 15:37:33 1.579420  82-107    23
#> 3:       3 2014-06-04 17:20:36 2014-06-05 04:22:12 2.123607 112-137    22
#> 4:       4 2014-06-05 13:55:36 2014-06-05 18:55:02 3.102805 150-167    16
#> 5:       5 2014-06-05 22:21:00 2014-06-09 20:01:01 1.984879 177-351   142
#> 6:       6 2014-06-10 09:31:53 2014-06-21 02:07:48 1.487050 362-862   434
#>             tenure   dist_to_next elongation                   geometry
#>         <difftime>        <units>      <num>                <sfc_POINT>
#> 1:  1.2326736 days  82997.623 [m]  0.0000000 POINT (-156.6314 71.32439)
#> 2:  0.3322454 days   4482.812 [m]  0.0000000 POINT (-155.0001 71.10669)
#> 3:  0.4594444 days 111252.561 [m]  0.0000000 POINT (-154.9363 71.04556)
#> 4:  0.2079398 days 204855.239 [m]  0.2138568 POINT (-152.6047 70.64368)
#> 5:  3.9027894 days 399667.879 [m]  0.9685747 POINT (-148.9947 70.35698)
#> 6: 10.6916088 days         NA [m]  0.0000000  POINT (-156.6161 71.2825)
#>                         site_poly           site_poly_center
#>                     <sfc_POLYGON>                <sfc_POINT>
#> 1: POLYGON ((-156.739 71.325, ... POINT (-156.6436 71.33192)
#> 2: POLYGON ((-155.022 71.096, ... POINT (-154.9999 71.10656)
#> 3: POLYGON ((-154.964 71.044, ...  POINT (-154.9322 71.0448)
#> 4: POLYGON ((-152.609 70.629, ... POINT (-152.6166 70.64297)
#> 5: POLYGON ((-149.252 70.269, ...  POINT (-148.993 70.35612)
#> 6: POLYGON ((-156.777 71.291, ...  POINT (-156.616 71.28242)
```
