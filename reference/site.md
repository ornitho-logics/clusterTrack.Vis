# Create a Quarto index template for browsing saved maps

Copies a Quarto `index.qmd` template into `path`. Render the copied file
(for example with Quarto) to generate an `index.html` that can be used
to navigate to the saved map `.html` files in the folder.

## Usage

``` r
site(path)
```

## Arguments

- path:

  Directory where the template `index.qmd` should be copied.

## Examples

``` r
if (FALSE) { # \dontrun{

siteloc = "~/Desktop/temp"
if(dir.exists(siteloc)) unlink(siteloc)
data(mini_ruff)
mini_ruff = as_ctdf(mini_ruff) |> cluster_track()
map(mini_ruff) |> save_map(path=siteloc)

data(pesa56511)
pesa = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
map(pesa) |> save_map(path=siteloc)

data(ruff143789)
ruff = as_ctdf(ruff143789, time = "locationDate") |> cluster_track()
map(ruff) |> save_map(path=siteloc)

data(lbdo66862)
lbdo = as_ctdf(lbdo66862, time = "locationDate") |> cluster_track()
map(lbdo) |> save_map(path=siteloc)

site(siteloc)


} # }
```
