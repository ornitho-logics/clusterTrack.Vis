# Prepare thumbnail cards for saved maps

Create thumbnails for saved `.html` map files and return an HTML
gallery.

## Usage

``` r
prepare_thumbs(info_text, path, overwrite = FALSE, where = "assets/thumbs")
```

## Arguments

- info_text:

  Character vector of text shown below each thumbnail.

- path:

  Directory containing saved `.html` map files.

- overwrite:

  Logical. Regenerate existing thumbnails?

- where:

  Relative thumbnail directory inside `path`.

## Value

An `htmltools` HTML object.
