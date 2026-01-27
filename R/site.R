.layout_thumbs <- function(X, text_by_id) {
  tile_items <- glue::glue(
    '
  <a class="text-decoration-none" href="{html_path}">
    <div class="card shadow-sm h-100">
      <img
        class="card-img-top"
        src="{thumb_path}"
        alt="{title}"
        loading="lazy"
        style="height: 200px; width: 100%; object-fit: cover;"
      >
      <div class="card-body p-1">
        <div class="card-title small fw-semibold mb-0">{title}</div>
        <div class="card-text small text-muted">{info}</div>
      </div>
    </div>
  </a>
  ',
    html_path = X$html_path,
    thumb_path = X$thumb_path,
    title = X$name,
    info = text_by_id
  )

  tiles <- glue::glue(
    '
  <div class="gallery">
  {paste0(tile_items, collapse = "\n")}
  </div>

  <style>
    .gallery {{
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
      gap: 0.75rem;
      align-items: stretch;
    }}
  </style>
  '
  )

  htmltools::HTML(tiles)
}


#' @export
prepare_thumbs <- function(
  info_text,
  path,
  overwrite = FALSE,
  where = "assets/thumbs"
) {
  if (missing(path)) {
    path = getwd()
  }

  path = path.expand(path)

  hh = list.files(
    path,
    pattern = "\\.html$",
    full.names = TRUE
  )
  hh = hh[basename(hh) != "index.html"]

  thumb_dir = paste(path, where, sep = .Platform$file.sep)
  if (overwrite) {
    unlink(thumb_dir, recursive = TRUE, force = TRUE)
  }
  if (!dir.exists(thumb_dir)) {
    dir.create(thumb_dir)
  }

  X = data.table(
    name = str_remove(basename(hh), "\\.html$"),
    html_path = hh
  )
  X[, thumb_path := str_replace(basename(hh), "\\.html$", ".webp")]
  X[, thumb_path := paste(thumb_dir, thumb_path, sep = .Platform$file.sep)]

  X[, todo := !file.exists(thumb_path)]

  if (nrow(X[(todo)]) > 0) {
    X[
      (todo),
      {
        webshot2::webshot(
          url = html_path,
          file = thumb_path,
          vwidth = 500,
          vheight = 500,
          delay = 2
        )
      },
      by = name
    ]
  }

  X[, thumb_path := str_replace(thumb_path, path, ".")]
  X[, html_path := basename(html_path)]
  X[, todo := NULL]
  X

  # make layout
  if (missing(info_text)) {
    info_text = ""
  }

  .layout_thumbs(X, info_text)
}


#' Create a Quarto index template for browsing saved maps
#'
#' Copies a Quarto `index.qmd` template into `path`. Render the copied file
#' (for example with Quarto) to generate an `index.html` that can be used to
#' navigate to the saved map `.html` files in the folder.
#'
#' @param path Directory where the template `index.qmd` should be copied.
#' @export
#' @examples
#' \dontrun{
#' siteloc = "~/Desktop/temp"
#' data(mini_ruff)
#' mini_ruff = as_ctdf(mini_ruff) |> cluster_track()
#' map(mini_ruff, path = siteloc, name = "(ruff_143789_subset")
#'
#' data(pesa56511)
#' pesa = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#' map(pesa, path = siteloc)
#'
#' data(ruff143789)
#' ruff = as_ctdf(ruff143789, time = "locationDate") |> cluster_track()
#' map(ruff, path = siteloc)
#'
#' data(lbdo66862)
#' lbdo = as_ctdf(lbdo66862, time = "locationDate") |> cluster_track()
#' map(lbdo, path = siteloc)
#'
#' site(siteloc)
#'
#'
#' }

site <- function(path) {
  file.copy(
    system.file("site_template/index.qmd", package = "clusterTrack.Vis"),
    path
  )
  message(
    "Copied Quarto template to: ",
    file.path(path, "index.qmd"),
    "\n",
    "Next:\n",
    "  1) Open and edit index.qmd (optional).\n",
    "  2) Render it to create index.html, e.g.:\n",
    "     quarto render ",
    shQuote(file.path(path, "index.qmd")),
    "\n",
    "  3) Open ",
    shQuote(file.path(path, "index.html")),
    " in a browser."
  )
}
