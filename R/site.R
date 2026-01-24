.make_index <- function(path) {
  hh = list.files(
    path,
    pattern = "\\.html$",
    full.names = FALSE
  ) |>
    setdiff("index.html")

  X = data.table(
    file = hh,
    title = str_remove(basename(hh), "\\.html$")
  )

  page =
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$title("clusterTrack output")
      ),
      htmltools::tags$body(
        htmltools::tags$h1("clusterTrack output"),

        htmltools::tags$ul(
          lapply(seq_len(nrow(X)), function(i) {
            htmltools::tags$li(htmltools::tags$a(
              href = X$file[i],
              X$title[i]
            ))
          })
        )
      )
    )

  htmltools::save_html(page, file = paste0(path, "/index.html"))
}


#' Build a simple static site index for a folder of leaflet/htmlwidgets maps
#'
#' Creates an `index.html` with links and inline previews for all `.html` maps
#' under `path`
#'
#' @param path Directory containing saved map `.html` files.
#' @return Invisibly, a list with paths (`index`, `assets_dir`, `libs_dir`,
#'   `metadata_dir`) and a `data.table` summary of discovered maps.
#' @export

#' \dontrun{
#' siteloc = "~/Desktop/temp"
#' data(mini_ruff)
#' mini_ruff = as_ctdf(mini_ruff) |> cluster_track()
#' map(mini_ruff, path = siteloc)
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
  .make_index(path)
  servr::daemon_list() |> servr::daemon_stop()
  servr::httw(path, browser = TRUE, pattern = ".html")
}
