.fix_dateline = function(x) {
  x |>
    st_wrap_dateline(
      options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
      quiet = TRUE
    ) |>
    st_shift_longitude()
}


.mcp <- function(x, p = 0.95) {
  d = st_distance(x, st_union(x) |> st_centroid())
  st_union(x[d <= quantile(d, p), ]) |>
    st_convex_hull()
}


.sitepoly <- function(x) {
  #  .mcp(x, p)
  st_union(x) |> st_concave_hull(ratio = 0.9)
}

#' Extended summary of a ctdf
#'
#' Extends the clusterTrack::summary(ctdf)
#'
#' @param ctdf A `ctdf` object.
#' @export
#' @examples
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#'
#' summarise_ctdf(ctdf)

summarise_ctdf <- function(ctdf) {
  x = summary(ctdf) |>
    st_as_sf() |>
    st_transform(crs = 4326) |>
    data.table()

  polys = ctdf[
    cluster > 0,
    .(
      site_poly = .sitepoly(location) |>
        st_transform(crs = 4326)
    ),
    by = cluster
  ]

  polys[, site_poly_center := st_centroid(polys$site_poly)]

  o = merge(x, polys, by = "cluster")
  o
}
