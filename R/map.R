#' Map and save a ctdf object
#'
#' Visualize clustered track data on an interactive leaflet map and optionally
#' save the resulting widget with [save_map()].
#'
#' `map()` builds a `leaflet` map from a `ctdf` object. The returned map stores
#' the input object name in its `map_name` attribute, which is used by
#' `save_map()` as the output HTML filename.
#'
#' @param ctdf A `ctdf` object produced by [cluster_track()].
#' @param fix_dateline Logical. If `TRUE`, geometries are adjusted with [sf::st_wrap_dateline()].
#'
#' @return
#' `map()` returns a `leaflet` map object with a `map_name` attribute.
#'
#' `save_map()` invisibly returns the written HTML file path.
#'
#' @seealso [cluster_track()], [htmlwidgets::saveWidget()]
#'
#' @export
#'
#' @examples
#' data(mini_ruff)
#'
#' ctdf = as_ctdf(mini_ruff)
#' map(ctdf)
#'
#' ctdf = cluster_track(ctdf)
#'
#' mm = map(ctdf)
#'
#' \dontrun{
#' mm |>
#'   leaflet::addMarkers(lng = 11, lat = 47) |>
#'   save_map(path = "~/Desktop/temp")
#' }

map <- function(ctdf, fix_dateline = FALSE) {
  clusterTrack:::.check_ctdf(ctdf)

  all_track = as_ctdf_track(ctdf) |> st_transform(crs = "OGC:CRS84") |> setDT()
  all_track[, let(segement = factor(.putative_cluster))]
  all_track = st_as_sf(all_track)
  if (fix_dateline) {
    all_track = .fix_dateline(all_track)
  }

  if (nrow(ctdf[!is.na(cluster)]) == 0) {
    warning("this ctdf does not have any clusters!")

    mm = .map_empty() |>
      addPolylines(
        data = all_track,
        color = "#d32013cc",
        weight = 2
      )
    attr(mm, "map_name") = deparse1(substitute(ctdf))
    return(mm)
  }

  N = glue(
    "<b>N</b>:<br>
            - fixes:{nrow(ctdf)} <br>
            - clusters: {max(ctdf$cluster)}"
  )

  cluster_par = attr(ctdf, "cluster_params")
  cluster_par = glue(" - {names(cluster_par)} = {cluster_par}") |>
    paste(collapse = "<br>")
  cluster_par = glue("<b>Parameters</b>:<br>{cluster_par}")

  pack = glue("<i>clusterTrack v.{ packageVersion('clusterTrack')}</i>")

  nfo = c(N, cluster_par, pack)

  CD = st_as_sf(ctdf) |> st_transform("OGC:CRS84")

  if (fix_dateline) {
    CD = .fix_dateline(CD)
  }

  CD$pt_lab = Map(
    function(.id, timestamp, lof) {
      HTML(glue(
        "
      .id:  {.id}  <br>
      time: {format(timestamp, '%d-%b-%y %H:%M')} <br>
      lof:  {round(lof, 2)}
      "
      ))
    },
    CD$.id,
    CD$timestamp,
    CD$lof
  )

  move_points = CD[CD$cluster == 0, ]
  site_points = CD[CD$cluster != 0, ]

  sites = summarise_ctdf(ctdf)
  sites[tenure < 1, Tenure := glue_data(.SD, "{round(tenure*24)}[h]")]
  sites[tenure > 1, Tenure := glue_data(.SD, "{round(tenure,1)}[d]")]
  sites[,
    lab := glue_data(
      .SD,
      '
    <table class="popup-table">
      <tr><th>tenure</th><td>{Tenure}</td></tr>
      <tr><th>start</th><td>{format(start, "%d-%b-%y %Hh")}</td></tr>
      <tr><th>stop</th><td>{format(stop, "%d-%b-%y %Hh")}</td></tr>
      <tr><th>elongation</th><td>&nbsp;{round(elongation, 2)}</td></tr>
      <tr><th>lof_q95</th><td>{round(lof_q95, 2)}</td></tr>
      <tr><th>ids</th><td>{ids}</td></tr>
      <tr><th>N</th><td>{N}</td></tr>
    </table>
    '
    )
  ]

  polys = sites[, .(cluster, site_poly)] |> st_as_sf()
  if (fix_dateline) {
    polys = .fix_dateline(polys)
  }

  sites_sf = sites[, .(cluster, lab, site_poly_center, stop)] |>
    st_as_sf()

  if (fix_dateline) {
    sites_sf = .fix_dateline(sites_sf)
  }

  # map elements
  pal = colorFactor(
    viridisLite::viridis(unique(sites$cluster) |> length()),
    sites$cluster
  )

  clust_ico = awesomeIcons(
    icon = NULL,
    text = as.character(sites$cluster),
    iconColor = "black",
    library = "fa",
    markerColor = "white"
  )

  # build map
  mm =
    .map_empty() |>
    addPolygons(
      data = polys,
      fillColor = ~ pal(cluster),
      fillOpacity = 0.5,
      weight = 0,
      group = "Clusters"
    ) |>
    addPolylines(
      data = all_track,
      color = "#7e7f81cc",
      weight = 1.5,
      opacity = 0.5,
      group = "All Track"
    ) |>
    addCircleMarkers(
      data = move_points,
      label = ~pt_lab,
      radius = 4,
      color = "#7e7f81cc",
      stroke = FALSE,
      opacity = 0.3,
      fillOpacity = 1,
      group = "Site Track"
    ) |>
    addCircleMarkers(
      data = site_points,
      label = ~pt_lab,
      radius = 5,
      color = ~ pal(cluster),
      stroke = FALSE,
      fillOpacity = 1,
      group = "Site Track"
    ) |>
    addAwesomeMarkers(
      data = sites_sf,
      icon = clust_ico,
      popup = ~lab,
      group = "Cluster Icons",
      popupOptions = popupOptions(
        autoPan = FALSE,
        autoClose = FALSE,
        keepInView = TRUE,
        closeOnClick = FALSE
      )
    ) |>
    addBootstrapDependency() |>
    addEasyButton(easyButton(
      icon = '<i class="fa fa-dot-circle-o" style="color:red; font-weight:bold;"></i>',
      title = "clusterTrack",
      onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) |>
    addTimeslider(
      data = sites_sf,
      options = timesliderOptions(
        timeAttribute = "stop",
        range = FALSE,
        rezoom = FALSE
      )
    ) |>
    appendContent(
      .info_box(nfo)
    )

  attr(mm, "map_name") = deparse1(substitute(ctdf))

  mm
}


#' @describeIn map Save a leaflet map produced by `map()` as an HTML widget.
#'
#' @param x A `leaflet` map object produced by `map()`.
#' @param path Character. Directory where the HTML map will be saved.
#' @param selfcontained Logical. Passed to [htmlwidgets::saveWidget()].
#'    Set it to `TRUE` only when you want to save a single map.
#' @param libdir Character. Directory for widget assets when
#'   `selfcontained = FALSE`.
#'
#' @export
save_map <- function(x, path, selfcontained = FALSE, libdir = "assets") {
  name = attr(x, "map_name", exact = TRUE)

  if (is.null(name) || is.na(name) || !nzchar(name)) {
    stop(
      "`x` does not have a `map_name` attribute. ",
      "Create it with `map()` before calling `save_map()`.",
      call. = FALSE
    )
  }

  if (!selfcontained) {
    x = x |>
      appendContent(
        .navbuttons()
      )
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  file = file.path(path, paste0(name, ".html"))

  saveWidget(
    widget = x,
    file = file,
    title = name,
    selfcontained = selfcontained,
    libdir = libdir
  )

  invisible(file)
}
