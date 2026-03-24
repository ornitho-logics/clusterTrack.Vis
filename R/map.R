.map_empty = function() {
  leaflet(
    options = leafletOptions(
      zoomSnap = 0.25
    )
  ) |>

    addProviderTiles(
      "CartoDB.Positron",
      group = "Light",
      options = providerTileOptions(
        maxNativeZoom = 18,
        maxZoom = 21
      )
    ) |>
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Satellite"
    ) |>
    addProviderTiles(
      "OpenTopoMap",
      group = "Topo"
    ) |>
    addLayersControl(
      baseGroups = c(
        "Light",
        "Satellite",
        "Topo"
      ),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) |>
    addFullscreenControl(
      position = "topleft",
      pseudoFullscreen = FALSE
    ) |>
    addMeasure(
      position = "topleft",
      primaryLengthUnit = "kilometers",
      secondaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters"
    ) |>
    addScaleBar(position = "bottomleft")
}

.info_box <- function(items) {
  html_list =
    sapply(items, function(item) glue::glue("<li>{item}</li>")) |>
    glue_collapse()
  html_list = glue::glue("<ul>{html_list}</ul>")

  htmltools::HTML(
    glue::glue(
      '<div class="modal" id="infobox" tabindex="-1" role="dialog">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-body">
              {html_list}
            </div>
          </div>
        </div>
      </div>'
    )
  )
}

#' Map a ctdf object
#'
#' Visualize clustered track data on an interactive map.
#'
#' @param ctdf A `ctdf` object produced by `cluster_track()`.
#' @param path Optional `character`. File path where the HTML map will be saved.
#'             If omitted, the function returns the `leaflet` map object.
#' @param name When path is given, the name of the html file.
#' @return A `leaflet` map object if `path` is not provided.
#'        If `path` is provided, an HTML file (and its `assets/` directory) is written to disk.
#' @seealso [clusterTrack::cluster_track()], [htmlwidgets::saveWidget()]
#
#'
#' @param ctdf A `ctdf` object.
#' @export
#' @examples
#' data(mini_ruff)
#' ctdf = as_ctdf(mini_ruff)
#' map(ctdf) # empty map
#' cluster_track(ctdf)
#' map(ctdf)
#' map(ctdf, path = "~/Desktop/temp")

map <- function(ctdf, path, name, fix_dateline = FALSE) {
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

  CD = mutate(
    CD,
    pt_lab = glue(
      "
        .id: {`.id`}  <br>
        time:    {format(timestamp, '%d-%b-%y %H:%M')} <br>
        lof: {round(lof,2)} 
      "
    )
  ) |>
    rowwise() |>
    mutate(pt_lab = htmltools::HTML(pt_lab))

  move_points = dplyr::filter(CD, cluster == 0)
  site_points = dplyr::filter(CD, cluster != 0)

  sites = summarise_ctdf(ctdf)
  sites[tenure < 1, Tenure := glue_data(.SD, "{round(tenure*24)}[h]")]
  sites[tenure > 1, Tenure := glue_data(.SD, "{round(tenure,1)}[d]")]
  sites[,
    lab := glue::glue_data(
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
    viridis::viridis(unique(sites$cluster) |> length()),
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

  if ("true_cluster" %in% names(CD)) {
    mm =
      mm |>
      addCircleMarkers(
        data = dplyr::filter(CD, true_cluster > 0),
        radius = 6,
        color = ~"#e75d01",
        fillOpacity = 0,
        weight = 1.5,
        label = ~ paste("true clust:", true_cluster)
      )
  }

  if (!missing(path)) {
    if (missing(name)) {
      name = as.character(substitute(ctdf))
    }
    this_path = paste0(path, "/", name, ".html")

    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    saveWidget(
      mm,
      file = this_path,
      title = name,
      selfcontained = FALSE,
      libdir = "assets"
    )

    message(name)
  } else {
    return(mm)
  }
}
