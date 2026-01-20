info_box <- function(items) {
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
#' @param prop Numeric (0 < prop ≤ 1). Fraction of each cluster’s points
#'            used to construct the site polygons on the map.
#'            lower values produce tighter, core‐area convex‐hull polygons;
#'            prop = 1 (default) includes all points in the hull.
#' @return A `leaflet` map object if `path` is not provided.
#'        If `path` is provided, an HTML file (and its `maplibs/` directory) is written to disk.
#' @seealso [clusterTrack::cluster_track()], [htmlwidgets::saveWidget()]
#
#'
#' @param ctdf A `ctdf` object.
#' @export
#' @examples
#' require(clusterTrack.Vis)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#' map(ctdf)

map <- function(ctdf, path, prop = 1, fix_dateline = FALSE) {
  #TODO: make it work on no cluster ctdf!
  clusterTrack:::.check_ctdf(ctdf)

  if (nrow(ctdf[!is.na(cluster)]) == 0) {
    stop("this ctdf does not have any clusters!")
  }

  N = glue(
    "<b>N</b>:<br>
            - fixes:{nrow(ctdf)} <br>
            - segments: {max(ctdf$.putative_cluster, na.rm = TRUE)} <br>
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
        .move_seg: {`.move_seg`}  <br>
        time:    {format(timestamp, '%d-%b-%y %H:%M')} <br>
        p.clus: {`.putative_cluster`}  <br>
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
    lab := glue_data(
      .SD,
      "tenure:{Tenure}                    <br/>
    start:{format(start, '%d-%b-%y %Hh')} <br/>
    stop:{format(stop, '%d-%b-%y %Hh')}   <br/>
    ids:{ids}        <br/>
    N:{N}"
    )
  ]

  all_track = as_ctdf_track(ctdf) |> st_transform(crs = "OGC:CRS84") |> setDT()
  all_track[, let(segement = factor(.putative_cluster))]
  all_track = st_as_sf(all_track)
  if (fix_dateline) {
    all_track = .fix_dateline(all_track)
  }

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
    leaflet() |>
    addTiles(group = "OSM Default") |>
    addProviderTiles("OpenTopoMap", group = "Open Topo Map") |>
    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") |>
    addProviderTiles(
      "Esri.WorldGrayCanvas",
      group = "Esri World Gray Canvas"
    ) |>
    addLayersControl(
      baseGroups = c(
        "Esri World Gray Canvas",
        "Open Topo Map",
        "OSM Default",
        "Esri World Imagery"
      ),
      position = "topleft",
      options = layersControlOptions(collapsed = TRUE)
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
    addScaleBar(position = "bottomleft") |>
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
      info_box(nfo)
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
    saveWidget(
      mm,
      path,
      title = basename(path) |> str_remove('.html'),
      selfcontained = FALSE,
      libdir = "maplibs"
    )
  } else {
    return(mm)
  }
}
