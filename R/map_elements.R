.map_empty <- function() {
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

  HTML(
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


.navbuttons_dependency <- function() {
  htmlDependency(
    name = "clusterTrack-navbuttons",
    version = as.character(utils::packageVersion("clusterTrack.Vis")),
    src = "map",
    stylesheet = "navbuttons.css",
    script = "navbuttons.js"
  )
}


.nav_is_available <- function(x) {
  !is.null(x) && !is.na(x) && nzchar(x)
}


.navbutton <- function(href, label) {
  if (!.nav_is_available(href)) {
    return(
      tags$span(
        class = "ct-navbutton ct-navbutton-disabled",
        label
      )
    )
  }

  tags$a(
    class = "ct-navbutton",
    href = href,
    label
  )
}


.navbuttons <- function(
  next_map = NULL,
  prev_map = NULL,
  home_map = "index.html"
) {
  nav_value <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(x)) {
      return("")
    }

    x
  }

  nav = tags$div(
    class = "ct-navbuttons",
    `data-prev-map` = nav_value(prev_map),
    `data-next-map` = nav_value(next_map),

    tags$span(
      class = "ct-navbutton ct-navbutton-prev",
      `data-nav-dir` = "prev",
      "Previous"
    ),

    tags$a(
      class = "ct-navbutton ct-navbutton-home",
      href = home_map,
      title = "Index",
      `aria-label` = "Index",
      "Home"
    ),

    tags$span(
      class = "ct-navbutton ct-navbutton-next",
      `data-nav-dir` = "next",
      "Next"
    )
  )

  attachDependencies(
    nav,
    .navbuttons_dependency()
  )
}

.html_has_navbuttons <- function(file) {
  html = readLines(file, warn = FALSE, encoding = "UTF-8")
  any(grepl("ct-navbuttons", html, fixed = TRUE))
}


.update_single_map_navigation <- function(file, prev_map = "", next_map = "") {
  html = readLines(file, warn = FALSE, encoding = "UTF-8")
  html = paste(html, collapse = "\n")

  if (!grepl("ct-navbuttons", html, fixed = TRUE)) {
    return(invisible(FALSE))
  }

  prev_map = htmlEscape(prev_map)
  next_map = htmlEscape(next_map)

  html = sub(
    'data-prev-map="[^"]*"',
    paste0('data-prev-map="', prev_map, '"'),
    html
  )

  html = sub(
    'data-next-map="[^"]*"',
    paste0('data-next-map="', next_map, '"'),
    html
  )

  writeLines(html, file, useBytes = TRUE)

  invisible(TRUE)
}


.update_map_navigation = function(path) {
  path = path.expand(path)

  files = list.files(
    path = path,
    pattern = "\\.html$",
    full.names = TRUE
  )

  files = files[basename(files) != "index.html"]

  if (!length(files)) {
    return(invisible(character()))
  }

  has_nav = vapply(files, .html_has_navbuttons, logical(1))
  files = files[has_nav]

  if (!length(files)) {
    return(invisible(character()))
  }

  X = data.table(
    file = normalizePath(files, mustWork = TRUE),
    name = basename(files)
  )

  setorder(X, name)

  for (i in seq_len(nrow(X))) {
    prev_map = if (i > 1) X$name[i - 1] else ""
    next_map = if (i < nrow(X)) X$name[i + 1] else ""

    .update_single_map_navigation(
      file = X$file[i],
      prev_map = prev_map,
      next_map = next_map
    )
  }

  invisible(X$file)
}
