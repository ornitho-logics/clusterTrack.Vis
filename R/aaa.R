#' @import clusterTrack
#' @import data.table stringr
#' @import sf leaflet ggplot2
#' @import yyjsonr

#' @importFrom stats quantile

#' @importFrom leaflet.extras2 addTimeslider timesliderOptions
#' @importFrom leaflet.extras addBootstrapDependency addFullscreenControl

#' @importFrom htmlwidgets appendContent saveWidget
#' @importFrom htmltools HTML tagList tags htmlDependency attachDependencies htmlEscape

#' @importFrom glue glue glue_data glue_collapse
#' @importFrom stringr str_remove
#
#'
#'
NULL


utils::globalVariables(c(
  ".",
  ".putative_cluster",
  "cluster",
  "html_path",
  "lab",
  "location",
  "name",
  "site_poly",
  "site_poly_center",
  "start",
  "stop",
  "tenure",
  "Tenure",
  "thumb_path",
  "timestamp",
  "todo"
))
