.hist_ctdf_ggplot <- function(ctdf, binwidth) {
  vlines = summary(ctdf)[,
    .(timestamp = c(start, stop), which = rep(c("start", "stop"), each = 1)),
    by = cluster
  ]

  ggplot(ctdf[cluster > 0], aes(x = timestamp)) +
    geom_histogram(
      binwidth = binwidth,
      boundary = 0,
      closed = "right",
      fill = '#FFB703'
    ) +
    geom_vline(
      data = vlines,
      aes(xintercept = timestamp, linetype = which),
      inherit.aes = FALSE,
      linewidth = 0.5
    ) +

    facet_wrap(
      ~cluster,
      ncol = 1,
      strip.position = "left",
      scales = "free_y"
    ) +

    labs(x = "Time", y = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      panel.spacing.y = grid::unit(0.05, "lines"),
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 0),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.2),
      panel.grid.minor.x = element_line(linewidth = 0.1),
      plot.margin = margin(2, 2, 2, 2),
      legend.position = "top",
      legend.title = element_blank()
    )
}


#' Histogram of observation times by cluster
#'
#' Plot per-cluster histograms of observation times with cluster start/stop markers,
#' stacked vertically.
#'
#' @param ctdf A `ctdf` object.
#' @param binwidth Bin width in seconds (POSIXct is binned in seconds). Defaults to 1 hour.
#' @method hist ctdf
#' @importFrom graphics hist
#'
#' @return A `ggplot`.
#'
#' @export
#' @examples
#' require(clusterTrack.Vis)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#' hist(ctdf)

hist.ctdf <- function(ctdf, binwidth = 3600) {
  clusterTrack:::.check_ctdf(ctdf)

  x = copy(ctdf)
  if (x[, is.na(cluster) |> all()]) {
    x[, cluster := .putative_cluster]
    warning("No final cluster found, plotting `.putative_cluster`-s")
  }

  if (x[, is.na(cluster) |> all()]) {
    stop('This ctdf does not contain any clusters.')
  }

  .hist_ctdf_ggplot(x, binwidth = binwidth)
}
