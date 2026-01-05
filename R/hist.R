.hist_ctdf_ggplot <- function(ctdf, binwidth) {
  vlines = summary(ctdf)[,
    .(timestamp = c(start, stop), which = rep(c("start", "stop"), each = 1)),
    by = cluster
  ]

  ggplot(ctdf[cluster > 0], aes(x = timestamp)) +
    geom_histogram(binwidth = binwidth, boundary = 0, fill = '#FFB703') +
    geom_vline(
      data = vlines,
      aes(xintercept = timestamp, linetype = which),
      inherit.aes = FALSE,
      linewidth = 0.3
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


library(data.table)
library(plotly)

.hist_ctdf_plotly = function(ctdf, binwidth) {
  clsum = summary(ctdf)
  vlines = clsum[,
    .(timestamp = c(start, stop), which = c("start", "stop")),
    by = cluster
  ]

  DT = ctdf[cluster > 0]
  tz = attr(DT$timestamp, "tzone")
  if (is.null(tz) || identical(tz, "")) {
    tz = "UTC"
  }

  DT[,
    bin := as.POSIXct(
      floor(as.numeric(timestamp) / binwidth) * binwidth,
      origin = "1970-01-01",
      tz = tz
    )
  ]

  binned = DT[, .N, by = .(cluster, bin)][order(cluster, bin)]
  binned[, bin_end := bin + binwidth]

  clusters = sort(unique(binned$cluster))

  figs = lapply(clusters, \(cl) {
    d = binned[cluster == cl]
    ymax = max(d$N, na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) {
      ymax = 1
    }

    d[,
      hover := sprintf(
        "cluster=%s<br>%s to %s<br>N=%d",
        cluster,
        format(bin, "%Y-%m-%d %H:%M:%S"),
        format(bin_end, "%Y-%m-%d %H:%M:%S"),
        N
      )
    ]

    p = plot_ly(
      data = d,
      x = ~bin,
      y = ~N,
      type = "bar",
      name = "counts",
      showlegend = FALSE,
      hovertext = ~hover,
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = "#FFB703")
    ) |>
      layout(
        yaxis = list(
          title = as.character(cl),
          showticklabels = FALSE,
          ticks = ""
        ),
        xaxis = list(title = ""),
        bargap = 0
      )

    vv = vlines[cluster == cl]

    add_vlines = function(p, times, dash) {
      if (!length(times)) {
        return(p)
      }
      xs = as.POSIXct(unlist(lapply(times, \(tt) c(tt, tt, NA))), tz = tz)
      ys = unlist(lapply(times, \(tt) c(0, ymax, NA)))

      p |>
        add_trace(
          type = "scatter",
          mode = "lines",
          x = xs,
          y = ys,
          inherit = FALSE,
          hoverinfo = "skip",
          showlegend = FALSE,
          line = list(width = 1, dash = dash, color = "black")
        )
    }

    p = add_vlines(p, vv[which == "start", timestamp], "solid")
    p = add_vlines(p, vv[which == "stop", timestamp], "dash")
    p
  })

  n = length(figs)
  panel_h = 90
  top_h = 30
  bottom_h = 40
  left_h = 70

  fig = subplot(figs, nrows = n, shareX = TRUE, titleY = TRUE) |>
    layout(
      xaxis = list(title = "Time"),
      margin = list(t = top_h, l = 70, r = 10, b = bottom_h),
      showlegend = FALSE
    )

  fig
}


#' Histogram of observation times by cluster
#'
#' Plot per-cluster histograms of observation times with cluster start/stop markers,
#' stacked vertically. Returns either a static `ggplot` or an interactive `plotly` object.
#'
#' @param ctdf A `ctdf` object.
#' @param binwidth Bin width in seconds (POSIXct is binned in seconds). Defaults to 3 hours.
#' @param static Logical. If `TRUE`, return a static `ggplot`; otherwise return an interactive `plotly` object.
#' @method hist ctdf
#' @importFrom graphics hist
#'
#'
#' @return A `ggplot` (if `static = TRUE`) or a `plotly` htmlwidget (if `static = FALSE`).
#'
#' @export
#' @examples
#' require(clusterTrack.Vis)
#' data(pesa56511)
#' ctdf = as_ctdf(pesa56511, time = "locationDate") |> cluster_track()
#' hist(ctdf)

hist.ctdf <- function(ctdf, binwidth = 3 * 3600, static = FALSE, ...) {
  clusterTrack:::.check_ctdf(ctdf)

  if (static) {
    .hist_ctdf_ggplot(ctdf, binwidth = binwidth)
  } else {
    .hist_ctdf_plotly(ctdf, binwidth = binwidth)
  }
}
