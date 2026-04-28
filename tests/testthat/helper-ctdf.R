make_clustered_ctdf = function() {
  e = new.env(parent = emptyenv())
  data("mini_ruff", package = "clusterTrack", envir = e)

  clusterTrack::as_ctdf(e$mini_ruff) |>
    clusterTrack::cluster_track()
}
