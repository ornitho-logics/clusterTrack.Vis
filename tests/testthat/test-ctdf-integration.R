test_that("summarise_ctdf() returns one row per detected site with geometry columns", {
  ctdf = make_clustered_ctdf()
  out = summarise_ctdf(ctdf)

  expect_s3_class(out, "data.table")
  expect_true(nrow(out) > 0)

  expect_true(all(
    c(
      "cluster",
      "start",
      "stop",
      "tenure",
      "site_poly",
      "site_poly_center"
    ) %in%
      names(out)
  ))

  expect_true(all(out$cluster > 0))
  expect_true(inherits(out$site_poly, "sfc"))
  expect_true(inherits(out$site_poly_center, "sfc"))
})

test_that("hist.ctdf() returns a ggplot object", {
  skip_if_not_installed("clusterTrack")
  skip_if_not_installed("ggplot2")

  ctdf = make_clustered_ctdf()
  g = hist(ctdf)

  expect_s3_class(g, "ggplot")
  expect_true(length(g$layers) >= 2)
})

test_that("hist.ctdf() plots putative clusters without changing the ctdf", {
  ctdf = make_clustered_ctdf()
  ctdf$.putative_cluster = ctdf$cluster
  ctdf$cluster[] = NA
  original = data.table::copy(ctdf)

  expect_warning(g <- hist(ctdf, binwidth = 7200), "No final cluster found")

  expect_equal(g$layers[[1]]$stat_params$binwidth, 7200)
  expect_equal(
    nrow(g$layers[[2]]$data),
    2 * length(unique(ctdf$.putative_cluster[ctdf$.putative_cluster > 0]))
  )
  expect_identical(ctdf, original)
})

test_that("map() returns a leaflet widget with map_name", {
  skip_if_not_installed("clusterTrack")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  ctdf = make_clustered_ctdf()
  mm = map(ctdf)

  expect_s3_class(mm, "leaflet")
  expect_identical(attr(mm, "map_name", exact = TRUE), "ctdf")
  expect_true(length(mm$x$calls) > 0)
})

test_that("map() draws the track when there are no clusters", {
  e = new.env(parent = emptyenv())
  data("mini_ruff", package = "clusterTrack", envir = e)
  ctdf = clusterTrack::as_ctdf(e$mini_ruff)

  expect_warning(mm <- map(ctdf), "does not have any clusters")

  methods = vapply(mm$x$calls, `[[`, character(1), "method")
  expect_s3_class(mm, "leaflet")
  expect_identical(attr(mm, "map_name", exact = TRUE), "ctdf")
  expect_true("addPolylines" %in% methods)
  expect_false("addPolygons" %in% methods)
})
