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
