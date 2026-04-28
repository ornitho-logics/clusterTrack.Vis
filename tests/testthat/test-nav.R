test_that(".nav_is_available() handles missing and present paths", {
  f = clusterTrack.Vis:::.nav_is_available

  expect_true(f("a.html"))
  expect_false(f(NULL))
  expect_false(f(NA_character_))
  expect_false(f(""))
})

test_that(".navbutton() creates disabled and linked buttons", {
  disabled = as.character(clusterTrack.Vis:::.navbutton("", "Previous"))
  linked = as.character(clusterTrack.Vis:::.navbutton("a.html", "Previous"))

  expect_true(grepl("ct-navbutton-disabled", disabled, fixed = TRUE))
  expect_true(grepl("Previous", disabled, fixed = TRUE))

  expect_true(grepl('href="a.html"', linked, fixed = TRUE))
  expect_true(grepl("ct-navbutton", linked, fixed = TRUE))
  expect_false(grepl("ct-navbutton-disabled", linked, fixed = TRUE))
})

test_that(".navbuttons() stores navigation targets and dependency", {
  nav = clusterTrack.Vis:::.navbuttons(
    prev_map = "a.html",
    next_map = "b.html",
    home_map = "index.html"
  )

  html = as.character(nav)

  expect_true(grepl('data-prev-map="a.html"', html, fixed = TRUE))
  expect_true(grepl('data-next-map="b.html"', html, fixed = TRUE))
  expect_true(grepl('href="index.html"', html, fixed = TRUE))

  deps = htmltools::htmlDependencies(nav)
  dep_names = vapply(deps, function(x) x$name, character(1))

  expect_true("clusterTrack-navbuttons" %in% dep_names)
})
