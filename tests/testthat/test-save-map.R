test_that("save_map() requires a map_name attribute", {
  expect_error(
    save_map(list(), tempdir()),
    "`map_name` attribute",
    fixed = TRUE
  )
})

test_that("save_map() writes an html file and injects navbuttons", {
  path = tempfile("ctvis-")
  dir.create(path)

  x = leaflet::leaflet()
  attr(x, "map_name") = "a"

  out = save_map(
    x = x,
    path = path,
    selfcontained = FALSE
  )

  expect_equal(basename(out), "a.html")
  expect_true(file.exists(out))

  html = paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_true(grepl("ct-navbuttons", html, fixed = TRUE))
})
