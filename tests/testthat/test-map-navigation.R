nav_html = function() {
  '<html><body><div class="ct-navbuttons" data-prev-map="" data-next-map=""></div></body></html>'
}

test_that(".update_map_navigation() links map files in sorted order", {
  path = tempfile("ctvis-")
  dir.create(path)

  writeLines(nav_html(), file.path(path, "b.html"), useBytes = TRUE)
  writeLines(nav_html(), file.path(path, "a.html"), useBytes = TRUE)
  writeLines(nav_html(), file.path(path, "c.html"), useBytes = TRUE)
  writeLines(nav_html(), file.path(path, "index.html"), useBytes = TRUE)

  out = clusterTrack.Vis:::.update_map_navigation(path)

  expect_equal(basename(out), c("a.html", "b.html", "c.html"))

  a = paste(readLines(file.path(path, "a.html"), warn = FALSE), collapse = "\n")
  b = paste(readLines(file.path(path, "b.html"), warn = FALSE), collapse = "\n")
  c = paste(readLines(file.path(path, "c.html"), warn = FALSE), collapse = "\n")
  index = paste(
    readLines(file.path(path, "index.html"), warn = FALSE),
    collapse = "\n"
  )

  expect_true(grepl('data-prev-map=""', a, fixed = TRUE))
  expect_true(grepl('data-next-map="b.html"', a, fixed = TRUE))

  expect_true(grepl('data-prev-map="a.html"', b, fixed = TRUE))
  expect_true(grepl('data-next-map="c.html"', b, fixed = TRUE))

  expect_true(grepl('data-prev-map="b.html"', c, fixed = TRUE))
  expect_true(grepl('data-next-map=""', c, fixed = TRUE))

  expect_true(grepl('data-prev-map=""', index, fixed = TRUE))
  expect_true(grepl('data-next-map=""', index, fixed = TRUE))
})

test_that(".update_map_navigation() ignores html files without navbuttons", {
  path = tempfile("ctvis-")
  dir.create(path)

  writeLines("<html></html>", file.path(path, "a.html"), useBytes = TRUE)

  out = clusterTrack.Vis:::.update_map_navigation(path)

  expect_identical(out, character())
})
