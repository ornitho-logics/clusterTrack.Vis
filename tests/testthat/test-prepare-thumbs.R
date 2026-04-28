test_that("prepare_thumbs() builds a gallery from existing thumbnails", {
  path = tempfile("ctvis-")
  dir.create(path)

  writeLines("<html>a</html>", file.path(path, "a.html"), useBytes = TRUE)
  writeLines("<html>b</html>", file.path(path, "b.html"), useBytes = TRUE)
  writeLines(
    "<html>index</html>",
    file.path(path, "index.html"),
    useBytes = TRUE
  )

  thumb_dir = file.path(path, "assets", "thumbs")
  dir.create(thumb_dir, recursive = TRUE)

  file.create(file.path(thumb_dir, "a.webp"))
  file.create(file.path(thumb_dir, "b.webp"))

  html = prepare_thumbs(
    info_text = c("info a", "info b"),
    path = path
  )

  txt = as.character(html)

  expect_true(grepl("a.html", txt, fixed = TRUE))
  expect_true(grepl("b.html", txt, fixed = TRUE))
  expect_false(grepl("index.html", txt, fixed = TRUE))

  expect_true(grepl("a.webp", txt, fixed = TRUE))
  expect_true(grepl("b.webp", txt, fixed = TRUE))

  expect_true(grepl("info a", txt, fixed = TRUE))
  expect_true(grepl("info b", txt, fixed = TRUE))
})
