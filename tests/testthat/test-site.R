test_that("site() creates index.qmd and updates existing map navigation", {
  path = tempfile("ctvis-")
  dir.create(path)

  writeLines(
    '<html><body><div class="ct-navbuttons" data-prev-map="" data-next-map=""></div></body></html>',
    file.path(path, "a.html"),
    useBytes = TRUE
  )

  expect_message(
    index <- site(path),
    "Copied Quarto template"
  )

  expect_equal(basename(index), "index.qmd")
  expect_true(file.exists(index))

  html = paste(
    readLines(file.path(path, "a.html"), warn = FALSE),
    collapse = "\n"
  )

  expect_true(grepl("ct-navbuttons", html, fixed = TRUE))
})
