test_that("placeholders are filled and special characters escaped", {
  dir <- withr::local_tempdir()
  create_paper(dir, title = "X & Y: 50% of the story", email = "a@b.c")
  tex <- readLines(file.path(dir, "main.tex"))

  expect_false(any(grepl("\\{\\{[a-z_]+\\}\\}", tex)))       # no {{name}} left
  expect_true(any(grepl("X \\& Y: 50\\% of the story", tex, fixed = TRUE)))
  expect_true(any(grepl("\\textbf{X", tex, fixed = TRUE)))   # backslashes survive
  expect_true(file.exists(file.path(dir, "references.bib")))
})

test_that("an existing main.tex is never overwritten", {
  dir <- withr::local_tempdir()
  writeLines("my draft", file.path(dir, "main.tex"))
  create_paper(dir)
  expect_equal(readLines(file.path(dir, "main.tex")), "my draft")
})
