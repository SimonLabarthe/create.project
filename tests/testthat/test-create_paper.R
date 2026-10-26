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

test_that("software papers get their own body and the package name", {
  dir <- withr::local_tempdir()
  create_paper(dir, "software", package = "coolfun",
               repo_url = "https://github.com/me/coolfun")
  tex <- paste(readLines(file.path(dir, "main.tex")), collapse = "\n")

  expect_match(tex, "Statement of Need", fixed = TRUE)
  expect_false(grepl("Theoretical Framework", tex, fixed = TRUE))
  expect_match(tex, "remotes::install_github(\"me/coolfun\")", fixed = TRUE)
  expect_match(tex, "\\usepackage{listings}", fixed = TRUE)   # shared preamble
})

test_that("example figures land in the figures folder", {
  dir <- withr::local_tempdir()
  create_paper(file.path(dir, "5_docs"), figures_dir = "../4_outputs/figures/")
  expect_true(file.exists(file.path(dir, "4_outputs", "figures", "example_duel.pdf")))
})

test_that("quarto papers get main.qmd, the LaTeX header and the reference style", {
  dir <- withr::local_tempdir()
  create_paper(dir, title = "X & Y: 50% of *the* story", format = "quarto")
  qmd <- readLines(file.path(dir, "main.qmd"))

  expect_false(any(grepl("\\{\\{[a-z_]+\\}\\}", qmd)))         # no {{name}} left
  expect_true(any(grepl("{{< pagebreak >}}", qmd, fixed = TRUE))) # Quarto shortcodes survive
  expect_true("title: '**X & Y: 50% of \\*the\\* story**'" %in% qmd)
  expect_true("csl: apa.csl" %in% qmd)
  expect_true(all(file.exists(file.path(dir, c("header.tex", "apa.csl", "references.bib")))))
  expect_false(file.exists(file.path(dir, "main.tex")))
})

test_that("quarto: email and affiliation go in a LaTeX author block", {
  dir <- withr::local_tempdir()
  create_paper(dir, author = "O'Brien", format = "quarto")
  qmd <- readLines(file.path(dir, "main.qmd"))
  expect_true("author: 'O''Brien'" %in% qmd)                    # quote doubled for YAML
  expect_false(any(grepl("AtBeginDocument", qmd)))              # name only: no block

  dir <- withr::local_tempdir()
  create_paper(dir, "software", email = "a@b.c", affiliation = "Uni", format = "quarto")
  qmd <- paste(readLines(file.path(dir, "main.qmd")), collapse = "\n")
  expect_match(qmd, "\\AtBeginDocument{\\author{Simon Labarthe \\\\ \\href{mailto:a@b.c}", fixed = TRUE)
  expect_match(qmd, "Statement of Need", fixed = TRUE)
})

test_that("an existing main.qmd is never overwritten", {
  dir <- withr::local_tempdir()
  writeLines("my draft", file.path(dir, "main.qmd"))
  create_paper(dir, format = "quarto")
  expect_equal(readLines(file.path(dir, "main.qmd")), "my draft")
})

test_that("other reference styles are downloaded from the Zotero repository", {
  dir <- withr::local_tempdir()
  create_paper(dir, bib_style = "chicago-author-date", format = "quarto")
  expect_true("csl: https://www.zotero.org/styles/chicago-author-date" %in% readLines(file.path(dir, "main.qmd")))
  expect_false(file.exists(file.path(dir, "apa.csl")))
})
