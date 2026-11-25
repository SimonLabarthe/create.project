new_project <- function(type, env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "my_project")
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = withr::local_tempdir(.local_envir = env),
                      .local_envir = env)
  # HERE: ask = FALSE, otherwise running the tests from RStudio (interactive) would stop and wait for answers
  suppressMessages(create_project(path, type, ask = FALSE))
  path
}

test_that("research layout has the numbered pipeline, renv and a paper", {
  path <- new_project("research")
  expect_true(all(dir.exists(file.path(path, c("1_raw_data", "2_scripts", "3_derived_data",
                                               "4_outputs/figures", "5_docs", "6_archives",
                                               "renv")))))
  # HERE: main.qmd instead of main.tex: quarto is the default now
  expect_true(file.exists(file.path(path, "5_docs", "main.qmd")))
  expect_true(file.exists(file.path(path, "my_project.Rproj")))
  expect_equal(gert::git_branch(repo = path), "main")
})

test_that("package layout follows R package conventions", {
  path <- new_project("package")
  expect_true(all(dir.exists(file.path(path, c("R", "tests/testthat", "vignettes",
                                               "data-raw", "inst", "paper")))))
  expect_false(dir.exists(file.path(path, "1_raw_data")))
  expect_false(dir.exists(file.path(path, "renv")))
  expect_true("^paper$" %in% readLines(file.path(path, ".Rbuildignore")))
  # HERE: main.qmd instead of main.tex
  expect_true(any(grepl("Statement of Need", readLines(file.path(path, "paper", "main.qmd")))))
})

test_that("re-running keeps existing files", {
  path <- new_project("research")
  writeLines("my notes", file.path(path, "README.md"))
  # HERE: ask = FALSE
  suppressMessages(create_project(path, "research", ask = FALSE))
  expect_equal(readLines(file.path(path, "README.md")), "my notes")
})

# HERE: replaces "research without RENV_PATHS_LIBRARY_ROOT fails": it no longer fails, it writes the setting. the helper gets a TEMPORARY .Renviron, because on Windows "~" ignores a fake HOME and the test would write into your real one
test_that("missing RENV_PATHS_LIBRARY_ROOT is set up automatically", {
  renviron <- file.path(withr::local_tempdir(), ".Renviron")
  writeLines("SOMETHING_ELSE=1", renviron)
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = "")
  suppressMessages(set_renv_library_root(renviron))
  expect_true(nzchar(Sys.getenv("RENV_PATHS_LIBRARY_ROOT")))
  lines <- readLines(renviron)
  expect_equal(lines[1], "SOMETHING_ELSE=1")
  expect_match(lines[2], "^RENV_PATHS_LIBRARY_ROOT=.*renv/library$")
})

# HERE: new test, an existing setting is never touched
test_that("an existing RENV_PATHS_LIBRARY_ROOT is left alone", {
  renviron <- file.path(withr::local_tempdir(), ".Renviron")
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = "D:/mine")
  set_renv_library_root(renviron)
  expect_false(file.exists(renviron))
  expect_equal(Sys.getenv("RENV_PATHS_LIBRARY_ROOT"), "D:/mine")
})

# HERE: new test, catches the old bug where your name went into the subtitle and your email into the author
test_that("author lands in the author field of the paper", {
  path <- new_project("research")
  qmd  <- readLines(file.path(path, "5_docs", "main.qmd"))
  expect_true(any(grepl("^author: 'Simon Labarthe", qmd)))
  expect_false(any(grepl("^author: 'simon.labarthe@", qmd)))
})

# HERE: new test of the questions. local_mocked_bindings() replaces the ask_* helpers with fake answers for this test only, so nobody has to type
test_that("ask = TRUE asks the folder, the type, quarto y/n and GitHub y/n", {
  folder <- file.path(withr::local_tempdir(), "asked_project")
  asked  <- character()
  local_mocked_bindings(
    ask_text   = function(question) folder,
    ask_choice = function(question, choices) { asked <<- c(asked, "type"); "package" },
    ask_yes_no = function(question, default = TRUE) { asked <<- c(asked, question); FALSE }
  )
  suppressMessages(create_project(ask = TRUE))
  expect_equal(asked[1], "type")
  expect_match(asked[2], "Quarto")
  expect_match(asked[3], "GitHub")
  expect_true(file.exists(file.path(folder, "paper", "main.tex")))
  expect_false(dir.exists(file.path(folder, "renv")))
})

# HERE: new test, what you type in the call is never asked again
test_that("arguments given in the call are not asked", {
  path <- file.path(withr::local_tempdir(), "p")
  local_mocked_bindings(
    ask_text   = function(...) stop("should not ask the folder"),
    ask_choice = function(...) stop("should not ask the type"),
    ask_yes_no = function(...) stop("should not ask y/n")
  )
  expect_no_error(suppressMessages(
    create_project(path, type = "package", format = "quarto", publish_github = FALSE, ask = TRUE)
  ))
})
