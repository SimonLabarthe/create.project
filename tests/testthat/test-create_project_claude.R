# test-create_project_claude.R: proposed version (by Claude): quarto default,
# author/email in the right place, RENV_PATHS_LIBRARY_ROOT set automatically.

new_project <- function(type, env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "my_project")
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = withr::local_tempdir(.local_envir = env),
                      .local_envir = env)
  suppressMessages(create_project(path, type))
  path
}

test_that("research layout has the numbered pipeline, renv and a paper", {
  path <- new_project("research")
  expect_true(all(dir.exists(file.path(path, c("1_raw_data", "2_scripts", "3_derived_data",
                                                "4_outputs/figures", "5_docs", "6_archives",
                                                "renv")))))
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
  expect_true(any(grepl("Statement of Need", readLines(file.path(path, "paper", "main.qmd")))))
})

test_that("re-running keeps existing files", {
  path <- new_project("research")
  writeLines("my notes", file.path(path, "README.md"))
  suppressMessages(create_project(path, "research"))
  expect_equal(readLines(file.path(path, "README.md")), "my notes")
})

# CLAUDE: replaces "research without RENV_PATHS_LIBRARY_ROOT fails": it no longer
# fails, it writes the setting to ~/.Renviron. the helper is called directly with
# a TEMPORARY .Renviron: on Windows "~" ignores a fake HOME, so going through
# create_project() here would write into your real ~/.Renviron
test_that("missing RENV_PATHS_LIBRARY_ROOT is set up automatically", {
  renviron <- file.path(withr::local_tempdir(), ".Renviron")
  writeLines("SOMETHING_ELSE=1", renviron)                   # existing content must survive
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = "")
  suppressMessages(set_renv_library_root(renviron))
  expect_true(nzchar(Sys.getenv("RENV_PATHS_LIBRARY_ROOT")))
  lines <- readLines(renviron)
  expect_equal(lines[1], "SOMETHING_ELSE=1")
  expect_match(lines[2], "^RENV_PATHS_LIBRARY_ROOT=.*renv/library$")
})

test_that("an existing RENV_PATHS_LIBRARY_ROOT is left alone", {
  renviron <- file.path(withr::local_tempdir(), ".Renviron")
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = "D:/mine")
  set_renv_library_root(renviron)
  expect_false(file.exists(renviron))                       # nothing written
  expect_equal(Sys.getenv("RENV_PATHS_LIBRARY_ROOT"), "D:/mine")
})

# CLAUDE: new. catches the old positional bug (name in the subtitle, email as author)
test_that("author lands in the author field of the paper", {
  path <- new_project("research")
  qmd  <- readLines(file.path(path, "5_docs", "main.qmd"))
  expect_true(any(grepl("^author: 'Simon Labarthe", qmd)))
  expect_false(any(grepl("^author: 'simon.labarthe@", qmd)))
})
