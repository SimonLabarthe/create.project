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
  expect_true(file.exists(file.path(path, "5_docs", "main.tex")))
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
  expect_true(any(grepl("Statement of Need", readLines(file.path(path, "paper", "main.tex")))))
})

test_that("re-running keeps existing files", {
  path <- new_project("research")
  writeLines("my notes", file.path(path, "README.md"))
  suppressMessages(create_project(path, "research"))
  expect_equal(readLines(file.path(path, "README.md")), "my notes")
})

test_that("research without RENV_PATHS_LIBRARY_ROOT fails before writing anything", {
  withr::local_envvar(RENV_PATHS_LIBRARY_ROOT = "")
  path <- file.path(withr::local_tempdir(), "x")
  expect_error(create_project(path), "RENV_PATHS_LIBRARY_ROOT")
  expect_false(dir.exists(path))
})
