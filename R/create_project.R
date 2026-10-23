# create a reproducible project: social science research or R package

#' create a reproducible R project
#'
#' * `type = "research"`: numbered data pipeline (`1_raw_data/` ... `6_archives/`),
#'   renv, paper in `5_docs/`.
#' * `type = "package"`: standard R package (`R/`, `man/`, `tests/`, `vignettes/`,
#'   `data-raw/`, `inst/`), paper in `paper/`.
#'
#' both get DESCRIPTION, MIT license, README, ignore files and a first git
#' commit on `main`. every step skips what already exists.
#'
#' @param path           project folder, created if needed; its name becomes the project name
#' @param type           "research" or "package"
#' @param title          paper title
#' @param author,email   used in DESCRIPTION, LICENSE, git and the paper
#' @param github_user    GitHub account, used for the repo URL
#' @param publish_github TRUE creates a private GitHub repo and pushes
#'   (needs a token once: `usethis::create_github_token()`)
#' @return the project path, invisibly
#' @export
#' @examples
#' \dontrun{
#' create_project("~/OneDrive/university/26b/seminar/my_paper", title = "Why does X affect Y?")
#' create_project("~/OneDrive/personal/project/coolfun", type = "package")
#' }
create_project <- function(path,
                           type           = c("research", "package"),
                           title          = "[Insert Title]",
                           author         = "Simon Labarthe",
                           email          = "simon.labarthe@outlook.com",
                           github_user    = "SimonLabarthe",
                           publish_github = FALSE) {
  type <- match.arg(type)
  if (type == "research" && !nzchar(Sys.getenv("RENV_PATHS_LIBRARY_ROOT"))) {
    stop("RENV_PATHS_LIBRARY_ROOT is not set: see 'once per computer' in the README")
  }

  name     <- basename(fs::path_abs(path))
  pkg      <- gsub("[^A-Za-z0-9.]", ".", name)   # valid R package name
  # use_github() names the repo after the package, not the folder
  repo_url <- sprintf("https://github.com/%s/%s", github_user, pkg)

  # 1. skeleton: DESCRIPTION, NAMESPACE, R/
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    usethis::create_package(
      path,
      fields = list(
        Package     = pkg,
        `Authors@R` = utils::as.person(sprintf("%s <%s> [aut, cre]", author, email)),
        URL         = repo_url
      ),
      rstudio = FALSE, check_name = FALSE, open = FALSE
    )
  }
  usethis::local_project(path)   # working directory = path until we return

  # .Rproj named after the folder, unless RStudio already made one
  # (usethis would name it after the package: my.project.Rproj)
  if (!length(Sys.glob("*.Rproj"))) {
    file.copy(system.file("templates", "project.Rproj", package = "create.project", mustWork = TRUE),
              paste0(name, ".Rproj"))
  }
  usethis::use_build_ignore(c("^.*\\.Rproj$", "^\\.Rproj\\.user$"), escape = FALSE)
  usethis::use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData", ".Renviron", latex_junk))
  usethis::use_mit_license(author)

  # 2. layout + paper
  if (type == "research") {
    research_layout(author)
    create_paper("5_docs", "research", title, author, email,
                 figures_dir = "../4_outputs/figures/", repo_url = repo_url)
  } else {
    package_layout()
    create_paper("paper", "software", title, author, email,
                 figures_dir = "figures/", repo_url = repo_url, package = pkg)
  }
  write_readme(type, name, repo_url)

  # 3. renv: last before git, so its files are in the first commit.
  # load = FALSE, otherwise R switches to the new, empty library mid-run
  if (type == "research" && !dir.exists("renv")) {
    renv::init(project = ".", bare = TRUE, restart = FALSE, load = FALSE)
  }

  # 4. git
  if (!dir.exists(".git")) {
    gert::git_init(".")
    gert::git_config_set("user.name",  author)
    gert::git_config_set("user.email", email)
    gert::git_add(".")
    gert::git_commit("chore: initial project scaffold")
    if (gert::git_branch() == "master") gert::git_branch_move("master", "main")
  }
  if (publish_github) usethis::use_github(private = TRUE)

  message("done: open ", name, ".Rproj")
  invisible(path)
}

# LaTeX build files, ignored in both layouts
latex_junk <- c("*.aux", "*.log", "*.out", "*.toc", "*.bbl", "*.bcf", "*.blg",
                "*.run.xml", "*.fls", "*.fdb_latexmk", "*.synctex.gz")

research_layout <- function(author) {
  fs::dir_create(c("1_raw_data", "2_scripts", "3_derived_data",
                   "4_outputs/figures", "4_outputs/tables", "5_docs", "6_archives"))

  # numbering = execution order
  scripts <- c(
    "01_extract_raw_data.R"     = "extract raw data into 1_raw_data/",
    "02_clean_and_merge_data.R" = "clean and merge into 3_derived_data/",
    "03_descriptive_analysis.R" = "descriptive tables into 4_outputs/tables/",
    "04_regression_analysis.R"  = "estimation",
    "05_figures_and_tables.R"   = "figures and tables for the paper"
  )
  for (nm in names(scripts)) {
    write_if_missing(file.path("2_scripts", nm),
                     c(paste("#", scripts[[nm]]), paste("# author:", author),
                       paste("# created:", Sys.Date()), ""))
  }

  # data and outputs are regenerated by the scripts: never in git
  usethis::use_git_ignore(c("1_raw_data/", "3_derived_data/", "4_outputs/", "*.pdf"))
  usethis::use_build_ignore(c("1_raw_data", "2_scripts", "3_derived_data",
                              "4_outputs", "5_docs", "6_archives"))
}

package_layout <- function() {
  usethis::use_testthat()                         # tests/testthat/
  usethis::use_vignette("introduction")           # vignettes/
  usethis::use_data_raw("DATASET", open = FALSE)  # data-raw/ builds data/

  # R CMD check fails on an empty tests/testthat/: start with one function + its test
  write_if_missing("R/hello.R", c(
    "#' say hello (example: replace with your own functions)",
    "#'",
    "#' @param name who to greet",
    "#' @return the greeting",
    "#' @export",
    "hello <- function(name = \"world\") paste0(\"hello, \", name, \"!\")"
  ))
  write_if_missing("tests/testthat/test-hello.R", c(
    "test_that(\"hello greets by name\", {",
    "  expect_equal(hello(\"you\"), \"hello, you!\")",
    "})"
  ))

  # citation built from DESCRIPTION: nothing to fill in by hand
  fs::dir_create("inst")
  write_if_missing("inst/CITATION", "citation(auto = meta)")

  usethis::use_build_ignore("paper")
  usethis::use_git_ignore("paper/*.pdf")
}

write_readme <- function(type, name, repo_url) {
  body <- if (type == "research") {
    c("## reproduce", "",
      "1. `renv::restore()`",
      "2. run the scripts in `2_scripts/` in numeric order",
      "3. compile `5_docs/main.tex`")
  } else {
    c("## install", "",
      "```r",
      sprintf("remotes::install_github(\"%s\")", sub("^https://github.com/", "", repo_url)),
      "```")
  }
  write_if_missing("README.md", c(paste("#", name), "", "[One line: what this is.]", "", body))
}

write_if_missing <- function(path, lines) {
  if (!file.exists(path)) writeLines(lines, path)
}
