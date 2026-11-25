# create a reproducible project: social science research or R package

# ============================================================================
# create_project_claude.R  --  proposed version of create_project.R (by Claude)
#
# WHAT CHANGED (search "CLAUDE:" to jump to each one)
#   1. format defaults to "quarto" (was "latex"), so every new project gets
#      apa.csl + the automatic word count without asking. see create_paper_claude.R
#   2. bug fix: create_paper() was called with unnamed arguments
#      (title, author, email in that order). your local create_paper() now has
#      a `subtitle` argument between title and author, so by position your
#      NAME went into the subtitle and your EMAIL into the author. every
#      argument is now passed by name (name = value), which can't shift.
#   3. the "once per computer" step is automatic: if RENV_PATHS_LIBRARY_ROOT
#      is missing, create_project() writes it to your ~/.Renviron itself
#      (before: it stopped with an error and you had to do it by hand).
#      that's what makes the "one line in the console" README possible.
#
# HOW TO ADOPT: compare with create_project.R, delete create_project.R and
# rename this file to create_project.R, then devtools::document().
# ============================================================================

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
#' @param format         the paper: "quarto" (main.qmd, default: APA references
#'   via apa.csl + automatic word count) or "latex" (main.tex), see [create_paper()]
#' @return the project path, invisibly
#' @export
#' @examples
#' \dontrun{
#' create_project("~path", title = "Why does X affect Y?")
#' create_project("~path", type = "package")
#' create_project("~path", format = "latex")
#' }
create_project <- function(path,
                           type           = c("research", "package"),
                           title          = "[Insert Title]",
                           author         = "[Your Name]",
                           email          = "gmail@chucknorris.com",
                           github_user    = "You",
                           publish_github = FALSE,
                           format         = c("quarto", "latex"),
                           ask            = interactive()) {

  # missing(x) is TRUE when you did not give x in the call: only those are asked, what you typed is never asked again
  if (ask && missing(path)) {
    path <- ask_text("project folder (e.g. C:/Users/you/:")
  }
  # HERE: utils::menu() prints a numbered list and returns the number you type
  if (ask && missing(type)) {
    type <- ask_choice("type of project?", c("research", "package"))
  }
  if (ask && missing(format)) {
    format <- if (ask_yes_no("Quarto paper (APA references + automatic word count)? no = LaTeX")) "quarto" else "latex"
  }
  if (ask && missing(publish_github)) {
    publish_github <- ask_yes_no("create a private GitHub repo and push?", default = FALSE)
  }

  type   <- match.arg(type)     # checks the value is one of the allowed ones
  format <- match.arg(format)
  if (type == "research") set_renv_library_root()

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
    # CLAUDE: 2. every argument named (was: create_paper("5_docs", "research", title, author, email, ...))
    create_paper(dir = "5_docs", type = "research", title = title, author = author, email = email,
                 figures_dir = "../4_outputs/figures/", repo_url = repo_url, format = format)
  } else {
    package_layout()
    create_paper(dir = "paper", type = "software", title = title, author = author, email = email,
                 figures_dir = "figures/", repo_url = repo_url, package = pkg, format = format)
  }
  write_readme(type, name, repo_url, format)

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

# LaTeX and Quarto build files, ignored in both layouts
latex_junk <- c("*.aux", "*.log", "*.out", "*.toc", "*.bbl", "*.bcf", "*.blg",
                "*.run.xml", "*.fls", "*.fdb_latexmk", "*.synctex.gz",
                ".quarto/", "*_files/")

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

write_readme <- function(type, name, repo_url, format) {
  body <- if (type == "research") {
    c("## reproduce", "",
      "1. `renv::restore()`",
      "2. run the scripts in `2_scripts/` in numeric order",
      if (format == "quarto") "3. render `5_docs/main.qmd`" else "3. compile `5_docs/main.tex` (with biber)")
  } else {
    c("## install", "",
      "```r",
      sprintf("remotes::install_github(\"%s\")", sub("^https://github.com/", "", repo_url)),
      "```")
  }
  write_if_missing("README.md", c(paste("#", name), "", "[One line: what this is.]", "", body))
}

# asks a yes/no question in the console. readline() waits for you to type and press Enter; Enter alone = the default (the capital letter in [Y/n])
ask_yes_no <- function(question, default = TRUE) {
  hint   <- if (default) "[Y/n]" else "[y/N]"
  answer <- tolower(trimws(readline(paste(question, hint, ""))))
  if (answer == "") return(default)
  answer %in% c("y", "yes", "o", "oui")
}

# asks to pick one option from a numbered list. menu() returns 0 if you type 0 (= cancel)
ask_choice <- function(question, choices) {
  pick <- utils::menu(choices, title = question)
  if (pick == 0) stop("cancelled", call. = FALSE)
  choices[pick]
}

# asks for free text (the folder). gsub() removes quotes you may have pasted around the path, and chartr() turns Windows \ into /
ask_text <- function(question) {
  answer <- trimws(gsub("^[\"']|[\"']$", "", trimws(readline(paste(question, "")))))
  if (answer == "") stop("no folder given", call. = FALSE)
  chartr("\\", "/", answer)
}

# the old "once per computer" README step, done by code.
# renv keeps each project's packages in a library folder. by default that folder
# is inside the project, i.e. in OneDrive: thousands of small files to sync.
# the environment variable RENV_PATHS_LIBRARY_ROOT moves it to a local folder.
# `renviron` is an argument only so the tests can point it at a temporary file
set_renv_library_root <- function(renviron = path.expand("~/.Renviron")) {
  # nzchar() = "is the string non-empty?": already set -> nothing to do
  if (nzchar(Sys.getenv("RENV_PATHS_LIBRARY_ROOT"))) return(invisible())

  # local (never synced) folder: AppData/Local on Windows, ~/.local/share elsewhere.
  # .Platform$OS.type is "windows" or "unix"; chartr() turns \ into / for R
  root <- if (.Platform$OS.type == "windows") {
    chartr("\\", "/", file.path(Sys.getenv("LOCALAPPDATA"), "renv", "library"))
  } else {
    path.expand("~/.local/share/renv/library")
  }

  # ~/.Renviron is read by R at every start-up: the setting becomes permanent.
  # (same file that usethis::edit_r_environ() opens)
  cat(sprintf("RENV_PATHS_LIBRARY_ROOT=%s\n", root),
      file = renviron, append = TRUE)   # append = TRUE: never erase what is already there

  # .Renviron is only read at start-up, so set it for this session too
  Sys.setenv(RENV_PATHS_LIBRARY_ROOT = root)
  message("once per computer: RENV_PATHS_LIBRARY_ROOT=", root, " added to ", renviron)
  invisible(root)
}

write_if_missing <- function(path, lines) {
  if (!file.exists(path)) writeLines(lines, path)
}
