# ============================================================================
# create_paper_claude.R  --  proposed version of create_paper.R (by Claude)
#
# WHAT CHANGED compared to create_paper.R (search "CLAUDE:" to jump to each one)
#   1. format now defaults to "quarto" (was "latex"). why: the two things you
#      use in every recent paper, apa.csl (reference style) and the word count
#      (andrewheiss/wordcount Quarto extension), only exist in the Quarto
#      template. so making Quarto the default = making them the default.
#      LaTeX is still one argument away: create_paper(format = "latex").
#   2. bug fix: the `subtitle` argument existed but was never sent to the
#      template, so research.qmd kept a literal '{{subtitle}}' in the PDF.
#      it is now passed to quarto_values() and filled in.
#
# HOW TO ADOPT: compare with create_paper.R, then delete create_paper.R and
# rename this file to create_paper.R. (while both exist, R loads files in
# alphabetical order, so this one silently wins: don't leave both.)
# then run devtools::document() so the help page (man/) shows the new default.
# ============================================================================

# create a paper skeleton (main.tex or main.qmd + references.bib) from inst/templates/paper/

#' create a paper skeleton, in LaTeX or Quarto
#'
#' writes the paper and `references.bib` into `dir`. the `{{name}}`
#' placeholders are filled from the arguments; everything in `[...]` is
#' writing guidance for you to replace. existing files are kept.
#'
#' * `format = "latex"`: `main.tex`, the shared preamble
#'   (`inst/templates/paper/preamble.tex`) followed by the body for `type`.
#'   compile it with [compile_paper()], not RStudio's "Compile PDF" button
#'   (the button runs BibTeX, so the references stay empty).
#' * `format = "quarto"`: `main.qmd` (`inst/templates/paper/quarto/`), text in
#'   Markdown, tables and numbers computed by R when the paper is rendered.
#'   comes with `header.tex` (same look as the LaTeX paper), `apa.csl`
#'   (reference style) and the `wordcount` extension in `_extensions/`
#'   (prints the word count under the title). render it with the Render
#'   button in RStudio.
#'
#' * `type = "research"`: empirical social science (theory, hypotheses,
#'   research design, results, robustness).
#' * `type = "software"`: a paper introducing an R package (statement of
#'   need, comparison with existing tools, design, usage, validation).
#'
#' @param dir         folder where the paper is written (created if needed)
#' @param type        "research" or "software"
#' @param title       paper title
#' @param author      author name
#' @param email       author email, or NULL
#' @param affiliation institution, or NULL
#' @param student_id  student id, or NULL
#' @param date        date printed under the title. NULL: the current month
#'   (latex), or the day the paper is rendered (quarto)
#' @param bib_style   reference style. latex: a biblatex style, e.g. "apa",
#'   "authoryear", "chicago-authordate". quarto: "apa" (shipped), or the name
#'   of any style on <https://www.zotero.org/styles>, e.g. "chicago-author-date"
#'   (downloaded when the paper is rendered)
#' @param figures_dir where the paper looks for figures, relative to `dir` (keep the trailing /)
#' @param repo_url    repository URL, printed in the appendix / availability section
#' @param package     software only: package name used in the text and code examples
#' @param format      "quarto" (default: apa.csl + word count) or "latex"
#' @return the path of main.tex or main.qmd, invisibly
#' @export
#' @examples
#' \dontrun{
#' create_paper("5_docs", title = "Why does X affect Y?")
#' create_paper("5_docs", format = "latex")   # LaTeX instead of the Quarto default
#' create_paper("paper", type = "software", package = "package")
#' }
create_paper <- function(dir         = "5_docs",
                         type        = c("research", "software"),
                         title       = "Your Title",
                         subtitle    = "Your Subtitle",
                         author      = "Your Name",
                         email       = NULL,
                         affiliation = NULL,
                         student_id  = NULL,
                         date        = NULL,
                         bib_style   = "apa",
                         figures_dir = "../4_outputs/figures/",
                         repo_url    = "[link]",
                         package     = "[package]",
                         format      = c("quarto", "latex")) {
  type     <- match.arg(type)
  format   <- match.arg(format)
  quarto   <- format == "quarto"
  paper    <- file.path(dir, if (quarto) "main.qmd" else "main.tex")
  bib_path <- file.path(dir, "references.bib")
  fs::dir_create(dir)

  if (!file.exists(paper)) {
    fig_dir <- file.path(dir, figures_dir)
    fs::dir_create(fig_dir)
    examples <- list.files(system.file("templates", "paper", "figures", package = "create.project"),
                           full.names = TRUE)
    file.copy(examples, fig_dir, overwrite = FALSE)

    if (quarto) {
      # affiliation, email and student id go in a footnote on the author's name
      thanks <- c(
        if (!is.null(affiliation)) escape_latex(affiliation),
        if (!is.null(email))       sprintf("\\href{mailto:%s}{%s}", email, escape_latex(email)),
        if (!is.null(student_id))  paste0("Student no.~", escape_latex(student_id))
      )
      parts  <- system.file("templates", "paper", "quarto", paste0(type, ".qmd"),
                            package = "create.project", mustWork = TRUE)
      # subtitle is now passed on (it was dropped before)
      values <- quarto_values(title, subtitle, author, thanks, if (is.null(date)) "today" else date,
                              bib_style, figures_dir, repo_url, package)
      # the LaTeX look (header.tex), the APA reference style (apa.csl) and
      # the word count extension (format: wordcount-pdf)
      support <- c("header.tex", if (bib_style == "apa") "apa.csl")
      file.copy(system.file("templates", "paper", "quarto", support, package = "create.project"),
                dir, overwrite = FALSE)
      if (!dir.exists(file.path(dir, "_extensions"))) {
        fs::dir_copy(system.file("templates", "paper", "quarto", "_extensions",
                                 package = "create.project", mustWork = TRUE),
                     file.path(dir, "_extensions"))
      }
    } else {
      author_block <- paste(c(
        escape_latex(author),
        if (!is.null(email))       sprintf("\\href{mailto:%s}{\\texttt{%s}}", email, email),
        if (!is.null(affiliation)) escape_latex(affiliation),
        if (!is.null(student_id))  paste("Student ID:", escape_latex(student_id))
      ), collapse = " \\\\ ")
      parts  <- system.file("templates", "paper", c("preamble.tex", paste0(type, ".tex")),
                            package = "create.project", mustWork = TRUE)
      values <- latex_values(title, author, author_block,
                             if (is.null(date)) format(Sys.Date(), "%B %Y") else date,
                             bib_style, figures_dir, repo_url, package)
    }
    tex <- paste(unlist(lapply(parts, readLines, encoding = "UTF-8")), collapse = "\n")

    # regmatches<- inserts the value literally: gsub() would read the
    # backslashes of LaTeX commands as escapes (\textbf -> tab + "extbf")
    for (nm in names(values)) {
      hits <- gregexpr(paste0("{{", nm, "}}"), tex, fixed = TRUE)
      regmatches(tex, hits) <- list(rep(values[[nm]], sum(hits[[1]] > 0)))
    }
    writeLines(enc2utf8(tex), paper, useBytes = TRUE)
  }

  if (!file.exists(bib_path)) {
    writeLines(c(
      "% export from Zotero (Better BibTeX) or paste entries here",
      "% delete this example once you have real references",
      "@article{author2020,",
      "  author  = {Author, Anna},",
      "  title   = {An Example Article},",
      "  journal = {Journal of Examples},",
      "  year    = {2020}",
      "}"
    ), bib_path)
  }

  invisible(paper)
}

#' compile a paper to PDF, with its references
#'
#' `main.tex`: runs pdflatex and Biber as many times as needed (with
#' `tinytex::latexmk()`). RStudio's "Compile PDF" button runs BibTeX instead
#' of Biber, so with the button the bibliography stays empty.
#' `main.qmd`: renders it with Quarto (same as the Render button).
#'
#' @param dir folder that contains main.tex or main.qmd
#' @return the path of the PDF, invisibly
#' @export
#' @examples
#' \dontrun{
#' compile_paper("5_docs")
#' }
compile_paper <- function(dir = "5_docs") {
  qmd <- file.path(dir, "main.qmd")
  tex <- file.path(dir, "main.tex")
  if (file.exists(qmd)) {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      stop("open main.qmd in RStudio and click Render, or install.packages(\"quarto\")")
    }
    quarto::quarto_render(qmd)
  } else if (file.exists(tex)) {
    if (!requireNamespace("tinytex", quietly = TRUE)) stop("install.packages(\"tinytex\")")
    # latexmk() compiles in the working directory: move into dir, so that
    # references.bib and figures/ are found and the output lands next to main.tex
    owd <- setwd(dir)
    on.exit(setwd(owd))
    tinytex::latexmk("main.tex", bib_engine = "biber")
  } else {
    stop("no main.tex or main.qmd in ", dir)
  }
  invisible(file.path(dir, "main.pdf"))
}

# values of the {{name}} placeholders in the LaTeX templates
latex_values <- function(title, author, author_block, date, bib_style,
                         figures_dir, repo_url, package) {
  list(
    title        = escape_latex(title),
    title_plain  = escape_latex(title),
    author       = escape_latex(author),
    author_block = author_block,
    date         = date,
    date_iso     = format(Sys.Date()),
    thanks       = "",
    bib_style    = bib_style,
    bib_file     = "references.bib",
    figures_dir  = figures_dir,
    repo_url     = if (grepl("^http", repo_url)) sprintf("\\url{%s}", repo_url) else repo_url,
    repo         = github_repo(repo_url),
    package      = package
  )
}

# values of the {{name}} placeholders in the Quarto templates. title and
# author sit in single-quoted YAML strings that Quarto reads as Markdown.
# thanks: the LaTeX lines of the footnote on the author's name (none = no footnote)
quarto_values <- function(title, subtitle, author, thanks, date, bib_style,
                          figures_dir, repo_url, package) {
  list(
    title         = yaml_quote(escape_markdown(title)),
    subtitle      = yaml_quote(escape_markdown(subtitle)),
    # raw LaTeX inline (`...`{=latex}), so that Markdown leaves the \thanks alone
    author        = yaml_quote(paste0(
      escape_markdown(author),
      if (length(thanks)) paste0("`\\thanks{", paste(thanks, collapse = ". \\\\\\indent "), ".}`{=latex}")
    )),
    # "today" is left to Quarto, which prints the day the paper is rendered
    date          = if (identical(date, "today")) "today" else paste0("'", yaml_quote(date), "'"),
    date_iso      = format(Sys.Date()),
    csl           = if (bib_style == "apa") "apa.csl" else paste0("https://www.zotero.org/styles/", bib_style),
    figures_dir   = figures_dir,
    repo_url      = if (grepl("^http", repo_url)) sprintf("<%s>", repo_url) else repo_url,
    repo          = github_repo(repo_url),
    package       = package
  )
}

# "https://github.com/me/pkg" -> "me/pkg", for remotes::install_github()
github_repo <- function(repo_url) {
  if (grepl("^https://github.com/", repo_url)) sub("^https://github.com/", "", repo_url) else "[user/repo]"
}

# escape the characters LaTeX treats as commands
escape_latex <- function(x) {
  map <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%", "$" = "\\$",
           "#" = "\\#", "_" = "\\_", "{" = "\\{", "}" = "\\}",
           "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}")
  chars <- strsplit(x, "")[[1]]
  hit   <- chars %in% names(map)
  chars[hit] <- map[chars[hit]]
  paste(chars, collapse = "")
}

# escape the characters Markdown treats as formatting (*bold*, _italic_, [link], $math$...)
escape_markdown <- function(x) gsub("([][\\\\`*_<>#$@~^|])", "\\\\\\1", x)

# a single-quoted YAML string only needs its quotes doubled
yaml_quote <- function(x) gsub("'", "''", x, fixed = TRUE)
