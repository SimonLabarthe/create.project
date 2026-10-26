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
#'   comes with `header.tex` (same look as the LaTeX paper) and `apa.csl`
#'   (reference style). render it with the Render button in RStudio.
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
#' @param date        date printed under the title
#' @param bib_style   reference style. latex: a biblatex style, e.g. "apa",
#'   "authoryear", "chicago-authordate". quarto: "apa" (shipped), or the name
#'   of any style on <https://www.zotero.org/styles>, e.g. "chicago-author-date"
#'   (downloaded when the paper is rendered)
#' @param figures_dir where the paper looks for figures, relative to `dir` (keep the trailing /)
#' @param repo_url    repository URL, printed in the appendix / availability section
#' @param package     software only: package name used in the text and code examples
#' @param format      "latex" or "quarto"
#' @return the path of main.tex or main.qmd, invisibly
#' @export
#' @examples
#' \dontrun{
#' create_paper("5_docs", title = "Why does X affect Y?")
#' create_paper("5_docs", format = "quarto")
#' create_paper("paper", type = "software", package = "package")
#' }
create_paper <- function(dir         = "5_docs",
                         type        = c("research", "software"),
                         title       = "[Insert Title]",
                         author      = "Simon Labarthe",
                         email       = NULL,
                         affiliation = NULL,
                         student_id  = NULL,
                         date        = format(Sys.Date(), "%B %Y"),
                         bib_style   = "apa",
                         figures_dir = "../4_outputs/figures/",
                         repo_url    = "[link]",
                         package     = "[package]",
                         format      = c("latex", "quarto")) {
  type     <- match.arg(type)
  format   <- match.arg(format)
  quarto   <- format == "quarto"
  paper    <- file.path(dir, if (quarto) "main.qmd" else "main.tex")
  bib_path <- file.path(dir, "references.bib")
  fs::dir_create(dir)

  if (!file.exists(paper)) {
    # example figures (example_*.pdf, made by data-raw/example_figures.R),
    # so the skeleton shows real figures: replace them with yours
    fig_dir <- file.path(dir, figures_dir)
    fs::dir_create(fig_dir)
    examples <- list.files(system.file("templates", "paper", "figures", package = "create.project"),
                           full.names = TRUE)
    file.copy(examples, fig_dir, overwrite = FALSE)

    author_block <- paste(c(
      escape_latex(author),
      if (!is.null(email))       sprintf("\\href{mailto:%s}{\\texttt{%s}}", email, email),
      if (!is.null(affiliation)) escape_latex(affiliation),
      if (!is.null(student_id))  paste("Student ID:", escape_latex(student_id))
    ), collapse = " \\\\ ")

    if (quarto) {
      parts  <- system.file("templates", "paper", "quarto", paste0(type, ".qmd"),
                            package = "create.project", mustWork = TRUE)
      extra  <- !is.null(email) || !is.null(affiliation) || !is.null(student_id)
      values <- quarto_values(title, author, if (extra) author_block, date, bib_style,
                              figures_dir, repo_url, package)
      # the LaTeX look (header.tex) and the APA reference style (apa.csl)
      support <- c("header.tex", if (bib_style == "apa") "apa.csl")
      file.copy(system.file("templates", "paper", "quarto", support, package = "create.project"),
                dir, overwrite = FALSE)
    } else {
      parts  <- system.file("templates", "paper", c("preamble.tex", paste0(type, ".tex")),
                            package = "create.project", mustWork = TRUE)
      values <- latex_values(title, author, author_block, date, bib_style,
                             figures_dir, repo_url, package)
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

# values of the {{name}} placeholders in the Quarto templates. title, author
# and date sit in single-quoted YAML strings that Quarto reads as Markdown.
# author_block is NULL when there is only a name
quarto_values <- function(title, author, author_block, date, bib_style,
                          figures_dir, repo_url, package) {
  list(
    title         = yaml_quote(escape_markdown(title)),
    author        = yaml_quote(escape_markdown(author)),
    # Quarto prints only the author's name: the full block (name, email,
    # affiliation) replaces it at \begin{document}
    author_header = if (is.null(author_block)) "" else
      paste0("\n      - text: |\n          \\AtBeginDocument{\\author{", author_block, "}}"),
    date          = yaml_quote(date),
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
