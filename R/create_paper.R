# create a paper skeleton (main.tex + references.bib) from inst/templates/paper/

#' create a LaTeX paper skeleton
#'
#' writes `main.tex` and `references.bib` into `dir`. `main.tex` is the shared
#' preamble (`inst/templates/paper/preamble.tex`) followed by the body for
#' `type`. the `{{name}}` placeholders are filled from the arguments;
#' everything in `[...]` is writing guidance for you to replace. existing
#' files are kept.
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
#' @param bib_style   biblatex style, e.g. "apa", "authoryear", "chicago-authordate"
#' @param figures_dir where LaTeX looks for figures, relative to `dir` (keep the trailing /)
#' @param repo_url    repository URL, printed in the appendix / availability section
#' @param package     software only: package name used in the text and code examples
#' @return the path of main.tex, invisibly
#' @export
#' @examples
#' \dontrun{
#' create_paper("5_docs", title = "Why does X affect Y?")
#' create_paper("paper", type = "software", package = "coolfun")
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
                         package     = "[package]") {
  type     <- match.arg(type)
  tex_path <- file.path(dir, "main.tex")
  bib_path <- file.path(dir, "references.bib")
  fs::dir_create(dir)

  if (!file.exists(tex_path)) {
    parts <- system.file("templates", "paper", c("preamble.tex", paste0(type, ".tex")),
                         package = "create.project", mustWork = TRUE)
    tex   <- paste(unlist(lapply(parts, readLines, encoding = "UTF-8")), collapse = "\n")

    author_block <- paste(c(
      escape_latex(author),
      if (!is.null(email))       sprintf("\\href{mailto:%s}{\\texttt{%s}}", email, email),
      if (!is.null(affiliation)) escape_latex(affiliation),
      if (!is.null(student_id))  paste("Student ID:", escape_latex(student_id))
    ), collapse = " \\\\ ")

    values <- list(
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
      repo         = if (grepl("^https://github.com/", repo_url)) sub("^https://github.com/", "", repo_url) else "[user/repo]",
      package      = package
    )

    # regmatches<- inserts the value literally: gsub() would read the
    # backslashes of LaTeX commands as escapes (\textbf -> tab + "extbf")
    for (nm in names(values)) {
      hits <- gregexpr(paste0("{{", nm, "}}"), tex, fixed = TRUE)
      regmatches(tex, hits) <- list(rep(values[[nm]], sum(hits[[1]] > 0)))
    }
    writeLines(enc2utf8(tex), tex_path, useBytes = TRUE)
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

  invisible(tex_path)
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
