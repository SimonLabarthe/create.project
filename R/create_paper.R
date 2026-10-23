# create a paper skeleton (main.tex + references.bib) from inst/templates/paper.tex

#' create a LaTeX paper skeleton
#'
#' writes `main.tex` and `references.bib` into `dir`. the template's
#' `{{name}}` placeholders are filled from the arguments; everything in
#' `[...]` is writing guidance for you to replace. existing files are kept.
#'
#' @param dir         folder where the paper is written (created if needed)
#' @param title       paper title
#' @param author      author name
#' @param email       author email, or NULL
#' @param affiliation institution, or NULL
#' @param student_id  student id, or NULL
#' @param date        date printed under the title
#' @param bib_style   biblatex style, e.g. "apa", "authoryear", "chicago-authordate"
#' @param figures_dir where LaTeX looks for figures, relative to `dir` (keep the trailing /)
#' @param repo_url    replication repository, printed in the appendix
#' @return the path of main.tex, invisibly
#' @export
#' @examples
#' \dontrun{
#' create_paper("5_docs", title = "Why does X affect Y?")
#' }
create_paper <- function(dir         = "5_docs",
                         title       = "[Insert Title]",
                         author      = "Simon Labarthe",
                         email       = NULL,
                         affiliation = NULL,
                         student_id  = NULL,
                         date        = format(Sys.Date(), "%B %Y"),
                         bib_style   = "apa",
                         figures_dir = "../4_outputs/figures/",
                         repo_url    = "[link]") {
  tex_path <- file.path(dir, "main.tex")
  bib_path <- file.path(dir, "references.bib")
  fs::dir_create(dir)

  if (!file.exists(tex_path)) {
    template <- system.file("templates", "paper.tex", package = "create.project", mustWork = TRUE)
    tex      <- paste(readLines(template, encoding = "UTF-8"), collapse = "\n")

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
      repo_url     = if (grepl("^http", repo_url)) sprintf("\\url{%s}", repo_url) else repo_url
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
