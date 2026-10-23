# figures for paper/main.tex: three pages of a freshly generated research paper
# run from the project root, with the package installed (devtools::install())

library(create.project)

demo <- file.path(tempdir(), "demo", "5_docs")
create_paper(demo, "research")

old <- setwd(demo)
tinytex::latexmk("main.tex", engine = "pdflatex", bib_engine = "biber")
setwd(old)

pages <- c(3, 6, 7)
pdftools::pdf_convert(file.path(demo, "main.pdf"), pages = pages, dpi = 100,
                      filenames = file.path("paper", "figures", paste0("demo_page", pages, ".png")))
