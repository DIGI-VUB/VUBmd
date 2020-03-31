#' @title Use slides with the VUB theme
#' @description Use slides with the VUB theme
#' @param subtitle The subtitle.
#' @param location The date and place of the event.
#' @param institute The affiliation of the authors.
#' @param toc_name Name of the table of contents. Defaults to "Overzicht".
#' @param fontsize The fontsite of the document. Defaults to 10pt.
#' @param codesize The fontsize of the code, relative to the fontsize of the text (= normal size).
#' Allowed values are "normalsize", "small", "footnotesize", "scriptsize" and "tiny". Defaults to "footnotesize".
#' @param lang The language of the document. Defaults to "dutch"
#' @param slide_level Indicate which heading level is used for the frame titles
#' @param keep_tex Keep the tex file. Defaults to FALSE.
#' @param toc display a table of content after the title slide
#' @param ... extra parameters
#' @export
#' @examples
#' outformat <- vub_slides()
vub_slides <- function(subtitle = "Provide a subtitle", location = "VUB - Brussels", institute, toc_name, slide_level = 2,
                       fontsize,
                       codesize = c("footnotesize", "scriptsize", "tiny", "small", "normalsize"), lang = "dutch",
                       toc = FALSE, keep_tex = FALSE){
  codesize <- match.arg(codesize)
  template <- system.file(package = "VUBmd", "pandoc", "VUB_SLIDES_ENTRYPOINT.tex")
  args <- c(
    "--slide-level", as.character(slide_level),
    "--template", template,
    rmarkdown::pandoc_variable_arg("mylanguage", lang),
    rmarkdown::pandoc_variable_arg("codesize", codesize)
  )
  args <- c(args, "--pdf-engine", "xelatex")
  if (toc) {
    args <- c(args, pandoc_variable_arg("toc", "true"))
    if (!missing(toc_name)) {
      args <- c(args, pandoc_variable_arg("toc_name", toc_name))
    }
  }
  # citations
  args <- c(args, paste0("--", "natbib"))
  # subtitle / location / institute
  if (!missing(subtitle)) {
    args <- c(args, pandoc_variable_arg("subtitle", subtitle))
  }
  if (!missing(location)) {
    args <- c(args, pandoc_variable_arg("location", location))
  }
  if (!missing(fontsize)) {
    args <- c(args, pandoc_variable_arg("fontsize", fontsize))
  }
  if (!missing(institute)) {
    args <- c(args, pandoc_variable_arg("institute", institute))
  }

  rmarkdown::output_format(
    knitr = knitr_options(
      opts_knit = list(
        width = 80,
        concordance = TRUE
      ),
      opts_chunk = list(
        dev = "pdf",
        dev.args = list(bg = "transparent"),
        dpi = 300,
        fig.width = 4.5,
        fig.height = 2.8
      )
    ),
    pandoc = pandoc_options(
      to = "beamer",
      latex_engine = "xelatex",
      args = args,
      keep_tex = keep_tex
    ),
    clean_supporting = !keep_tex
  )
}
