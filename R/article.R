#' @title Use article with the VUB theme
#' @description Use article with the VUB theme
#' @param subtitle An optional subtitle.
#' @param reportnr The report number. Defaults to the date and time of compilation.
#' @param faculty The faculty.
#' @param codesize The fontsize of the code, relative to the fontsize of the text (= normal size).
#' Allowed values are "normalsize", "small", "footnotesize", "scriptsize" and "tiny". Defaults to "footnotesize".
#' @param keep_tex Keep the tex file. Defaults to FALSE.
#' @param floatbarrier Should float barriers be placed?
#'   Defaults to NA (only float barriers before starting a new chapter `#`).
#'   Options are "section" (`##`), "subsection" (`###`) and
#'   "subsubsection" (`####`).
#' @param fig_crop \code{TRUE} to automatically apply the \code{pdfcrop} utility
#'   (if available) to pdf figures.
#' @param pandoc_args Additional command line options to pass to pandoc
#' @param includes Passed on to \code{\link{includes_to_pandoc_args}}
#' @param ... extra parameters: see details
#'
#' @details
#' Available extra parameters:
#' - `lof`: display a list of figures. Defaults to TRUE
#' - `lot`: display a list of tables. Defaults to TRUE
#' - `hyphenation`: the correct hyphenation for certain words.
#' - `cover`: an optional pdf file. The first two pages will be prepended to the
#' report.
#' @export
#' @examples
#' outformat <- vub_article()
vub_article <- function(
  subtitle,
  reportnr,
  faculty = "DIGI: Brussels Platform for Digital Humanities",
  floatbarrier = c(NA, "section", "subsection", "subsubsection"),
  codesize = c("footnotesize", "scriptsize", "tiny", "small", "normalsize"),
  keep_tex = FALSE,
  fig_crop = TRUE,
  includes = NULL,
  pandoc_args = NULL,
  ...
) {
  floatbarrier <- match.arg(floatbarrier)
  extra <- list(...)
  codesize <- match.arg(codesize)

  template <- system.file(package = "VUBmd", "pandoc", "VUB_REPORT_ENTRYPOINT.tex")

  args <- c(
    "--template", template,
    pandoc_variable_arg("codesize", codesize),
    pandoc_variable_arg("faculty", faculty),
    "--pdf-engine", "xelatex",
    pandoc_args,
    # citations
    c(paste0("--", "natbib"), ""),
    # content includes
    includes_to_pandoc_args(includes),
    ifelse(
      rep(missing(reportnr), 2), "", pandoc_variable_arg("reportnr", reportnr)
    ),
    ifelse(
      rep(missing(subtitle), 2), "", pandoc_variable_arg("subtitle", subtitle)
    )
  )
  args <- args[args != ""]
  if ("lof" %in% names(extra) && extra$lof) {
    args <- c(args, pandoc_variable_arg("lof", TRUE))
  }
  if ("lot" %in% names(extra) && extra$lot) {
    args <- c(args, pandoc_variable_arg("lot", TRUE))
  }
  extra <- extra[!names(extra) %in% c("lof", "lot")]
  args <- c(
    args,
    sapply(
      names(extra),
      function(x) {
        pandoc_variable_arg(x, extra[[x]])
      }
    )
  )
  vars <- switch(
    floatbarrier,
    section = "",
    subsection = c("", "sub"),
    subsubsection = c("", "sub", "subsub")
  )
  floating <- lapply(
    sprintf("floatbarrier%ssection", vars),
    pandoc_variable_arg,
    value = TRUE
  )
  args <- c(args, unlist(floating))
  opts_chunk <- list(
    latex.options = "{}",
    dev = "cairo_pdf",
    fig.align = "center",
    dpi = 300,
    fig.width = 4.5,
    fig.height = 2.9
  )
  knit_hooks <- NULL
  crop <- fig_crop &&
    !identical(.Platform$OS.type, "windows") &&
    nzchar(Sys.which("pdfcrop"))
  if (crop) {
    knit_hooks <- list(crop = knitr::hook_pdfcrop)
    opts_chunk$crop <- TRUE
  }

  post_processor <- function(metadata, input, output, clean, verbose) {
    text <- readLines(output, warn = FALSE)

    # move frontmatter before toc
    mainmatter <- grep("\\\\mainmatter", text) #nolint
    if (length(mainmatter)) {
      starttoc <- grep("%starttoc", text)
      endtoc <- grep("%endtoc", text)
      text <- text[
        c(
          1:(starttoc - 1),              # preamble
          (endtoc + 1):(mainmatter - 1), # frontmatter
          (starttoc + 1):(endtoc - 1),   # toc
          (mainmatter + 1):length(text)  # mainmatter
        )
      ]
    }

    # move appendix after bibliography
    appendix <- grep("\\\\appendix", text) #nolint
    startbib <- grep("%startbib", text)
    endbib <- grep("%endbib", text)
    if (length(appendix) & length(startbib)) {
      text <- text[
        c(
          1:(appendix - 1),              # mainmatter
          (startbib + 1):(endbib - 1),   # bibliography
          (appendix):(startbib - 1),     # appendix
          (endbib + 1):length(text)      # backmatter
        )
      ]
    }

    writeLines(enc2utf8(text), output, useBytes = FALSE)
    output
  }

  output_format(
    knitr = knitr_options(
      opts_knit = list(
        width = 96,
        concordance = TRUE
      ),
      opts_chunk = opts_chunk,
      knit_hooks = knit_hooks
    ),
    pandoc = pandoc_options(
      to = "latex",
      latex_engine = "xelatex",
      args = args,
      keep_tex = keep_tex
    ),
    #post_processor = post_processor,
    clean_supporting = !keep_tex
  )
}
