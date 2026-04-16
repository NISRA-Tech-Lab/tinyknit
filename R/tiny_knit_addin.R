#' Run tiny_knit on the currently open R Markdown file
#'
#' This addin runs [tiny_knit()] on the R Markdown file open in the
#' RStudio Source pane.
#'
#' @export
tiny_knit_addin <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This command must be run from RStudio.", call. = FALSE)
  }

  tiny_knit()
}
