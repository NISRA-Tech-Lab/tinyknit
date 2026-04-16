#' Remove generated `_tiny.Rmd` file
#'
#' Deletes the `_tiny.Rmd` file associated with a given R Markdown (`.Rmd`)
#' input, if it exists in the same directory.
#'
#' @param doc A character string. Path to an input `.Rmd` file.
#'   The corresponding `_tiny.Rmd` file is assumed to have the same name
#'   with a `_tiny` suffix in the same directory.
#'
#' @return Invisibly returns `TRUE` if the file was deleted, `FALSE` if the file
#'   did not exist or could not be removed.
#'
#' @examples
#' \dontrun{
#' remove_tiny_script("report.Rmd")
#' }
#'
#' @export
remove_tiny_script <- function (doc) {

  tiny_doc <- sub(".Rmd", "_tiny.Rmd", doc)
  if (file.exists(tiny_doc)) file.remove(tiny_doc)

}
