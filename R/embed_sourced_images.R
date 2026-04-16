#' Embed non-inline image sources as base64 data URIs
#'
#' Scans HTML lines for `<img>` tags and replaces image file references with
#' base64-encoded data URIs, where the image is not already embedded. This helps
#' create more self-contained HTML output.
#'
#' @param doc A character string. Path to the source `.Rmd` file. This is used
#'   to resolve image paths relative to the document directory.
#' @param lines A character vector of HTML lines to search and modify.
#'
#' @return A character vector containing the modified HTML lines.
#'
#' @details
#' The function looks for `<img>` tags in `lines`. If an image source does not
#' already begin with `data:image`, the file is read and converted to a base64
#' data URI using [base64enc::base64encode()].
#'
#' Relative paths containing the parent folder name are rewritten using the
#' directory of `doc` before encoding.
#'
#' @importFrom base64enc base64encode
#'
#' @examples
#' \dontrun{
#' html_lines <- readLines("report.html")
#' html_lines <- embed_sourced_images("report.Rmd", html_lines)
#' }
#'
#' @export
embed_sourced_images <- function (doc, lines) {

  imgs <- lines[grepl("<img", lines)]

  for (img in imgs) {
    if (!grepl("data:image", img)) {
      name <- sub('".*', '', sub(paste0('.*src="'), '', img))
      path <- sub(paste0("../", basename(dirname(doc))),
                  dirname(doc),
                  name,
                  fixed = TRUE)
      lines <- sub(name,
                        paste0("data:image/png;base64,", base64enc::base64encode(path)),
                        lines)
    }
  }

  return(lines)

}
