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
#' Multiple `<img>` tags on the same line are processed individually.
#'
#' Relative paths containing the parent folder name are rewritten using the
#' directory of `doc` before encoding.
#'
#' @importFrom base64enc base64encode
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' html_lines <- readLines("report.html")
#' html_lines <- embed_sourced_images("report.Rmd", html_lines)
#' }
#'
#' @export
embed_sourced_images <- function(doc, lines) {

  img_lines <- which(grepl("<img", lines, fixed = TRUE))

  for (i in img_lines) {
    line <- lines[i]

    img_tags <- regmatches(line, gregexpr("<img[^>]+/?>", line, perl = TRUE))[[1]]

    if (length(img_tags) == 0) {
      next
    }

    for (img in img_tags) {
      if (!grepl('src="data:image', img, fixed = TRUE)) {

        name <- sub('".*', "", sub('.*src="', "", img))
        path <- sub(
          paste0("../", basename(dirname(doc))),
          dirname(doc),
          name,
          fixed = TRUE
        )

        ext <- tolower(tools::file_ext(path))
        mime <- switch(
          ext,
          png  = "image/png",
          jpg  = "image/jpeg",
          jpeg = "image/jpeg",
          gif  = "image/gif",
          svg  = "image/svg+xml",
          NULL
        )

        if (is.null(mime)) {
          next
        }

        new_src <- paste0(
          "data:", mime, ";base64,",
          base64enc::base64encode(path)
        )

        line <- sub(name, new_src, line, fixed = TRUE)
      }
    }

    lines[i] <- line
  }

  lines
}
