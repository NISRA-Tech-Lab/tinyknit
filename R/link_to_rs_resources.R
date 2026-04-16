#' Replace local HTML dependencies with rs-resources links or embedded data URIs
#'
#' Scans HTML lines for references to local JavaScript, CSS, and image files.
#' Where a matching JavaScript or CSS file is available in `file_list`, the local
#' reference is replaced with the corresponding hosted file on
#' `rs-resources`. Otherwise, the file is embedded directly into the HTML as a
#' base64-encoded data URI. PNG images in `figure-html` paths are also embedded
#' as base64 data URIs.
#'
#' @param files_dir A character string giving the relative directory in which
#'   supporting HTML dependency files are stored.
#' @param output_dir A character string giving the path to the output directory
#'   containing the rendered HTML dependencies.
#' @param lines A character vector of HTML lines to search and modify.
#' @param file_list A character vector of file paths available in the
#'   `rs-resources` repository.
#'
#' @return A character vector containing the modified HTML lines.
#'
#' @details
#' The function looks for lines containing `files_dir` and processes three kinds
#' of assets:
#' \itemize{
#'   \item JavaScript files (`.js`)
#'   \item CSS files (`.css`)
#'   \item PNG images referenced in `figure-html`
#' }
#'
#' If a JavaScript or CSS file is not found in `file_list`, a message is printed
#' advising the user to contact Tech Lab, and the file is embedded directly in
#' the HTML.
#'
#' @importFrom base64enc base64encode
#'
#' @examples
#' \dontrun{
#' html_lines <- readLines("report.html")
#' out <- link_to_rs_resources(
#'   files_dir = "report_files/",
#'   output_dir = "demo_outputs",
#'   lines = html_lines,
#'   file_list = c("/js/example.js", "/css/example.css")
#' )
#' }
#'
#' @export
link_to_rs_resources <- function (files_dir, output_dir, lines, file_list) {

  links <- lines[grepl(files_dir, lines)]

  for (link in links) {
    if (grepl(".js", link, fixed = TRUE)) {
      name <- sub('\".*', '', sub(paste0('<script src="', files_dir), '', link))
      if (paste0("/js", name) %in% file_list) {
        lines <- sub(paste0(files_dir, name), paste0("https://nisra-tech-lab.github.io/rs-resources/js", name), lines)
      } else {
        print(paste0(name, " not found on rs-resources, please email techlab@nisra.gov.uk"))
        lines[which(lines == link)] <-
          paste0('<script type="text/javascript" src="data:text/javascript;base64,', base64enc::base64encode(paste0(output_dir, "/", files_dir, name)),'"></script>')

      }
    } else if (grepl(".css", link, fixed = TRUE)) {
      name <- sub('\".*', '', sub(paste0('<link href="', files_dir), '', link))
      if (paste0("/js", name) %in% file_list) {
        lines <- sub(paste0(files_dir, name),
                          paste0("https://nisra-tech-lab.github.io/rs-resources/js", name),
                          lines,
                          fixed = TRUE)
      } else {
        print(paste0(name, " not found on rs-resources, please email techlab@nisra.gov.uk"))
        lines[which(lines == link)] <-
          paste0('<link rel="stylesheet" href="data:text/css;base64,', base64enc::base64encode(paste0(output_dir, "/", files_dir, name)),'"></link>')
      }
    } else if (grepl("figure-html", link, fixed = TRUE)) {
      name <- sub('".*', '', sub(paste0('.*src="', files_dir), '', link))
      lines <- sub(paste0(files_dir, name),
                        paste0("data:image/png;base64,", base64enc::base64encode(paste0(output_dir, "/", files_dir, name))),
                        lines,
                        fixed = TRUE)
    }
  }

  return(lines)
}
