#' Remove self-contained option from R Markdown YAML
#'
#' Reads an R Markdown (`.Rmd`) file, modifies the YAML header to ensure
#' `self_contained: no` is set for `html_document`, and writes a modified
#' copy to the same directory with a `_tiny.Rmd` suffix.
#'
#' @param doc A character string. Path to an input `.Rmd` file.
#'   The file must contain a valid YAML header delimited by `---`.
#'
#' @return Invisibly returns the path to the modified `.Rmd` file.
#'
#' @details
#' If `self_contained` is not present in the YAML, it will be added.
#' If it is present and set to `yes`, it will be changed to `no`.
#'
#' @examples
#' \dontrun{
#' remove_self_contained("report.Rmd")
#' }
#'
#' @export
remove_self_contained <- function (doc) {

  if (!is.character(doc) || length(doc) != 1 || !grepl("\\.Rmd$", doc)) {
    stop("`doc` must be a path to a single .Rmd file")
  }

  raw_rmd <- readLines(doc)

  yaml_bounds <- which(raw_rmd == "---")
  yaml_raw <- raw_rmd[yaml_bounds[1]:yaml_bounds[2]]

  if (length(which(grepl("self_contained", yaml_raw))) == 0) {
    html_doc_line <- which(grepl("html_document", yaml_raw))
    yaml_new <- c(yaml_raw[1:html_doc_line], "    self_contained: no", yaml_raw[(html_doc_line + 1):length(yaml_raw)])
  } else {
    yaml_new <- sub("self_contained: yes", "self_contained: no", yaml_raw)
  }

 rmd_new <- c(yaml_new, raw_rmd[(yaml_bounds[2] + 1):length(raw_rmd)])

 tiny_filename <- sub(".Rmd", "_tiny.Rmd", doc)

 writeLines(rmd_new, tiny_filename)

 return(tiny_filename)

}
