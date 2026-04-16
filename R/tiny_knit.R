#' Render an R Markdown file as a reduced-size HTML report
#'
#' Creates a modified copy of an input `.Rmd` file with
#' `self_contained: no`, renders it to HTML, rewrites selected dependency links,
#' embeds remaining local assets where needed, and saves a timestamped output
#' HTML file in an `_outputs` directory beside the source file.
#'
#' @param doc A character string. Path to a single input `.Rmd` file.
#'
#' @return Invisibly returns the path to the generated HTML file.
#'
#' @details
#' This function:
#' \itemize{
#'   \item creates a temporary `_tiny.Rmd` version of the source file
#'   \item renders the document to HTML using [rmarkdown::render()]
#'   \item replaces selected Tech Lab-authored dependencies with hosted versions
#'   \item rewrites local JavaScript and CSS references to hosted resources where available
#'   \item embeds other local assets, including sourced images, as base64 data URIs
#'   \item saves the final HTML with a timestamped filename in an output folder
#' }
#'
#' The output directory is created in the same parent directory as `doc`, with
#' the name `"<parent-folder>_outputs"`.
#'
#' @importFrom xfun sans_ext
#' @importFrom httr GET content
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#'
#' @examples
#' \dontrun{
#' tiny_knit("report.Rmd")
#' }
#'
#' @export
tiny_knit <- function (doc) {

  if (!is.character(doc) || length(doc) != 1 || !grepl("\\.[Rr]md$", doc)) {
    stop("`doc` must be a path to a single .Rmd file", call. = FALSE)
  }

  tiny_rmd <- remove_self_contained(doc)
  output_dir <- paste0(dirname(doc), "/", basename(dirname(doc)), "_outputs")
  output_file <- sub("\\.[Rr]md$", ".html", basename(doc))

  #' @importFrom xfun sans_ext
  output_files <- paste0(xfun::sans_ext(output_file),"_files")

  if(!dir.exists(output_dir)) dir.create(output_dir)

  #' @importFrom rmarkdown render
  rmarkdown::render(input = tiny_rmd,
                   output_format = "html_document",
                   output_file = output_file,
                   output_dir = output_dir)

  remove_tiny_script(doc)

  output_raw <- readLines(paste0(output_dir, "/", output_file))

  # TechLab authored scripts ####
  ## style.css ####
  output_new <- output_raw

  output_new[which(grepl("style.css", output_new))] <- '<link rel="stylesheet" href="https://nisra-tech-lab.github.io/rs-resources/css/style.css" type="text/css" />'

  ## cookies_script.js ####
  output_new <- sub("cookies_script.js", "https://nisra-tech-lab.github.io/rs-resources/js/cookies_script.js", output_new)

  ## annotation.js ####
  output_new <- sub("annotation.js", "https://nisra-tech-lab.github.io/rs-resources/js/annotation.js", output_new)


  # Fix links to local files ####
  res <- httr::GET("https://nisra-tech-lab.github.io/rs-resources/file_list.txt")
  txt <- httr::content(res, "text", encoding = "UTF-8")
  file_list <- readLines(textConnection(txt))

  ## Link to RS-resources ####
  output_new <- link_to_rs_resources(output_files, output_dir, output_new, file_list)

  ## other sourced images ####
  output_new <- embed_sourced_images(doc, output_new)

  # Write out html
  html_file_name <- paste0(output_dir, "/", sub(".html", paste0("_", format(Sys.time(), "%d-%m-%Y_%H%M"), ".html"), output_file))
  writeLines(output_new, html_file_name)
  file.remove(paste0(output_dir, "/", output_file))
  message("Tiny output created at: ", html_file_name)
  utils::browseURL(html_file_name)
  invisible(html_file_name)

}

