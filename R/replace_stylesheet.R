#' Replace a local stylesheet link with a remote or inline stylesheet
#'
#' Locates an HTML `<link>` element referencing a local CSS file and compares
#' the local stylesheet with a remote stylesheet retrieved from a raw URL.
#'
#' When the normalised local and remote CSS content match, the local link is
#' replaced with a link to the remote stylesheet. When the files differ, or
#' when the remote stylesheet cannot be retrieved, the local CSS is embedded
#' directly in the HTML using a `<style>` element.
#'
#' @param html A character vector containing HTML. Each element may contain one
#'   or more complete HTML tags.
#' @param local_css A length-one character string giving the path to the local
#'   CSS file.
#' @param remote_css A length-one character string giving the raw URL from
#'   which the remote CSS should be retrieved.
#' @param linked_css The CSS filename or relative path to identify in the HTML.
#'   By default, this is the basename of `local_css`.
#' @param timeout A positive numeric value giving the connection timeout in
#'   seconds. Defaults to `30`.
#' @param warn Logical. When `TRUE`, warnings are issued if no matching
#'   stylesheet link is found or the remote stylesheet cannot be retrieved.
#'
#' @return A character vector with matching stylesheet links replaced by
#'   either a remote `<link>` element or an inline `<style>` element.
#'
#' @details
#' The function handles optional whitespace around HTML attribute equals signs
#' and supports both single-quoted and double-quoted attribute values.
#'
#' For example, all of the following can be matched:
#'
#' ```
#' <link rel="stylesheet" href="style.css">
#' <link rel = "stylesheet" href = "style.css">
#' <link href='../css/style.css' rel='stylesheet'>
#' ```
#'
#' CSS comparison normalises:
#'
#' * UTF-8 byte-order marks;
#' * Windows and Unix line endings;
#' * leading and trailing whitespace.
#'
#' Internal CSS whitespace and comments remain significant.
#'
#' If the remote stylesheet cannot be retrieved, the local stylesheet is
#' embedded inline. This ensures that the resulting HTML remains usable when
#' internet access is unavailable.
#'
#' @examples
#' html <- c(
#'   "<!doctype html>",
#'   "<html>",
#'   "<head>",
#'   '<link rel="stylesheet" href="style.css" type="text/css" />',
#'   "</head>",
#'   "<body></body>",
#'   "</html>"
#' )
#'
#' \dontrun{
#' html <- replace_stylesheet(
#'   html = html,
#'   local_css = "style.css"
#' )
#' }
#'
#' @export
replace_stylesheet <- function(
    html,
    local_css,
    remote_css = paste0(
      "https://raw.githubusercontent.com/",
      "NISRA-Tech-Lab/rs-resources/",
      "main/css/style.css"
    ),
    linked_css = basename(local_css),
    timeout = 30,
    warn = TRUE
) {
  validate_stylesheet_inputs(
    html = html,
    local_css = local_css,
    remote_css = remote_css,
    linked_css = linked_css,
    timeout = timeout,
    warn = warn
  )

  local_css <- normalizePath(
    local_css,
    winslash = "/",
    mustWork = TRUE
  )

  local_text <- read_local_css(local_css)

  link_pattern <- stylesheet_link_pattern(linked_css)

  matching_elements <- grepl(
    pattern = link_pattern,
    x = html,
    ignore.case = TRUE,
    perl = TRUE
  )

  if (!any(matching_elements)) {
    if (warn) {
      warning(
        sprintf(
          "No stylesheet link referencing '%s' was found.",
          linked_css
        ),
        call. = FALSE
      )
    }

    return(html)
  }

  remote_text <- retrieve_remote_css(
    remote_css = remote_css,
    timeout = timeout,
    warn = warn
  )

  css_matches <- !is.null(remote_text) &&
    identical(
      normalise_css(local_text),
      normalise_css(remote_text)
    )

  if (css_matches) {
    replacement <- create_remote_stylesheet_link(remote_css)
  } else {
    replacement <- create_inline_stylesheet(local_text)
  }

  html[matching_elements] <- replace_matching_links(
    html = html[matching_elements],
    pattern = link_pattern,
    replacement = replacement
  )

  html
}


#' Validate stylesheet replacement inputs
#'
#' @param html Character vector containing HTML.
#' @param local_css Path to the local CSS file.
#' @param remote_css Raw URL of the remote CSS file.
#' @param linked_css CSS filename or relative path to locate in the HTML.
#' @param timeout Connection timeout in seconds.
#' @param warn Logical controlling warning messages.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
validate_stylesheet_inputs <- function(
    html,
    local_css,
    remote_css,
    linked_css,
    timeout,
    warn
) {
  if (!is.character(html) || anyNA(html)) {
    stop(
      "`html` must be a character vector without missing values.",
      call. = FALSE
    )
  }

  if (
    !is.character(local_css) ||
    length(local_css) != 1L ||
    is.na(local_css) ||
    !nzchar(local_css)
  ) {
    stop(
      "`local_css` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (!file.exists(local_css)) {
    stop(
      sprintf(
        "The local CSS file does not exist: %s",
        local_css
      ),
      call. = FALSE
    )
  }

  if (dir.exists(local_css)) {
    stop(
      sprintf(
        "`local_css` refers to a directory: %s",
        local_css
      ),
      call. = FALSE
    )
  }

  if (
    !is.character(remote_css) ||
    length(remote_css) != 1L ||
    is.na(remote_css) ||
    !nzchar(remote_css)
  ) {
    stop(
      "`remote_css` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (
    !is.character(linked_css) ||
    length(linked_css) != 1L ||
    is.na(linked_css) ||
    !nzchar(linked_css)
  ) {
    stop(
      "`linked_css` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(timeout) ||
    length(timeout) != 1L ||
    is.na(timeout) ||
    !is.finite(timeout) ||
    timeout <= 0
  ) {
    stop(
      "`timeout` must be one positive finite number.",
      call. = FALSE
    )
  }

  if (
    !is.logical(warn) ||
    length(warn) != 1L ||
    is.na(warn)
  ) {
    stop(
      "`warn` must be either TRUE or FALSE.",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Read a local CSS file
#'
#' @param path Path to a local CSS file.
#'
#' @return A length-one character string containing the CSS.
#'
#' @keywords internal
read_local_css <- function(path) {
  css_lines <- readLines(
    con = path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  paste(css_lines, collapse = "\n")
}


#' Retrieve CSS from a raw URL
#'
#' @param remote_css Raw URL of the remote CSS file.
#' @param timeout Connection timeout in seconds.
#' @param warn Logical controlling warning messages.
#'
#' @return A length-one character string containing the remote CSS, or `NULL`
#'   when retrieval fails.
#'
#' @keywords internal
retrieve_remote_css <- function(
    remote_css,
    timeout = 30,
    warn = TRUE
) {
  previous_timeout <- getOption("timeout")

  on.exit(
    options(timeout = previous_timeout),
    add = TRUE
  )

  options(
    timeout = max(timeout, previous_timeout)
  )

  tryCatch(
    {
      css_lines <- suppressWarnings(
        readLines(
          con = remote_css,
          warn = FALSE,
          encoding = "UTF-8"
        )
      )

      paste(css_lines, collapse = "\n")
    },
    error = function(error) {
      if (warn) {
        warning(
          paste0(
            "The remote stylesheet could not be retrieved. ",
            "The local stylesheet will be embedded inline. ",
            "Details: ",
            conditionMessage(error)
          ),
          call. = FALSE
        )
      }

      NULL
    }
  )
}


#' Normalise CSS content before comparison
#'
#' @param css A length-one character string containing CSS.
#'
#' @return A normalised length-one character string.
#'
#' @keywords internal
normalise_css <- function(css) {
  css <- enc2utf8(css)

  # Remove a UTF-8 byte-order mark.
  css <- sub(
    pattern = "^\ufeff",
    replacement = "",
    x = css
  )

  # Normalise CRLF and CR line endings to LF.
  css <- gsub(
    pattern = "\r\n?",
    replacement = "\n",
    x = css,
    perl = TRUE
  )

  trimws(css)
}


#' Construct a stylesheet link-matching regular expression
#'
#' @param linked_css CSS filename or relative path to locate.
#'
#' @return A Perl-compatible regular expression.
#'
#' @keywords internal
stylesheet_link_pattern <- function(linked_css) {
  linked_css <- gsub(
    pattern = "\\\\",
    replacement = "/",
    x = linked_css
  )

  escaped_css <- escape_regex(linked_css)

  paste0(
    "<link\\b",
    "[^>]*",
    "\\bhref\\s*=\\s*",
    "([\"'])",
    "(?:[^\"']*/)?",
    escaped_css,
    "(?:[?#][^\"']*)?",
    "\\1",
    "[^>]*",
    "/?>"
  )
}


#' Escape text for literal use in a regular expression
#'
#' @param x A length-one character string.
#'
#' @return The escaped character string.
#'
#' @keywords internal
escape_regex <- function(x) {
  gsub(
    pattern = "([][{}()+*^$|\\\\?.])",
    replacement = "\\\\\\1",
    x = x,
    perl = TRUE
  )
}


#' Replace matching stylesheet links
#'
#' @param html Character vector containing HTML.
#' @param pattern Regular expression matching stylesheet links.
#' @param replacement Replacement HTML.
#'
#' @return The updated HTML character vector.
#'
#' @keywords internal
replace_matching_links <- function(
    html,
    pattern,
    replacement
) {
  matches <- gregexpr(
    pattern = pattern,
    text = html,
    ignore.case = TRUE,
    perl = TRUE
  )

  for (i in seq_along(html)) {
    if (
      length(matches[[i]]) == 1L &&
      identical(matches[[i]][1L], -1L)
    ) {
      next
    }

    regmatches(
      x = html[i],
      m = list(matches[[i]])
    ) <- list(
      rep(replacement, length(matches[[i]]))
    )
  }

  html
}


#' Create a remote stylesheet link
#'
#' @param remote_css URL of the remote stylesheet.
#'
#' @return An HTML `<link>` element.
#'
#' @keywords internal
create_remote_stylesheet_link <- function(remote_css) {
  sprintf(
    paste0(
      '<link rel="stylesheet" href="%s" ',
      'type="text/css" />'
    ),
    escape_html_attribute(remote_css)
  )
}


#' Create an inline stylesheet
#'
#' @param css A length-one character string containing CSS.
#'
#' @return An HTML `<style>` element containing the CSS.
#'
#' @keywords internal
create_inline_stylesheet <- function(css) {
  # Prevent CSS comments or strings containing a closing style tag from
  # terminating the HTML style element.
  css <- gsub(
    pattern = "(?i)</style",
    replacement = "<\\\\/style",
    x = css,
    perl = TRUE
  )

  paste0(
    '<style type="text/css">\n',
    css,
    "\n</style>"
  )
}


#' Escape text for use in a double-quoted HTML attribute
#'
#' @param x A length-one character string.
#'
#' @return HTML-escaped text.
#'
#' @keywords internal
escape_html_attribute <- function(x) {
  x <- gsub(
    pattern = "&",
    replacement = "&amp;",
    x = x,
    fixed = TRUE
  )

  x <- gsub(
    pattern = '"',
    replacement = "&quot;",
    x = x,
    fixed = TRUE
  )

  x <- gsub(
    pattern = "<",
    replacement = "&lt;",
    x = x,
    fixed = TRUE
  )

  x <- gsub(
    pattern = ">",
    replacement = "&gt;",
    x = x,
    fixed = TRUE
  )

  x
}
