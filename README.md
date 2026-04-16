# tinyknit

Create lightweight HTML outputs from R Markdown by reducing file size and externalising or embedding dependencies.

## Overview

`tinyknit` provides a simple workflow to render `.Rmd` files into smaller, more shareable HTML documents. It does this by:

- Disabling `self_contained` mode
- Linking to shared external resources (Tech Lab `rs-resources`) where available
- Embedding remaining local assets (JS, CSS, images) as base64
- Producing a timestamped HTML output in a dedicated folder

This is particularly useful for publishing reports that need to be lightweight and portable.

---

## Installation

You can install the development version from source:

```r
devtools::install("NISRA-Tech-Lab/tinyknit")
```

---

## Usage

```r
library(tinyknit)

tiny_knit("path/to/report.Rmd")
```

This will:

1. Create a temporary `_tiny.Rmd` version of your file
1. Render it to HTML
1. Replace or embed dependencies
1. Save the final output in a new folder: `<parent-folder>_outputs/`
1. Open the result in your browser

---

## Output

The final HTML will be:

* Timestamped
* Smaller than standard self-contained outputs
* Suitable for sharing or publishing

Example:

```
demo_outputs/
  demo_report_16-04-2026_1430.html
```

---

## Key Functions

* tiny_knit() – main user-facing function
* remove_self_contained() – modifies YAML to disable self-contained mode
* link_to_rs_resources() – replaces local dependencies with hosted versions
* embed_sourced_images() – embeds images as base64
* remove_tiny_script() – cleans up temporary files

---

## Dependencies

* rmarkdown
* xfun
* httr
* base64enc

---


## Notes

* The package assumes standard R Markdown HTML output structure

* External resources are sourced from:\
https://nisra-tech-lab.github.io/rs-resources/

* If a dependency is not found, it is embedded directly

---

## Development

Typical workflow:

```r
devtools::document()
devtools::install()
devtools::check()
```

---

## License

MIT © NISRA Tech Lab


---
