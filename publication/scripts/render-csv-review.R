# Display a saved CSV directly; keep formulas and numeric text literal.
render_csv_review <- function(relative, output_root = "outputs") {
  escape_html <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    gsub('"', "&quot;", text, fixed = TRUE)
  }
  data <- read.csv(
    file.path(output_root, sub("^publication/outputs/", "", relative)), colClasses = "character", na.strings = NULL,
    check.names = FALSE, strip.white = FALSE, fileEncoding = "UTF-8-BOM"
  )
  name <- escape_html(basename(relative))
  cat(
    '\n<details data-review-source="', escape_html(relative), '">\n',
    "<summary>", name, " &mdash; ", nrow(data), " rows, ", ncol(data),
    " columns</summary>\n\n",
    "[Download CSV](repo-files/", relative, ")\n\n",
    # Raw HTML keeps R formula tildes from becoming Markdown subscripts.
    "```{=html}\n",
    '<div class="table-responsive" tabindex="0" role="region" aria-label="',
    name, '" style="max-height: 32rem; overflow: auto;">\n',
    '<table class="table table-striped table-sm">\n',
    "<caption>Complete contents of ", name, "</caption>\n",
    "<thead><tr>",
    paste0('<th scope="col">', escape_html(names(data)), "</th>", collapse = ""),
    "</tr></thead>\n<tbody>\n", sep = ""
  )
  for (row in seq_len(nrow(data))) {
    cells <- escape_html(unlist(data[row, , drop = FALSE], use.names = FALSE))
    cat("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>\n", sep = "")
  }
  cat("</tbody></table></div>\n```\n\n</details>\n\n")
}
