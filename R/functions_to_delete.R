#' Function that writes a (full or selective) report to a (temporary) file
#'
#' this is necessary as markdown::render takes file as an argument
#' @param report_contents the content to write
#' @param tempfile where to write the report
#' @param fix_image_width whether to set all the images to fixed width
#' @return NULL, output is a file as specified in the argument
write_report_to_file <- function(report_contents, tempfile, fix_image_width = FALSE) {
  file_conn <- file(tempfile)
  writeLines(
    report_contents,
    file_conn
  )
  close(file_conn)
  if (fix_image_width) {
    ensure_images_fit_page(tempfile, 6, "in", TRUE)
  }
  return(invisible(NULL))
}


render_rtf <- function(input_file, output_file, res_path, report_version) {
  fs <- file.size(input_file)
  rmarkdown::render(
    input = input_file,
    output_file = output_file,
    output_format = rmarkdown::rtf_document(
      toc = TRUE,
      toc_depth = 2,
      number_sections = FALSE,
      pandoc_args = c(
        paste0("--resource-path=", res_path),
        "--self-contained"
      )
    )
  )
  # I found that in some cases the rendering silently overwrites the markdown file
  # Cause unknown, maybe due to some weird blank characters instead of space?
  # Therefore added a control to throw error if the file is truncated in the process
  if (file.size(input_file) != fs) stop("Rtf rendering issue - md file invisibly truncated!")
  rtf_postprocess(output_file, report_version)
  return(invisible(NULL))
}

render_html <- function(input_file, output_file, report_version) {
  rmarkdown::render(
    input = input_file,
    output_file = output_file,
    output_format = rmarkdown::html_document(
      toc = TRUE,
      toc_float = FALSE,
      toc_depth = 2,
      number_sections = FALSE,
      self_contained = FALSE,
      fig_caption = FALSE
    )
  )
  html_postprocess(output_file, report_version)
  return(invisible(NULL))
}
