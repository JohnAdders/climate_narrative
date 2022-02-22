# top down refactor
get_report_settings <- function(file_format, report_version, rep_type, inst_type, 
  report_sector_selection, report_scenario_selection, override_materiality
){
  settings <- list(
    file_format=file_format,
    report_version=report_version,
    rep_type=rep_type,
    inst_type=inst_type, 
    report_sector_selection=report_sector_selection,
    report_scenario_selection=report_scenario_selection,
    override_materiality=override_materiality
  )
  if (rep_type == "inst"){
    #inputs <- get_inputs(all_inputs(), input$inst_type, input$report_sector_selection)
    settings$include_exposures <- TRUE
    if (report_sector_selection == "") {
      settings$exec_summary_layout <- 1
    } else {
      settings$exec_summary_layout <- 2
    }
  } else {
    settings$exec_summary_layout <- 2
    #inputs <- get_inputs(all_inputs(), "", input$report_sector_selection, FALSE, "High")
    settings$include_exposures <- FALSE
  }
  if (file_format == "html"){
    settings$output_format <- rmarkdown::html_document(
      toc = TRUE,
      toc_float = FALSE,
      toc_depth = 2,
      number_sections = FALSE,
      self_contained = FALSE,
      fig_caption = FALSE
    )
  } else {
    settings$output_format <- rmarkdown::rtf_document(
      toc = TRUE,
      toc_depth = 2,
      number_sections = FALSE,
      pandoc_args = c(
        paste0("--resource-path=", res_path),
        "--self-contained"
      )
    )
  }
  settings$fix_image_width <- TRUE
  settings$md_file <- tempfile(fileext=".md")
  settings$output_file <- "C:/Users/kopalski/Desktop/temp/temp.html"
  settings$exposure_classes_names <- names(global$exposure_classes)
  return(settings)
}

produce_report <- function(all_inputs, settings){ 
  report_version <- settings$report_version
  md_file <- settings$md_file
  output_file <- settings$output_file
  #toc_output_file <- settings$toc_output_file # global$sidebar_toc (HTML only)
  output_format <- settings$output_format # a full rmarkdown specs
  file_format <- output_format$pandoc$to
  is_rtf <- (file_format == "rtf")
  inst_type <- settings$inst_type
  rep_type <- settings$rep_type
  report_sector_selection <- settings$report_sector_selection
  report_scenario_selection <- settings$report_scenario_selection
  exec_summary_layout <- settings$exec_summary_layout
  include_exposures <- settings$include_exposures
  fix_image_width <- settings$fix_image_width
  exposure_classes_names <- settings$exposure_classes_names
  override_materiality <- settings$override_materiality
  print("settings read")
  inputs <- get_inputs(exposure_classes_names, all_inputs, inst_type, report_sector_selection, override_materiality)
  print("inputs produced")

  report_contents <- get_report_contents(
    global$tabs,
    global$scenarios,
    global$sections,
    global$exposure_classes,
    inputs,
    report_version,
    report_scenario_selection,
    is_rtf,
    exec_summary_layout,
    include_exposures
  )
  print("contents produced")
  file_conn <- file(md_file)
  writeLines(
    report_contents,
    file_conn
  )
  close(file_conn)
  print("contents saved to file")
  if (fix_image_width){
    ensure_images_fit_page(md_file, 6, TRUE)
  }
  print("images fixed")
  rmarkdown::render(
    input = md_file,
    output_file = output_file,
    output_format = output_format
  )
  print("file rendered to file")
  if (is_rtf){
    rtf_postprocess(output_file, global$report_version)
  } else {
    html_postprocess(output_file, global$report_version)
  }
  print("file post-processed")
  return(invisible(NULL))
}