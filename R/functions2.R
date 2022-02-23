get_report_settings <- function(
  output_file,
  file_format,
  report_version,
  sidebar_toc, 
  rep_type,
  inst_type, 
  report_sector_selection,
  report_scenario_selection
){
  # hierarchical structure?
  settings <- list(
    tabs=global$tabs,
    scenarios=global$scenarios,
    sections=global$sections,
    exposure_classes=global$exposure_classes,
    file_format=file_format,
    report_version=report_version,
    sidebar_toc=sidebar_toc,
    rep_type=rep_type,
    inst_type=inst_type, 
    report_sector_selection=report_sector_selection,
    report_scenario_selection=report_scenario_selection
  )
  if (rep_type == "inst"){
    #inputs <- get_inputs(all_inputs(), input$inst_type, input$report_sector_selection)
    settings$override_materiality <- ""
    settings$include_exposures <- TRUE
    if (report_sector_selection == "") {
      settings$exec_summary_layout <- 1
    } else {
      settings$exec_summary_layout <- 2
    }
  } else {
    settings$exec_summary_layout <- 2
    #inputs <- get_inputs(all_inputs(), "", input$report_sector_selection, FALSE, "High")
    settings$override_materiality <- "High"
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
        paste0("--resource-path=", system.file("www", package = "climate.narrative")),
        "--self-contained"
      )
    )
  }
  settings$image_width <- 6
  settings$image_width_unit <- "in"
  settings$image_width_fix <- TRUE
  settings$md_file <- tempfile(fileext=".md")
  settings$output_file <- output_file
  settings$exposure_classes_names <- sapply(global$exposure_classes, `[[`, i = "name")
  return(settings)
}

produce_report <- function(all_inputs, settings){ 
  tabs <- settings$tabs
  scenarios <- settings$scenarios
  sections <- settings$sections
  exposure_classes <- settings$exposure_classes
  report_version <- settings$report_version
  sidebar_toc <- settings$sidebar_toc
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
  image_width <- settings$image_width
  image_width_unit <- settings$image_width_unit
  image_width_fix <- settings$image_width_fix
  exposure_classes_names <- settings$exposure_classes_names
  override_materiality <- settings$override_materiality
  inputs <- get_inputs(exposure_classes_names, all_inputs, inst_type, report_sector_selection, FALSE, override_materiality)
  report_contents <- get_report_contents(
    tabs,
    scenarios,
    sections,
    exposure_classes,
    inputs,
    report_version,
    report_scenario_selection,
    is_rtf,
    exec_summary_layout,
    include_exposures
  )
  file_conn <- file(md_file)
  writeLines(
    report_contents,
    file_conn
  )
  close(file_conn)
  if (image_width_fix){
    ensure_images_fit_page(md_file, image_width, image_width_unit, image_width_fix)
  }
  rmarkdown::render(
    input = md_file,
    output_file = output_file,
    output_format = output_format
  )
  if (is_rtf){
    rtf_postprocess(output_file, report_version)
  } else {
    html_postprocess(output_file, report_version, sidebar_toc)
  }
  return(invisible(NULL))
}