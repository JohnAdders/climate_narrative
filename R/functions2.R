# refactor - top down

produce_report <- function(all_inputs, settings){ 
  report_version <- settings$report_version
  md_file <- settings$md_file
  output_file <- settings$output_file
  toc_output_file <- settings$toc_output_file # global$sidebar_toc (HTML only)
  output_format <- settings$output_format # a full rmarkdown specs?
  institution_type <- settings$institution_type
  report_type <- settings$report_type
  sector_selection <- settings$sector_selection
  scenario_selection <- settings$scenario_selection
  exec_summary_layout <- settings$exec_summary_layout
  include_exposures <- settings$include_exposures
  
  # TODO: check the function parameters below
  inputs <- get_inputs(all_inputs, input$inst_type, input$report_sector_selection, FALSE)

  # get_report_contents
  aggregated_inputs <- aggregate_inputs(inputs)
  scenario_no <- get_scenario_no(report_scenario_selection, is_rtf)
  section_no <- get_section_no(is_rtf)
  out <- list()
  for (scenario in global$scenarios[scenario_no]) {
    out <- c(
      out,
      list(get_scenario_descriptions(
        aggregated_inputs,
        inputs,
        scenario,
        include_exposures
      ))
    )
  }
  for (section in global$sections[section_no]){
    out <- c(
        out,
        list(get_section_descriptions(
          section,
          list(
            report_version=report_version,
            aggregated_inputs=aggregated_inputs,
            inputs=inputs,
            scenario_no=scenario_no,
            exec_summary_layout=exec_summary_layout
          )
        ))
      )
  }
  # order the scenario and non-scenario sections
  scenario_pos <- sapply(global$scenarios, function(sce) sce$position)[scenario_no]
  section_pos <- sapply(global$sections, function(s) s$position)[section_no]
  out <- out[order(c(scenario_pos, section_pos))]
  out <- add_path_to_graphs(out)
  return(out)

  # write_report_to_file
  file_conn <- file(tempfile)
  writeLines(
    report_contents,
    file_conn
  )
  close(file_conn)
  if (fix_image_width){
    ensure_images_fit_page(tempfile, 6, "in", TRUE)
  }

  # render_html/render_rtf
  rmarkdown::render( #html
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
    rmarkdown::render( #rtf
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
  # html postprocess below
    html_postprocess()
    rtf_postprocess(input_file, global$report_version)
}


html_postprocess <- function(){
  # replace back the images links
  file_conn <- file(output_file)
  temp <- readLines(file_conn)
  temp <- gsub(
    system.file("www", package = "climate.narrative"),
    "climate_narrative",
    temp
  )
  if (global$report_version >= 2){
    temp <- gsub(
      "(<h[1-5]?>)(.*)(</h[1-5]?>)",
      "<div class=\"inline\"> \\1\\2\\3 <a href='#top'>&uarr;</a> </div>",
      temp,
      perl=TRUE
    )    
  }
  # extract the table of contents
  if (global$sidebar_toc){
    toc_start <- grep("<div id=\"TOC\">", temp)
    div_end <- grep("</div>", temp)
    toc_end <- min(div_end[div_end > toc_start])
    toc <- temp[toc_start:toc_end]
    output$html_report_nav <- renderUI(HTML(toc))
    temp <- temp[-(toc_start:toc_end)]
  }
  writeLines(
    temp,
    file_conn
  )
  close(file_conn)
}