#' Create a single list of settings from simple arguments
#'
#' @param content_files List of necessary global lists with report contents
#' @param output_file Path and filenamename of report to write
#' @param md_file Path and filename of intermediate markdown file
#' @param file_format Currently either "html" or "rtf"
#' @param report_version Integer controlling the version of the code used in report generating functions
#' @param rep_type Either "inst" for institutional report or "sect" for sectoral report
#' @param inst_type Institution type (relevant for institutional report only)
#' @param report_sector_selection Input used to filter report contents
#' @param report_scenario_selection Input used to filter report contents
#' @return list of lists
#'
get_report_settings <- function(content_files,
                                output_file,
                                md_file,
                                file_format,
                                report_version,
                                rep_type,
                                inst_type,
                                report_sector_selection,
                                report_scenario_selection) {
  # translating the parameters to complete setup
  if (rep_type == "inst") {
    override_materiality <- ""
    include_exposures <- TRUE
    if (report_sector_selection == "") {
      exec_summary_layout <- 1
    } else {
      exec_summary_layout <- 2
    }
  } else {
    if (report_sector_selection == "") {
      # scenario report
      exec_summary_layout <- 3
    } else {
      # sector report
      exec_summary_layout <- 2
    }
    override_materiality <- "High"
    include_exposures <- FALSE
  }
  if (file_format == "html") {
    if (report_version >= 6) {
      output_format <- rmarkdown::html_document(
        toc = TRUE,
        toc_depth = 2,
        toc_float = list(collapsed = FALSE),
        theme = "sandstone",
        self_contained = FALSE,
        fig_caption = FALSE,
        lib_dir = "lib"
      )
    } else {
      output_format <- rmarkdown::html_document(
        toc = TRUE,
        toc_depth = 2,
        toc_float = FALSE,
        self_contained = FALSE,
        fig_caption = FALSE
      )
    }
  } else {
    www_path <- system.file("www", package = "climate.narrative")
    if (www_path == "") {
      www_path <- paste0(getwd(), "/inst/www")
    }
    output_format <- rmarkdown::rtf_document(
      toc = TRUE,
      toc_depth = 2,
      pandoc_args = c(
        paste0("--resource-path=", www_path),
        "--self-contained"
      )
    )
  }
  image_width <- 6
  image_width_unit <- "in"
  image_width_fix <- TRUE

  # hierarchical structure
  content_files <- content_files

  filter_settings <- list(
    inst_type = ifelse(rep_type == "inst", inst_type, ""),
    # sector report exception: no sector filter means no sector, not all sectors
    report_sector_selection = ifelse(rep_type == "sect" && report_sector_selection == "", "dummy", report_sector_selection),
    override_materiality = override_materiality
  )

  content_settings <- list(
    report_version = report_version,
    is_rtf = (file_format == "rtf"),
    rep_type = rep_type,
    report_scenario_selection = report_scenario_selection,
    include_exposures = include_exposures,
    exec_summary_layout = exec_summary_layout,
    rep_type = rep_type
  )
  lib_path <- system.file(
    "www/lib",
    package = "climate.narrative"
  )
  if (lib_path == "") {
    lib_path <- "inst/www/lib"
  }
  render_settings <- list(
    md_file = md_file,
    output_file = output_file,
    output_format = output_format,
    available_libs = list.files(
      system.file(
        "www/lib",
        package = "climate.narrative"
      ),
      recursive = TRUE
    )
  )

  image_settings <- list(
    image_width = 6,
    image_width_unit = "in",
    image_width_fix = TRUE
  )

  postprocess_settings <- list(
    file_format = file_format,
    output_file = output_file,
    report_version = report_version
  )

  settings <- list(
    content_files = content_files,
    filter_settings = filter_settings,
    content_settings = content_settings,
    render_settings = render_settings,
    image_settings = image_settings,
    postprocess_settings = postprocess_settings
  )

  return(settings)
}

#' The highest level function for report production. Takes only two arguments
#'
#' @param all_inputs Table of user inputs
#' @param settings list of lists containing all necessary settings
#' @return NULL, report produced to file
#'
produce_report <- function(all_inputs, settings) {
  content_files <- settings$content_files
  filter_settings <- settings$filter_settings
  content_settings <- settings$content_settings
  render_settings <- settings$render_settings
  image_settings <- settings$image_settings
  postprocess_settings <- settings$postprocess_settings
  inputs <- filter_inputs(all_inputs, filter_settings)
  report_contents <- get_report_contents_2(
    content_files,
    inputs,
    content_settings
  )
  file_conn <- file(render_settings$md_file)
  writeLines(
    report_contents,
    file_conn
  )
  close(file_conn)
  if (image_settings$image_width_fix) {
    format_images_2(render_settings$md_file, image_settings)
  }
  rmarkdown::render(
    input = render_settings$md_file,
    output_file = render_settings$output_file,
    output_format = render_settings$output_format
  )
  check_required_libraries(render_settings)
  postprocess(postprocess_settings)
  return(invisible(NULL))
}

#' Check if the files (e.g. javascript) required by the report are available in the main package
#'
#' @param render_settings the list containing available_libs (vector of files in the package)
#' and output_file (path of the rendered report, where directory "lib" is searched)
check_required_libraries <- function(render_settings) {
  rendered_libs <- list.files(
    paste0(
      dirname(render_settings$output_file),
      "/lib"
    ),
    recursive = TRUE
  )
  missing_libs <- setdiff(rendered_libs, render_settings$available_libs)
  if (length(missing_libs)) {
    warning(
      paste0(
        length(missing_libs),
        " library files are contained in the report but not in the package www/lib directory. First of the missing files is: ",
        missing_libs[1]
      )
    )
  }
  return(NULL)
}

# refactored parameter version of existing functions
filter_inputs <- function(all_inputs_table, filter_settings) {
  get_inputs(all_inputs_table, filter_settings$inst_type, filter_settings$report_sector_selection, FALSE, filter_settings$override_materiality)
}

get_report_contents_2 <- function(content_files, inputs, content_settings) {
  if (content_settings$rep_type %in% c("inst", "sect")) {
    return(
      get_report_contents(
        content_files$tabs,
        content_files$scenarios,
        content_files$sections,
        content_files$exposure_classes,
        inputs,
        content_settings$report_version,
        content_settings$report_scenario_selection,
        content_settings$is_rtf,
        content_settings$exec_summary_layout,
        content_settings$include_exposures,
        content_settings$rep_type
      )
    )
  } else { # test all sector report
    return(
      get_test_report(
        content_files$exposure_classes
      )
    )
  }
}

format_images_2 <- function(md_file, image_settings) {
  format_images(md_file, image_settings$image_width, image_settings$image_width_unit, image_settings$image_width_fix)
}

postprocess <- function(postprocess_settings) {
  if (postprocess_settings$file_format == "rtf") {
    rtf_postprocess(postprocess_settings$output_file, postprocess_settings$report_version)
  } else {
    html_postprocess(postprocess_settings$output_file, postprocess_settings$report_version)
  }
}
