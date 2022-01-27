#' Remove special characters (in particular spaces) from a string
#' and optionally make it camelcase.
#'
#' @param text Text to process
#' @param make_camelcase Should we make text camel case
#'
remove_special_characters <- function(text, make_camelcase = TRUE) {
  out <- text
  if (make_camelcase) {
    out <- gsub("\\ (\\w?)", "\\U\\1", tolower(out), perl = TRUE)
    out <- gsub("\\_(\\w?)", "\\U\\1", out, perl = TRUE)
    out <- gsub("\\.(\\w?)", "\\U\\1", out, perl = TRUE)
  }
  gsub("[_. ]", "", out)
}

#' Read all files from a directory as a named R list.
#' Handles yaml/csv/R files.
#'
#' @param directory Directory to process
#' @param file_format File format to interpret files as. One of yml, csv, r or auto
#' @param in_package Is the directory in a package
#' @param remove_special_characters_from_names Should we remove special characters
#'
#' @importFrom utils read.csv
#' @importFrom yaml read_yaml
#'
read_dir <- function(directory, file_format = "auto", in_package = TRUE, remove_special_characters_from_names = TRUE) {
  if (in_package) directory <- system.file(directory, package = "climate.narrative")
  file_list <- dir(path = directory)
  file_format <- tolower(file_format)
  if (file_format == "auto") {
    file_format <- tolower(strsplit(file_list[1], ".", fixed = T)[[1]][2])
  }
  list <- lapply(
    file_list,
    function(file) {
      switch(file_format,
        yml = read_yaml(paste0(directory, "/", file)),
        csv = read.csv(paste0(directory, "/", file), stringsAsFactors = FALSE),
        r = source(paste0(directory, "/", file)),
        stop("Error (function read_dir): file format ", file_format, " not handled")
      )
    }
  )
  names_to_be <- sapply(
    dir(path = directory),
    function(file) {
      strsplit(file, ".", fixed = T)[[1]][1]
    }
  )
  if (remove_special_characters_from_names) names_to_be <- remove_special_characters(names_to_be)
  names(list) <- names_to_be
  return(list)
}

#' Helper function to enable using names to refer tabs instead of numbers
#' 
#' @param tab_name name of the tab to convert
tab_name_to_number <- function(tab_name){
  as.integer(factor(tab_name, global$ordered_tabs))
}


#' Add element to the list (shortcut)
#'
#' @param previous_list List to add items to
#' @param item_to_add Item to add to list
#'
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

#' Make the first letter of a string upper case
#' 
#' @param string the string to convert
#' 
capitalize <- function(string) {
  return(paste0(toupper(substring(string, 1, 1)), substring(string, 2)))
}

#' Format (camelcase) string in order to look better in final output (spaces, capitalisation)
#' 
#' @param camelcase the string to convert
#' 
restore_spaces <- function(camelcase) {
  s <- gsub("([A-Z])([a-z])", " \\1\\L\\2", camelcase, perl = TRUE)
  s <- sub("^ ", "", s) # remove first space
  # ensure no space after opening parenthesis "("
  s <- gsub("( ", "(", s, fixed = TRUE)
  s <- capitalize(s)
  # manually substitute texts with where simple capitalisation rule fails
  substitutions <- data.frame(
    from = c("Sme", "Smes", "Uk", "Us", "And", "To", "To(non-sme)", "to(non-sme)"),
    to = c("SME", "SMEs", "UK", "US", "and", "to", "to (non-SME)", "to (non-SME)")
  )
  for (i in 1:nrow(substitutions)) {
    s <- gsub(substitutions$from[i], substitutions$to[i], s, fixed = TRUE)
  }
  s
}

#' Produce a matrix of tooltips (strings) by concatenating column-specific (if any)
#' and product-specific text (if any)
#' @param exposure_matrix the matrix of exposures in the specific format
#' Its first column is name, second is product, the others contain exposure names or blank cells
#' 
produce_tooltip_matrix <- function(exposure_matrix) {
  out <- matrix(
    "",
    nrow = nrow(exposure_matrix),
    ncol = ncol(exposure_matrix) - 2
  )
  for (i in 1:nrow(out)) {
    row_tooltip <- global$products[[remove_special_characters(exposure_matrix[i, 2])]][["tooltip"]]
    for (j in 1:ncol(out)) {
      exposure_class <- exposure_matrix[i, j + 2]
      if (exposure_class != "") {
        exposure_class_tooltip <- global$exposure_classes[[exposure_class]][["tooltip"]]
        if (!is.null(exposure_class_tooltip)) {
          if (!is.null(row_tooltip)) {
            out[i, j] <- paste0(row_tooltip, "<br>", exposure_class_tooltip)
          } else {
            out[i, j] <- exposure_class_tooltip
          }
        } else {
          if (!is.null(row_tooltip)) {
            out[i, j] <- row_tooltip
          }
        }
      }
    }
  }
  out
}

#' Produce the layout of questionnaire tabs (cell, row, whole table)
#'
#' @param exposure_item Name of exposure item or blank for empty
#' @param prefix Prefix to apply to name
#' @param tooltip_text Tooltip text to show
#' @param dev Are we in development mode
#' @param width Width of dropdown
#'
#' @importFrom tippy tippy_this
#'
exposure_grid_cell <- function(exposure_item, prefix, tooltip_text = "", dev = FALSE, width = NULL) {
  if (exposure_item == "") {
    form <- p("")
  } else {
    id <- paste(prefix, remove_special_characters(exposure_item), sep = "_")
    form <- selectInput(
      inputId = id,
      label = NULL,
      choices = c("", "Low", "Medium", "High"),
      selected = ifelse(dev, "High", ""),
      # to allow empty string as a valid option I do not use selectize
      selectize = FALSE,
      width = width
    )
    if (tooltip_text != "") {
      return(div(
        form,
        tippy_this(id, tooltip_text),
      ))
    } else {
      return(form)
    }
  }
}

#' Grid table of inputs (everything happens in matching server function)
#'
#' @param label Label for UI component
#'
exposure_grid_ui <- function(label) {
  tableOutput(label)
}

#' Produce a table of inputs with selectInput fields
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param exposure_matrix Exposure matrix to show
#' @param tooltip_matrix Matrix of tooltips
#' @param label Label for grid
#' @param dev Are we in developement mode
#' @param width Width of each dropdown
#'
exposure_grid_server <- function(input,
                                 output,
                                 exposure_matrix,
                                 tooltip_matrix,
                                 label,
                                 dev = FALSE,
                                 width = NULL) {
  layout <- matrix("", nrow = nrow(exposure_matrix), ncol = ncol(exposure_matrix) - 1)
  colnames(layout) <- colnames(exposure_matrix)[-(2)]
  for (i in 1:nrow(layout)) {
    layout[i, 1] <- as.character(div(exposure_matrix[i, 1], class = "verticalcenter"))
    for (j in 2:ncol(layout)) {
      layout[i, j] <- as.character(
        exposure_grid_cell(
          exposure_matrix[i, j + 1],
          paste(
            label,
            remove_special_characters(exposure_matrix[i, 1]),
            remove_special_characters(exposure_matrix[i, 2]),
            remove_special_characters(colnames(exposure_matrix)[j + 1]),
            sep = "_"
          ),
          tooltip_matrix[i, j - 1],
          dev,
          width
        )
      )
    }
  }
  output[[label]] <- renderTable(
    layout,
    sanitize.text.function = function(x) x,
    sanitize.colnames.function = function(x) gsub(".", " ", x, fixed = TRUE),
    align = "c"
  )
}

#' Insert spaces to the string so that line has exactly given number of characters.
#'
#' @param string String to break
#' @param line_width Width of line
#' @param location starting column
#' @param n_char Number of characters
#'
string_break_line_with_spaces <- function(string, line_width, location, n_char = 1) {
  paste0(
    substring(string, 1, location - 1),
    paste(rep(" ", (1 - location) %% line_width), collapse = ""),
    substring(string, location + n_char)
  )
}

#' Add spaces to a string so that it can be split into blocks of exactly the same length
#' without breaking words.
#'
#' @param string String to break
#' @param line_width Width of a line
#'
#' @importFrom stats na.omit
#' @importFrom stringi stri_locate_all
#'
string_add_spaces_to_make_equal_lines <- function(string, line_width) {
  out <- string
  newline_locations <- na.omit(stringi::stri_locate_all(out, fixed = "<br>")[[1]][, 1])
  i <- 1
  while (i * line_width < nchar(out)) {
    for (loc in newline_locations[newline_locations <= 1 + i * line_width]) {
      out <- string_break_line_with_spaces(out, line_width, loc, 4)
    }
    space_locations <- stringi::stri_locate_all(out, fixed = " ")[[1]][, 1]
    last_space <- na.omit(max(space_locations[space_locations <= 1 + line_width * i]))
    if (length(last_space)) out <- string_break_line_with_spaces(out, line_width, last_space, 1)
    newline_locations <- na.omit(stringi::stri_locate_all(out, fixed = "<br>")[[1]][, 1])
    i <- i + 1
  }
  return(out)
}

#' Format the string by appending spaces so it exactly fills the lines of given length.
#'
#' Additionally, if the string contains at least one "br" tag
#' format output as a bulleted list.
#'
#' @param string String to Format
#' @param col_width Width of column
#'
string_format_lines <- function(string, col_width) {
  if (grepl("<br>", string)) {
    out <- paste0("- ", gsub("<br>", "<br> - ", string))
  } else {
    out <- string
  }
  out <- string_add_spaces_to_make_equal_lines(out, col_width)
  return(out)
}

#' Produce a markdown table out of R data frame.
#'
#' Create a markdown table, allowing multiline cell entries (lines need to be separated by br tag)

#' The function splits the text automatically and adds spaces to match the desired column width
#' without breaking words.
#'
#' @param table R data frame with data to display
#' @param dot_to_space Convert dots in headings to spaces
#' @param col_widths Width of columns
#'
table_to_markdown_multiline <- function(table, dot_to_space = TRUE, col_widths = NULL) {
  headers <- colnames(table)
  if (is.null(col_widths)) {
    col_widths <- pmax(apply(table, 2, function(x) max(nchar(x))), nchar(headers)) + 4
  }
  if (dot_to_space) {
    headers <- gsub(".", " ", headers, fixed = TRUE)
  }
  for (i in 1:ncol(table)) {
    table[, i] <- as.character(table[, i])
    table[, i] <- gsub("\n", " ", table[, i])
    for (j in 1:nrow(table)) {
      table[j, i] <- string_format_lines(table[j, i], col_widths[i] - 2)
    }
  }
  split_rows <- ceiling(c(
    max(nchar(headers) / (col_widths - 2)),
    apply(table, 1, function(x) max(nchar(x) / (col_widths - 2)))
  ))
  out <- matrix("", nrow = 0, ncol = ncol(table))
  emptyline <- rep("", ncol(table))
  sepline <- emptyline
  for (i in 1:length(sepline)) {
    sepline[i] <- paste0(paste(rep("-", col_widths[i]), collapse = ""), "+")
  }
  sepline[1] <- paste0("+", sepline[1])
  rowsout <- matrix(emptyline, nrow = split_rows[1], ncol = length(emptyline), byrow = F)
  for (i in 1:ncol(out)) {
    cell_text <- string_format_lines(headers[i], col_widths[i] - 2)
    cell_text <- paste0(
      cell_text,
      paste(rep(" ", (col_widths[i] - 2) * split_rows[1] - nchar(cell_text)), collapse = "")
    )
    for (k in 1:split_rows[1]) {
      rowsout[k, i] <- paste0(
        " ",
        substr(cell_text, 1 + (k - 1) * (col_widths[i] - 2), k * (col_widths[i] - 2)),
        " |"
      )
      if (i == 1) rowsout[k, i] <- paste0("|", rowsout[k, i])
    }
  }
  out <- rbind(out, rowsout, gsub("-", "=", sepline))
  for (j in 1:nrow(table)) {
    rowsout <- matrix(emptyline, nrow = split_rows[j + 1], ncol = length(emptyline), byrow = F)
    for (i in 1:ncol(out)) {
      cell_text <- table[j, i]
      temp2 <- paste0(
        cell_text,
        paste(rep(" ", (col_widths[i] - 2) * split_rows[j + 1] - nchar(cell_text)), collapse = "")
      )
      for (k in 1:split_rows[j + 1]) {
        rowsout[k, i] <- paste0(
          " ",
          substr(temp2, 1 + (k - 1) * (col_widths[i] - 2), k * (col_widths[i] - 2)),
          " |"
        )
        if (i == 1) rowsout[k, i] <- paste0("|", rowsout[k, i])
      }
    }
    out <- rbind(out, rowsout, sepline)
  }

  out2 <- paste0(
    paste(sepline, collapse = ""),
    "\n",
    paste(apply(out, 1, paste, collapse = ""), collapse = "\n"),
    "\n"
  )
  return(out2)
}

#' A simple version of function to produce markdown tables from R table
#'
#' It does not handle multiline cells.
#'
#' @param table R data frame
#' @param additional_spaces Additional space to add
#' @param dot_to_space Convert dots in titles to spaces
#'
table_to_markdown <- function(table, additional_spaces = 3, dot_to_space = TRUE) {
  headers <- colnames(table)
  if (dot_to_space) {
    headers <- gsub(".", " ", headers, fixed = TRUE)
  }
  collapsor <- paste0(
    paste(
      rep("&nbsp;", additional_spaces),
      collapse = ""
    ),
    " | "
  )
  out <- paste(headers, collapse = collapsor)
  out <- paste0(
    out,
    "\n",
    paste(
      rep("---", ncol(table)),
      collapse = " | "
    ),
    "\n"
  )
  if (nrow(table)) {
    for (i in 1:nrow(table)) {
      out <- paste0(
        out,
        gsub("\n", " ", paste(table[i, ], collapse = collapsor)),
        "\n"
      )
    }
  }
  out <- paste0(out, "\n\n")
  return(out)
}

#' Produce report content for a given item
#'
#' @param item name of item for which a report is to be produced
#' @param type_item_inputs table of (disaggregated) inputs to produce a table of contributing rows
#' @return markdown-formatted report section (h2)
#'
#' @importFrom stats aggregate
#'
get_exposure_description <- function(item, type_item_inputs) {
  if (is.null(global$exposure_classes[[item]])) warning(paste("No exposure class file for ", item))
  ordered_type_item_inputs <- type_item_inputs[order(type_item_inputs$materiality), ]
  # conversion from factor back to string to ensure proper printing below
  ordered_type_item_inputs$materiality <- as.character(ordered_type_item_inputs$materiality)
  # add unique identifier if rownames are not unique (i.e. the same item in multiple columns)
  ordered_type_item_inputs$rowname_unique <- ordered_type_item_inputs$rowname
  duplicates <- which(duplicated(ordered_type_item_inputs$rowname) | duplicated(ordered_type_item_inputs$rowname, fromLast = TRUE))
  for (i in duplicates) {
    ordered_type_item_inputs$rowname_unique[i] <- paste0(
      ordered_type_item_inputs$rowname[i],
      " (",
      capitalize(ordered_type_item_inputs$colname[i]),
      ")"
    )
  }
  ordered_aggregate_inputs_text <- stats::aggregate(
    ordered_type_item_inputs[, c("rowname_unique", "materiality")],
    by = list(
      Product.description = ordered_type_item_inputs$product_description,
      Product.text = ordered_type_item_inputs$product_text
    ),
    FUN = function(texts) {
      paste(
        restore_spaces(texts),
        collapse = "<br>"
      )
    }
  )
  ordered_aggregate_inputs_num <- stats::aggregate(
    ordered_type_item_inputs[, c("materiality_num")],
    by = list(
      Product.description = ordered_type_item_inputs$product_description,
      Product.text = ordered_type_item_inputs$product_text
    ),
    FUN = function(x) {
      cut(
        sum(x),
        breaks = c(0, 4.5, 9.5, 100),
        labels = c("Low", "Medium", "High")
      )
    }
  )
  ordered_aggregate_inputs <- merge(ordered_aggregate_inputs_text, ordered_aggregate_inputs_num)
  colnames(ordered_aggregate_inputs)[3:5] <- c("Exposure.row", "Materiality", "Product materiality")
  out <- paste0(
    "## ",
    global$exposure_classes[[item]][["name"]],
    "\n\n",
    global$exposure_classes[[item]][["description"]],
    "\n\nThe following rows contribute: \n\n",
    table_to_markdown_multiline(ordered_aggregate_inputs[, 1:4], TRUE, c(15, 30, 25, 15)),
    "\n\n"
  )
}

#' Produce appendix for a given item
#'
#' @param item name of item for which appendix is to be produced
#' @return markdown-formatted appendix section (h3)
#'
get_exposure_appendix <- function(item) {
  appendix <- global$exposure_classes[[item]][["appendix"]]
  if (is.null(appendix)) {
    return(c())
  } else {
    return(
      paste0(
        "### Appendix",
        "\n\n",
        global$exposure_classes[[item]][["appendix"]],
        "\n\n"
      )
    )
  }
}

#' Lower level report helper function responsible for single risk
#' (transition/physical) description.
#'
#' @param item Exposure class name
#' @param products Product name
#' @param materiality Materiality of item
#' @param physical_or_transition Type of scenario
#' @param high_or_low Is scenario high or low
#' 
get_exposure_risk_description <- function(item, products, materiality, physical_or_transition, high_or_low) {
  if (high_or_low == FALSE) {
    return("")
  }

  # define header depending on physical/transition and low/high
  if (physical_or_transition == "transition") {
    riskname <- switch(high_or_low,
      high = "Disorderly transition",
      low = "Orderly transition"
    )
  } else {
    riskname <- switch(high_or_low,
      high = "High physical risk",
      low = "Low physical risk"
    )
  }
  header_text <- paste0(
    global$exposure_classes[[item]][["name"]],
    " --- ",
    riskname
  )
  out <- paste0(
    "### ",
    capitalize(header_text),
    " --- Summary\n\n",
    global$exposure_classes[[item]][[physical_or_transition]][[high_or_low]][["always"]],
    "\n\n"
  )
  if (materiality == "High") {
    out <- paste0(
      out,
      "### ",
      capitalize(header_text),
      " --- Details\n\n",
      global$exposure_classes[[item]][[physical_or_transition]][[high_or_low]][["high_materiality"]],
      "\n\n"
    )
  }
  for (product in products) {
    out <- paste0(
      out,
      global$exposure_classes[[item]][[physical_or_transition]][[high_or_low]][[product]],
      "\n\n"
    )
  }
  return(out)
}

#' Lower level report helper function responsible for single scenario description
#'
#' @param aggregated_table Table of all possible items
#' @param type_inputs Drop box items
#' @param scenario Scenario name
#'
get_scenario_descriptions <- function(aggregated_table, type_inputs, scenario) {
  if (is.null(scenario)) warning(paste("No scenario file for ", scenario))
  name <- scenario$name
  description <- scenario$description
  is_scenario <- scenario$is_scenario
  transition <- scenario$transition
  physical <- scenario$physical
  out <- ""
  if (!is.null(name)) out <- paste0("# ", name, "\n\n")
  if (!is.null(description)) out <- paste0(out, description, "\n\n")
  if (nrow(aggregated_table) & is_scenario) {
    for (i in 1:nrow(aggregated_table)) {
      item <- aggregated_table$item[i]
      materiality <- aggregated_table$materiality_num[i]
      type_item_inputs <- type_inputs[type_inputs$item == item, ]
      products <- unique(type_item_inputs$product)
      out <- paste0(
        out,
        get_exposure_description(item, type_item_inputs),
        get_exposure_risk_description(item, products, materiality, "transition", transition),
        get_exposure_risk_description(item, products, materiality, "physical", physical),
        get_exposure_appendix(item)
      )
    }
  }
  return(out)
}

#' Function to produce references section (for all items)
#'
#' @param aggregated_table aggregated inputs
#' @param type_inputs disaggregated inputs
#' @return markdown-formatted references section (h2)
#'
get_references <- function(aggregated_table, type_inputs) {
  out <- ""
  if (nrow(aggregated_table)) {
    out <- paste0(
      out,
      "# References\n\n"
    )
    for (i in 1:nrow(aggregated_table)) {
      item <- aggregated_table$item[i]
      if (length(global$exposure_classes[[item]][["references"]])) {
        out <- paste0(
          out,
          "\n\n## ",
          global$exposure_classes[[item]][["name"]],
          "\n\n",
          global$exposure_classes[[item]][["references"]]
        )
      }
    }
  }
  # Do not show the section if there are no references
  if (out == "# References\n\n") {
    out <- ""
  }
  return(out)
}

#' Heartbeat function (server part) to prevent app closing due to inactivity
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session  Shiny session
#'
heartbeat <- function(input, output, session) {
  beep <- reactiveTimer(55 * 1000)
  output[["__heartbeat"]] <- renderText({
    beep()
    " "
  })
}

#' Heartbeat function (ui part) to prevent app closing due to inactivity
#'
heartbeat_footer <- function() {
  list(
    hr(),
    tag(
      "footer",
      list(
        p("Copyright 2021 The Climate Financial Risk Forum"),
        p(
          a(href = "https://github.com/JohnAdders/climate_narrative", "Source Code", target = "_blank"),
          " | ",
          a(href = "mailto:john.adcock@aviva.com?subject=Climate%20Narrative%20support%20request", "Beta Support"),
          " | ",
          a(href = "https://github.com/JohnAdders/climate_narrative/issues?q=is%3Aissue+is%3Aopen+label%3Abug", "Known Issues", target = "_blank"),
          " | ",
          a(href = "https://github.com/JohnAdders/climate_narrative/wiki/Contributors", "Contributors", target = "_blank")
        )
      )
    )
  )
}

#' Google captcha function, UI part.
#'
#' based on
#' https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R
#'
#' @param site_key Site key from google
#' 
recaptcha_ui <- function(site_key) {
  tagList(tags$head(
    tags$script(src = paste0("https://www.google.com/recaptcha/api.js?render=", site_key)),
  ))
}

#' Google captcha function, actual javascript call.
#'
#' Based on
#' https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R
#'
#' @param site_key, Site key from google
#' @param action Name of action to test
#' @param field_id Field to trigger
#'
#' @importFrom shinyjs runjs
#' 
recaptcha_js <- function(site_key, action, field_id) {
  runjs(paste0("
        grecaptcha.ready(function () {
          grecaptcha.execute('", site_key, "', { action: '", action, "' }).then(function (token) {
            Shiny.onInputChange('", field_id, "',token);
          });
        });
      "))
}

#' Google captcha function, server part.
#'
#' Based on
#' https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R
#'
#' @param secret_key Secret key from google
#' @param recaptcha_response Response from google
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' 
recaptcha_server <- function(secret_key, recaptcha_response) {
  response <- httr::POST(
    "https://www.google.com/recaptcha/api/siteverify",
    body = list(
      secret = secret_key,
      response = recaptcha_response
    )
  )

  if (response$status_code == 200) {
    return(jsonlite::fromJSON(httr::content(response, "text")))
  }
}

#' Produce a footer HTML text explaining materiality levels.
#'
#' @param asset_or_liability A string with the type of exposure page
#' @param is_asset_mananger Is this display for an asset manager
#'
generic_footer <- function(asset_or_liability, is_asset_mananger = FALSE) {
  if (asset_or_liability == "asset") {
    case_name <- "asset class and sector"
    if (is_asset_mananger) {
      total_name <- "assets under management"
    } else {
      total_name <- "assets"
    }
  } else {
    case_name <- "liability class"
    total_name <- "premium income"
  }
  p(
    list(
      paste0("Enter your firm's exposures by ", case_name, " using the following definitions:"),
      tags$ul(
        tags$li(paste("\"High\": One of your top 5 exposures or more than 10% of total", total_name)),
        tags$li(paste("\"Medium\": 5% - 10% of total", total_name)),
        tags$li(paste("\"Low\": below 5% of total", total_name)),
        tags$li(paste("blank: immaterial or no exposure"))
      )
    )
  )
}

# The functions below are writing a report to (temporary) file first
# this is necessary as markdown::render takes file as an argument
# there are 3 versions of the report, in each separate temp file:
#    full report for email RTF,
#    single scenario for HTML
#    single scenario with common sectors (e.g. introduction) for button RTF

#' Function that writes a full report to (temporary) file
#'
#' this is necessary as markdown::render takes file as an argument
#' not used at the moment, but do not delete - will be sent by email probably
#' @param report_contents the content to write
#' @param report_version variable that can be used to control the multiple report versions in a single code
#' @param tempfile where to write the report
#' @return NULL, output is a file as specified in the argument
produce_full_report <- function(report_contents, report_version, tempfile){
  file_conn <- file(tempfile)
  writeLines(unlist(report_contents), file_conn)
  close(file_conn)
  return(invisible(NULL))
}
  
#' Function that writes a full report to (temporary) file
#'
#' this is necessary as markdown::render takes file as an argument
#' @param report_contents the content to write
#' @param report_version variable that can be used to control the multiple report versions in a single code
#' @param report_scenario_selection (user-friendly) scenario name (or empty string)
#' @param is_rtf a flag that triggers several format specific settings:
#' - TRUE: include non-scenario sections (e.g. intro)
#' - FALSE: include the links to page top (note requires proper report_version as well)
#' @param tempfile where to write the report
#' @return NULL, output is a file as specified in the argument
produce_selective_report <- function(report_contents, report_version, report_scenario_selection, is_rtf, tempfile){
  if (report_scenario_selection == ""){
    scenario_no <- which(sapply(global$scenarios, function(sce) !is.null(sce$name))) 
  } else {
    scenario_no <- which(sapply(global$scenarios, `[[`, i = "name") == report_scenario_selection)
  }
  if (is_rtf){
    scenario_no <- sort(
      c(scenario_no, which(sapply(global$scenarios, function(sce) !sce$is_scenario)))
    )
  }
  file_conn <- file(tempfile)
  # offset is for the title (and optionally exec summary), not included in 'scenarios' but included in 'report_contents'
  if (report_version >= 3) {
    contents <- report_contents[c(2, 2 + scenario_no, length(report_contents))]
    contents[[1]] <- contents[[1]][c(1, 2, 1 + scenario_no, length(contents[[1]]))]
    contents[[1]] <- paste(contents[[1]], collapse="\n")
  } else {
    contents <- report_contents[c(1 + scenario_no, length(report_contents))]
  }
  contents <- add_path_to_graphs(contents)
  if (!is_rtf && report_version >= 2){
    for (header_tag in c("\n# ","\n## ","\n### ")){
      contents = gsub(
        header_tag,
        paste0("\n<a href='#top'>go to top</a> \n\n", header_tag),
        contents,
      )
    }
    # also add link at the end
    contents = paste0(contents,"\n\n<a href='#top'>go to top</a>\n")
  }
  writeLines(
    contents,
    file_conn
  )
  close(file_conn)
  return(invisible(NULL))
}

#' Go through the markdown file, find all png images and scale down where relevant
#' 
#' @param filename the path to file to convert
#' @param max_width_inch maximum width of image in inches (wider will be scaled down to this value)
#' default is 7 inches which roughly matches vertical A4 page with margins
#' @return NULL, changes file specified as an argument in place
#' @importFrom stringi stri_match_first
#' 
ensure_images_fit_page <- function(filename, max_width_inch=7){
  file_conn <- file(filename)
  markdown <- readLines(file_conn)
  graph_lines <- grep("^!\\[",markdown) 
  for (i in graph_lines){
    image_name <- substring(stringi::stri_match_first(markdown[i], regex="\\([[:graph:]]*.png"),2)
    image_attributes <- attributes(png::readPNG(paste0(system.file("www", package = "climate.narrative"), "/", image_name), info=TRUE))$info
    if (is.null(image_attributes$dpi)) image_attributes$dpi <- c(96, 96)
    if (image_attributes$dim[1]/image_attributes$dpi[1] > max_width_inch){
      print(paste0("image ", image_name, " has width > ", max_width_inch, "inch, resizing"))
      markdown[i] <- paste0(markdown[i], "{ width=", max_width_inch,"in }")
    }
  }
  writeLines(markdown, file_conn)
  close(file_conn)
  return(invisible(NULL))
}
#' Fix the table of contents in the RTF file
#' 
#' ToC in pandoc output is not working out of the box. The ToC is a list of hyperlinks,
#' but there are no corresponding bookmarks in headers of respective sections
#' 
#' @param filename name of file to convert
#' @return NULL, changes file specified as an argument in place
#' @importFrom stringi stri_match_first
#' 
rtf_fix_table_of_contents <- function(filename){
  file_conn <- file(filename)
  rtf <- readLines(file_conn)
  # First identify the table of contents - look for hyperlinks
  hyperlink_lines <- grep(
    "HYPERLINK \"#[[:graph:]]*\"\\}\\}\\{\\\\fldrslt\\{\\\\ul$",
    rtf
  )
  # The headers are not unique. I take advantage of the fact that ToC is ordered
  # and look for the first header with a given name after previous match
  search_position <- 1
  for (i in hyperlink_lines){
    bookmark_text <- stringi::stri_match_first(rtf[i], regex="HYPERLINK \"#[[:graph:]]*\"\\}\\}\\{")
    bookmark_text <- substring(bookmark_text, 13, nchar(bookmark_text) - 4)
    bookmark_row <- grep(
        paste0(" ", rtf[i + 1],"\\\\p"), 
        rtf[search_position : length(rtf)]
      )[1] + search_position - 1
    search_position <- bookmark_row
    # Appending the bookmark matching the ToC hyperlink
    rtf[bookmark_row] <- paste0(
      rtf[bookmark_row],
      "{\\*\\bkmkstart ",
      bookmark_text,
      "}{\\*\\bkmkend ",
      bookmark_text,
      "}"
    )
  }
  writeLines(rtf, file_conn)
  close(file_conn)
  return(invisible(NULL))
}

#' Center images in RTF
#' 
#' By default pandoc rtf aligns pictures left, to center them (consistently with HTML)
#' manual intervention is required
#' @param filename name of file to convert
#' @return NULL, changes file specified as an argument in place
#' 
rtf_center_images <- function(filename){
  file_conn <- file(filename)
  rtf <- readLines(file_conn)
  image_lines <- grep("{\\pict", rtf, fixed = TRUE)
  for (i in image_lines){
    rtf[i] <- gsub("\\ql", "\\qc", rtf[i], fixed = TRUE)
  }
  writeLines(rtf, file_conn)
  close(file_conn)
  return(invisible(NULL))
}

#' Add path to graphs
#' 
#' By default pandoc rtf is run in its own location and does not locate the images properly
#' manual intervention is required to explicitly point the images
#' @param x text to convert
#' @return updated text
#' 
add_path_to_graphs <- function(x) {
  gsub(
    "\\(([[:graph:]]*)(.png)",
    paste0(
      "(",
      system.file("www", package = "climate.narrative"),
      "/",
      "\\1\\2"
    ),
    x,
    perl=T
  )
}