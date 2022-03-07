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
tab_name_to_number <- function(tab_name) {
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
#'
#' @inherit exposure_grid_server
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
#' @param id unique input id or blank for empty
#' @param tooltip_text Tooltip text to show
#' @param dev Are we in development mode
#' @param width Width of dropdown
#'
#' @importFrom tippy tippy_this
#'
exposure_grid_cell <- function(id, tooltip_text = "", dev = FALSE, width = NULL) {
  if (id == "") {
    form <- p("")
  } else {
    form <- selectInput(
      inputId = id,
      label = NULL,
      choices = c("N/A", "Low", "Medium", "High"),
      selected = ifelse(dev, "Medium", "N/A"),
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
  transpose <- (ncol(exposure_matrix) > 3)
  layout <- matrix("", nrow = nrow(exposure_matrix), ncol = ncol(exposure_matrix) - 1)
  colnames(layout) <- colnames(exposure_matrix)[-(2)]
  input_ids <- get_input_ids(exposure_matrix, label)
  for (i in 1:nrow(layout)) {
    layout[i, 1] <- exposure_matrix[i, 1]
    for (j in 2:ncol(layout)) {
      layout[i, j] <- as.character(
        exposure_grid_cell(
          input_ids[i, j - 1],
          tooltip_matrix[i, j - 1],
          dev,
          width
        )
      )
    }
  }
  if (transpose) {
    layout0 <- layout
    layout <- t(layout)
    row_headers <- gsub(".", " ", rownames(layout), fixed = TRUE)
    col_names <- layout[1, ]
    # remove first row, but ensure no degeneration to vector
    if (nrow(layout) > 2) {
      layout <- layout[-1, ]
    } else {
      layout <- t(layout[-1, ])
    }
    layout <- cbind(row_headers[-1], layout)
    colnames(layout) <- c(row_headers[1], col_names)
  }
  for (i in 1:nrow(layout)) {
    layout[i, 1] <- as.character(div(layout[i, 1], class = "verticalcenter"))
  }
  output[[label]] <- renderTable(
    layout,
    sanitize.text.function = function(x) x,
    sanitize.colnames.function = function(x) gsub(".", " ", x, fixed = TRUE),
    align = "c"
  )
}

#' Produce a table of input names for an input table
#'
#' @param exposure_matrix Exposure matrix upon which the input table is based
#' @param label Label for grid
#' @param tab NULL by default. If given, the first two arguments are ignored and inferred from tab
#'
get_input_ids <- function(exposure_matrix, label, tab = NULL) {
  if (!is.null(tab)) {
    exposure_matrix <- tab$exposure
    label <- paste(tab$type, tab$subtype, sep = "_")
  }
  input_ids <- matrix("", nrow = nrow(exposure_matrix), ncol = ncol(exposure_matrix) - 2)
  colnames(input_ids) <- colnames(exposure_matrix)[-(1:2)]
  for (i in 1:nrow(input_ids)) {
    for (j in 1:ncol(input_ids)) {
      if (exposure_matrix[i, j + 2] != "") {
        input_ids[i, j] <- paste(
          label,
          remove_special_characters(exposure_matrix[i, 1]),
          remove_special_characters(exposure_matrix[i, 2]),
          remove_special_characters(colnames(exposure_matrix)[j + 2]),
          remove_special_characters(exposure_matrix[i, j + 2]),
          sep = "_"
        )
      }
    }
  }
  return(input_ids)
}

#' Produce a matrix of input values from a matrix of input names (simple double loop)
#'
#' @param inputs dataframe of (all) inputs to look for
#' @param ids_matrix matrix of ids
#'
get_input_values <- function(inputs, ids_matrix) {
  values <- ids_matrix
  for (i in 1:nrow(values)) {
    for (j in 1:ncol(values)) {
      id <- ids_matrix[i, j]
      if (id != "") {
        value <- inputs[inputs$names == id, "values"]
        if (length(value)) {
          values[i, j] <- value
        } else {
          values[i, j] <- ""
        }
      }
    }
  }
  return(values)
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
#' @inherit string_break_line_with_spaces
#'
#' @importFrom stats na.omit
#' @importFrom stringi stri_locate_all
#'
string_add_spaces_to_make_equal_lines <- function(string, line_width) {
  out <- string
  newline_locations <- na.omit(stringi::stri_locate_all(out, fixed = "<br>")[[1]][, 1])
  i <- 1
  while (i * line_width < nchar(out) || length(newline_locations)) {
    if (any(newline_locations <= 1 + i * line_width)) {
      loc <- min(newline_locations)
      out <- string_break_line_with_spaces(out, line_width, loc, 4)
      newline_locations <- na.omit(stringi::stri_locate_all(out, fixed = "<br>")[[1]][, 1])
    } else {
      space_locations <- stringi::stri_locate_all(out, fixed = " ")[[1]][, 1]
      last_space <- na.omit(max(space_locations[space_locations <= 1 + line_width * i]))
      if (length(last_space)) out <- string_break_line_with_spaces(out, line_width, last_space, 1)
      newline_locations <- na.omit(stringi::stri_locate_all(out, fixed = "<br>")[[1]][, 1])
    }
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
    headers[i] <- string_format_lines(headers[i], col_widths[i] - 2)
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
  # ensure split rows is not zero
  split_rows <- pmax(split_rows, 1)
  out <- matrix("", nrow = 0, ncol = ncol(table))
  emptyline <- rep("", ncol(table))
  sepline <- emptyline
  for (i in 1:length(sepline)) {
    sepline[i] <- paste0(paste(rep("-", col_widths[i]), collapse = ""), "+")
  }
  sepline[1] <- paste0("+", sepline[1])
  rowsout <- matrix(emptyline, nrow = split_rows[1], ncol = length(emptyline), byrow = F)
  for (i in 1:ncol(out)) {
    cell_text <- headers[i]
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
#' @param include_exposures whether to include tables with contributing exposures
#' @return markdown-formatted report section (h2)
#'
#' @importFrom stats aggregate
#'
get_exposure_description <- function(item, type_item_inputs, include_exposures) {
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
    global$exposure_classes[[item]][["description"]]
  )
  if (include_exposures) {
    out <- paste0(
      out,
      "\n\nThe following rows contribute: \n\n",
      table_to_markdown_multiline(ordered_aggregate_inputs[, 1:4], TRUE, c(15, 30, 25, 15)),
      "\n\n"
    )
  }
  return(out)
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
#' @param include_oneliner FALSE by default, normally oneliner is for executive summary
#'
get_exposure_risk_description <- function(item,
                                          products,
                                          materiality,
                                          physical_or_transition,
                                          high_or_low,
                                          include_oneliner = FALSE) {
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
  header_text <- capitalize(header_text)
  content <- global$exposure_classes[[item]][[physical_or_transition]][[high_or_low]]
  out <- ""
  if (include_oneliner) {
    out <- paste0(
      "### ",
      header_text,
      " --- One-liner\n\n",
      content[["exec_description"]],
      "\n\n"
    )
  }
  out <- paste0(
    out,
    "### ",
    header_text,
    " --- Summary\n\n",
    content[["always"]],
    "\n\n"
  )
  if (materiality == "High") {
    out <- paste0(
      out,
      "### ",
      header_text,
      " --- Details\n\n",
      content[["high_materiality"]],
      "\n\n"
    )
  }
  for (product in products) {
    out <- paste0(
      out,
      content[[product]],
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
#' @param include_exposures whether to include tables with contributing exposures
#'
get_scenario_descriptions <- function(aggregated_table, type_inputs, scenario, include_exposures) {
  if (is.null(scenario)) warning(paste("No scenario file for ", scenario))
  name <- scenario$name
  description <- scenario$description
  transition <- scenario$transition
  physical <- scenario$physical
  out <- ""
  if (!is.null(name)) out <- paste0("# ", name, "\n\n")
  if (!is.null(description)) out <- paste0(out, description, "\n\n")
  if (nrow(aggregated_table)) {
    for (i in 1:nrow(aggregated_table)) {
      item <- aggregated_table$item[i]
      materiality <- aggregated_table$materiality_num[i]
      type_item_inputs <- type_inputs[type_inputs$item == item, ]
      products <- unique(type_item_inputs$product)
      out <- paste0(
        out,
        get_exposure_description(item, type_item_inputs, include_exposures),
        get_exposure_risk_description(item, products, materiality, "transition", transition),
        get_exposure_risk_description(item, products, materiality, "physical", physical),
        get_exposure_appendix(item)
      )
    }
  }
  return(out)
}

#' Lower level report helper function responsible for single section description
#'
#' @param section Section name
#' @param additional_pars List of additional parameters that may be required by special content functions
#'
get_section_descriptions <- function(section, additional_pars = list()) {
  out <- ""
  content_function <- section$special_content_function
  if (!is.null(content_function)) {
    if (content_function == "get_executive_summary") {
      if (additional_pars$report_version >= 3) {
        out <- paste0(
          out,
          get_executive_summary(additional_pars$aggregated_inputs, additional_pars$inputs, additional_pars$scenario_no, additional_pars$exec_summary_layout)
        )
      }
    } else if (content_function == "get_references") {
      out <- paste0(
        out,
        get_references(additional_pars$aggregated_inputs$item)
      )
    } else {
      stop(paste("Invalid content function name", content_function))
    }
  } else {
    if (!is.null(section$name)) out <- paste0("# ", section$name, "\n\n")
    if (!is.null(section$description)) out <- paste0(out, section$description, "\n\n")
  }
  return(out)
}

#' Function to produce references section (for all items)
#'
#' @param items vector of items to get references for
#' @return markdown-formatted references section (h2)
#'
get_references <- function(items) {
  out <- ""
  if (length(items)) {
    out <- paste0(
      out,
      "# References\n\n"
    )
    for (item in items) {
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
        p("Copyright 2022 The Climate Financial Risk Forum"),
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
        tags$li(paste("\"N/A\": immaterial or no exposure"))
      )
    )
  )
}

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

#' Go through the markdown file, find all png images and scale down where relevant
#'
#' @param filename markdown file to process
#' @param target_width target width of images
#' default is 7 inches which roughly matches vertical A4 page with margins
#' @param target_width_units either "in" for inches of "%"
#' @param fix_width if TRUE all images will be scaled exactly to target_width. otherwise,
#' only the larger images will be scaled down
#' @param min_pixels_to_rescale for fix_width=TRUE, pictures narrower than this number of pixels are not scaled up
#' this is to prevent ugly look of upscaled low resolution images
#' @return NULL, changes file specified as an argument in place
#' @importFrom stringi stri_match_first
#'
ensure_images_fit_page <- function(filename, target_width = 7, target_width_units = c("in", "%"), fix_width, min_pixels_to_rescale = 300) {
  file_conn <- file(filename)
  markdown <- readLines(file_conn)
  graph_lines <- grep("^!\\[", markdown)
  for (i in graph_lines) {
    image_name <- substring(stringi::stri_match_first(markdown[i], regex = "\\([[:graph:]]*.png"), 2)
    image_attributes <- attributes(png::readPNG(paste0(image_name), info = TRUE))$info
    if (is.null(image_attributes$dpi)) image_attributes$dpi <- c(96, 96)
    if (fix_width && image_attributes$dim[1] > min_pixels_to_rescale) {
      markdown[i] <- paste0(markdown[i], "{ width=", target_width, target_width_units, " }")
    } else if (target_width_units == "in" && image_attributes$dim[1] / image_attributes$dpi[1] > target_width) {
      warning(paste0("image ", image_name, " has width > ", target_width, target_width_units, ", resizing"))
      markdown[i] <- paste0(markdown[i], "{ width=", max_width_inch, target_width_units, " }")
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
#' @inherit rtf_postprocess
#' @return NULL, changes file specified as an argument in place
#' @importFrom stringi stri_match_first
#'
rtf_fix_table_of_contents <- function(filename) {
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
  for (i in hyperlink_lines) {
    bookmark_text <- stringi::stri_match_first(rtf[i], regex = "HYPERLINK \"#[[:graph:]]*\"\\}\\}\\{")
    bookmark_text <- substring(bookmark_text, 13, nchar(bookmark_text) - 4)
    bookmark_row <- grep(
      paste0(" ", rtf[i + 1], "\\p"),
      rtf[search_position:length(rtf)],
      fixed = TRUE,
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
#' @inherit rtf_postprocess
#' @return NULL, changes file specified as an argument in place
#'
rtf_center_images <- function(filename) {
  file_conn <- file(filename)
  rtf <- readLines(file_conn)
  image_lines <- grep("{\\pict", rtf, fixed = TRUE)
  for (i in image_lines) {
    rtf[i] <- gsub("\\ql", "\\qc", rtf[i], fixed = TRUE)
  }
  writeLines(rtf, file_conn)
  close(file_conn)
  return(invisible(NULL))
}

#' Fix the RTF file
#'
#' It is not possible to set some options using markdown syntax. Also, there seems to be a bug
#' in pandoc which breaks the TOC (links are not working)
#' @param filename name of file to convert
#' @param report_version enables different versions of the reports within a single code, see global file for possible choices and their meaning
#' @return NULL, changes file specified as an argument in place

rtf_postprocess <- function(filename, report_version) {
  rtf_fix_table_of_contents(filename)
  rtf_center_images(filename)
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
    perl = T
  )
}

#' One of the functions comprising the executive summary text
#'
#' @inherit get_executive_summary
#' @param exposure_exec bool whether to include a short description for the sector
#' (applicable in single sector context only)
#'
get_executive_summary_scenarios <- function(aggregated_inputs, inputs, scenario_no, exposure_exec = FALSE) {
  out <- ""
  for (scenario in global$scenarios[scenario_no]) {
    out <- paste0(
      out,
      "## ",
      scenario$name,
      "\n\n",
      scenario$exec_description,
      "\n\n"
    )
    risk <- scenario$exec_short
    risk_intensity <- scenario[[risk]]
    if (exposure_exec) {
      if (nrow(aggregated_inputs) > 1) {
        warning("Multiple sectors not compatible with this executive summary layout, using the first sector only")
      }
      item <- aggregated_inputs$item[1]
      materiality <- aggregated_inputs$materiality[1]
      if (materiality == "High") {
        text <- global$exposure_classes[[item]][[risk]][[risk_intensity]][["always"]]
      } else if (materiality %in% c("Medium", "Low")) {
        text <- global$exposure_classes[[item]][[risk]][[risk_intensity]]["exec_description"]
      } else {
        stop(paste("Unknown materiality level: ", materiality))
      }
      out <- paste0(
        out,
        "##### ",
        global$exposure_classes[[item]][["name"]],
        "\n\n",
        global$exposure_classes[[item]][[risk]][[risk_intensity]]["always"],
        "\n\n"
      )
    }
  }
  out <- paste(out, collapse = "\n")
  return(out)
}


#' Function to combine an executive summary from lower level functions
#'
#' @param aggregated_inputs data frame of aggregated inputs (implemented in the reactive expression)
#' @param inputs data frame of all inputs (implemented in the reactive expression)
#' @param scenario_no integer or vector of integers indicating which of the global$scenarios should be included
#' @param layout determines the structure of the output
#'
#' 1 for full three subsections (inputs, scenarios, exposures)
#' 2 for one subsection (scenarios, including also exposures)
#' @return string - executive summary text
#'
get_executive_summary <- function(aggregated_inputs, inputs, scenario_no, layout = 1) {
  out_0 <- "# Executive summary\n\n"
  exec <- data.frame(low = rep(FALSE, 2), high = rep(FALSE, 2))
  rownames(exec) <- c("transition", "physical")
  for (i in scenario_no) {
    risk <- global$scenarios[[i]][["exec_short"]]
    exec[risk, global$scenarios[[i]][[risk]]] <- TRUE
  }
  if (layout == 1) {
    out_1 <- get_executive_summary_inputs(aggregated_inputs, inputs)
    out_2 <- get_executive_summary_scenarios(aggregated_inputs, inputs, scenario_no)
    out_3 <- get_executive_summary_exposures(aggregated_inputs, inputs, scenario_no, exec)
  } else if (layout == 2) {
    out_1 <- ""
    out_2 <- get_executive_summary_scenarios(aggregated_inputs, inputs, scenario_no, TRUE)
    out_3 <- ""
  } else {
    stop("Invalid layout parameter")
  }
  # the final output is a string
  return(paste0(out_0, out_1, out_2, out_3))
}

#' Helper function that translate the value of input field to the scenario number(s)
#'
#' @param report_scenario_selection value of the input
#' @param is_rtf bool whether to include sections that are for RTF only
#' @return vector of integers
#'
get_scenario_no <- function(report_scenario_selection, is_rtf) {
  if (report_scenario_selection == "") {
    scenario_no <- which(sapply(global$scenarios, function(sce) !is.null(sce$name)))
  } else {
    scenario_no <- which(sapply(global$scenarios, `[[`, i = "name") == report_scenario_selection)
  }
  return(scenario_no)
}

#' Helper function that returns non-scenario section number(s)
#'
#' @inherit get_scenario_no
#'
get_section_no <- function(is_rtf) {
  if (is_rtf) {
    indicator_function <- function(s) s$include_in_RTF
  } else {
    indicator_function <- function(s) s$include_in_HTML
  }
  return(which(sapply(global$sections, indicator_function)))
}

#' Function that generates a markdown content of the report
#'
#' @param inputs data frame of all relevant inputs (the reactive expression all_inputs or its subset obtained with get_inputs)
#' @param report_version enables different versions of the reports within a single code, see global file for possible choices and their meaning
#' @param report_scenario_selection (user-friendly) scenario name (or empty string)
#' @param is_rtf a flag that triggers several format specific settings:
#' - TRUE: include non-scenario sections (e.g. intro)
#' - FALSE: include the links to page top (note requires proper report_version as well)
#' @param exec_summary_layout determines the structure of the executive summary (see get_executive_summary function)
#' @param include_exposures whether to include tables with contributing exposures
#' @return vector of string - executive summary text (3 items + 1 per scenario + 1 item at the end)
#'
get_report_contents <- function(inputs, report_version, report_scenario_selection, is_rtf, exec_summary_layout = 1, include_exposures) {
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
  for (section in global$sections[section_no]) {
    out <- c(
      out,
      list(get_section_descriptions(
        section,
        list(
          report_version = report_version,
          aggregated_inputs = aggregated_inputs,
          inputs = inputs,
          scenario_no = scenario_no,
          exec_summary_layout = exec_summary_layout
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
}

#' One of the functions comprising the executive summary text
#'
#' @inherit get_executive_summary
#' @param exec the table of bools, determining which of the descriptions
#' (in both high materiality and others) will be used in the report
#'
#' Rows denote risks (physical/transition)
#' Columns denote risk intensity (high/low)
#'
get_executive_summary_exposures <- function(aggregated_inputs,
                                            inputs,
                                            scenario_no,
                                            exec) {
  out_exp <- "## Exposures\n\nThis report considers the following exposures:\n\n"
  out_exp <- paste0(out_exp, "### High materiality exposures\n\n")
  high_counter <- 0
  for (i in 1:nrow(aggregated_inputs)) {
    item <- aggregated_inputs[i, 1]
    materiality <- aggregated_inputs[i, 2]
    if (materiality == "High") {
      high_counter <- high_counter + 1
      out_exp <- paste0(
        out_exp,
        "#### ",
        global$exposure_classes[[item]][["name"]],
        "\n\n"
      )
      for (risk in rownames(exec)) {
        for (risk_intensity in colnames(exec)) {
          if (exec[risk, risk_intensity]) {
            out_exp <- paste0(
              out_exp,
              global$exposure_classes[[item]][[risk]][[risk_intensity]]["always"],
              "\n\n"
            )
          }
        }
      }
    }
  }
  if (high_counter == 0) {
    out_exp <- paste0(out_exp, "None\n\n")
  }
  out_exp <- paste0(out_exp, "### Other exposures\n\n")
  less_material <- aggregated_inputs[aggregated_inputs[, 2] != "High", ]
  if (nrow(less_material)) {
    less_material$risk.description <- rep(NA, nrow(less_material))
    less_material$name <- rep(NA, nrow(less_material))
    for (i in 1:nrow(less_material)) {
      item <- less_material[i, 1]
      less_material$name[i] <- global$exposure_classes[[item]][["name"]]
      risk_description <- ""
      for (risk in rownames(exec)) {
        for (risk_intensity in colnames(exec)) {
          if (exec[risk, risk_intensity]) {
            risk_description <- paste(
              risk_description,
              global$exposure_classes[[item]][[risk]][[risk_intensity]][["exec_description"]],
              sep = "<br>"
            )
          }
        }
      }
      risk_description <- substring(risk_description, 5)
      less_material$risk.description[i] <- risk_description
    }
    for (i in 1:ncol(less_material)) {
      colnames(less_material)[i] <- capitalize(colnames(less_material)[i])
    }
    out_exp <- paste0(
      out_exp,
      table_to_markdown_multiline(
        less_material[, c("Name", "Materiality", "Risk.description")],
        col_widths = c(20, 20, 60)
      )
    )
  } else {
    out_exp <- paste0(out_exp, "None\n\n")
  }
  return(out_exp)
}

#' Helpder function - the name is self-explanatory
#'
#' @param data matrix or data.frame, possibly containing missing value
#' @param empty_strings which strings should be considered as "empty"?
#' @param ignore_cols indices of columns which should be ignored for emptiness check
#' @return input object without rows and columns where all entries are empty (i.e. NA or "")
#'
delete_empty_rows_and_columns <- function(data, empty_strings = list("", "N/A"), ignore_cols = c()) {
  data <- as.data.frame(data)
  empty_strings <- unlist(empty_strings)
  use_cols <- setdiff(1:ncol(data), ignore_cols)
  empty_columns <- apply(data, 2, function(x) all(is.na(x) | x %in% empty_strings))
  empty_rows <- apply(data, 1, function(x) all(is.na(x[use_cols]) | x[use_cols] %in% empty_strings))
  if (all(empty_columns[use_cols])) {
    return(NULL)
  } else {
    data[, empty_columns] <- NULL
    if (sum(!empty_rows) > 1) {
      colnames(data)[1] <- ""
      return(data[!empty_rows, ])
    } else {
      # a bit clumsy, but need to add separately one row case - by default converted by R to vector
      out <- as.data.frame(t(t(data[!empty_rows, ])))
      colnames(out)[1] <- ""
      return(out)
    }
  }
}

#' One of the functions comprising the executive summary text
#'
#' @inherit get_executive_summary
#'
get_executive_summary_inputs <- function(aggregated_inputs, inputs) {
  out <- "## Inputs\n\n"
  for (tab in global$tabs) {
    if (!is.null(tab$exposure)) {
      ids <- get_input_ids(tab = tab)
      values <- get_input_values(inputs, ids)
      values <- cbind(tab$exposure[, 1], values)
      values_trimmed <- delete_empty_rows_and_columns(values, ignore_cols = 1)
      if (!is.null(values_trimmed)) {
        transpose <- (ncol(tab$exposure) > 3)
        if (transpose) {
          values_trimmed <- as.data.frame(t(values_trimmed))
          row_names <- rownames(values_trimmed)
          row_names <- gsub(".", " ", row_names, fixed = TRUE)
          values_trimmed <- cbind(row_names, values_trimmed)
          colnames(values_trimmed) <- values_trimmed[1, ]
          values_trimmed <- values_trimmed[-1, ]
        }
        ncol <- ncol(values_trimmed)
        out <- paste0(
          out,
          "### ",
          tab$tab_title,
          "\n\n"
        )
        # if table has more than 4 columns split it in parts
        nparts <- ceiling((ncol - 1) / 3)
        for (i in 1:nparts) {
          cols_to_use <- c(1, 3 * i + (-1:1))
          cols_to_use <- cols_to_use[cols_to_use <= ncol]
          out <- paste0(
            out,
            table_to_markdown_multiline(values_trimmed[, cols_to_use], col_widths = c(30, rep(20, length(cols_to_use)-1)))
          )
        }
      }
    }
  }
  return(out)
}


#' Karnan's request for easier change comparison - single sector
#'
#' @param item sector name
#'
get_exposure_test_description <- function(item) {
  exposure_class <- global$exposure_classes[[item]]
  out <- paste0(
    "## ",
    exposure_class$name,
    "\n\n",
    exposure_class$description,
    "\n\n"
  )

  for (risk in c("transition", "physical")) {
    for (risk_intensity in c("low", "high")) {
      out <- paste0(
        out,
        get_exposure_risk_description(item, c(), "High", risk, risk_intensity, TRUE)
      )
    }
  }
  return(out)
}

#' Karnan's request for easier change comparison - loop over all sectors
#'
get_test_report <- function() {
  out <- "# Test report\n\n"
  for (i in 1:length(global$exposure_classes)) {
    exposure_class <- global$exposure_classes[[i]]
    out <- paste0(out, get_exposure_test_description(names(global$exposure_classes)[i]))
    out <- paste0(out, get_exposure_appendix(names(global$exposure_classes)[i]))
  }
  out <- add_path_to_graphs(out)
  return(out)
}

#' Filter the inputs depending on institution type, sector. Optionally aggregate and override all materialities
#'
#' @param all_inputs_table data frame of all inputs (usually the reactive expression all_inputs)
#' @param inst_type institution type to filter (or "" for no filtering)
#' @param sector sector to filter (or "" for no filtering)
#' @param aggregate bool, whether to aggregate the inputs by sector
#' @param override_materiality ignore the actual inputs and set all materialities to a level (no override by default)
#'
get_inputs <- function(all_inputs_table, inst_type = "", sector = "", aggregate = FALSE, override_materiality = "") {
  out <- all_inputs_table
  if (override_materiality != "") {
    out$materiality <- factor(override_materiality, levels = c("N/A", "Low", "Medium", "High"), ordered = T)
    out$materiality_num <- (as.integer(out$materiality) - 1)^2 + (as.integer(out$materiality) > 2)
  } else {}
  if (inst_type != "") {
    out <- out[(which(out$type == inst_type & out$materiality != "N/A")), ]
  }
  if (sector != "") {
    out <- out[out$item == sector, ]
  }
  if (aggregate) {
    out <- aggregate_inputs(out)
  }
  return(out)
}

#' Aggregate the inputs by sector
#'
#' @param inputs data frame of all inputs (usually the reactive expression all_inputs or its subset)
#'
aggregate_inputs <- function(inputs) {
  aggregated_inputs_factor <- stats::aggregate(materiality ~ item, FUN = max, data = inputs)
  aggregated_inputs_numeric <- stats::aggregate(
    materiality_num ~ item,
    FUN = function(x) {
      cut(
        sum(x),
        breaks = c(0, 4.5, 9.5, 100),
        labels = c("Low", "Medium", "High")
      )
    },
    data = inputs
  )
  out <- merge(aggregated_inputs_factor, aggregated_inputs_numeric)
  out <- out[order(out$materiality, out$materiality_num, decreasing = TRUE), ]
  return(out)
}

#' Read the markdown section from relevant yaml file and save as output
#'
#' This function should be used on the server side.
#' Corresponding UI side call should be: uiOutput(output_name)
#'
#' @param output Shiny output
#' @param output_name Name of slot created in shiny output (to be used in UI part of shiny app)
#' @param section_name Name of yaml file within section directory read from
#' (description section only, other YAML fields are ignored)
#'
#' @importFrom markdown markdownToHTML
#'
include_markdown_section <- function(output, output_name, section_name) {
  text <- unlist(
    strsplit(
      global$sections[[section_name]]$description,
      "\n"
    )
  )
  output[[output_name]] <- renderUI({
    HTML(
      markdown::markdownToHTML(
        text = text,
        fragment.only = TRUE
      )
    )
  })
}

render_rtf <- function(input_file, output_file, res_path) {
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
  rtf_postprocess(output_file, global$report_version)
  return(invisible(NULL))
}

render_html <- function(input_file, output_file) {
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
  html_postprocess(output_file, global$report_version)
  return(invisible(NULL))
}

html_postprocess <- function(file, report_version) {
  # replace back the images links
  file_conn <- file(file)
  temp <- readLines(file_conn)
  temp <- gsub(
    system.file("www", package = "climate.narrative"),
    "climate_narrative",
    temp
  )
  if (report_version >= 2) {
    temp <- gsub(
      "(<h[1-5]?>)(.*)(</h[1-5]?>)",
      "<div class=\"inline\"> \\1\\2\\3 <a href='#top'>&uarr;</a> </div>",
      temp,
      perl = TRUE
    )
  }
  # extract the table of contents
  writeLines(
    temp,
    file_conn
  )
  close(file_conn)
  return(invisible(NULL))
}
