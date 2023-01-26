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
read_dir <- function(directory,
                     file_format = "auto",
                     in_package = system.file(directory, package = "climate.narrative") != "",
                     remove_special_characters_from_names = TRUE) {
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
#' @param tab_name Name of the tab to convert
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
  s <- gsub("( ", " (", s, fixed = TRUE)
  s <- capitalize(s)
  # manually substitute texts with where simple capitalisation rule fails
  substitutions <- data.frame(
    from = c("Sme", "Smes", "Uk", "Us", "And", "To"),
    to = c("SME", "SMEs", "UK", "US", "and", "to")
  )
  for (i in 1:nrow(substitutions)) {
    s <- gsub(substitutions$from[i], substitutions$to[i], s, fixed = TRUE)
  }
  s
}


#' Produce the layout of questionnaire tabs (cell, row, whole table)
#'
#' @param id unique input id or blank for empty
#' @param dev Are we in development mode
#' @param width Width of dropdown
#'
exposure_grid_cell <- function(id, dev = FALSE, width = NULL) {
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
  }
  return(form)
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
#' @param label Label for grid
#' @param dev Are we in developement mode
#' @param width Width of each dropdown
#'
exposure_grid_server <- function(input,
                                 output,
                                 exposure_matrix,
                                 label,
                                 dev = FALSE,
                                 width = NULL) {
  transpose <- (ncol(exposure_matrix) > 3 & nrow(exposure_matrix) > 1)
  layout <- matrix("", nrow = nrow(exposure_matrix), ncol = ncol(exposure_matrix) - 1)
  colnames(layout) <- colnames(exposure_matrix)[-(2)]
  input_ids <- get_input_ids(exposure_matrix, label)
  for (i in 1:nrow(layout)) {
    layout[i, 1] <- exposure_matrix[i, 1]
    for (j in 2:ncol(layout)) {
      layout[i, j] <- as.character(
        exposure_grid_cell(
          input_ids[i, j - 1],
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
  input_ids <- as.data.frame(input_ids, stringsAsFactors = FALSE)
  for (i in 1:ncol(input_ids)) {
    input_ids[, i] <- as.character(input_ids[, i])
  }
  colnames(input_ids) <- as.character(colnames(exposure_matrix)[-(1:2)])
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
#' @param ids dataframe of ids
#'
get_input_values <- function(inputs, ids) {
  values <- ids
  for (i in 1:nrow(values)) {
    for (j in 1:ncol(values)) {
      id <- ids[i, j]
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
#' @param location Position of the (first character of) the break in the string
#' @param n_char Number of characters that are to be replaced with a (manual space-based) linebreak
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
      if (length(last_space)) {
        out <- string_break_line_with_spaces(out, line_width, last_space, 1)
      } else {
        warning(paste0("too long text to break: ", string))
      }
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
  out_2 <- paste0(
    paste(sepline, collapse = ""),
    "\n",
    paste(apply(out, 1, paste, collapse = ""), collapse = "\n"),
    "\n\n"
  )
  return(out_2)
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


#' Aggregate multiple numerical materiality exposures into a single, qualitative
#'
#' The thresholds are defined so that 10 and above is "high",
#' anything between 5 (incl.) and 10 (excl.) is medium.
#' instead of 1000 I could use infinity (but up to at least 100 classes it makes no difference)
#'
#' @param x a vector of numeric materialities
#'
aggregate_quantitative_to_qualitative_materiality <- function(x) {
  cut(
    sum(x),
    breaks = c(0, 4.5, 9.5, 1000),
    labels = c("Low", "Medium", "High")
  )
}

#' Produce report content for a given item
#'
#' @param item Name of item for which a report is to be produced
#' @param type_item_inputs Table of (disaggregated) inputs to produce a table of contributing rows
#' @param exposure_classes List of sectors from which the texts are extracted
#' @param include_exposures Whether to include tables with contributing exposures
#' @param header_level what level of markdown header should be produced
#' @return markdown-formatted report section (h2)
#'
#' @importFrom stats aggregate
#'
get_exposure_description <- function(item, type_item_inputs, exposure_classes, include_exposures, header_level = 2) {
  if (is.null(exposure_classes[[item]])) warning(paste("No exposure class file for ", item))
  ordered_type_item_inputs <- type_item_inputs[order(type_item_inputs$materiality), ]
  # explicit conversion from factor back to string to avoid implicit conversion to number below
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
    FUN = aggregate_quantitative_to_qualitative_materiality
  )
  ordered_aggregate_inputs <- merge(ordered_aggregate_inputs_text, ordered_aggregate_inputs_num)
  colnames(ordered_aggregate_inputs)[3:5] <- c("Exposure.row", "Materiality", "Product materiality")
  out <- paste0(
    strrep("#", header_level),
    " ",
    exposure_classes[[item]][["name"]],
    "\n\n",
    exposure_classes[[item]][["description"]],
    "\n\n"
  )
  if (include_exposures) {
    out <- paste0(
      out,
      "The following rows contribute: \n\n",
      table_to_markdown_multiline(ordered_aggregate_inputs[, 1:4], TRUE, c(15, 30, 25, 15)),
      "\n\n"
    )
  }
  return(out)
}

#' Produce appendix for a given item
#'
#' @param item Name of item for which appendix is to be produced
#' @param exposure_classes List of sectors from which the appendix text are extracted
#' @return Markdown-formatted appendix section (h3)
#'
get_exposure_appendix <- function(item, exposure_classes) {
  appendix <- exposure_classes[[item]][["appendix"]]
  if (is.null(appendix)) {
    return(c())
  } else {
    return(
      paste0(
        "#### Appendix",
        "\n\n",
        exposure_classes[[item]][["appendix"]],
        "\n\n"
      )
    )
  }
}

#' Lower level report helper function responsible for single risk
#' (transition/physical) description.
#'
#' @inherit get_standard_report_contents
#' @param item Exposure class name
#' @param products Product name
#' @param materiality Materiality of item
#' @param physical_or_transition Type of scenario
#' @param high_or_low Is scenario high or low
#' @param include_oneliner FALSE by default, normally oneliner is for executive summary
#' @param header_level what level of markdown header should be produced
#'
get_exposure_risk_description <- function(item,
                                          products,
                                          materiality,
                                          exposure_classes,
                                          physical_or_transition,
                                          high_or_low,
                                          include_oneliner = FALSE,
                                          header_level = 4) {
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
    exposure_classes[[item]][["name"]],
    " --- ",
    riskname
  )
  header_text <- capitalize(header_text)
  content <- exposure_classes[[item]][[physical_or_transition]][[high_or_low]]
  out <- ""
  if (include_oneliner) {
    out <- paste0(
      strrep("#", header_level),
      " ",
      header_text,
      " --- One-liner\n\n",
      content[["exec_description"]],
      "\n\n"
    )
  }
  out <- paste0(
    out,
    strrep("#", header_level),
    " ",
    header_text,
    " --- Summary\n\n",
    content[["always"]],
    "\n\n"
  )
  if (materiality == "High") {
    out <- paste0(
      out,
      strrep("#", header_level),
      " ",
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
#' @param aggregated_table_by_item Table of all possible items
#' @param aggregated_table_by_group As above but less granular
#' @param type_inputs Drop box items
#' @param scenario Scenario name
#' @param include_exposures Flag whether to include tables with contributing exposures
#' @param exposure_classes List of exposure classes (sectors) from which the texts are extracted
#'
get_scenario_descriptions <- function(aggregated_table_by_item,
                                      aggregated_table_by_group,
                                      type_inputs,
                                      scenario,
                                      exposure_classes,
                                      include_exposures) {
  if (is.null(scenario)) warning(paste("No scenario file for ", scenario))
  name <- scenario$name
  description <- scenario$description
  transition <- scenario$transition
  physical <- scenario$physical
  out <- ""
  if (!is.null(name)) out <- paste0("# ", name, "\n\n")
  if (!is.null(description)) out <- paste0(out, description, "\n\n")
  if (nrow(aggregated_table_by_group)) {
    A_or_L_header <- (length(unique(aggregated_table_by_group$A_or_L)) > 1)
    for (i in 1:nrow(aggregated_table_by_group)) {
      if (A_or_L_header && (i == 1 || aggregated_table_by_group$A_or_L[i] != aggregated_table_by_group$A_or_L[i - 1])) {
        out <- paste0(
          out,
          "# ",
          ifelse(aggregated_table_by_group$A_or_L[i] == "A", "Assets", "Liabilities"),
          "\n\n"
        )
      }
      group <- aggregated_table_by_group$item[i]
      materiality <- aggregated_table_by_group$materiality_num[i]
      type_group_inputs <- type_inputs[(type_inputs$exposure_group == group) | (type_inputs$item == group), ]
      products <- unique(type_group_inputs$product)
      out <- paste0(
        out,
        get_exposure_description(group, type_group_inputs, exposure_classes, include_exposures, ifelse(A_or_L_header, 2, 1))
      )
      unique_items <- unique(type_group_inputs$item)
      if (length(unique_items) > 1) {
        # calculate item-specific materiality only if there is more than one item
        for (item in unique_items) {
          item_materiality <- aggregate_quantitative_to_qualitative_materiality(type_group_inputs[type_group_inputs$item == item, ]$materiality_num)
          item_products <- unique(type_group_inputs[type_group_inputs$item == item, ]$products)
          out <- paste0(
            out,
            get_exposure_risk_description(item, item_products, item_materiality, exposure_classes, "transition", transition),
            get_exposure_risk_description(item, item_products, item_materiality, exposure_classes, "physical", physical)
          )
        }
      } else {
        out <- paste0(
          out,
          get_exposure_risk_description(unique_items, products, materiality, exposure_classes, "transition", transition),
          get_exposure_risk_description(unique_items, products, materiality, exposure_classes, "physical", physical)
        )
      }
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
      out <- paste0(
        out,
        get_executive_summary(
          additional_pars$tabs,
          additional_pars$scenarios,
          additional_pars$exposure_classes,
          additional_pars$aggregated_inputs_by_item,
          additional_pars$inputs,
          additional_pars$scenario_no,
          additional_pars$exec_summary_layout
        )
      )
    } else if (content_function == "get_references") {
      out <- paste0(
        out,
        get_references(additional_pars$aggregated_inputs_by_group$item, additional_pars$exposure_classes)
      )
    } else if (content_function == "get_appendices") {
      out <- paste0(
        out,
        get_appendices(additional_pars$aggregated_inputs_by_group$item, additional_pars$exposure_classes)
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
#' @inherit get_standard_report_contents
#' @param items vector of items to get references for
#' @return markdown-formatted references section(h1)
#'
get_references <- function(items, exposure_classes) {
  out <- ""
  if (length(items)) {
    out <- paste0(
      out,
      "# References\n\n"
    )
    for (item in items) {
      if (length(exposure_classes[[item]][["references"]])) {
        out <- paste0(
          out,
          "### ",
          exposure_classes[[item]][["name"]],
          "\n\n",
          exposure_classes[[item]][["references"]],
          "\n\n"
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

#' Function to produce appendices (as references)
#'
#' @inherit get_references
#'
get_appendices <- function(items, exposure_classes) {
  out <- ""
  if (length(items)) {
    out <- paste0(
      out,
      "# Appendices\n\n"
    )
    for (item in items) {
      if (length(exposure_classes[[item]][["appendix"]])) {
        out <- paste0(
          out,
          "### ",
          exposure_classes[[item]][["name"]],
          "\n\n",
          exposure_classes[[item]][["appendix"]],
          "\n\n"
        )
      }
    }
  }
  # Do not show the section if there are no references
  if (out == "# Appendices\n\n") {
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
        p("Copyright 2023 The Climate Financial Risk Forum"),
        p(
          a(href = "https://github.com/JohnAdders/climate_narrative", "Source Code", target = "_blank"),
          " | ",
          a(href = "mailto:john.adcock@aviva.com?subject=Climate%20Narrative%20support%20request", "Support"),
          " | ",
          a(href = "https://github.com/JohnAdders/climate_narrative/issues?q=is%3Aissue+is%3Aopen+label%3Abug", "Known Issues", target = "_blank"),
          " | ",
          a(href = "https://github.com/JohnAdders/climate_narrative/wiki/Contributors", "Contributors", target = "_blank"),
          " | ",
          a(
            href = "climate_narrative/climate-financial-risk-forum-climate-risk-product-providers-latest.xlsx",
            "CFRF - Data and Tools Providers"
          ),
          " | ",
          a(
            href = "https://github.com/JohnAdders/climate_narrative/wiki/NGFS-Scenario-Data-used-in-Charts-and-Graphs",
            "Data used in Charts",
            target = "_blank"
          )
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

#' Produce a helper HTML text explaining materiality levels.
#'
#' @param asset_or_liability A string with the type of exposure page
#' @param is_asset_mananger Is this display for an asset manager
#'
generic_helper <- function(asset_or_liability, is_asset_mananger = FALSE) {
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
  helpText(
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


#' Go through the markdown file, find all png images and scale down where relevant
#'
#' @param filename markdown file to process
#' @param image_settings the list of necessary setting, comprising of:
#' - target_width target width of images
#' default is 7 inches which roughly matches vertical A4 page with margins
#' - target_width_units either "in" for inches of "%"
#' - fix_width if TRUE all images will be scaled exactly to target_width. otherwise,
#' only the larger images will be scaled down
#' - min_pixels_to_rescale for fix_width=TRUE, pictures narrower than this number of pixels are not scaled up
#' this is to prevent ugly look of upscaled low resolution images
#' - max_height maximum height of picture after rescaling. Another mechanism to prevent
#' too large upscaled pictures (in particular square or portrait layout)
#' @return NULL, changes file specified as an argument in place
#' @importFrom stringi stri_match_first
#'
format_images <- function(filename, image_settings) {
  target_width <- image_settings$image_width
  target_width_unit <- image_settings$image_width_unit
  fix_width <- image_settings$image_width_fix
  min_pixels_to_rescale <- image_settings$image_min_pixels_to_rescale
  max_height <- image_settings$image_max_height
  file_conn <- file(filename)
  markdown <- readLines(file_conn)
  graph_lines <- grep("^!\\[", markdown)
  for (i in graph_lines) {
    image_name <- substring(stringi::stri_match_first(markdown[i], regex = "\\([[:graph:]]*.png"), 2)
    if (!file.exists(image_name)) {
      image_name <- paste0("inst/www", image_name)
    }
    if (file.exists(image_name)) {
      image_attributes <- attributes(png::readPNG(paste0(image_name), info = TRUE))$info
      if (is.null(image_attributes$dpi)) image_attributes$dpi <- c(96, 96)
      width_px <- image_attributes$dim[1]
      height_px <- image_attributes$dim[2]
      width_in <- width_px / image_attributes$dpi[1]
      height_in <- height_px / image_attributes$dpi[2]
      target_width_after_max <- round(pmin(target_width, max_height / height_in * width_in), 2)
      if (fix_width && width_px > min_pixels_to_rescale) {
        markdown[i] <- paste0(markdown[i], "{ width=", target_width_after_max, target_width_unit, " }")
      } else if (target_width_unit == "in" && image_attributes$dim[1] / image_attributes$dpi[1] > target_width) {
        warning(paste0("image ", image_name, " has width > ", target_width_after_max, target_width_unit, ", resizing"))
        markdown[i] <- paste0(markdown[i], "{ width=", target_width_after_max, target_width_unit, " }")
      }
    } else {
      warning(paste0("Image file ", image_name, " does not exist"))
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
rtf_fix_table_of_contents <- function(filename, dev) {
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
    bookmark_rows <- grep(
      paste0("[[:digit:]]", " ", rtf[i + 1], "\\\\p"),
      rtf[search_position:length(rtf)],
      perl = TRUE
    )
    if (length(bookmark_rows) > 1 && dev) {
      warning(
        paste0(
          "Ambiguous table of content entry. Header ",
          bookmark_text,
          " is not unique, using the first match. Please check the table of content"
        )
      )
    } else if (length(bookmark_rows) == 0) {
      stop(
        paste0(
          "Error in fixing RTF table of content, header ",
          bookmark_text,
          " not found"
        )
      )
    }
    bookmark_row <- bookmark_rows[1] + search_position - 1
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
    # Limit of 40 characters!
    if (nchar(bookmark_text) > 40) {
      if (dev) {
        warning(
          paste0(
            "Too long header for a bookmark, truncating: ",
            bookmark_text
          )
        )
      }
      rtf <- gsub(bookmark_text, substr(bookmark_text, 1, 40), rtf)
    }
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
#' @param dev developmen mode flag (used only to decide whether a warning is raised)
#' @return NULL, changes file specified as an argument in place

rtf_postprocess <- function(filename, dev) {
  rtf_fix_table_of_contents(filename, dev)
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
  www_path <- system.file("www", package = "climate.narrative")
  if (www_path == "") {
    www_path <- paste0(getwd(), "/inst/www")
  }
  updated_text <- gsub(
    "\\(([[:graph:]]*)(.png)",
    paste0(
      "(",
      www_path,
      "/",
      "\\1\\2"
    ),
    x,
    perl = T
  )
  # transform back pictures that are directly embedded into HTML and so start with data
  updated_text <- gsub(
    paste0(
      "\\(",
      www_path,
      "/data:([[:graph:]]*)(.png)"
    ),
    paste0(
      "(",
      "data:\\1\\2"
    ),
    updated_text,
    perl = T
  )
  updated_text
}

#' One of the functions comprising the executive summary text
#'
#' @inherit get_executive_summary
#' @param exposure_exec bool whether to include a short description for the sector
#' (applicable in single sector context only)
#'
get_executive_summary_scenarios <- function(scenarios, exposure_classes, aggregated_inputs, inputs, scenario_no, exposure_exec = FALSE) {
  out <- ""
  for (scenario in scenarios[scenario_no]) {
    out <- paste0(
      out,
      "## Summary of ",
      scenario$name,
      "{.unlisted .unnumbered}",
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
        text <- exposure_classes[[item]][[risk]][[risk_intensity]][["always"]]
      } else if (materiality %in% c("Medium", "Low")) {
        text <- exposure_classes[[item]][[risk]][[risk_intensity]]["exec_description"]
      } else {
        stop(paste("Unknown materiality level: ", materiality))
      }
      out <- paste0(
        out,
        "##### Summary of ",
        exposure_classes[[item]][["name"]],
        "\n\n",
        exposure_classes[[item]][[risk]][[risk_intensity]]["always"],
        "\n\n"
      )
    }
  }
  out <- paste(out, collapse = "\n")
  return(out)
}


#' Function to combine an executive summary from lower level functions
#'
#' @param tabs List of tabs (required for summary of user inputs)
#' @param scenarios List of scenarios
#' @param exposure_classes List of sectors
#' @param aggregated_inputs data frame of aggregated inputs (implemented in the reactive expression)
#' @param inputs data frame of all inputs (implemented in the reactive expression)
#' @param scenario_no integer or vector of integers indicating which of the global$scenarios should be included
#' @param layout determines the structure of the output
#'
#' 1 for full three subsections (inputs, scenarios, exposures)
#' 2 for one subsection (scenarios, including also exposures)
#' @return string - executive summary text
#'
get_executive_summary <- function(tabs, scenarios, exposure_classes, aggregated_inputs, inputs, scenario_no, layout = 1) {
  out_0 <- "# Executive summary\n\n"
  exec <- data.frame(low = rep(FALSE, 2), high = rep(FALSE, 2))
  rownames(exec) <- c("transition", "physical")
  for (i in scenario_no) {
    risk <- scenarios[[i]][["exec_short"]]
    exec[risk, scenarios[[i]][[risk]]] <- TRUE
  }
  if (layout == 1) {
    out_1 <- get_executive_summary_inputs(tabs, aggregated_inputs, inputs)
    out_2 <- get_executive_summary_scenarios(scenarios, exposure_classes, aggregated_inputs, inputs, scenario_no)
    out_3 <- get_executive_summary_exposures(exposure_classes, aggregated_inputs, inputs, scenario_no, exec)
  } else if (layout == 2) {
    # sector report - no inputs and no exposures
    out_1 <- ""
    out_2 <- get_executive_summary_scenarios(scenarios, exposure_classes, aggregated_inputs, inputs, scenario_no, TRUE)
    out_3 <- ""
  } else if (layout == 3) {
    # scenario report - no inputs and no exposures
    out_1 <- ""
    out_2 <- get_executive_summary_scenarios(scenarios, exposure_classes, aggregated_inputs, inputs, scenario_no)
    out_3 <- ""
  } else {
    stop("Invalid layout parameter")
  }
  # the final output is a string
  return(paste0(out_0, out_1, out_2, out_3))
}

#' Helper function that translate the value of input field to the scenario number(s)
#'
#' @inherit get_standard_report_contents
#' @return vector of integers
#'
get_scenario_no <- function(scenarios, report_scenario_selection, is_rtf) {
  if (report_scenario_selection == "") {
    scenario_no <- which(sapply(scenarios, function(sce) !is.null(sce$name)))
  } else {
    scenario_no <- which(sapply(scenarios, `[[`, i = "name") == report_scenario_selection)
  }
  return(scenario_no)
}

#' Helper function that returns non-scenario section number(s)
#'
#' @inherit get_standard_report_contents
#' @param sections List of non-scenario report sections
#'
get_section_no <- function(sections, is_rtf, rep_type) {
  if (is_rtf) {
    indicator_function <- function(s) s$include_in_RTF
  } else {
    indicator_function <- function(s) s$include_in_HTML
  }
  # if relevant, additionally check rep_type condition
  if (!is.null(rep_type)) {
    indicator_function_2 <- function(s) {
      if (is.null(s$rep_type)) {
        return(indicator_function(s))
      } else if (s$rep_type == rep_type) {
        return(indicator_function(s))
      } else {
        return(FALSE)
      }
    }
  } else {
    # otherwise do not change the function
    indicator_function_2 <- indicator_function
  }
  return(which(sapply(sections, indicator_function_2)))
}

#' Function that generates a markdown content of the report
#'
#' @param tabs List of tabs (required for the executive summary)
#' @param scenarios List of scenarios
#' @param sections List of non-scenario report sections
#' @param exposure_classes List of exposure classes (sectors)
#' @param inputs data frame of all relevant inputs (the reactive expression all_inputs or its subset obtained with get_inputs)
#' @param report_scenario_selection (user-friendly) scenario name (or empty string)
#' @param is_rtf a flag that triggers several format specific settings:
#' - TRUE: include non-scenario sections (e.g. intro)
#' - FALSE: include the links to page top
#' @param exec_summary_layout determines the structure of the executive summary (see get_executive_summary function)
#' @param include_exposures whether to include tables with contributing exposures
#' @param rep_type additional possibility to filter report sections, by default (NULL) no filtering
#' @return vector of string - executive summary text (3 items + 1 per scenario + 1 item at the end)
#'
get_standard_report_contents <- function(tabs,
                                         scenarios,
                                         sections,
                                         exposure_classes,
                                         inputs,
                                         report_scenario_selection,
                                         is_rtf,
                                         exec_summary_layout = 1,
                                         include_exposures,
                                         rep_type = NULL) {
  aggregated_inputs_by_item <- aggregate_inputs(inputs)
  aggregated_inputs_by_group <- aggregate_inputs(inputs, "group")
  scenario_no <- get_scenario_no(scenarios, report_scenario_selection, is_rtf)
  section_no <- get_section_no(sections, is_rtf, rep_type)
  out <- list()
  for (scenario in scenarios[scenario_no]) {
    out <- c(
      out,
      list(get_scenario_descriptions(
        aggregated_inputs_by_item,
        aggregated_inputs_by_group,
        inputs,
        scenario,
        exposure_classes,
        include_exposures
      ))
    )
  }
  for (section in sections[section_no]) {
    out <- c(
      out,
      list(get_section_descriptions(
        section,
        list(
          aggregated_inputs_by_item = aggregated_inputs_by_item,
          aggregated_inputs_by_group = aggregated_inputs_by_group,
          inputs = inputs,
          scenario_no = scenario_no,
          exec_summary_layout = exec_summary_layout,
          scenarios = scenarios,
          exposure_classes = exposure_classes,
          tabs = tabs
        )
      ))
    )
  }
  # order the scenario and non-scenario sections
  scenario_pos <- sapply(scenarios, function(sce) sce$position)[scenario_no]
  section_pos <- sapply(sections, function(s) s$position)[section_no]
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
get_executive_summary_exposures <- function(exposure_classes,
                                            aggregated_inputs,
                                            inputs,
                                            scenario_no,
                                            exec) {
  out_exp <- "## Summary of exposures{.unlisted .unnumbered}\n\nThis report considers the following exposures:\n\n"
  out_exp <- paste0(out_exp, "### High materiality exposures\n\n")
  high_counter <- 0
  A_or_L_header <- (length(unique(aggregated_inputs$A_or_L[aggregated_inputs$materiality_num == "High"])) > 1)
  for (i in 1:nrow(aggregated_inputs)) {
    if (A_or_L_header && (i == 1 || aggregated_inputs$A_or_L[i] != aggregated_inputs$A_or_L[i - 1])) {
      if (high_counter == 0 && i > 1) {
        out_exp <- paste0(out_exp, "None\n\n")
      }
      high_counter <- 0
      out_exp <- paste0(
        out_exp,
        "#### Summary of ",
        ifelse(aggregated_inputs$A_or_L[i] == "A", "Assets", "Liabilities"),
        "\n\n"
      )
    }
    item <- aggregated_inputs$item[i]
    materiality <- aggregated_inputs$materiality_num[i]
    if (materiality == "High") {
      high_counter <- high_counter + 1
      out_exp <- paste0(
        out_exp,
        "##### Summary of ",
        exposure_classes[[item]][["name"]],
        "\n\n"
      )
      for (risk in rownames(exec)) {
        for (risk_intensity in colnames(exec)) {
          if (exec[risk, risk_intensity]) {
            out_exp <- paste0(
              out_exp,
              exposure_classes[[item]][[risk]][[risk_intensity]]["always"],
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
  less_material <- aggregated_inputs[aggregated_inputs$materiality_num != "High", ]
  groups <- setdiff(unique(less_material$exposure_group), "")
  if (nrow(less_material)) {
    less_material$risk.description <- rep(NA, nrow(less_material))
    less_material$name <- rep(NA, nrow(less_material))
    for (i in 1:nrow(less_material)) {
      item <- less_material$item[i]
      less_material$name[i] <- exposure_classes[[item]][["name"]]
    }
    for (i in 1:ncol(less_material)) {
      colnames(less_material)[i] <- capitalize(colnames(less_material)[i])
    }
    for (i in seq_along(groups)) {
      group_rows <- less_material$Exposure_group == groups[i]
      group_data <- less_material[group_rows, ]
      group_data$Materiality <- as.character(group_data$Materiality)
      group_data$Materiality_num <- as.character(group_data$Materiality_num)
      aggregate_row <- stats::aggregate(
        cbind(Materiality, Materiality_num, Name) ~ A_or_L + Exposure_group,
        data = group_data,
        FUN = function(x) paste(as.character(x), collapse = "<br>")
      )
      aggregate_row$Item <- groups[i]
      aggregate_row$Risk.description <- NA
      less_material <- rbind(
        less_material[!less_material$Exposure_group == groups[i], ],
        aggregate_row
      )
    }
    for (i in 1:nrow(less_material)) {
      item <- less_material$Item[i]
      risk_description <- ""
      for (risk in rownames(exec)) {
        for (risk_intensity in colnames(exec)) {
          if (exec[risk, risk_intensity]) {
            risk_description <- paste(
              risk_description,
              exposure_classes[[item]][[risk]][[risk_intensity]][["exec_description"]],
              sep = "<br>"
            )
          }
        }
      }
      risk_description <- substring(risk_description, 5)
      less_material$Risk.description[i] <- risk_description
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

#' Helper function - the name is self-explanatory
#'
#' @param data matrix or data.frame, possibly containing missing value
#' @param empty_strings which strings should be considered as "empty"?
#' @param ignore_cols indices of columns which should be ignored for emptiness check
#' @return input object without rows and columns where all entries are empty (i.e. NA or ""). NULL when the input is all empty
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
get_executive_summary_inputs <- function(tabs, aggregated_inputs, inputs) {
  out <- "## Summary of inputs{.unlisted .unnumbered}\n\n"
  for (tab in tabs) {
    if (!is.null(tab$exposure)) {
      ids <- get_input_ids(tab = tab)
      values <- get_input_values(inputs, ids)
      values <- cbind(tab$exposure[, 1], values)
      colnames(values)[1] <- ""
      values_trimmed <- delete_empty_rows_and_columns(values, ignore_cols = 1)
      if (!is.null(values_trimmed)) {
        transpose <- (ncol(tab$exposure) > 3 & nrow(tab$exposure) > 1)
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
          "#### ",
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
            table_to_markdown_multiline(
              values_trimmed[, cols_to_use],
              col_widths = c(30, rep(20, length(cols_to_use) - 1))
            )
          )
        }
      }
    }
  }
  return(out)
}

#' Helper function to merge lists of lists with the same (possibly nested) structure, ultimately containing strings
#'
#' @param list_1 list 1
#' @param list_2 list 2
#' @param name_1 first name (to be appended at the lowest level)
#' @param name_2 second name (to be appended at the lowest level)
#' @param sep separator between names and list contents
#'
paste_recursive <- function(list_1, list_2, name_1, name_2, sep = "\n\n") {
  if (length(list_1) > 1) {
    return(mapply(paste_recursive, list_1, list_2, MoreArgs = list(name_1 = name_1, name_2 = name_2), SIMPLIFY = FALSE))
  } else {
    return(paste0(name_1, sep, list_1, sep, name_2, sep, list_2))
  }
}

#' Karnan's request for easier change comparison - single sector
#'
#' @inherit get_standard_report_contents
#' @param item_name sector name (group level)
#' @param subitem_names names of the sectors (lower level), by default empty vector corresponding to no subitems
#'
get_exposure_test_description <- function(exposure_classes, item_name, subitem_names = c()) {
  exposure_class <- exposure_classes[[item_name]]
  out <- paste0(
    "## ",
    exposure_class$name,
    "\n\n",
    exposure_class$description,
    "\n\n"
  )

  if (length(subitem_names) == 0) {
    subitem_names <- item_name
  }

  for (risk in c("transition", "physical")) {
    for (risk_intensity in c("low", "high")) {
      exposure_classes[["temp"]] <- exposure_classes[[subitem_names[1]]]
      if (length(subitem_names) > 1) {
        for (subitem in subitem_names[-1]) {
          exposure_classes[["temp"]] <- mapply(paste_recursive, exposure_classes[["temp"]], exposure_classes[[subitem]], MoreArgs = list(name_1 = "", name_2 = exposure_classes[[subitem]][["name"]]), SIMPLIFY = FALSE)
        }
        exposure_classes[["temp"]][["name"]] <- gsub("\n", " ", exposure_classes[["temp"]][["name"]], fixed = TRUE)
      }
      out <- paste0(
        out,
        get_exposure_risk_description("temp", c(), "High", exposure_classes, risk, risk_intensity, TRUE)
      )
      exposure_classes[["temp"]] <- NULL
    }
  }
  return(out)
}

#' Karnan's request for easier change comparison - loop over all sectors
#'
#' @inherit get_standard_report_contents
#'
get_test_report <- function(exposure_classes) {
  out <- "# Test report\n\n"
  test_report_positions <- sapply(exposure_classes, function(ec) ec$test_report_position)
  test_order <- order(test_report_positions)
  for (i in test_order) {
    if (is.null(exposure_classes[[i]]$group)) {
      subitem_selection <- sapply(exposure_classes, function(ec) !is.null(ec$group) && ec$group == names(exposure_classes)[i])
      subitem_names <- names(exposure_classes)[subitem_selection]
      subitem_positions <- sapply(exposure_classes[subitem_selection], function(ec) ec$test_report_position)
      subitem_order <- order(subitem_positions)
      subitem_names <- subitem_names[subitem_order]
      out <- paste0(out, get_exposure_test_description(exposure_classes, names(exposure_classes)[i], subitem_names))
      out <- paste0(out, get_exposure_appendix(names(exposure_classes)[i], exposure_classes))
    }
  }
  out <- add_path_to_graphs(out)
  return(out)
}

#' Filter the inputs depending on institution type, sector. Optionally aggregate and override all materialities
#'
#' @param all_inputs_table data frame of all inputs (usually the reactive expression all_inputs)
#' @param filter_settings list of relevant setting, comprising of:
#' - inst_type institution type to filter (or "" for no filtering)
#' - sector sector to filter (or "" for no filtering)
#' - aggregate bool, whether to aggregate the inputs by sector
#' - override_materiality ignore the actual inputs and set all materialities to a level (no override by default)
#'
filter_inputs <- function(all_inputs_table, filter_settings) {
  inst_type <- filter_settings$inst_type
  sector <- filter_settings$report_sector_selection
  override_materiality <- filter_settings$override_materiality
  out <- all_inputs_table
  if (override_materiality != "") {
    out$materiality <- factor(override_materiality, levels = c("N/A", "Low", "Medium", "High"), ordered = T)
    out$materiality_num <- (as.integer(out$materiality) - 1)^2 + (as.integer(out$materiality) > 2)
  }
  if (inst_type != "") {
    out <- out[(which(out$type == inst_type & out$materiality != "N/A")), ]
  }
  if (sector != "") {
    out <- out[out$item == sector, ]
  }
  return(out)
}

#' Aggregate the inputs by sector
#'
#' @param inputs data frame of all inputs (usually the reactive expression all_inputs or its subset)
#' @param by level of aggregation (by default "item" ie sector, also possible "group")
aggregate_inputs <- function(inputs, by = "item") {
  if (nrow(inputs) == 0) {
    return(
      data.frame(
        A_or_L = character(),
        item = character(),
        exposure_group = character(),
        materiality = character(),
        materiality_num = character()
      )
    )
  }
  if (by == "group") {
    for (i in 1:nrow(inputs)) {
      if (inputs$exposure_group[i] != "") {
        inputs$item[i] <- inputs$exposure_group[i]
      }
    }
  }
  aggregated_inputs_factor <- stats::aggregate(materiality ~ A_or_L + item + exposure_group, FUN = max, data = inputs)
  aggregated_inputs_numeric <- stats::aggregate(
    materiality_num ~ A_or_L + item + exposure_group,
    FUN = aggregate_quantitative_to_qualitative_materiality,
    data = inputs
  )
  out <- merge(aggregated_inputs_factor, aggregated_inputs_numeric)
  out <- out[order(out$A_or_L, out$materiality, out$materiality_num, decreasing = TRUE), ]
  return(out)
}

#' Read the markdown section from relevant yaml file and save as output
#'
#' This function should be used on the server side.
#' Corresponding UI side call should be: uiOutput(output_name)
#' All links found in the markdown are modified to be open in the new page (by adding target = "_blank")
#'
#' @param output Shiny output
#' @param output_name Name of slot created in shiny output (to be used in UI part of shiny app)
#' @param section_name Name of yaml file within section directory read from
#' (description section only, other YAML fields are ignored)
#'
include_markdown_section <- function(output, output_name, section_name) {
  text <- unlist(
    strsplit(
      global$sections[[section_name]]$description,
      "\n"
    )
  )
  include_markdown_text(text, output, output_name)
}

#' Converts text to HTML and passes to the output
#'
#' @inherit include_markdown_section
#' @param text text to be converted
#' @param add_new_tab_ref if TRUE (the default) all the links will be appended with target="_blank" (to open in the new window)
#'
#' @importFrom markdown markdownToHTML
#' @importFrom stringi stri_replace_all_regex
#'
include_markdown_text <- function(text, output, output_name, add_new_tab_ref = TRUE) {
  html_text <- markdown::markdownToHTML(
    text = text,
    fragment.only = TRUE
  )
  if (add_new_tab_ref) {
    html_text <- stringi::stri_replace_all_regex(html_text, "(<a href=\"[:graph:]*\")>", "$1 target=\"_blank\" >")
  }
  output[[output_name]] <- renderUI({
    HTML(html_text)
  })
}

#' Process the HTML file manually to correct some known issues
#'
#' Includes fixing image links (so they work in shiny)
#' and adding arrow to all headers (report version 5 only)
#'
#' @param file the location of HTML file
#'
html_postprocess <- function(file) {
  # replace back the images links
  file_conn <- file(file)
  temp <- readLines(file_conn)
  www_path <- system.file("www", package = "climate.narrative")
  if (www_path == "") {
    www_path <- paste0(getwd(), "/inst/www")
  }
  temp <- gsub(
    www_path,
    "climate_narrative",
    temp
  )
  close(file_conn)
  separate_toc(file, temp)
  return(invisible(NULL))
}

#' Create a single list of settings from simple arguments
#'
#' @param content_files List of necessary global lists with report contents
#' @param output_file Path and filenamename of report to write
#' @param md_file Path and filename of intermediate markdown file
#' @param file_format Currently either "html" or "rtf"
#' @param dev Development mode flag (used only to decide if a warning is raised in a lower level function)
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
                                dev,
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
    output_format <- rmarkdown::html_document(
      toc = TRUE,
      toc_depth = 2,
      toc_float = FALSE,
      self_contained = FALSE,
      fig_caption = FALSE
    )
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
    image_width_fix = TRUE,
    image_min_pixels_to_rescale = 300,
    image_max_height = 4
  )

  postprocess_settings <- list(
    file_format = file_format,
    output_file = output_file,
    dev = dev
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
#' The highest level function for report production. Takes only three arguments, passes the first two ones to the actual report
#' producing function (either using promise or not, depending on the third argument)
#'
#' @param all_inputs Table of user inputs
#' @param settings list of lists containing all necessary settings
#' @param async whether to use promises to delegate report production to a new thread. FALSE by default for backward compatibility
#' @param sleep number of seconds to sleep during report production. Useful for checking async functionality
#' @importFrom promises future_promise
#' @return NULL if async=FALSE, promise if async=TRUE. The actual report is produced to file in both cases
#'
produce_report <- function(all_inputs, settings, async = FALSE, sleep = 0) {
  if (async) {
    return(
      promises::future_promise({
        produce_report_(all_inputs, settings, sleep)
      })
    )
  } else {
    produce_report_(all_inputs, settings, sleep)
    return(invisible(NULL))
  }
}

#' Function where actual report production takes place
#'
#' @inherit produce_report
#' @return NULL, report produced to file
#'
produce_report_ <- function(all_inputs, settings, sleep) {
  alternative_sleep(sleep)
  content_files <- settings$content_files
  filter_settings <- settings$filter_settings
  content_settings <- settings$content_settings
  render_settings <- settings$render_settings
  image_settings <- settings$image_settings
  postprocess_settings <- settings$postprocess_settings
  inputs <- filter_inputs(all_inputs, filter_settings)
  report_contents <- get_report_contents(
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
    format_images(render_settings$md_file, image_settings)
  }
  rmarkdown::render(
    input = render_settings$md_file,
    output_file = render_settings$output_file,
    output_format = render_settings$output_format
  )
  postprocess(postprocess_settings)
  return(invisible(NULL))
}

#' Gets either a standard or test report contents
#'
#' @param content_files list of all yaml content files
#' @param inputs user dropdown choices
#' @param content_settings all configuration options (see get_standard_report_content for details)
#'
get_report_contents <- function(content_files, inputs, content_settings) {
  if (content_settings$rep_type %in% c("inst", "sect")) {
    return(
      get_standard_report_contents(
        content_files$tabs,
        content_files$scenarios,
        content_files$sections,
        content_files$exposure_classes,
        inputs,
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

#' Wrapper calling either rtf_postprocess or html_postprocess
#'
#' @param postprocess_settings a list with:
#' - file_format (either "rtf" or "html")
#' - output_file (path to the file)
#' - dev (flag, TRUE for developer version of the tool, passed to RTF only)
#'
postprocess <- function(postprocess_settings) {
  if (postprocess_settings$file_format == "rtf") {
    rtf_postprocess(postprocess_settings$output_file, postprocess_settings$dev)
  } else {
    html_postprocess(postprocess_settings$output_file)
  }
}

#' Helper function to separate the table of contents from the main document
#'
#' @param filename HTML file to parse
#' @param file_contents (optionally) conents of the HTML file (if already parsed, to avoid duplicating this work)
#' @param toc_label a string to label the table of contents (or NULL for no label)
#' @return NULL. The HTML file given as argument filename is updated as file without ToC,
#' which is saved as a separate HTML in the same directory as filename (with the name appended with "_toc")
#'
separate_toc <- function(filename, file_contents = NULL, toc_label = "Table of contents") {
  if (is.null(file_contents)) {
    file_conn <- file(filename)
    file_contents <- readLines(file_conn)
    close(file_conn)
  }
  toc_start <- grep("<div id=\"TOC\">", file_contents)
  div_end <- grep("</div>", file_contents)
  toc_end <- min(div_end[div_end > toc_start])
  toc <- file_contents[toc_start:toc_end]
  if (!is.null(toc_label)) {
    toc <- c(
      "<label>",
      toc_label,
      "</label>",
      toc
    )
  }
  no_toc <- file_contents[-(toc_start:toc_end)]
  file_conn <- file(filename)
  writeLines(no_toc, file_conn)
  close(file_conn)
  file_conn <- file(paste0(substr(filename, 1, nchar(filename) - 5), "_toc.html"))
  writeLines(toc, file_conn)
  close(file_conn)
  return(invisible(NULL))
}

#' Parse the yaml file, replace the selected piece and save (overwrite)
#'
#' Note: the function currently works for literal style (i.e. header ends with pipe symbol | after the colon)
#'
#' @param yaml_file_location location of YAML file to update.
#' @param section_subsection vector of strings - recursively go down the YAML structure to find the desired location.
#' @param new_text new content.
#'
replace_yaml_subsection <- function(yaml_file_location, section_subsection, new_text) {
  # Read yaml file first
  file_conn <- file(yaml_file_location)
  yaml_file <- readLines(file_conn)
  close(file_conn)

  # Find the (sub)section
  subsection_location <- find_yaml_subsection(yaml_file, section_subsection)
  # Ensure the section is written in literal style (this is implicit assumption of the function so far)
  section_header <- yaml_file[subsection_location$start - 1]
  if (!grepl("|", section_header, fixed = TRUE)) {
    stop("The section seems not to be written in literal YAML style. Function replace_yaml_subsection will not work as expected")
  }
  old_text_indented <- paste(yaml_file[subsection_location$start:subsection_location$end], collapse = "\n")
  # Indent the new text appropriately
  new_text_indented <- gsub(
    "([[:graph:]])((\\n)+)([[:graph:]])",
    paste0("\\1\\2", subsection_location$indentation, "\\4"),
    new_text
  )
  # Define updated yaml as concatenation of updated subsection and the remaining (unchanged) parts
  yaml_file_updated <- paste0(subsection_location$indentation, new_text_indented)
  new_text_indented <- strsplit(new_text_indented, "\n")
  if (subsection_location$start > 1) {
    yaml_file_updated <- c(yaml_file[1:(subsection_location$start - 1)], yaml_file_updated)
  }
  if (subsection_location$end < length(yaml_file)) {
    yaml_file_updated <- c(yaml_file_updated, yaml_file[(subsection_location$end + 1):length(yaml_file)])
  }
  # Overwrite the previous file
  file_conn <- file(paste0(yaml_file_location))
  writeLines(yaml_file_updated, file_conn)
  close(file_conn)
}

#' Parse the (subset of) YAML text and find the matching section
#'
#' @param string YAML text to parse.
#' @param start First line of parsing (choose 1 to parse the text from the start)
#' @param end Last line of parsing (choose length(string) to parse the text to the end)
#' @param indentation The indentation of the whole block of text to strip first.
#' @param section_name The header to look for.
#' @importFrom stringr str_extract
#' @return List with the following named elements:
#'   start (first line of section found, excluding the name),
#'   end (last line of section found)
#'   indentation (indentation applied within the section)
#'
find_yaml_section <- function(string, start, end, indentation, section_name) {
  header_rows_1 <- grep(paste0("^", indentation, "[[:graph:]]+", ":", "[[:space:]]"), string[start:end])
  header_rows_2 <- grep(paste0("^", indentation, "[[:graph:]]+", ":", "$"), string[start:end])
  header_rows <- sort(c(header_rows_1, header_rows_2))
  headers <- gsub(
    paste0(
      indentation,
      "(([[:graph:]]|[[:blank:]])+):([[:graph:]]|[[:blank:]])*$"
    ),
    "\\1",
    string[header_rows + start - 1]
  )
  index <- which(headers == section_name)
  if (length(index) == 0) {
    stop(
      paste0(
        "Function find_yaml_section. No matching section found named: ",
        section_name
      )
    )
  } else if (length(index) > 1) {
    warning(
      paste0(
        "Multiple matching sections found named: ",
        section_name
      )
    )
  }
  indentation <- stringr::str_extract(string[header_rows[index] + start], "^(([[:blank:]])*)([[:graph:]])")
  indentation <- substring(indentation, 1, nchar(indentation) - 1)
  return(
    list(
      start = (header_rows[index] + start),
      end = (header_rows[index + 1] + start - 2),
      indentation = indentation
    )
  )
}

#' Recursively applies find_yaml_section function to find the subsection in a nested structure
#'
#' @param string String to parse.
#' @param section_subsection The vector of string. Its lenght represents level of nesting, each value is a name
#'   of section at corresponding level (starting from the top level)
#'
find_yaml_subsection <- function(string, section_subsection) {
  start <- 1
  end <- length(string)
  indentation <- ""
  for (s in section_subsection) {
    out <- find_yaml_section(string, start, end, indentation, s)
    start <- out$start
    end <- out$end
    indentation <- out$indentation
  }
  return(out)
}

#' Replacement for Sys.sleep()
#'
#' @param time Time to sleep (in seconds)
#' @importFrom stats rnorm
#'
alternative_sleep <- function(time) {
  end_time <- Sys.time() + time
  while (Sys.time() < end_time) {
    # some time consuming operation, but preferably not much memory consuming
    rnorm(1)
  }
}

#' Helper function that transforms descriptive (factor) to numeric materiality
#'
#' @param materiality Descriptive materiality, factor with following levels:
#'   - 1: "N/A"
#'   - 2: "Low"
#'   - 3: "Medium"
#'   - 4: "High"
#' @return Integer corresponding to the percentage threshold (0, 1, 5, 10 respectively)
#'
materiality_num <- function(materiality){
  (as.integer(materiality) - 1)^2 + (as.integer(materiality) > 2)
}