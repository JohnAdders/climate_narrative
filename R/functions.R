#' Remove special characters (in particular spaces) from a string 
#' 
#' Additionally (optionally) make it camelcase.
remove_special_characters <- function(text, make_camelcase=TRUE) {
  out <- text
  if(make_camelcase){
    out <- gsub('\\ (\\w?)', '\\U\\1', tolower(out), perl=TRUE)
    out <- gsub('\\_(\\w?)', '\\U\\1', out, perl=TRUE)
    out <- gsub('\\.(\\w?)', '\\U\\1', out, perl=TRUE)
  }
  gsub("[_. ]", "", out)
}

#' Read all files from a directory as a named R list
#' 
#' Handles yaml/csv/R files.
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
        yml = yaml::read_yaml(paste0(directory, "/", file)),
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
tab_name_to_number <- function(tab_name){
  as.integer(factor(tab_name, global$ordered_tabs))
}


#' Add element to the list (shortcut)
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

#' Make the first letter of a string upper case
capitalize <- function(input_string) {
  return(paste0(toupper(substring(input_string, 1, 1)), substring(input_string, 2)))
}

#' Format (camelcase) string in order to look better in final output (spaces, capitalisation)
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
  for(i in 1:nrow(substitutions)){
    s <- gsub(substitutions$from[i], substitutions$to[i], s, fixed = TRUE)
  }
  s
}

#' Produce a matrix of tooltips (strings) by concatenating column-specific (if any)
#' 
#' and product-specific text (if any)
produce_tooltip_matrix <- function(exposure_matrix) {
  out <- matrix(
    "",
    nrow = nrow(exposure_matrix),
    ncol = ncol(exposure_matrix) - 2
  )
  for (i in 1:nrow(out)){
    row_tooltip <- global$products[[remove_special_characters(exposure_matrix[i, 2])]][["tooltip"]]
    for (j in 1:ncol(out)){
      exposure_class <- exposure_matrix[i, j + 2]
      if (exposure_class != ""){
        exposure_class_tooltip <- global$exposure_classes[[exposure_class]][["tooltip"]]
        if (!is.null(exposure_class_tooltip)){
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
        tippy::tippy_this(id, tooltip_text),
      ))
    } else {
      return(form)
    }
  }
}

#' Grid table of inputs (everything happens in matching server function)
exposure_grid_ui <- function(label) {
  tableOutput(label)
}

#' Produce a table of inputs with selectInput fields
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
          tooltip_matrix[i,j-1],
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

#' Insert spaces to the string so that line has exactly given number of characters
string_break_line_with_spaces <- function(string, line_width, location, n_char=1){
  paste0(
    substring(string, 1, location - 1),
    paste(rep(" ", (1-location) %% line_width), collapse=""),
    substring(string, location + n_char)
  )
}

#' Add spaces to a string so that it can be split into blocks of exactly the same length
#' 
#' without breaking words
string_add_spaces_to_make_equal_lines <- function(string, line_width){
  out <- string
  newline_locations <- na.omit(stringi::stri_locate_all(out, fixed = "<br>")[[1]][,1])
  i <- 1
  while(i * line_width < nchar(out)){
    for(loc in newline_locations[newline_locations <= 1 + i * line_width]){
      out <- string_break_line_with_spaces(out, line_width, loc, 4)
    }
    space_locations <- stringi::stri_locate_all(out, fixed = " ") [[1]][,1]
    last_space <- na.omit(max(space_locations[space_locations <= 1 + line_width * i]))
    if(length(last_space)) out <- string_break_line_with_spaces(out, line_width, last_space, 1)
    newline_locations <- na.omit(stringi::stri_locate_all(out, fixed="<br>")[[1]][,1])
    i <- i + 1
  }
  return(out)
}

#' Format the string by appending spaces so it exactly fills the lines of given length
#' 
#' additionally, if the string contains at least one "br" tag
#' format output as a bulleted list
string_format_lines <- function(string, col_width){
  if (grepl("<br>", string)){
    out <- paste0("- ",gsub("<br>", "<br> - ", string))
  } else {
    out <- string
  }
  out <- string_add_spaces_to_make_equal_lines(out, col_width)
  return(out)
}

#' Produce a markdown table out of R data frame
#' 
#' Create a markdown table, allowing multiline cell entries (lines need to be separated by br tag)
#' R table headers cannot contain spaces, to get space in the output use a dot
#' (it will be replaced with space if dot_to_space=T as in default)
#' The function splits the text automatically and adds spaces to match the desired column width
#' without breaking words
table_to_markdown_multiline <- function(table, dot_to_space = TRUE, col_widths=NULL) {
  headers <- colnames(table)
  if(is.null(col_widths)){
    col_widths <- pmax(apply(table, 2, function(x) max(nchar(x))), nchar(headers)) + 4
  }
  if (dot_to_space) {
    headers <- gsub(".", " ", headers, fixed = TRUE)
  }
  for(i in 1:ncol(table)) {
    table[,i] <- as.character(table[,i])
    table[,i] <- gsub("\n", " ", table[,i])
    for (j in 1:nrow(table)) {
      table[j,i] <- string_format_lines(table[j,i], col_widths[i] - 2)
    }
  }
  split_rows <- ceiling(c(
    max(nchar(headers) / (col_widths - 2)),
    apply(table, 1, function(x) max(nchar(x)/(col_widths - 2)))
  ))
  out <- matrix("", nrow=0,ncol=ncol(table))
  emptyline <- rep("", ncol(table))
  sepline <- emptyline
  for (i in 1:length(sepline)) {
    sepline[i] <- paste0(paste(rep("-", col_widths[i]), collapse=""), "+")
  }
  sepline[1] <- paste0("+", sepline[1])
  rowsout <- matrix(emptyline, nrow=split_rows[1], ncol=length(emptyline), byrow=F)
  for (i in 1:ncol(out)) {
    cell_text <- string_format_lines(headers[i], col_widths[i] - 2)
    cell_text <- paste0(
      cell_text,
      paste(rep(" ", (col_widths[i] - 2) * split_rows[1] - nchar(cell_text)), collapse="")
    )
    for(k in 1:split_rows[1]){
      rowsout[k,i] <- paste0(
        " ",
        substr(cell_text, 1 + (k - 1) * (col_widths[i] - 2), k * (col_widths[i] - 2)),
        " |"
      )
      if(i == 1) rowsout[k, i] <- paste0("|", rowsout[k, i])
    }
  }
  out <- rbind(out, rowsout, gsub("-", "=", sepline))
  for (j in 1:nrow(table)){
    rowsout <- matrix(emptyline, nrow=split_rows[j + 1], ncol=length(emptyline), byrow=F)
    for (i in 1:ncol(out)) {
      cell_text<- table[j, i]
      temp2 <- paste0(
        cell_text,
        paste(rep(" ", (col_widths[i] - 2) * split_rows[j + 1] - nchar(cell_text)), collapse="")
      )
      for (k in 1:split_rows[j+1]){
        rowsout[k,i] <- paste0(
          " ",
          substr(temp2, 1 + (k - 1)*(col_widths[i] - 2), k * (col_widths[i] - 2)),
          " |"
        )
        if(i == 1) rowsout[k, i] <- paste0("|", rowsout[k, i])
      }
    }
    out <- rbind(out, rowsout, sepline)
  }

  out2 <- paste0(
    paste(sepline,collapse=""),
    "\n",
    paste(apply(out, 1, paste, collapse=""), collapse="\n"),
    "\n"
  )
  return(out2)
}

#' A simple version of function to produce markdown tables from R table
#' 
#' It does not handle multiline cells
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
get_exposure_description <- function(item, type_item_inputs) {
  if (is.null(global$exposure_classes[[item]])) warning(paste("No exposure class file for ", item))
  ordered_type_item_inputs <- type_item_inputs[order(type_item_inputs$materiality), ]
  # conversion from factor back to string to ensure proper printing below
  ordered_type_item_inputs$materiality <- as.character(ordered_type_item_inputs$materiality)
  # add unique identifier if rownames are not unique (i.e. the same item in multiple columns)
  ordered_type_item_inputs$rowname_unique <- ordered_type_item_inputs$rowname
  duplicates <- which(duplicated(ordered_type_item_inputs$rowname)|duplicated(ordered_type_item_inputs$rowname, fromLast = TRUE))
  for (i in duplicates){
    ordered_type_item_inputs$rowname_unique[i] <- paste0(
      ordered_type_item_inputs$rowname[i],
      " (",
      capitalize(ordered_type_item_inputs$colname[i]),
      ")"
    )
  }
  ordered_aggregate_inputs_text <- aggregate(
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
  ordered_aggregate_inputs_num <- aggregate(
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
get_exposure_appendix <- function(item){
  appendix <- global$exposure_classes[[item]][["appendix"]]
  if(is.null(appendix)){
    return (c())
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

#' Lower level report helper function responsible for single risk (transition/physical) description
get_exposure_risk_description <- function(item, products, materiality, physical_or_transition, high_or_low) {
  if (high_or_low == FALSE) {
    return("")
  }

  # define header depending on physical/transition and low/high
  if (physical_or_transition == "transition") {
    riskname <- switch(high_or_low, high = "Disorderly transition", low = "Orderly transition")
  } else {
    riskname <- switch(high_or_low, high = "High physical risk", low = "Low physical risk")
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
get_scenario_descriptions <- function(aggregated_table, type_inputs, scenario) {
  if(is.null(scenario)) warning(paste("No scenario file for ", scenario))
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
get_references <- function(aggregated_table, type_inputs) {
  out <- ""
  if (nrow(aggregated_table)) {
    out <- paste0(
        out,
        "# References\n\n"
      )
    for (i in 1:nrow(aggregated_table)) {
      item <- aggregated_table$item[i]
      if (length(global$exposure_classes[[item]][["references"]])){
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
  if(out == "# References\n\n"){
    out = ""
  }
  return(out)
}

#' Heartbeat function (server part) to prevent app closing due to inactivity
heartbeat <- function(input, output, session) {
  beep <- reactiveTimer(55 * 1000)
  output[["__heartbeat"]] <- renderText({
    beep()
    " "
  })
}

#' Heartbeat function (ui part) to prevent app closing due to inactivity
heartbeat_footer <- function() {
  list(
    hr(),
    tag("footer",
      list(
        p("Copyright 2021 The Climate Financial Risk Forum"),
        p(
          a(href="https://github.com/JohnAdders/climate_narrative", "Source Code", target="_blank"),
          " | ",
          a(href="mailto:john.adcock@aviva.com?subject=Climate%20Narrative%20support%20request", "Beta Support"),
          " | ",
          a(href="https://github.com/JohnAdders/climate_narrative/issues?q=is%3Aissue+is%3Aopen+label%3Abug", "Known Issues", target="_blank"),
          " | ",
          a(href="https://github.com/JohnAdders/climate_narrative/wiki/Contributors", "Contributors", target="_blank")
        )
      )
    )
  )
}

#' Google captcha function (UI part)
#' 
#' based on
#' https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R
GreCAPTCHAv3Ui <- function(siteKey) {
  tagList(tags$head(
    tags$script(src = paste0("https://www.google.com/recaptcha/api.js?render=", siteKey)),
  ))
}

#' Google captcha function (actual javascript call)
#' 
#' based on
#' https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R
GreCAPTCHAv3js <- function(siteKey, action, fieldID) {
  shinyjs::runjs(paste0("
        grecaptcha.ready(function () {
          grecaptcha.execute('", siteKey, "', { action: '", action, "' }).then(function (token) {
			      Shiny.onInputChange('", fieldID, "',token);
      		});
	      });
      "))
}

#' Google captcha function (server part)
#' 
#' based on
#' https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R
GreCAPTCHAv3Server <- function(secretKey, reCaptchaResponse) {
  gResponse <- httr::POST(
    "https://www.google.com/recaptcha/api/siteverify",
    body = list(
      secret = secretKey,
      response = reCaptchaResponse
    )
  )

  if (gResponse$status_code == 200) {
    return(jsonlite::fromJSON(httr::content(gResponse, "text")))
  }
}

#' Produce a footer HTML text explaining materiality levels
generic_footer <- function(asset_or_liability = c("asset","liability"), is_asset_mananger = FALSE){
  if(asset_or_liability == "asset"){
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
      paste0("Enter your firm's exposures by", case_name, "using the following definitions:"),
      tags$ul(
        tags$li(paste("\"High\": One of your top 5 exposures or more than 10% of total", total_name)),
        tags$li(paste("\"Medium\": 5% - 10% of total", total_name)),
        tags$li(paste("\"Low\": below 5% of total", total_name)),
        tags$li(paste("blank: immaterial or no exposure"))
      )
    )
  )
}
