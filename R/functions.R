# helper function - a shortcut function to add element to the list
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

# helper function to get object by its name but return NULL (not error) if it does not exist
get_or_null <- function(name) {
  if (exists(name)) {
    return(get(name))
  } else {
    return(NULL)
  }
}

# helper function to make the first letter of a string upper case
capitalize <- function(input_string) {
  return(paste0(toupper(substring(input_string, 1, 1)), substring(input_string, 2)))
}

restore_spaces <- function(camelcase) {
  s <- gsub("([A-Z])([a-z])", " \\1\\L\\2", camelcase, perl = TRUE)
  sub("^ ", "", s) # remove first space
}

produce_tooltip_matrix <- function(exposure_matrix) {
  out <- matrix(
    "",
    nrow = nrow(exposure_matrix),
    ncol = ncol(exposure_matrix) - 2
  )
  for (i in 1:nrow(out)){
    row_tooltip <- products[[remove_special_characters(exposure_matrix[i, 2])]][["tooltip"]]
    for (j in 1:ncol(out)){
      exposure_class <- exposure_matrix[i, j + 2]
      if (exposure_class != ""){
        exposure_class_tooltip <- exposure_classes[[exposure_class]][["tooltip"]]
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
# helper functions to produce the layout of tabs (cell, row, whole table)
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

exposure_grid_ui <- function(label) {
  tableOutput(label)
}

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

string_add_spaces_to_make_equal_lines <- function(string, line_width){
# this function adds spaces so that string can be split into blocks of exactly the same length
# without breaking words
  out <- string
  locations <- str_locate_all(out, " ") [[1]][,1]
  i <- 1
  while(i * line_width < nchar(out)){
    last_space <- max(locations[locations <= 1 + line_width*i])
    out <- paste0(
      substring(out, 1, last_space - 1),
      paste(rep(" ", (1 - last_space) %% line_width), collapse=""),
      substring(out, last_space + 1)
    )
    locations <- str_locate_all(out, " ") [[1]][,1]
    i <- i + 1
  }
  return(out)
}

string_replace_newline_with_spaces <- function(string, line_width){
# the function replaces "<br>" string with the number of spaces to fill the line to the end
  out <- string
  locations <- str_locate_all(out, "<br>") [[1]]
  if (nrow(locations)) {
    locations <- locations[locations[,1] > 1 & locations[,1] < nchar(out) - 6 + 1, 1]
  }
  while (length(locations)){
    out <- paste0(
      substring(out, 1, locations[1] - 1),
      paste(rep(" ", (1-locations[1]) %% line_width), collapse=""),
      substring(out, locations[1] + 4)
    )
    locations <- str_locate_all(out, "<br>")[[1]]
    if (nrow(locations)) locations <- locations[locations[,1] > 1 & locations[,1] < nchar(out),1]
  }
  return (out)
}

string_format_lines <- function(string, col_width){
# the function formats the string by appending spaces so it exactly fills the lines of given length
# Additionally, if the string contains at least one "<br>" the function formats output as bulleted list
  if (grepl("<br>", string)){
    out <- paste0("- ",gsub("<br>", "<br> - ", string))
  } else {
    out <- string
  }
  out <- string_replace_newline_with_spaces(out, col_width)
  out <- string_add_spaces_to_make_equal_lines(out, col_width)
  return(out)
}

table_to_markdown_multiline <- function(table, dot_to_space = TRUE, col_widths=NULL) {
# helper function to produce a markdown table out of R data frame
# creates a markdown table, allowing multiline cell entries (lines need to be separated by <br>)
# R table headers cannot contain spaces, to get space in the output use a dot
# (it will be replaced with space if dot_to_space=T as in default)
# function splits the text automatically and adds spaces to match the desired column width
# without breaking words
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
      table[j,i] <- string_format_lines(table[j,i], col_widths[i]-2)
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
    sepline[i] <- paste0(paste(rep("-", col_widths[i]), collapse=""),"+")
  }
  sepline[1] <- paste0("+", sepline[1])
  rowsout <- matrix(emptyline, nrow=split_rows[1], ncol=length(emptyline), byrow=F)
  for (i in 1:ncol(out)) {
    cell_text <- string_format_lines(headers[i], col_widths[i]-2)
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

# a simpler version of function to produce markdown tables from R table
# does not handle multiline cells
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

get_exposure_description <- function(item, type_item_inputs) {
  if(is.null(exposure_classes[[item]])) warning(paste("No exposure class file for ", item))
  ordered_type_item_inputs <- type_item_inputs[order(type_item_inputs$materiality), ]
  # conversion from factor back to string to ensure proper printing below
  ordered_type_item_inputs$materiality <- as.character(ordered_type_item_inputs$materiality)
  ordered_aggregate_inputs <- aggregate(
    ordered_type_item_inputs[, c("rowname", "materiality")],
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
  colnames(ordered_aggregate_inputs)[3:4] <- c("Exposure.row", "Materiality")  
  out <- paste0(
    "## ",
    exposure_classes[[item]][["name"]],
    "\n\n",
    exposure_classes[[item]][["description"]],
    "\n\nThe following rows contribute: \n\n",
    table_to_markdown_multiline(ordered_aggregate_inputs, TRUE, c(15,30,20,15)),
    "\n\n"
  )
}

get_exposure_appendix <- function(item){
  appendix <- exposure_classes[[item]][["appendix"]]
  if(is.null(appendix)){
    return (c())
  } else {
    return(
      paste0(
        "### Appendix",
        "\n\n",
        exposure_classes[[item]][["appendix"]],
        "\n\n"
      )
    )
  }
}

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
    exposure_classes[[item]][["name"]],
    " --- ",
    riskname
  )
  out <- paste0(
    "### ",
    capitalize(header_text),
    " --- Summary\n\n",
    exposure_classes[[item]][[physical_or_transition]][[high_or_low]][["always"]],
    "\n\n"
  )
  if (materiality == "High") {
    out <- paste0(
      out,
      "### ",
      capitalize(header_text),
      " --- Details\n\n",
      exposure_classes[[item]][[physical_or_transition]][[high_or_low]][["high_materiality"]],
      "\n\n"
    )
  }
  for (product in products) {
    out <- paste0(
      out,
      exposure_classes[[item]][[physical_or_transition]][[high_or_low]][[product]],
      "\n\n"
    )
  }
  return(out)
}

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
      materiality <- aggregated_table$materiality[i]
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

get_references <- function(aggregated_table, type_inputs) {
  out <- ""
  if (nrow(aggregated_table)) {
    out <- paste0(
        out,
        "# References\n\n"
      )
    for (i in 1:nrow(aggregated_table)) {
      item <- aggregated_table$item[i]
      if (length(exposure_classes[[item]][["references"]])){
        out <- paste0(
          out,
          "## ",
          exposure_classes[[item]][["name"]],
          "\n\n",
          exposure_classes[[item]][["references"]]
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

# heartbeat function to prevent app closing due to inactivity
heartbeat <- function(input, output, session) {
  beep <- reactiveTimer(55 * 1000)
  output[["__heartbeat"]] <- renderText({
    beep()
    " "
  })
}

heartbeat_footer <- function() {
  list(
    hr(),
    tag("footer", list(
      img(src = "aviva_logo.png", alt = "Aviva logo", height = 50),
      p("Developed in Aviva by Krzysztof Opalski, John Adcock")
    ))
  )
}

### captcha functions copied from https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R

GreCAPTCHAv3Ui <- function(siteKey) {
  tagList(tags$head(
    tags$script(src = paste0("https://www.google.com/recaptcha/api.js?render=", siteKey)),
  ))
}

GreCAPTCHAv3js <- function(siteKey, action, fieldID) {
  runjs(paste0("
        grecaptcha.ready(function () {
          grecaptcha.execute('", siteKey, "', { action: '", action, "' }).then(function (token) {
			      Shiny.onInputChange('", fieldID, "',token);
      		});
	      });
      "))
}

GreCAPTCHAv3Server <- function(secretKey, reCaptchaResponse) {
  gResponse <- POST(
    "https://www.google.com/recaptcha/api/siteverify",
    body = list(
      secret = secretKey,
      response = reCaptchaResponse
    )
  )

  if (gResponse$status_code == 200) {
    return(fromJSON(content(gResponse, "text")))
  }
}
