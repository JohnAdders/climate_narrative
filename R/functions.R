# helper function - a shortcut function to add element to the list
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

# helper function to get object by its name but return NULL (not error) if it does not exist
get_or_null <- function(name) if (exists(name)) {
  return(get(name))
} else {
  return(NULL)
}

# helper function to make the first letter of a string upper case
capitalize <- function(input_string) {
  return(paste0(toupper(substring(input_string, 1, 1)), substring(input_string, 2)))
}


# helper functions to produce the layout of tabs (cell, row, whole table)
exposure_grid_cell <- function(exposure_item, prefix, tooltip_text = NULL, dev = FALSE, width=NULL) {
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
    if (!is.null(tooltip_text)) {
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
  colnames(layout) <- colnames(exposure_matrix)[-2]
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
          # disable tooltips for now
          # tooltip_matrix[i,j-1],
          NULL,
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
    align='c'
  )
}

# helper function to produce a markdown report
table_to_markdown <- function(table, additional_spaces = 3, dot_to_space = TRUE) {
  headers <- colnames(table)
  if (dot_to_space) {
    headers <- gsub(".", "&nbsp;", headers, fixed = TRUE)
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
        paste(table[i, ], collapse = collapsor),
        "\n"
      )
    }
  }
  out <- paste0(out, "\n\n")
  return(out)
}

get_exposure_description <- function(item, type_item_inputs) {
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
        gsub(" ", "&nbsp;", texts),
        collapse = "<br />"
      )
    }
  )
  colnames(ordered_type_item_inputs)[3:4] <- c("Exposure.row", "Materiality")
  out <- paste0(
    "## ",
    exposure_classes[[item]][["name"]],
    "\n\n",
    exposure_classes[[item]][["description"]],
    "\n\nThe following rows contribute: \n\n",
    table_to_markdown(ordered_aggregate_inputs),
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

  # supress high/low for transition risk only
  if (physical_or_transition == "transition") {
    physical_or_transition_text <- physical_or_transition
  } else {
    physical_or_transition_text <- paste(high_or_low, physical_or_transition)
  }

  out <- paste0(
    "### ",
    capitalize(physical_or_transition_text),
    " risk\n\n",
    exposure_classes[[item]][[physical_or_transition]][[high_or_low]][["always"]],
    "\n\n"
  )
  if (materiality == "High") {
    out <- paste0(
      out,
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
