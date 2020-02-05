# Tab for multiple measurements ##############################################################

# UI
.multipleUI <- function(id, label="multiple measurements") {
  ns <- NS(id)

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Measurements to SDS"),
        
        conditionalPanel(
          condition = "output['multiple-uploaded'] == false",
          fileInput(ns('file'), 'Upload data file')
        ),
        
        conditionalPanel(
          condition = "output['multiple-uploaded'] == true",
          selectInput(ns("id_source"),  label = "ID",  choices = c('NOT POPULATED')),
          selectInput(ns("sex"),  label = "Sex",  choices = c('NOT POPULATED')),
          selectInput(ns("age_source"), "Age", c('NOT POPULATED')),
          selectInput(ns("age_unit"), "Unit of age", choices = c('Days', 'Weeks', 'Months', 'Years')),

          uiOutput(ns("measurement_inputs")),

          selectInput(ns("to_add"),
                      "Calculate",
                      c("SDS", "Centile", "% Predicted", "Predicted", "% CV", "Skewness"),
                      selected = "SDS",
                      multiple = TRUE,
                      selectize = TRUE),
          actionButton(ns('apply'), 'Apply'),
          downloadButton(ns("download_data"), "Download"),
          actionButton(ns("reset"), "Reset", icon("refresh"))
        )
      ),

      mainPanel(
        h3(textOutput(ns("growth_ref"))),
        DTOutput(ns("table")),
        br(),
        fluidRow(
          wellPanel(
            checkboxGroupInput(ns("plot_options"), label = "Plotting options",
                               choices = c("Group by ID" = "group_id",
                                           "Connect the points" = "connect_points"),
                               selected = c("connect_points", "group_id"),
                               inline = TRUE),
            radioButtons(ns("plot_y_axis"), label = "y-axis",
                         choices = list("Measurement" = "measurement",
                                        "Centile" = "centile",
                                        "SDS" = "sds"),
                         selected = "measurement", inline = TRUE)
          )
        ),
        br(),
        plotlyOutput(ns("measurements_plots"))
      )
    )
  )
}

# Server
.multiple <- function(input, output, session, globals) {
  # To use `renderUI` within modules, we need to wrap names with `ns()`
  ns <- session$ns

  # to hold the original uploaded data
  original_data <- reactiveValues(
    df = NULL,
    # `offset` is the index of the last column of the original data
    offset = 0,
    initialised = FALSE
  )
  
  # the original columns in the uploaded spreadhsheet
  original_columns <- reactiveVal(NULL)
  # list of column options
  column_options <- reactiveVal(NULL)

  # display the currently selected growth reference
  output$growth_ref <- renderText({
    globals$growthReferenceName
  })

  # create input tags for each measurement available in the selected growth reference
  output$measurement_inputs <- renderUI({
    choices <-  c('N/A', column_options())

    measures <- lapply(globals$growthReferenceMeasures,
                       function(measure) {
                         selectInput(ns(measure$code), paste0(measure$description, " (", measure$unit, ")"),
                                     choices = choices, selected = get_selected(measure$code, 'N/A'))
                       })
    do.call(tagList, measures)
  })


  # save uploaded data and update options
  observeEvent(input$file, {
    if(!is.null(input$file)) {
      df <- as.data.frame(read.csv(input$file$datapath))
      numeric_columns <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      df[,numeric_columns] <- round(df[,numeric_columns],
                                    digits = globals$roundToDigits)
      original_columns(names(df))
      # columns in the dataframe are prefixed
      column_options(paste('[Column]', isolate(original_columns()), sep = ' '))
      original_data$df <- df
      original_data$offset <- ncol(original_data$df)
      original_data$initialised = TRUE
      uploaded = TRUE

      # update ID dropdown
      selected <- get_selected("id", "")
      if (selected != "") {
        updateSelectInput(session, "id_source", choices = column_options(),  selected = selected)
        # The ID column may not be stored as a factor, which would make it hard to
        # use it as a filter in the DT table.
        id_column_name <- strsplit(selected, split=" ")[[1]][2]
        original_data$df[[id_column_name]] <- as.factor(original_data$df[[id_column_name]])
      }

      # update sex dropdown
      selected <- get_selected("sex", 'Male')
      if (selected != "") {
        choices <-  c('Male', 'Female', column_options())
        updateSelectInput(session, 'sex', choices = choices,  selected = selected)
      }

      # update age
      selected <- get_selected("age", '')
      if (selected != "") {
        updateSelectInput(session, 'age_source', choices = column_options(), selected = selected)
      }
    }
  })

  # outputs the datatable in the main panel
  output$table <- renderDT(datatable(original_data$df,
                                     filter = "top",
                                     rownames = FALSE,
                                     options = list(scrollX = TRUE,
                                                    sDom  = '<"top">lrt<"bottom">ip')))

  # fills the 'selected' argument for selectInput when column names matches a regular expression.
  # `code` is a short name for the item that we want to match in the list of columns
  get_selected <- function(code, default) {
    if (code == "age") {
      to_match = '[Aa]ge|[Yy]ears|[Dd]ays|[Ww]eeks'
    } else if (code == "id") {
      to_match = '[Ii][Dd]'
    } else if (code == "sex") {
      to_match = '[Ss]ex|[Gg]ender'
    } else if (code == "ht") {
      to_match = "[Hh]eight"
    } else if (code == "wt") {
      to_match = "[Ww]eight"
    } else if (code == "bmi") {
      to_match = "BMI|bmi"
    } else if (code == "head") {
      to_match = "[Hh]ead"
    } else {
      return(default)
    }
    match <- original_columns() %>% isolate %>% stringr::str_detect(to_match) %>% which
    { if (length(match) > 0) paste('[Column]', original_columns()[match[1]]) else default }
  }

  # when the 'apply' button is clicked
  observeEvent(input$apply, {
    df <- original_data$df[seq(1, original_data$offset)]
    # Initialise `new_columns` as a dataframe with the same rows as the original
    # data, but with no columns
    new_columns = df[0]
    # loop over every measurement
    for (item in globals$growthReferenceMeasures) {
      new_columns <- do_calculation_for_measurement(item, df, new_columns)
    }
    original_data$df <- cbind(df, new_columns)
  })

  # returns the sex column or value
  get_sex <- function() {
    sex_selected <- isolate(input$sex)
    if (startsWith(sex_selected, '[Column]')) {
      df <- isolate(original_data$df)
      sex_column_or_value = df[, strsplit(sex_selected, split=" ")[[1]][2]]
    } else {
      sex_column_or_value = sex_selected
    }
    return(sex_column_or_value)
  }

  # returns the age column
  get_id <- function() {
      df <- isolate(original_data$df)
      id_column <- isolate(input$id_source)
      ids <- df[, strsplit(id_column, split=" ")[[1]][[2]]]
      return(ids)
  }

  # returns the age column
  get_age <- function() {
      df <- isolate(original_data$df)
      age_column <- isolate(input$age_source)
      ages <- df[, strsplit(age_column, split=" ")[[1]][[2]]]
      return(ages)
  }

  # returns the age column in years
  get_age_in_years <- function() {
      age_column = get_age()
      age_unit <- stringr::str_to_lower(isolate(input$age_unit))

      # transform age into years if necessary
      if (age_unit != 'years') {
          args <- list()
          args[[ age_unit ]] <- age_column
          age_column <- do.call(.duration_in_years, args)
      }

      return(age_column)
  }

  # list of the various statistics and the associated function
  calculations <- list(
    list(name='SDS', column_name='SDS', func=.get_sds),
    list(name='Centile', column_name='Centile', func=.get_centile),
    list(name='% Predicted', column_name='PercPredicted', func=.get_perc_predicted),
    list(name='Predicted', column_name='Predicted', func=.get_predicted),
    list(name='% CV', column_name='PercCV', func=.get_perc_cv),
    list(name='Skewness', column_name='Skewness', func=.get_skewness)
  )

  do_calculation_for_measurement <- function(measurement, df, new_columns) {
      value <- input[[measurement$code]]
      code_name <- measurement$code
      if (!is.null(value) && value != 'N/A') {
        # a function that returns column name for a given statistics
        new_col <- function(stat, column, code_name) {
          paste('LMS', stat, column, code_name, sep = '_')
        }
        column = strsplit(value, split=" ")[[1]][2]
        sex_column_or_value <- get_sex()
        age_column_or_value <- get_age_in_years()

        lms_stats <- .measurement_to_scores(age_column_or_value, sex_column_or_value, code_name, df[, column], ref=globals$growthReference)

        # loop through each of the possible statistics and calculate if necessary
        for (calc in calculations) {
            # if the user asked for this statistics
            if (calc$name %in% input$to_add) {
              # calculate using the appropriate function and add a new column to the dataframe
              result <- calc$func(lms_stats)
              if (is.numeric(result)) {
                result <- round(result, digits = globals$roundToDigits)
              }
              new_columns[[new_col(calc$column_name, column, code_name)]] <- result
            }
        }

      }

      return(new_columns)
  }

  plot_measure <- function(measure, column_name, show_legend) {
    age_unit <- stringr::str_to_lower(isolate(input$age_unit))
    ages <- get_age()[input$table_rows_all]
    sexes <- get_sex()

    if (input$plot_y_axis == "measurement") {
      y_data <- original_data$df[input$table_rows_all,column_name]
      y_title <- paste0(measure$description, " (", measure$unit, ")")
    } else {
      z <- sitar::LMS2z(.duration_from_unit_to_years(ages, age_unit),
                        y = original_data$df[input$table_rows_all,column_name],
                        sex = sexes[input$table_rows_all], measure = measure$code,
                        ref = getExportedValue('sitar', globals$growthReference))
      if (input$plot_y_axis == "sds") {
        y_data <- signif(z, globals$roundToSignificantDigits)
        y_title <- paste0(measure$description, " (SDS)")
      } else {
        y_data <- signif(.sds_to_centile(z), globals$roundToSignificantDigits)
        y_title <- paste0(measure$description, " (centile)")
      }
    }

    plt <- plot_ly(type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = paste0("Age (", age_unit, ")")),
             yaxis = list(title = y_title))

    # Plot centiles if necessary, that is when all selected sexes are equal
    if (length(sexes) == 1 || length(unique(sexes[input$table_rows_all])) == 1) {
      if (length(sexes) == 1) {
        sex <- sexes
      } else {
        sex <- sexes[input$table_rows_all][1]
      }
      min_age <- min(ages)
      max_age <- max(ages)
      # Do not plot if extrema of ages are not finite, to avoid strange errors
      # when selecting the sex from the drop-down menu in the sidebar
      if (input$plot_y_axis == "measurement" &&  is.finite(min_age) && is.finite(max_age)) {
        # Ages used for plotting centiles.  Take extrema of `ages` and create a
        # sequence with an arbitrary decent sampling in between.
        centiles_ages <- seq(min(ages), max(ages), length.out = 200)
        centiles <- sitar::LMS2z(.duration_from_unit_to_years(centiles_ages, age_unit),
                                 as.matrix(rev(globals$z_scores)), sex = sex,
                                 measure = measure$code,
                                 ref = getExportedValue('sitar',
                                                        globals$growthReference),
                                 toz = FALSE)
        for (col in colnames(centiles)) {
          this_centile <- centiles[,col]
          plt <- add_lines(plt, x = centiles_ages, y = this_centile,
                           type = "scatter",
                           color = col,
                           legendgroup = col,
                           showlegend = show_legend,
                           opacity = 0.6,
                           line = list(dash='dash'),
                           hoverinfo = "name+text",
                           hovertext = paste0("(",
                                              signif(centiles_ages,
                                                     globals$roundToSignificantDigits),
                                              ", ",
                                              signif(centiles[,col],
                                                     globals$roundToSignificantDigits),
                                              ")")
                           )
        }
      }
    }

    if ("connect_points" %in% input$plot_options) {
      plot_mode <- "lines+markers"
    } else {
      plot_mode <- "markers"
    }
    if ("group_id" %in% input$plot_options) {
      # We need to convert to a plain vector because the `color` option below
      # doesn't play nicely with a factor
      plot_name  <- as.vector(get_id()[input$table_rows_all])
    } else {
      plot_name <- ""
    }
    # Now plot the datapoints from the table
    plt <- add_trace(plt, x = ages,
                     y = y_data,
                     color = plot_name,
                     mode = plot_mode,
                     showlegend = FALSE)
    plt
  }

  plot_all_measures <- function() {
    renderPlotly({
      if (original_data$initialised) {
        to_plot <- sapply(globals$growthReferenceMeasures,
                          function(measure) {
                            value <- input[[measure$code]]
                            !is.null(value) && value != 'N/A'
                          })
        first_code <- globals$growthReferenceMeasures[to_plot][1][[1]]$code
        plots <- lapply(globals$growthReferenceMeasures[to_plot],
                        function(measure) {
                          column_name <- sub("\\[Column\\] ", "", input[[measure$code]])
                          plot_measure(measure, column_name, measure$code == first_code)
                        })
        plots_number <- length(plots)
        # Plot only if there is something to plot
        if (plots_number >= 1) {
          subplot(plots, nrows = plots_number, shareX = TRUE, titleY = TRUE) %>%
            # This causes a warning to be issued, but this is really a bug in
            # plotly that doesn't allow to set the size of a subplot in a sane
            # way: https://github.com/ropensci/plotly/issues/1613
            layout(autosize = TRUE, height = plots_number * 400)
        }
      }
    })
  }

  output$measurements_plots <- plot_all_measures()
  observeEvent(input$plot_options,
               output$measurements_plots <- plot_all_measures())

  output$download_data <- downloadHandler(
    filename = 'lms_download.csv',
    content = function(file) {
      write.csv(original_data$df, file, row.names = FALSE)
    }
  )
  
  output$uploaded <- reactive(original_data$initialised)
  outputOptions(output, "uploaded", suspendWhenHidden = FALSE)
  
  # when the 'reset' button is clicked
  observeEvent(input$reset, {
    original_data$df <- NULL
    original_data$offset <- 0
    original_data$initialised <- FALSE
    uploaded <- FALSE
    reset("file")
  })
}
