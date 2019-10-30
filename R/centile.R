# Centile tab ##################################################################
.centileUI <- function(id, label="centile ui") {
  ns <- NS(id)

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Calculate centiles"),
        radioButtons(ns("measure"), label = h4("Measurement"),
                     choices = list(), selected = ""),
        checkboxGroupInput(ns("sex"), label = h4("Sex"),
                           choices = c("Female" = "f", "Male" = "m"),
                           selected = c("f", "m"), inline = TRUE),
        h4("Age"),
        radioButtons(ns("ageunit"), label = "Unit",
                     choices = list("Years" = "years", "Months" = "months",
                                    "Weeks" = "weeks", "Days" = "days"),
                     selected = "years", inline = TRUE),
        h5(textOutput(ns("rangeheader"))),
        numericInput(ns("agestart"), label = "From",
                     value = 0, min = 0, max = 23),
        numericInput(ns("agestop"), label = "To",
                     value = 23, min = 0, max = 23),
        numericInput(ns("agestep"), label = "Step",
                     value = 0.5, min = 0, max = 365.25, step = 0.1),
        radioButtons(ns("customcentiles"),
                     choices = list("Equally-spaced centiles" = FALSE,
                                    "Custom centiles" = TRUE),
                     selected = FALSE, label = h4("Centiles options"),
                     inline = FALSE),
        conditionalPanel(
          condition = "input['centile-customcentiles'] == 'FALSE'",
          numericInput(ns("ncentiles"), label =  h5("Number of centiles"),
                       value = 7, min = 1, max = 30),
          numericInput(ns("centilestep"), label = h5("Space between centiles"),
                       value = 0.666, min = 0, max = 10, step = 0.01)
        ),
        conditionalPanel(
          condition = "input['centile-customcentiles'] == 'TRUE'",
          radioButtons(ns("centiles_or_sds"),
                       choices = list("SDSs" = "use_sds",
                                      "Centiles" = "use_centiles"),
                       selected = "use_sds", label = "Options",
                       inline = TRUE),
          conditionalPanel(
            condition = "input['centile-centiles_or_sds'] == 'use_sds'",
            textInput(ns("sds"), "Insert SDSs",
                      value = "-2 -1.33 -0.67 0 0.67 1.333 2")
          ),
          conditionalPanel(
            condition = "input['centile-centiles_or_sds'] == 'use_centiles'",
            textInput(ns("centiles"), label = "Insert centiles",
                      value = "2 9 25 50 75 91 98")
          )
        ),
        radioButtons(ns("header_centile_or_sds"), label = h4("Header options"),
                     choices = list("Centiles" = "centile", "SDSs" = "sds"),
                     selected = "centile", inline = TRUE),

        conditionalPanel(
          condition = "output['centile-centileFemale'] || output['centile-centileMale']",
          downloadButton(ns("download_data"), "Download")
        )
      ),
      mainPanel(
        h3(tags$b(textOutput(ns("centilesTitle")))),
        h4(tags$b(textOutput(ns("centileFemaleCaption")))),
        tableOutput(ns("centileFemale")),
        h4(tags$b(textOutput(ns("centileMaleCaption")))),
        tableOutput(ns("centileMale"))
      )
    )
  )
}

# Centile server logic #########################################################
.centile <- function(input, output, session, globals) {
  ns <- session$ns

  # Reactive value holding the data for the download.  `f` is the data for
  # female, `m` for male, `total` is the table containing data about all sexes.
  output_data <- reactiveValues(f = NULL, m = NULL, total = NULL)

  # Function to generate equally spaced SDSs, centred around 0, given their
  # number and the step.
  get_sds_range <- function(ncentiles, step) {
    validate(
      need(is.finite(ncentiles) && ncentiles >= 1,
           "Please enter a value equal to or larger than 1 for the number of centiles"),
      need(is.finite(step) && step > 0,
           "Please enter a positive value for the space between centiles")
    )
    bound <- (as.integer(ncentiles) - 1) / 2
    (-bound):bound * step
  }

  # Title over the tables
  centilesTitle <- function() {
    renderText({
      references <- .get_references()
      reference_name <-
        names(references)[references == paste(globals$growthReference)]
      measurements <- globals$growthReferenceMeasures
      current_measurement_index <-
        sapply(measurements, function(m) {m$code}) == input$measure
      measurement <- measurements[current_measurement_index]
      measurement_description <- paste0(measurement[[1]]$description, " (",
                                        measurement[[1]]$unit, ")")
      paste(reference_name, "—", measurement_description)
    })
  }

  # Short caption for the table, given the sex
  centilesCaption <- function(sex) {
    renderText(
      if (sex %in% input$sex) {
        if (sex == "f") {
          "Female"
        } else if (sex == "m") {
          "Male"
        } else {
          "Unknown sex"
        }
      }
    )}

  # Return the table of the centiles for the given sex
  centiles <- function(sex) {
    renderTable({
      output_data[[sex]] <- NULL
      if (sex %in% input$sex) {
        if (input$customcentiles) {
          if (input$centiles_or_sds == "use_sds") {
            zs <- as.numeric(unlist(strsplit(input$sds, " ")))
            validate(
              need(length(zs) > 0, "Please insert a list SDSs")
            )
          } else if (input$centiles_or_sds == "use_centiles") {
            values <- as.numeric(unlist(strsplit(input$centiles, " ")))
            validate(
              need(length(values) > 0, "Please insert a list centiles"),
              need(all(values >= 0) && all(values <= 100),
                   "Centiles must be between 0 and 100")
            )
            zs <- qnorm(values / 100)
          }
        } else {
          zs <- get_sds_range(input$ncentiles, input$centilestep)
        }
        # Input validation.  This only checks the input is a finite value
        validate(
          need(is.finite(input$agestart),
               "Please insert a value for the start of the age range"),
          need(is.finite(input$agestop),
               "Please insert a value for the end of the age range"),
          need(is.finite(input$agestep) && input$agestep > 0,
               "Please insert a positive value for the step of the age range")
        )
        min_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStart,
                                                input$ageunit)
        max_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStop,
                                                input$ageunit)
        # Since we round `input$agestart` and `input$agestop` to 2 decimal
        # digits, these numbers can happen to be slightly off the allowed range.
        # When this happens, gracefully use these maximum values (we use a
        # tolerance of 0.01).  Note: we have to directly use years to avoid any
        # rounding, especially when the unit is set to weeks.
        if ((input$agestart < min_age) && (input$agestart >= (min_age - 0.01))) {
          agestart_years <- globals$growthReferenceAgeStart
        } else {
          agestart_years <- .duration_from_unit_to_years(input$agestart,
                                                         input$ageunit)
        }
        if ((input$agestop > max_age) && (input$agestop <= (max_age + 0.01))) {
          agestop_years <- globals$growthReferenceAgeStop
        } else {
          agestop_years <- .duration_from_unit_to_years(input$agestop,
                                                        input$ageunit)
        }
        # Additional validation.  This checks that the numbers are in the
        # allowed range.  This can't be in the `validate` above because we need
        # to validate `input$age*` before setting `age*_years`.
        validate(
          need(agestart_years >= globals$growthReferenceAgeStart,
               paste("The minimum allowed value for the age is",
                     min_age, input$ageunit)),
          need(agestart_years <= agestop_years,
               "The start of the age range must be less than or equal to the end age"),
          need(agestop_years <= globals$growthReferenceAgeStop,
               paste("The maximum allowed value for the age is",
                     max_age, input$ageunit))
        )
        ages <- seq(agestart_years, agestop_years,
                    by = .duration_from_unit_to_years(input$agestep,
                                                      input$ageunit))
        # Get the data as a dataframe
        df <- sitar::LMS2z(ages, as.matrix(zs), sex = sex,
                           measure = input$measure,
                           ref = getExportedValue('sitar',
                                                  globals$growthReference),
                           toz = FALSE)
        # Set as row names the age, with the given unit
        row.names(df) <- round(.duration_from_years_to_unit(ages, input$ageunit),
                               digits = 2)
        # When we ask to show the SDS in the header of the table, round them to
        # two digits
        if (input$header_centile_or_sds == "sds") {
          colnames(df) <- round(zs, digits = 2)
        }
        # Add the ages as first column
        df <- cbind(age_column_header =
                      round(.duration_from_years_to_unit(ages, input$ageunit),
                            digits = 2), df)
        # Rename the column header
        colnames(df)[colnames(df) == "age_column_header"] <- input$ageunit
        # Store the value for the download
        output_data[[sex]] <- as.data.frame(df)
        # Finally return the dataframe
        df
      }
    })
  }

  update_output_data <- function() {
    f <- output_data$f
    m <- output_data$m
    if ("f" %in% input$sex && ! is_empty(f)) {
      f$sex <- 2
      if ("m" %in% input$sex && ! is_empty(m)) {
        m$sex <- 1
        output_data$total <- rbind(f, m)
      } else {
        output_data$total <- f
      }
    } else {
      if ("m" %in% input$sex && ! is_empty(m)) {
        m$sex <- 1
        output_data$total <- m
      } else {
        output_data$total <- NULL
      }
    }
  }

  observeEvent(output_data$f, update_output_data(), ignoreNULL = FALSE)
  observeEvent(output_data$m, update_output_data(), ignoreNULL = FALSE)

  # Format the range label with the range of allowed ages
  format_range_label <- function() {
    renderText({
      min_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStart,
                                              input$ageunit)
      max_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStop,
                                              input$ageunit)
      paste0("Range (min: ", round(min_age, digits = 2), "; max: ",
             round(max_age, digits = 2), " ", input$ageunit, ")")
    })
  }

  # Render the main output objects
  output$rangeheader <- format_range_label() # This is actually in the side panel
  output$centilesTitle <- centilesTitle()
  output$centileFemaleCaption <- centilesCaption("f")
  output$centileFemale <- centiles("f")
  output$centileMaleCaption <- centilesCaption("m")
  output$centileMale <- centiles("m")
  output$download_data <- downloadHandler(
    filename = 'centiles.csv',
    content = function(file) {
      write.csv(output_data$total, file, row.names = FALSE)
    }
  )

  # When the selected measurement changes, update the title over the tables
  observeEvent(input$measure, centilesTitle())

  # Set the list of measurements according to those available in the currently
  # selected referernce data
  set_measurements_list <- function() {
    measurements <- sapply(globals$growthReferenceMeasures,
                           function(m) {paste0(m$description, " (", m$unit,")")})
    codes <- sapply(globals$growthReferenceMeasures, function(m) {m$code})
    updateRadioButtons(session, "measure",
                       choices = as.list(setNames(codes, measurements)),
                       selected = codes[1])
  }
  # When the measurements in reference data change, update the title and the
  # list of measurements
  observeEvent(globals$growthReferenceMeasures, {
    set_measurements_list()
    centilesTitle()
  })

  # Define some reactive variables relative to the age range: initial value,
  # last value, step.  All numerical values are in years
  agestart <- reactiveValues(old = 0, current = 0)
  agestop <- reactiveValues(old = 1, current = 1)
  agestep <- reactiveValues(old = 1, current = 1)
  # We need to remember the old age unit, in order to convert to the new one
  ageunit <- reactiveValues(old = "years", current = "years")

  # Update the reactive value holding the age unit
  update_age_unit <- function() {
    ageunit$old <- ageunit$current
    ageunit$current <- input$ageunit
  }

  # Update the numerical value of the age property `id`, scaling the value with
  # the currently selected age unit
  change_age_unit <- function(id) {
    new_value_years <- .duration_from_unit_to_years(input[[id]], ageunit$current)
    new_unit <- input$ageunit
    if (id == "agestart" || id == "agestop") {
      min_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStart,
                                              new_unit)
      max_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStop,
                                              new_unit)
    } else if (id == "agestep") {
      min_age <- 0
      # Set a somewhat arbitrary maximum values for the step of the age range,
      # based on the currently selected age unit
      if (new_unit == "years") {
        max_age <- 5
      } else if (new_unit == "months") {
        max_age <- 20
      } else if (new_unit == "weeks") {
        max_age <- 100
      } else if (new_unit == "days") {
        max_age <- 500
      } else {
        max_age <- 1
      }
    } else {
      min_age <- 0
      max_age <- 0
    }
    new_value <- round(.duration_from_years_to_unit(new_value_years, new_unit),
                       digits = 2)
    updateNumericInput(session, id, value = round(new_value, digits = 2),
                       min = min_age, max = max_age)
  }

  # Update the reactive value for the given age property
  update_age_property <- function(id) {
    var <- get(id)
    var$old <- var$current
    var$current <- .duration_from_unit_to_years(input[[id]], ageunit$current)
  }

  observeEvent(input$ageunit, {
    # When the age unit changes, scale all age-related values...
    for (id in list("agestart", "agestop", "agestep")) {
      change_age_unit(id);
    }
    # ...and then update the reactive values holding the age unit
    update_age_unit()
  })

  # Update the reactive values of the age range properties
  for (id in list("agestart", "agestop", "agestep")) {
    local({
      local_id <- id
      observeEvent(input[[local_id]], {
        update_age_property(local_id);
      })
    })
  }

  # Update the value of the maxim age using the maxim age in the reference data
  # currently available
  set_age_start_stop <- function() {
    min_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStart,
                                            input$ageunit)
    max_age <- .duration_from_years_to_unit(globals$growthReferenceAgeStop,
                                            input$ageunit)
    updateNumericInput(session, "agestart", value = 0,
                       min = min_age, max = max_age)
    updateNumericInput(session, "agestop", value = max_age,
                       min = min_age, max = max_age)
  }

  observeEvent(globals$growthReference, set_age_start_stop())
}
