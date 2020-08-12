#' @include functions.R
NULL

#-------------------------------------------------------------------------------
# The ui and server Shiny components to display and hold user preferences for
# current session and across repeat visits (using cookies)
#
# - Some global settings are also set here, to be used by other modules
# - Reading/writing of cookies should be done here to be picked up by other modules
#   in the application
#-------------------------------------------------------------------------------

.preferencesUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    .titleBar('globals', 'User preferences', NULL),
    selectInput(ns("growth_ref"), label = "Growth reference", choices=NULL),
    fileInput(ns('growth_ref_file'), 'Upload growth reference file')
  )
}

.preferences <- function(input, output, session) {
  # store the page load status
  status <- reactiveValues(loaded=FALSE, uuid=NULL)

  # prefix for user uploads
  USER_UPLOAD_DIR <- system.file('uploads', package='LMSgrowth2')
  FILE_PREFIX <- '__LMSG2__'
  GROWTH_REF_PREFIX <- paste0(FILE_PREFIX, 'GROWTH_REF__')

  # Object to store global values. Accessed using globalValues$<key>
  #
  #  roundToDigits (numeric): number of decimal digits for rounding
  #  roundToSignificantDigits (numeric): number of significant digits for rounding
  #  growthReference (string): the currently selected growth reference e.g. "uk90"
  #  growthReferenceData (dataframe): the dataframe of currently selected growth reference
  #  growthReferenceName (string): description of selected growth reference e.g. "UK 1990 growth reference"
  #  growthReferenceMeasures (character vector): code of measures available in currently selected growth reference
  #  growthReferenceAgeStart (numeric): the first age available in currently selected growth reference
  #  growthReferenceAgeStop (numeric): the last age available in currently selected growth reference
  #  growthReferenceSexes (string): the list of sexes available in currently selected growth reference
  
  globalValues <- reactiveValues(roundToDigits = 2,
                                 roundToSignificantDigits = 3,
                                 # Use the same default as `ncentiles` and
                                 # `centilestep` in `centile.R`.
                                 z_scores = .get_sds_range(7, 0.666))

  # populates the growth reference select input using sitar data and any uploaded by user
  update_growth_ref_input <- function(selected=NULL) {
    # get the default growth references from sitar
    all_gf <- .get_references()
    # check any growth references uploaded by the user
    user_dir <- file.path(USER_UPLOAD_DIR, isolate(status$uuid))
    if (dir.exists(user_dir)) {
      user_files <- list.files(user_dir, pattern=paste0(GROWTH_REF_PREFIX, '*'))
      user_gf_names <- stringr::str_match(user_files, paste0(GROWTH_REF_PREFIX, "(.*)"))
      all_gf <- c(all_gf, stats::setNames(as.list(user_files), user_gf_names[,2]))
    }

    # if current selected has not been specified, pick it up from user's cookie
    if (is.null(selected)){
      selected <- isolate(input$jscookie$growthRef)
    }
    
    # if selected not in list of available references, pick the first one
    if (!(selected %in% unlist(all_gf))) {
      selected = all_gf[[1]]
    }
    
    updateSelectInput(session, "growth_ref", choices = all_gf, selected = selected)
  }

  observe({
    # this block only executed once on page load (status$loaded)
    # and we wait for cookies to be initialised (input$jscookie)
    if (!status$loaded & !is.null(input$jscookie)) {
      # create user's uuid if one doesn't exist
      uuid_cookie <- isolate(input$jscookie$uuid)
      if (is.null(uuid_cookie)) {
        # couldn't figure out how to use shiny::snapshotPreprocessInput() so
        # overriding the randomly-generated uuid by checking for testmode
        if (isTRUE(getOption("shiny.testmode"))) {
            new_uuid = "shiny-testsmode-uuid"
        } else {
            new_uuid <- uuid::UUIDgenerate()
        }
        shinyjs::js$setcookie(name='uuid', value=new_uuid)
        status$uuid <- new_uuid
      } else {
        status$uuid <- uuid_cookie
      }

      # apply the user's saved growth reference, if any
      growth_ref_cookie <- isolate(input$jscookie$growthRef)
      if (is.null(growth_ref_cookie)) {
        start_growth_ref <- 'uk90'
      } else {
        start_growth_ref <- growth_ref_cookie
      }

      status$loaded <- TRUE
      update_growth_ref_input(selected=start_growth_ref)
    }
  })

  # if user selects growth_ref, then update the cookie
  observeEvent(input$growth_ref, {
    if (input$growth_ref != '') {
      # we have a growth ref selected, make sure it persists across sessions
      shinyjs::js$setcookie(name='growthRef', value=input$growth_ref)

      # if it's a user uploaded growth reference
      if (stringr::str_starts(input$growth_ref, GROWTH_REF_PREFIX)) {
        # get the file path to the growth references using the user's uuid
        user_growth_ref <- file.path(USER_UPLOAD_DIR, input$jscookie$uuid, input$growth_ref)
        # read in the growth reference and set the name
        growth_ref_data <- rio::import(user_growth_ref)
        globalValues$growthReferenceName <- stringr::str_replace(input$growth_ref, GROWTH_REF_PREFIX, '')
      } else {
        # it's a sitar growth reference, load the data and set the name
        references <- .get_references()
        growth_ref_data <- .get_sitar_data(input$growth_ref)
        globalValues$growthReferenceName <- names(references)[references == input$growth_ref]
      }

      globalValues$growthReference <- input$growth_ref
      globalValues$growthReferenceData <- growth_ref_data
      globalValues$growthReferenceMeasures <- .get_measures_for_data(growth_ref_data)
      globalValues$growthReferenceSexes <- .get_sexes_for_data(growth_ref_data)

      ages <- .get_ages_for_data(growth_ref_data)
      globalValues$growthReferenceAgeStart <- ages[1]
      globalValues$growthReferenceAgeStop <- ages[length(ages)]
    }
  })

  # when user uploads a growth reference file
  observeEvent(input$growth_ref_file, {
    # TODO: validate input!

    # save uploaded growth file to user's directory, creating the directory if it doesn't exist
    user_dir <- file.path(USER_UPLOAD_DIR, input$jscookie$uuid)
    dir.create(user_dir)
    target_filename <- paste0(GROWTH_REF_PREFIX, input$growth_ref_file$name)
    target_path <- file.path(user_dir, target_filename)
    file.copy(input$growth_ref_file$datapath, target_path)

    # add this growth references to available list
    update_growth_ref_input(selected=target_filename)
  })

  return(globalValues)
}
