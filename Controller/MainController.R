library(tidyverse)
library(shiny)
library(R6)
library(zip)
library(grid)
library(png)
library(shinyjs)

main_controller <- R6::R6Class(
  "main_controller",
  public = list(
    #' @title Initialize MainController with plot configuration
    #'
    #' @description Creates a new instance of the MainController class with the provided plot configuration. This constructor sets up the internal plot configuration that will be used throughout the application lifecycle.
    #'
    #' @details The initialize method is the R6 constructor for the MainController class. It stores the plot configuration in a private field for later use by other methods. The method returns the instance invisibly to allow for method chaining while maintaining clean console output.
    #'
    #' @typed plot_config (tibble) Configuration tibble containing available plot specifications
    #' @typedreturn (main_controller) Initialized MainController instance
    #' @examples
    #' config <- tibble(value = c("plot1", "plot2"), method = c("method1", "method2"))
    #' controller <- main_controller$new(config)
    initialize = function(plot_config) {
      private$plot_config <- plot_config
      invisible(self)
    },

    #' @title Retrieve the Shiny server function
    #'
    #' @description Returns the Shiny server function that handles all reactive logic for the application. This function contains all the necessary event handlers and reactive elements needed for the Shiny application to function properly.
    #'
    #' @details The get_server method provides access to the private make_server method which creates a complete Shiny server function. This server function includes handlers for file uploads, plot creation, downloads, resets, and session cleanup. It follows the standard Shiny server function pattern with input, output, and session parameters.
    #'
    #' @typedreturn (function) Shiny server function with input, output, and session parameters
    #' @examples
    #' controller <- main_controller$new(plot_config)
    #' server_func <- controller$get_server()
    get_server = function() {
      private$make_server()
    }
  ),
  private = list(
    plot_config = NULL,
    #' @title Create the main Shiny server function
    #'
    #' @description Creates and returns the main Shiny server function that contains all reactive logic and event handlers for the application. This function sets up the reactive values and initializes all component handlers.
    #'
    #' @details The make_server method creates a complete Shiny server function with reactive values for application state and plot data. It calls private setup methods to configure file uploads, plot creation, downloads, resets, and session cleanup. The returned function follows the standard Shiny server pattern.
    #'
    #' @typedreturn (function) Complete Shiny server function ready to be used in shinyApp
    #' @examples
    #' controller <- main_controller$new(plot_config)
    #' server <- controller$make_server()
    make_server = function() {
      function(input, output, session) {
        values <- reactiveValues(
          app_state = tibble(
            uploaded_file = character(),
            temp_dir      = character(),
            plots_created = logical()
          ),
          plot_data = list()
        )

        private$setupFileUpload(input, values)
        private$setupPlotCreation(input, output, values)
        private$setupDownloadHandler(output, values)
        private$setupResetHandler(input, output, values)
        private$setupSessionCleanup(session, values)
      }
    },
    #' @title Setup file upload event handler
    #'
    #' @description Configures the reactive event handler for file uploads, updating application state when a file is successfully uploaded.
    #'
    #' @details This method sets up an observeEvent that watches for file upload events and updates the reactive values with the uploaded file path, temporary directory, and resets the plots creation status. It also displays a success notification to the user.
    #'
    #' @typed input (list) Shiny input object containing reactive input values
    #' @typed values (reactiveValues) Reactive values object for storing application state
    #' @examples
    #' private$setupFileUpload(input, values)
    setupFileUpload = function(input, values) {
      observeEvent(input$file_upload, {
        req(input$file_upload)
        values$app_state <- tibble(
          uploaded_file = input$file_upload$datapath,
          temp_dir      = tempdir(),
          plots_created = FALSE
        )
        showNotification("Datei erfolgreich hochgeladen!", type = "message")
      })
      invisible(NULL)
    },
    #' @title Setup plot creation event handler
    #'
    #' @description Configures the reactive event handler for plot creation, managing the entire plot generation workflow including error handling and user notifications.
    #'
    #' @details This method sets up an observeEvent that watches for plot creation button clicks and orchestrates the complete plot generation process. It validates input requirements, displays progress notifications, creates plots using the plot generator, saves them as PNG files, and handles any errors that occur during the process.
    #'
    #' @typed input (list) Shiny input object containing reactive input values
    #' @typed output (list) Shiny output object for rendering plots
    #' @typed values (reactiveValues) Reactive values object for storing application state
    #' @examples
    #' private$setupPlotCreation(input, output, values)
    setupPlotCreation = function(input, output, values) {
      observeEvent(input$create_plots, {
        req(nrow(values$app_state) > 0)
        req(input$plot_types)

        showNotification(
          "Plots werden erstellt...",
          id       = "creating",
          duration = NULL,
          type     = "message"
        )

        tryCatch(
          {
            specs <- private$selectPlotSpecs(input$plot_types)
            if (nrow(specs) == 0) {
              stop("Keine gültigen Plot-Typen ausgewählt.")
            }
            private$renderPlots(
              specs,
              values$app_state$uploaded_file[1],
              values$app_state$temp_dir[1],
              output
            )

            values$app_state <- values$app_state %>%
              mutate(plots_created = TRUE)

            removeNotification("creating")
            showNotification("Plots erfolgreich erstellt!", type = "message")
          },
          error = function(e) {
            print(e)
            removeNotification("creating")
            showNotification(
              paste("Fehler beim Erstellen der Plots:", e$message),
              type = "error"
            )
          }
        )
      })
      invisible(NULL)
    },
    #' @title Setup download handler for plot files
    #'
    #' @description Configures the downloadHandler for creating ZIP files containing generated plots for user download.
    #'
    #' @details This method sets up a downloadHandler that creates a ZIP file with a timestamped filename containing all generated PNG plot files. The handler uses the createDownloadZip method to package the files and provides appropriate content type headers for browser download.
    #'
    #' @typed output (list) Shiny output object for rendering download handlers
    #' @typed values (reactiveValues) Reactive values object containing application state
    #' @examples
    #' private$setupDownloadHandler(output, values)
    setupDownloadHandler = function(output, values) {
      output$download_plots <- downloadHandler(
        filename = function() {
          paste0("bewerberdaten_plots_", Sys.Date(), ".zip")
        },
        content = function(file) {
          private$createDownloadZip(values$app_state, file)
        },
        contentType = "application/zip"
      )
      invisible(NULL)
    },
    #' @title Setup reset handler for application state
    #'
    #' @description Configures the reactive event handler for resetting the application state, clearing all plots and uploaded data.
    #'
    #' @details This method sets up an observeEvent that watches for reset button clicks and clears all application state including uploaded files, plot outputs, and temporary files. It also resets the UI elements and provides user feedback through notifications.
    #'
    #' @typed input (list) Shiny input object containing reactive input values
    #' @typed output (list) Shiny output object for rendering plots
    #' @typed values (reactiveValues) Reactive values object for storing application state
    #' @examples
    #' private$setupResetHandler(input, output, values)
    setupResetHandler = function(input, output, values) {
      observeEvent(input$reset_plots, {
        runjs('$("#file_upload").val("");')
        values$app_state <- tibble(
          uploaded_file = character(),
          temp_dir      = character(),
          plots_created = logical()
        )
        specs <- private$plot_config
        for (i in seq_len(nrow(specs))) {
          spec <- specs[i, ]
          output[[spec$output_dist]] <- renderPlot({
            NULL
          })
          output[[spec$output_cum]] <- renderPlot({
            NULL
          })
        }
        if (nrow(values$app_state) > 0 && !is.na(values$app_state$temp_dir[1])) {
          private$cleanupTempFiles(values$app_state$temp_dir[1])
        }
        showNotification("Alle Plots und Daten wurden zurückgesetzt!", type = "message")
      })
      invisible(NULL)
    },
    #' @title Setup session cleanup handler
    #'
    #' @description Configures the session cleanup handler to ensure temporary files are removed when the user session ends.
    #'
    #' @details This method sets up a session cleanup handler that automatically removes temporary PNG files when a user session terminates. This prevents accumulation of temporary files and ensures proper resource cleanup.
    #'
    #' @typed session (session) Shiny session object
    #' @typed values (reactiveValues) Reactive values object containing application state
    #' @examples
    #' private$setupSessionCleanup(session, values)
    setupSessionCleanup = function(session, values) {
      session$onSessionEnded(function() {
        isolate({
          if (nrow(values$app_state) > 0) {
            private$cleanupTempFiles(values$app_state$temp_dir[1])
          }
        })
      })
      invisible(NULL)
    },
    #' @title Select plot specifications based on selected types
    #'
    #' @description Filters the plot configuration to return only the specifications that match the selected plot types.
    #'
    #' @details This method takes the selected plot types from the user interface and filters the internal plot configuration to return only the matching specifications. It uses dplyr filtering to match values in the configuration tibble.
    #'
    #' @typed selected_types (character) Vector of selected plot type identifiers
    #' @typedreturn (tibble) Filtered plot configuration containing only selected types
    #' @examples
    #' specs <- private$selectPlotSpecs(c("type1", "type2"))
    selectPlotSpecs = function(selected_types) {
      private$plot_config %>%
        filter(value %in% selected_types)
    },
    #' @title Render plots using plot generator
    #'
    #' @description Creates and renders plots using the plot generator, saves them as PNG files, and displays them in the Shiny output.
    #'
    #' @details This method iterates through plot specifications, generates plots using the appropriate plot generator methods, renders them in the Shiny output, and saves them as PNG files in the temporary directory for later download.
    #'
    #' @typed specs (tibble) Plot specifications containing method and output identifiers
    #' @typed file_path (character) Path to the uploaded data file
    #' @typed temp_dir (character) Temporary directory for saving PNG files
    #' @typed output (list) Shiny output object for rendering plots
    #' @examples
    #' private$renderPlots(specs, "data.csv", tempdir(), output)
    renderPlots = function(specs, file_path, temp_dir, output) {
      pg <- plot_generator$new()
      for (i in seq_len(nrow(specs))) {
        local({
          spec <- specs[i, ]
          plots <- switch(as.character(spec$method),
            "process_file_application_date" = pg$process_file_application_date_output(file_path),
            "process_file_admission_date" = pg$process_file_admission_date_output(file_path),
            "process_file_application_admission_date_difference" = pg$process_file_application_admission_date_difference_output(file_path),
            "process_file_points_step1" = pg$process_file_points_step1_output(file_path),
            stop("Unbekannte Methode: ", as.character(spec$method))
          )
          private$renderPlotOutput(output, spec$output_dist, plots[[1]])
          private$renderPlotOutput(output, spec$output_cum, plots[[2]])
          png_filename_dist <- file.path(temp_dir, paste0(spec$output_dist, ".png"))
          png_filename_cum <- file.path(temp_dir, paste0(spec$output_cum, ".png"))
          ggsave(filename = png_filename_dist, plot = plots[[1]], width = 8, height = 6)
          ggsave(filename = png_filename_cum, plot = plots[[2]], width = 8, height = 6)
        })
      }
      invisible(NULL)
    },
    #' @title Render individual plot output
    #'
    #' @description Renders a single plot object to a Shiny output element.
    #'
    #' @details This helper method creates a renderPlot reactive output for a given plot object and assigns it to the specified output name in the Shiny output list. It uses the print function to ensure proper rendering of ggplot objects.
    #'
    #' @typed output (list) Shiny output object for rendering plots
    #' @typed output_name (character) Name of the output element to render to
    #' @typed plot_input (ggplot) Plot object to be rendered
    #' @examples
    #' private$renderPlotOutput(output, "plot1", my_plot)
    renderPlotOutput = function(output, output_name, plot_input) {
      output[[output_name]] <- renderPlot({
        print(plot_input)
      })
      invisible(NULL)
    },
    #' @title Create ZIP file for plot downloads
    #'
    #' @description Creates a ZIP archive containing all generated PNG plot files for user download.
    #'
    #' @details This method handles the creation of ZIP files for download by collecting all PNG files from the temporary directory and packaging them into a single archive. If no plots are available, it creates a text file with an appropriate message instead.
    #'
    #' @typed app_state (tibble) Application state containing temporary directory information
    #' @typed file (character) Output file path for the ZIP archive
    #' @examples
    #' private$createDownloadZip(values$app_state, "plots.zip")
    createDownloadZip = function(app_state, file) {
      if (nrow(app_state) == 0 || is.na(app_state$temp_dir[1])) {
        showNotification("Kein temporäres Verzeichnis verfügbar!", type = "warning")
        tmp_txt <- tempfile(fileext = ".txt")
        writeLines("Keine Plots vorhanden", con = tmp_txt)
        tmp_txt_rel <- basename(tmp_txt)
        zip::zip(zipfile = file, files = tmp_txt_rel, root = dirname(tmp_txt))
        return()
      }
      png_files_full <- list.files(
        app_state$temp_dir[1],
        pattern    = "\\.png$",
        full.names = TRUE
      )
      png_files_rel <- list.files(
        app_state$temp_dir[1],
        pattern    = "\\.png$",
        full.names = FALSE
      )
      if (length(png_files_full) > 0) {
        zip::zip(zipfile = file, files = png_files_rel, root = app_state$temp_dir[1])
        showNotification("ZIP-Datei erfolgreich erstellt!", type = "message")
      } else {
        showNotification("Keine Plots zum Download verfügbar!", type = "warning")
        tmp_txt <- tempfile(fileext = ".txt")
        writeLines("Keine Plots zum Download verfügbar!", con = tmp_txt)
        tmp_txt_rel <- basename(tmp_txt)
        zip::zip(zipfile = file, files = tmp_txt_rel, root = dirname(tmp_txt))
      }
      invisible(NULL)
    },
    #' @title Clean up temporary files
    #'
    #' @description Removes temporary PNG files from the specified directory to prevent accumulation of temporary files.
    #'
    #' @details This method searches for PNG files in the given temporary directory and removes them using the unlink function. It includes safety checks to ensure the directory exists and is not null before attempting cleanup.
    #'
    #' @typed temp_dir (character) Path to the temporary directory containing files to clean up
    #' @examples
    #' private$cleanupTempFiles(tempdir())
    cleanupTempFiles = function(temp_dir) {
      if (!is.null(temp_dir) && !is.na(temp_dir)) {
        pngs <- list.files(
          temp_dir,
          pattern    = "\\.png$",
          full.names = TRUE
        )
        unlink(pngs)
      }
      invisible(NULL)
    }
  )
)
