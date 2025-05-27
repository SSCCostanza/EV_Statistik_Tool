library(tidyverse)
library(shiny)
library(R6)
library(zip)
library(grid)
library(png)

main_controller <- R6::R6Class(
  "MainController",
  public = list(
    #' Create MainController with plot configuration
    #' @param plot_config tibble Configuration for available plots
    initialize = function(plot_config) {
      private$plot_config <- plot_config
      invisible(self)
    },

    #' Retrieve the server function
    #' @return function Shiny server function
    get_server = function() {
      private$make_server()
    }
  ),

  private = list(
    plot_config = NULL,

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
        private$setupSessionCleanup(session, values)
      }
    },

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
    },

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
            if(nrow(specs) == 0) {
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
    },

    setupDownloadHandler = function(output, values) {
      output$download_plots <- downloadHandler(
        filename = function() {
          paste0("bewerberdaten_plots_", Sys.Date(), ".zip")
        },
        content     = function(file) {
          private$createDownloadZip(values$app_state, file)
        },
        contentType = "application/zip"
      )
    },

    setupSessionCleanup = function(session, values) {
      session$onSessionEnded(function() {
        isolate({
          if (nrow(values$app_state) > 0) {
            private$cleanupTempFiles(values$app_state$temp_dir[1])
          }
        })
      })
    },

    selectPlotSpecs = function(selected_types) {
      private$plot_config %>%
        filter(value %in% selected_types)
    },

    renderPlots = function(specs, file_path, temp_dir, output) {
      pg <- plot_generator$new()
      for(i in seq_len(nrow(specs))) {
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
          # Speichere die erzeugten Plots als PNG-Dateien im temp_dir
          png_filename_dist <- file.path(temp_dir, paste0(spec$output_dist, ".png"))
          png_filename_cum  <- file.path(temp_dir, paste0(spec$output_cum, ".png"))
          ggsave(filename = png_filename_dist, plot = plots[[1]], width = 8, height = 6)
          ggsave(filename = png_filename_cum,  plot = plots[[2]], width = 8, height = 6)
        })
      }
    },

    renderPlotOutput = function(output, output_name, plot_input) {
      output[[output_name]] <- renderPlot({
        print(plot_input)
      })
    },

    createDownloadZip = function(app_state, file) {
      if (nrow(app_state) == 0 || is.na(app_state$temp_dir[1])) {
        showNotification("Kein temporäres Verzeichnis verfügbar!", type = "warning")
        tmp_txt <- tempfile(fileext = ".txt")
        writeLines("Keine Plots vorhanden", con = tmp_txt)
        tmp_txt_rel <- basename(tmp_txt)
        zip::zip(zipfile = file, files = tmp_txt_rel, root = dirname(tmp_txt))
        return()
      }

      # Ermittele absolute und relative Dateinamen
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
    },

    cleanupTempFiles = function(temp_dir) {
      if (!is.null(temp_dir) && !is.na(temp_dir)) {
        pngs <- list.files(
          temp_dir,
          pattern    = "\\.png$",
          full.names = TRUE
        )
        unlink(pngs)
      }
    }
  )
)