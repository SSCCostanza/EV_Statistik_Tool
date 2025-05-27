library(tidyverse)
library(R6)
library(shiny)
library(shinydashboard)
library(rlang)

main_view <- R6::R6Class(
    "main_view",
    public = list(
        #' Create MainView with plot configuration
        #' @param plot_config tibble Configuration for available plots
        initialize = function(plot_config) {
            private$plot_config <- plot_config
            private$setupUi()
            invisible(self)
        },

        #' Retrieve the UI object
        #' @return shiny.tagList UI for the application
        get_ui = function() {
            private$ui
        }
    ),
    private = list(
        plot_config = NULL,
        ui = NULL,
        setupUi = function() {
            private$ui <- tagList(
                private$createStyleTags(),
                private$createDashboardPage()
            )
        },

        # <editor-fold desc="Styling">
        createStyleTags = function() {
            tags$head(
                tags$style(HTML(private$getCss()))
            )
        },
        getCss = function() {
            "/* Allgemeine Styles */
            * {
                background-color: #000000 !important;
                color: #00FF00 !important;
                border-color: #00FF00 !important;
            }
            /* Nav-Tabs */
            .nav-tabs > li.active > a,
            .nav-tabs > li.active > a:focus,
            .nav-tabs > li.active > a:hover {
                background-color: #00FF00 !important;
                color: #000000 !important;
                border-color: #000000 !important;
            }
            /* CheckboxGroupInput-Styles */
            .checkboxGroupInput .checkbox input[type=\"checkbox\"] + label,
            .checkboxGroupInput .checkbox-inline input[type=\"checkbox\"] + label {
                background-color: #000000 !important;
                color: #00FF00 !important;
                border: 1px solid #00FF00 !important;
                padding: 2px 6px !important;
                margin: 2px !important;
                border-radius: 3px !important;
                display: inline-block !important;
            }
            .checkboxGroupInput .checkbox input[type=\"checkbox\"]:checked + label,
            .checkboxGroupInput .checkbox-inline input[type=\"checkbox\"]:checked + label {
                background-color: #00FF00 !important;
                color: #000000 !important;
                border: 1px solid #000000 !important;
            }
            /* Eigene Button-Styles */
            .create-plots-button {
                margin-bottom: 10px;
                width: 100%;
            }
            .download-plots-button {
                width: 100%;
            }"
        },
        # </editor-fold>

        # <editor-fold desc="Function">
        createDashboardPage = function() {
            dashboardPage(
                private$createHeader(),
                private$createSidebar(),
                private$createBody()
            )
        },

        # <editor-fold desc="Header">
        createHeader = function() {
            dashboardHeader(
                title = "Bewerberdaten Analyse"
            )
        },
        # </editor-fold>

        # <editor-fold desc="Sidebar">
        createSidebar = function() {
            dashboardSidebar(
                sidebarMenu(
                    menuItem(
                        "Upload & Analyse",
                        tabName = "analysis",
                        icon = icon("chart-bar")
                    )
                ),
                private$createFileInput(),
                private$createPlotTypeInput(),
                private$createCreateButton(),
                private$createDownloadButton()
            )
        },
        getPlotChoices = function() {
            private$plot_config %>%
                select(label, value) %>%
                deframe()
        },
        createFileInput = function() {
            fileInput(
                inputId = "file_upload",
                label = "CSV Datei hochladen:",
                accept = c(".csv"),
                multiple = FALSE
            )
        },
        createPlotTypeInput = function() {
            checkboxGroupInput(
                inputId = "plot_types",
                label = "Zu erstellende Plots:",
                choices = private$getPlotChoices(),
                selected = c("application_date")
            )
        },
        createCreateButton = function() {
            actionButton(
                inputId = "create_plots",                 # id zuerst
                label = "Plots erstellen",                # label als zweites
                class = "btn btn-primary btn-block create-plots-button" # class-Parameter
            )
        },
        createDownloadButton = function() {
            downloadButton(
                class = "btn-success download-plots-button",  # class zuerst
                outputId = "download_plots",                   # id als nächstes
                label = "Plots herunterladen (ZIP)"
            )
        },
        # </editor-fold>

        # <editor-fold desc="Body">
        createBody = function() {
            dashboardBody(
                fluidRow(
                    column(
                        width = 12,
                        tabsetPanel(
                            !!!private$createTabPanels()
                        )
                    )
                )
            )
        },
        createTabPanels = function() {
            private$plot_config %>%
                pmap(
                    function(tab_name, output_dist, output_cum, ...) {
                        tabPanel(
                            title = tab_name,
                            fluidRow(
                                column(
                                    width = 6,
                                    plotOutput(output_dist)
                                ),
                                column(
                                    width = 6,
                                    plotOutput(output_cum)
                                )
                            )
                        )
                    }
                )
        }
        # </editor-fold>

        # </editor-fold>
    )
)
