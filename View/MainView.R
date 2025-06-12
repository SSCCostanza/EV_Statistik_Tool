library(tidyverse)
library(R6)
library(shiny)
library(shinydashboard)
library(rlang)

main_view <- R6::R6Class(
    "main_view",
    public = list(
        #' @title Initialize MainView with plot configuration
        #'
        #' @description Creates a new instance of the MainView class with the provided plot configuration and sets up the user interface.
        #'
        #' @details This constructor method initializes the MainView R6 object by storing the plot configuration and calling the setupUi method to build the complete Shiny dashboard interface including styling, layout, and all UI components.
        #'
        #' @typed plot_config (tibble) Configuration tibble containing plot settings and metadata
        #' @typedreturn (main_view) Returns the initialized MainView instance invisibly
        #' @examples
        #' config <- tibble(label = "Test", value = "test", tab_name = "Test Tab")
        #' view <- main_view$new(config)
        initialize = function(plot_config) {
            private$plot_config <- plot_config
            private$setupUi()
            invisible(self)
        },

        #' @title Retrieve the complete UI object
        #'
        #' @description Returns the complete Shiny UI object that was created during initialization of the MainView instance.
        #'
        #' @details This method provides access to the fully configured Shiny dashboard UI including all styling, layout components, sidebar elements, and body content that was built during the setupUi process.
        #'
        #' @typedreturn (shiny.tag.list) Complete Shiny UI object for the application
        #' @examples
        #' config <- tibble(label = "Test", value = "test", tab_name = "Test Tab")
        #' view <- main_view$new(config)
        #' ui <- view$get_ui()
        get_ui = function() {
            private$ui
        }
    ),
    private = list(
        plot_config = NULL,
        ui = NULL,
        #' @title Setup the complete user interface
        #'
        #' @description Builds the complete Shiny dashboard UI by combining JavaScript dependencies, custom styling, and the main dashboard page structure.
        #'
        #' @details This method orchestrates the creation of the full UI by calling useShinyjs for JavaScript functionality, adding custom CSS styles through createStyleTags, and building the dashboard page layout through createDashboardPage. The result is stored in the private ui field.
        #'
        #' @typedreturn (invisible) No return value, modifies private$ui field
        #' @examples
        #' # Internal method called during initialization
        #' private$setupUi()
        setupUi = function() {
            private$ui <- tagList(
                useShinyjs(),
                private$createStyleTags(),
                private$createDashboardPage()
            )
            invisible(NULL)
        },

        #' @title Get application color scheme
        #'
        #' @description Returns a named character vector containing all color definitions used throughout the application interface.
        #'
        #' @details This method defines the complete color palette for the application including background colors, text colors, border colors, and specific styling for different UI components like tabs, checkboxes, and buttons. Colors are defined using hex color codes.
        #'
        #' @typedreturn (character) Named character vector with color definitions
        #' @examples
        #' # Internal method to get color scheme
        #' colors <- private$get_colors()
        get_colors = function() {
            return(c(
                "background"                = "#ffffff",
                "text"                     = "#0065BD",
                "border"                   = "#0065BD",

                "tab_active_background"    = "#CCCCCC",
                "tab_active_text"          = "#003359",
                "tab_active_border"        = "#003359",

                "checkbox_background"      = "#ffffff",
                "checkbox_text"           = "#0065BD",
                "checkbox_border"         = "#0065BD",

                "checkbox_checked_background" = "#CCCCCC",
                "checkbox_checked_text"   = "#003359",
                "checkbox_checked_border" = "#003359",

                "create_button_background" = "#ffffff",
                "create_button_text"       = "#A2AD00",
                "create_button_border"     = "#A2AD00",

                "reset_button_background"  = "#ffffff",
                "reset_button_text"        = "#E37222",
                "reset_button_border"      = "#E37222"
            ))
        },

        # <editor-fold desc="Styling">
        #' @title Create HTML head tags with custom styling
        #'
        #' @description Creates HTML head tags containing custom CSS styles for the application interface.
        #'
        #' @details This method generates the HTML head section with custom CSS styling by calling getCss() to retrieve the CSS rules and wrapping them in appropriate HTML tags for inclusion in the Shiny UI.
        #'
        #' @typedreturn (shiny.tag) HTML head tags containing custom CSS styles
        #' @examples
        #' # Internal method to create style tags
        #' style_tags <- private$createStyleTags()
        createStyleTags = function() {
            return(tags$head(
                tags$style(HTML(private$getCss()))
            ))
        },
        #' @title Generate custom CSS styles
        #'
        #' @description Creates a complete CSS stylesheet string with custom styling rules for all application components.
        #'
        #' @details This method generates comprehensive CSS rules using the color scheme from get_colors() and the glue package for string interpolation. It includes styling for general elements, navigation tabs, checkbox groups, and various button types with proper spacing and color application.
        #'
        #' @typedreturn (character) Complete CSS stylesheet as a single string
        #' @examples
        #' # Internal method to generate CSS
        #' css_string <- private$getCss()
        getCss = function() {
            colors <- private$get_colors()
            return(glue::glue("/* Allgemeine Styles */
            * {{
                background-color: {colors['background']} !important;
                color: {colors['text']} !important;
                border-color: {colors['border']} !important;
            }}
            /* Nav-Tabs */
            .nav-tabs > li.active > a,
            .nav-tabs > li.active > a:focus,
            .nav-tabs > li.active > a:hover {{
                background-color: {colors['tab_active_background']} !important;
                color: {colors['tab_active_text']} !important;
                border-color: {colors['tab_active_border']} !important;
            }}
            /* CheckboxGroupInput-Styles */
            .checkboxGroupInput .checkbox input[type=\"checkbox\"] + label,
            .checkboxGroupInput .checkbox-inline input[type=\"checkbox\"] + label {{
                background-color: {colors['checkbox_background']} !important;
                color: {colors['checkbox_text']} !important;
                border: 1px solid {colors['checkbox_border']} !important;
                padding: 2px 6px !important;
                margin: 2px !important;
                border-radius: 3px !important;
                display: inline-block !important;
            }}
            .checkboxGroupInput .checkbox input[type=\"checkbox\"]:checked + label,
            .checkboxGroupInput .checkbox-inline input[type=\"checkbox\"]:checked + label {{
                background-color: {colors['checkbox_checked_background']} !important;
                color: {colors['checkbox_checked_text']} !important;
                border: 1px solid {colors['checkbox_checked_border']} !important;
            }}
            /* Create Plots Button-Styles */
            .create-plots-button {{
                width: 100%;
                margin-left: 10px;
                margin-right: 10px;
                margin-top: 5px;
                margin-bottom: 5px;
                background-color: {colors['create_button_background']} !important;
                color: {colors['create_button_text']} !important;
                border-color: {colors['create_button_border']} !important;
            }}
            /* Download Button bleibt unverändert */
            .download-plots-button {{
                width: 100%;
                margin-left: 15px;
                margin-right: 10px;
                margin-top: 5px;
                margin-bottom: 5px;
            }}
            /* Reset Plots Button-Styles */
            .reset-plots-button {{
                width: 100%;
                margin-left: 10px;
                margin-right: 10px;
                margin-top: 5px;
                margin-bottom: 5px;
                background-color: {colors['reset_button_background']} !important;
                color: {colors['reset_button_text']} !important;
                border-color: {colors['reset_button_border']} !important;
            }}"))
        },
        # </editor-fold>

        # <editor-fold desc="Function">
        #' @title Create complete dashboard page structure
        #'
        #' @description Builds the main dashboard page by combining header, sidebar, and body components.
        #'
        #' @details This method creates the overall structure of the Shiny dashboard by calling the individual component creation methods and combining them into a single dashboardPage object from the shinydashboard package.
        #'
        #' @typedreturn (shiny.tag) Complete dashboard page structure
        #' @examples
        #' # Internal method to create dashboard page
        #' page <- private$createDashboardPage()
        createDashboardPage = function() {
            return(dashboardPage(
                private$createHeader(),
                private$createSidebar(),
                private$createBody()
            ))
        },

        # <editor-fold desc="Header">
        #' @title Create dashboard header component
        #'
        #' @description Creates the top header section of the dashboard with the application title.
        #'
        #' @details This method generates the dashboard header using shinydashboard's dashboardHeader function and sets the title to "Bewerberdaten Analyse" for the application.
        #'
        #' @typedreturn (shiny.tag) Dashboard header component
        #' @examples
        #' # Internal method to create header
        #' header <- private$createHeader()
        createHeader = function() {
            return(dashboardHeader(
                title = "Bewerberdaten Analyse"
            ))
        },
        # </editor-fold

        # <editor-fold desc="Sidebar">
        #' @title Create complete sidebar with all controls
        #'
        #' @description Builds the dashboard sidebar containing navigation menu and all input controls for file upload and plot configuration.
        #'
        #' @details This method creates the left sidebar panel including a navigation menu item and all necessary input controls: file upload input, plot type selection checkboxes, and action buttons for creating, downloading, and resetting plots.
        #'
        #' @typedreturn (shiny.tag) Complete sidebar component
        #' @examples
        #' # Internal method to create sidebar
        #' sidebar <- private$createSidebar()
        createSidebar = function() {
            return(dashboardSidebar(
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
                private$createDownloadButton(),
                private$createResetButton()
            ))
        },
        #' @title Extract plot choices from configuration
        #'
        #' @description Transforms the plot configuration tibble into a named character vector suitable for checkbox input choices.
        #'
        #' @details This method processes the plot_config tibble by selecting the label and value columns and converting them into a named character vector using dplyr's deframe function. This format is required for Shiny's checkboxGroupInput choices parameter.
        #'
        #' @typedreturn (character) Named character vector with plot choices
        #' @examples
        #' # Internal method to get plot choices
        #' choices <- private$getPlotChoices()
        getPlotChoices = function() {
            return(private$plot_config %>%
                select(label, value) %>%
                deframe())
        },
        #' @title Create file upload input control
        #'
        #' @description Creates a file input widget specifically configured for CSV file uploads.
        #'
        #' @details This method generates a Shiny fileInput control with German labeling, restricted to accept only CSV files, and configured for single file upload. The input has the ID "file_upload" for server-side access.
        #'
        #' @typedreturn (shiny.tag) File input control for CSV uploads
        #' @examples
        #' # Internal method to create file input
        #' file_input <- private$createFileInput()
        createFileInput = function() {
            return(fileInput(
                inputId = "file_upload",
                label = "CSV Datei hochladen:",
                accept = c(".csv"),
                multiple = FALSE
            ))
        },
        #' @title Create plot type selection input
        #'
        #' @description Creates a checkbox group input for selecting which plot types to generate.
        #'
        #' @details This method generates a checkboxGroupInput control with choices derived from the plot configuration. All plot types are selected by default, and the control has German labeling with the ID "plot_types" for server-side access.
        #'
        #' @typedreturn (shiny.tag) Checkbox group input for plot type selection
        #' @examples
        #' # Internal method to create plot type input
        #' plot_input <- private$createPlotTypeInput()
        createPlotTypeInput = function() {
            return(checkboxGroupInput(
                inputId = "plot_types",
                label = "Zu erstellende Plots:",
                choices = private$getPlotChoices(),
                selected = private$plot_config$value  # Alle Werte standardmäßig auswählen
            ))
        },
        #' @title Create plot creation action button
        #'
        #' @description Creates an action button for triggering the plot generation process.
        #'
        #' @details This method generates a primary action button with German labeling "Plots erstellen", custom CSS classes for styling, and the ID "create_plots" for server-side event handling. The button spans the full width of its container.
        #'
        #' @typedreturn (shiny.tag) Action button for plot creation
        #' @examples
        #' # Internal method to create create button
        #' create_btn <- private$createCreateButton()
        createCreateButton = function() {
            return(actionButton(
                inputId = "create_plots",                 # id zuerst
                label = "Plots erstellen",                # label als zweites
                class = "btn btn-primary btn-block create-plots-button" # class-Parameter
            ))
        },
        #' @title Create download button for plots
        #'
        #' @description Creates a download button for downloading generated plots as a ZIP file.
        #'
        #' @details This method generates a success-styled download button with German labeling for downloading plots in ZIP format. The button has the ID "download_plots" for server-side download handling and custom CSS classes for styling.
        #'
        #' @typedreturn (shiny.tag) Download button for plot ZIP files
        #' @examples
        #' # Internal method to create download button
        #' download_btn <- private$createDownloadButton()
        createDownloadButton = function() {
            return(downloadButton(
                class = "btn-success download-plots-button",  # class zuerst
                outputId = "download_plots",                   # id als nächstes
                label = "Plots herunterladen (ZIP)"
            ))
        },
        #' @title Create reset button for clearing plots
        #'
        #' @description Creates an action button for resetting and clearing all generated plots.
        #'
        #' @details This method generates a warning-styled action button with German labeling "Reset Plots" for clearing the plot display area. The button has the ID "reset_plots" for server-side event handling and custom CSS classes for styling with full container width.
        #'
        #' @typedreturn (shiny.tag) Action button for plot reset functionality
        #' @examples
        #' # Internal method to create reset button
        #' reset_btn <- private$createResetButton()
        createResetButton = function() {
            return(actionButton(
                inputId = "reset_plots",
                label = "Reset Plots",
                class = "btn btn-warning btn-block reset-plots-button"
            ))
        },
        # </editor-fold>

        # <editor-fold desc="Body">
        #' @title Create dashboard body with tabbed plot display
        #'
        #' @description Creates the main dashboard body containing a tabbed interface for displaying generated plots.
        #'
        #' @details This method generates the dashboard body using a fluid row layout with a full-width column containing a tabset panel. The tabs are dynamically created based on the plot configuration using the createTabPanels method.
        #'
        #' @typedreturn (shiny.tag) Dashboard body with tabbed plot interface
        #' @examples
        #' # Internal method to create dashboard body
        #' body <- private$createBody()
        createBody = function() {
            return(dashboardBody(
                fluidRow(
                    column(
                        width = 12,
                        tabsetPanel(
                            !!!private$createTabPanels()
                        )
                    )
                )
            ))
        },
        #' @title Create individual tab panels for plot display
        #'
        #' @description Generates tab panels dynamically based on plot configuration for displaying distribution and cumulative plots.
        #'
        #' @details This method uses purrr's pmap function to iterate over the plot configuration and create individual tab panels. Each tab contains two full-width plot output areas: one for distribution plots and one for cumulative plots, arranged in fluid rows.
        #'
        #' @typedreturn (list) List of tabPanel objects for the tabset
        #' @examples
        #' # Internal method to create tab panels
        #' tabs <- private$createTabPanels()
        createTabPanels = function() {
            return(private$plot_config %>%
                pmap(
                    function(tab_name, output_dist, output_cum, ...) {
                        tabPanel(
                            title = tab_name,
                            fluidRow(
                                column(
                                    width = 12,
                                    plotOutput(output_dist)
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    plotOutput(output_cum)
                                )
                            )
                        )
                    }
                ))
        }
        # </editor-fold>

        # </editor-fold>
    )
)
