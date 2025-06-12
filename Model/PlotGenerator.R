library(R6)
library(tidyverse)
library(lubridate)
library(e1071)
library(rlang)
library(tools)
library(checkmate)
library(glue)

#' @title R6 class for generating data visualization plots from CSV files
#'
#' @description Provides methods to process application and admission data files and generate distribution, dual-distribution, and cumulative distribution plots for both date and numeric data.
#'
#' @details This R6 class encapsulates a complete workflow for reading CSV files, transforming data, computing statistical summaries, and generating publication-ready plots. It supports both single file and batch processing modes, handles missing data appropriately, and provides consistent theming across all generated visualizations.
#'
#' @typedreturn (R6Class) Generator for creating plot_generator objects with public methods for file processing and plotting and private helper methods for data transformation and annotation
#' @examples
#' pg <- plot_generator$new()
#' pg$process_file_application_date("data/app.csv", "out")
#' df <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")))
#' summary <- pg$private$create_date_distribution(df, as.Date("2021-01-01"), as.Date("2021-01-02"), "day")
#' pg$plot_date_distribution(df, summary, "Applications")
plot_generator <- R6Class(
  "plot_generator",
  public = list(
    # <editor-fold desc="Single Files">

    #' @title Process a single application date file and generate plots
    #'
    #' @description Validate the input CSV file and output directory, then generate application date distribution and cumulative distribution plots.
    #'
    #' @details This method reads a CSV file containing application data, validates that required columns are present, parses the date column, computes distribution statistics, and saves both distribution and cumulative distribution plots to the target directory.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_file_application_date("data/app.csv", "out")
    process_file_application_date = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_application_date(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL) # Added explicit return value
    },
    #' @title Process application date file and return plots as objects
    #'
    #' @description Process a single application date CSV file and return the generated plots as ggplot objects instead of saving them to disk.
    #'
    #' @details This method performs the same analysis as process_file_application_date but returns the plot objects directly for programmatic use rather than saving them to files.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application dates
    #' @typedreturn (ggplot) The generated plot object for the application date distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' plot <- pg$process_file_application_date_output("data/app.csv")
    process_file_application_date_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_application_date_output(
        source_path = source_path
      )
      return(plot)
    },

    #' @title Process a single admission date file and generate plots
    #'
    #' @description Validate the input CSV file and output directory, then generate admission date distribution and cumulative distribution plots.
    #'
    #' @details This method reads a CSV file containing admission data, validates that required columns are present, parses the date column, computes distribution statistics split by admission status, and saves both distribution and cumulative distribution plots to the target directory.
    #'
    #' @typed source_path (character) Path to the input CSV file containing admission dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_file_admission_date("data/admit.csv", "out")
    process_file_admission_date = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_admission_date(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)
    },
    #' @title Process admission date file and return plots as objects
    #'
    #' @description Process a single admission date CSV file and return the generated plots as ggplot objects instead of saving them to disk.
    #'
    #' @details This method performs the same analysis as process_file_admission_date but returns the plot objects directly for programmatic use rather than saving them to files.
    #'
    #' @typed source_path (character) Path to the input CSV file containing admission dates
    #' @typedreturn (ggplot) The generated plot object for the admission date distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' plot <- pg$process_file_admission_date_output("data/admit.csv")
    process_file_admission_date_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_admission_date_output(
        source_path = source_path
      )
      return(plot)
    },

    #' @title Process application-admission date difference file and generate plots
    #'
    #' @description Validate the input CSV file and output directory, then generate difference distribution and cumulative distribution plots.
    #'
    #' @details This method reads a CSV file containing both application and admission dates, computes the day difference between them, and creates distribution plots split by admission status showing how long the admission process takes.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application and admission dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_file_application_admission_date_difference("data/diff.csv", "out")
    process_file_application_admission_date_difference = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_application_admission_date_difference(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)
    },
    #' @title Process application-admission date difference file and return plots
    #'
    #' @description Process a CSV file with application and admission dates and return the date difference plots as ggplot objects.
    #'
    #' @details This method performs the same analysis as process_file_application_admission_date_difference but returns the plot objects directly for programmatic use rather than saving them to files.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application and admission dates
    #' @typedreturn (ggplot) The generated plot object for the date difference distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' plot <- pg$process_file_application_admission_date_difference_output("data/diff.csv")
    process_file_application_admission_date_difference_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_application_admission_date_difference_output(
        source_path = source_path
      )
      return(plot)
    },

    #' @title Process a single points step1 file and generate plots
    #'
    #' @description Validate the input CSV file and output directory, then generate points step1 distribution and cumulative distribution plots.
    #'
    #' @details This method reads a CSV file containing points data from step 1 of an evaluation process, validates the numeric column, computes distribution statistics, and saves both distribution and cumulative distribution plots showing the score distribution.
    #'
    #' @typed source_path (character) Path to the input CSV file containing points step1 data
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_file_points_step1("data/points1.csv", "out")
    process_file_points_step1 = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_points_step1(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)
    },
    #' @title Process points step1 file and return plots as objects
    #'
    #' @description Process a single points step1 CSV file and return the generated plots as ggplot objects instead of saving them to disk.
    #'
    #' @details This method performs the same analysis as process_file_points_step1 but returns the plot objects directly for programmatic use rather than saving them to files.
    #'
    #' @typed source_path (character) Path to the input CSV file containing points step1 data
    #' @typedreturn (ggplot) The generated plot object for the points step1 distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' plot <- pg$process_file_points_step1_output("data/points1.csv")
    process_file_points_step1_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_points_step1_output(
        source_path = source_path
      )
      return(plot)
    },

    # </editor-fold>
    # <editor-fold desc="Multiple Files">

    #' @title Process multiple application date files in batch
    #'
    #' @description Validate multiple input CSV files and output directory, then process each file for application date plots.
    #'
    #' @details This method iterates through a vector of file paths and calls process_file_application_date for each file, enabling batch processing of multiple application date datasets with consistent output formatting.
    #'
    #' @typed source_paths (character) Vector of paths to input CSV files containing application dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing all files
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_batch_application_date(c("app1.csv","app2.csv"), "out")
    process_batch_application_date = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_application_date(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    },

    #' @title Process multiple admission date files in batch
    #'
    #' @description Validate multiple input CSV files and output directory, then process each file for admission date plots.
    #'
    #' @details This method iterates through a vector of file paths and calls process_file_admission_date for each file, enabling batch processing of multiple admission date datasets with consistent output formatting.
    #'
    #' @typed source_paths (character) Vector of paths to input CSV files containing admission dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing all files
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_batch_admission_date(c("ad1.csv","ad2.csv"), "out")
    process_batch_admission_date = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_admission_date(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    },

    #' @title Process multiple application-admission date difference files in batch
    #'
    #' @description Validate multiple input CSV files and output directory, then process each file for date difference plots.
    #'
    #' @details This method iterates through a vector of file paths and calls process_file_application_admission_date_difference for each file, enabling batch processing of multiple date difference datasets with consistent output formatting.
    #'
    #' @typed source_paths (character) Vector of paths to input CSV files containing application and admission dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing all files
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_batch_application_admission_date_difference(c("d1.csv","d2.csv"), "out")
    process_batch_application_admission_date_difference = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_application_admission_date_difference(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    },

    #' @title Process multiple points step1 files in batch
    #'
    #' @description Validate multiple input CSV files and output directory, then process each file for points step1 plots.
    #'
    #' @details This method iterates through a vector of file paths and calls process_file_points_step1 for each file, enabling batch processing of multiple points datasets with consistent output formatting.
    #'
    #' @typed source_paths (character) Vector of paths to input CSV files containing points step1 data
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing all files
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$process_batch_points_step1(c("p1.csv","p2.csv"), "out")
    process_batch_points_step1 = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_points_step1(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    }
    # </editor-fold>
  ),
  private = list(

    #' @title Retrieve all reusable color definitions for plots
    #'
    #' @description Return a named vector of hexadecimal color codes for use in plots and themes.
    #'
    #' @details This method provides a centralized color palette that ensures consistent visual styling across all plot types. The colors include theme elements like backgrounds and grids, as well as logical condition colors for data visualization.
    #'
    #' @typedreturn (character) Named vector of colors keyed by logical and theme element names
    #' @examples
    #' pg <- plot_generator$new()
    #' colors <- pg$private$get_colors()
    get_colors = function() {
      c(
        "TRUE" = "#A2AD00",
        "FALSE" = "#E37222",
        "background" = "#FFFFFF",
        "panel_background" = "#FFFFFF",
        "panel_grid_major" = "#0065BD",
        "panel_grid_minor" = "#0065BD",
        "bar_outline" = "#000000",
        "annotation_background" = "#F5F5F5",
        "annotation_border" = "#CCCCCC"
      )
    },

    #' @title Get annotation layout constants for consistent positioning
    #'
    #' @description Return a named list of constants used for annotation positioning and sizing.
    #'
    #' @details These constants control how the statistical information boxes appear on the plots. Think of these as design rules that determine the size and placement of the information boxes that show statistics like mean, mode, and standard deviation. The values ensure that annotation boxes look consistent across all plot types.
    #'
    #' @typedreturn (list) Named list with annotation layout constants
    #' @examples
    #' pg <- plot_generator$new()
    #' constants <- pg$private$get_annotation_constants()
    get_annotation_constants = function() {
      list(
        # How much empty space around the text inside the info box?
        rect_margin_left = 0.04, # Horizontal padding: 2% extra space on left of text
        # This prevents text from touching the left box edge
        rect_margin_right = 0.04, # Horizontal padding: 2% extra space on right of text
        # This prevents text from touching the right box edge
        rect_margin_top = 0.04, # Vertical padding: 2% extra space on the top
        # This prevents text from touching the top box edge
        rect_margin_bottom = 0.04, # Vertical padding: 2% extra space on the bottom
        # This prevents text from touching the bottom box edge
        # How much space between text lines in the info box?
        line_height_factor = 0.06, # Each line of text takes up 6% of the highest bar/point height
        # Example: if highest bar is 100, each text line needs 6 units of space
        # Where should the info boxes appear vertically?
        annotation_y_factor = 1.35 # Place info boxes 35% higher than the tallest bar or point
        # Example: if tallest bar reaches 100, boxes appear at 135
      )
    },

    # <editor-fold desc="Step 1: Load the data">

    #' @title Load CSV data from file and validate required columns
    #'
    #' @description Read a CSV file into a data frame and ensure that required date, numeric, and condition columns are present.
    #'
    #' @details This method handles the initial data loading step of the processing pipeline. It reads the CSV file, validates that all required columns exist, and ensures the data frame meets minimum structure requirements. The method aborts execution if the CSV file cannot be read or required columns are missing.
    #'
    #' @typed source_path (character) Path to the CSV file to read
    #' @typed date_columns (character) Character vector of column names to parse as dates
    #' @typed number_columns (character) Character vector of column names to parse as numeric
    #' @typed condition_columns (character) Character vector of column names for condition checks
    #' @typedreturn (data.frame) The data frame read from the CSV file after validation
    #' @examples
    #' pg <- plot_generator$new()
    #' data <- pg$private$load_data("path/to/file.csv", c("ABGESCHICKT_DATUM"), c("ERGEBNIS_STUFE1"), c("ZUL"))
    load_data = function(source_path, date_columns = character(0), number_columns = character(0), condition_columns = character(0)) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_character(date_columns, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 0, unique = TRUE, min.chars = 1)
      checkmate::assert_character(number_columns, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 0, unique = TRUE, min.chars = 1)
      checkmate::assert_character(condition_columns, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 0, unique = TRUE, min.chars = 1)
      data <- tryCatch(
        read_csv(source_path, show_col_types = FALSE),
        error = function(e) {
          abort(glue("Failed to read CSV: {e$message}"))
        }
      )
      checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 5)
      checkmate::assert_names(
        names(data),
        must.include = c("ABGESCHICKT_DATUM", "ZUL", "ZUL_DATUM", "ERGEBNIS_STUFE1", "STUFE2_FLAG", date_columns, number_columns, condition_columns)
      )
      return(data)
    },

    # </editor-fold>

    # <editor-fold desc="Step 2: Prepare the loaded data">

    #' @title Parse date time string to Date object
    #'
    #' @description Convert a date time string in dmy_hms format into a Date object.
    #'
    #' @details This method uses lubridate's dmy_hms function to parse date-time strings and convert them to Date objects for use in plotting. The method aborts if the conversion fails, ensuring data integrity throughout the pipeline.
    #'
    #' @typed string (character) One element vector containing the date time string in day month year hours minutes seconds format
    #' @typedreturn (Date) The parsed Date object
    #' @examples
    #' pg <- plot_generator$new()
    #' date_val <- pg$private$get_date("01.02.2021 14:30:00")
    get_date = function(string) {
      checkmate::assert_character(string, null.ok = FALSE, any.missing = TRUE, all.missing = FALSE, min.len = 1, unique = FALSE)
      date_value <- as.Date(dmy_hms(string))
      checkmate::assert_date(date_value, null.ok = FALSE)
      return(date_value)
    },

    #' @title Compute default placeholder date for missing values
    #'
    #' @description Select a default date placeholder based on observed interval in a date vector, choosing March first or October first of the latest year.
    #'
    #' @details This method analyzes the date range in the provided vector and selects an appropriate default date for missing values. It assumes all dates occur within one calendar year and chooses between March 1st and October 1st based on the observed date interval to provide meaningful defaults for academic or administrative datasets.
    #'
    #' @typed date_column (Date) Vector of dates to analyze for interval
    #' @typedreturn (Date) A single Date object serving as placeholder for missing values
    #' @examples
    #' pg <- plot_generator$new()
    #' default_date <- pg$private$get_default_date(as.Date(c("2022-03-02","2022-05-01")))
    get_default_date = function(date_column) {
      checkmate::assert_date(date_column, null.ok = FALSE, any.missing = TRUE, all.missing = TRUE, min.len = 0, unique = FALSE)
      min_date <- min(date_column, na.rm = TRUE)
      max_date <- max(date_column, na.rm = TRUE)
      year_max <- year(max_date)
      reference_mar <- ymd(paste0(year_max, "-03-01"))
      reference_oct <- ymd(paste0(year_max, "-10-01"))
      observed_interval <- interval(min_date, max_date)
      default_date <- if (reference_mar %within% observed_interval && !(reference_oct %within% observed_interval)) {
        reference_mar
      } else {
        reference_oct
      }
      checkmate::assert_date(default_date, null.ok = FALSE)
      return(default_date)
    },

    #' @title Calculate difference in days between two dates
    #'
    #' @description Compute the number of days from one Date to another.
    #'
    #' @details This method performs simple date arithmetic to calculate the temporal difference between two dates, returning the result as a numeric value representing days. The method supports negative values when the second date is before the first date.
    #'
    #' @typed date1 (Date) Starting date
    #' @typed date2 (Date) Ending date
    #' @typedreturn (numeric) Integer number of days between date1 and date2
    #' @examples
    #' pg <- plot_generator$new()
    #' diff_days <- pg$private$get_date_difference(as.Date("2021-01-01"), as.Date("2021-01-10"))
    get_date_difference = function(date1, date2) {
      checkmate::assert_date(date1, null.ok = FALSE)
      checkmate::assert_date(date2, null.ok = FALSE)
      date_diff <- as.numeric(date2 - date1, units = "days")
      checkmate::assert_numeric(date_diff, null.ok = FALSE)
      return(date_diff)
    },

    #' @title Coerce character or numeric input to numeric type
    #'
    #' @description Convert a numeric string or numeric vector to numeric type.
    #'
    #' @details This method handles type conversion for numeric data, accepting both character strings containing numbers and existing numeric values. Character input is converted via as.numeric, while numeric input is passed through unchanged. The method aborts for unsupported types.
    #'
    #' @typed string (character|numeric) Input value to convert
    #' @typedreturn (numeric) Numeric vector representing input values
    #' @examples
    #' pg <- plot_generator$new()
    #' num1 <- pg$private$get_number("42")
    #' num2 <- pg$private$get_number(3.14)
    get_number = function(string) {
      if (is.character(string)) {
        checkmate::assert_character(string, null.ok = FALSE, any.missing = FALSE, min.chars = 1)
        number_value <- as.numeric(string)
      } else if (is.numeric(string)) {
        number_value <- string
      } else {
        abort(glue("Unsupported type for number parsing: {class(x)}"))
      }
      checkmate::assert_numeric(number_value, null.ok = FALSE)
      return(number_value)
    },

    #' @title Provide default numeric placeholder for missing values
    #'
    #' @description Return an NA_real_ placeholder for missing numeric data.
    #'
    #' @details This method provides a consistent default value for missing numeric data throughout the processing pipeline. It always returns NA_real_ regardless of the input, ensuring uniform handling of missing values in numeric computations.
    #'
    #' @typed number_vector (numeric) Numeric vector to inspect for missing values
    #' @typedreturn (numeric) Single NA_real_ value as placeholder
    #' @examples
    #' pg <- plot_generator$new()
    #' default_val <- pg$private$get_default_number(c(1,2,NA))
    get_default_number = function(number_vector) {
      checkmate::assert_numeric(number_vector, null.ok = FALSE, any.missing = TRUE, all.missing = TRUE, min.len = 0, unique = FALSE)
      default_number <- NA_real_
      checkmate::assert_numeric(default_number, null.ok = FALSE)
      return(default_number)
    },

    #' @title Compute difference between two numeric values
    #'
    #' @description Subtract number1 from number2 to yield a numeric difference.
    #'
    #' @details This method performs simple arithmetic subtraction between two numeric values, supporting both scalar and vector operations. The result represents the elementwise difference when vectors are provided.
    #'
    #' @typed number1 (numeric) First numeric value
    #' @typed number2 (numeric) Second numeric value
    #' @typedreturn (numeric) Result of number2 minus number1
    #' @examples
    #' pg <- plot_generator$new()
    #' diff_val <- pg$private$get_number_difference(10, 15)
    get_number_difference = function(number1, number2) {
      checkmate::assert_numeric(number1, null.ok = FALSE)
      checkmate::assert_numeric(number2, null.ok = FALSE)
      number_diff <- number2 - number1
      checkmate::assert_numeric(number_diff, null.ok = FALSE)
      return(number_diff)
    },

    #' @title Prepare a single column with parsing and default value filling
    #'
    #' @description Apply a parse function to transform a column, fill missing values using default function, and store results in a new column.
    #'
    #' @details This method provides a generic framework for column transformation that applies a parsing function to each value in a column, computes default values for missing entries, and creates a new column with the transformed data. It uses dplyr::mutate and rlang::sym for dynamic column evaluation.
    #'
    #' @typed data (data.frame) Data frame containing the original column
    #' @typed column (character) Name of the column to parse
    #' @typed parse_function (function) Function applied to each value to parse
    #' @typed default_function (function) Function applied to column to compute default value for missing entries
    #' @typed new_column (character) Name for the newly created column with parsed or default values
    #' @typedreturn (data.frame) Data frame augmented with new_column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(date_str = c("01.01.2021",""))
    #' result <- pg$private$prepare_column(df, "date_str", private$get_date, private$get_default_date, "DATE")
    prepare_column = function(data, column, parse_function, default_function, new_column) {
      checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1)
      checkmate::assert_names(names(data), must.include = column)
      checkmate::assert_string(column, null.ok = FALSE, min.chars = 1)
      checkmate::assert_function(parse_function, null.ok = FALSE)
      checkmate::assert_function(default_function, null.ok = FALSE)
      checkmate::assert_string(new_column, null.ok = FALSE, min.chars = 1)
      transformed_data <- data %>%
        mutate(!!sym(column) := parse_function(!!sym(column))) %>%
        mutate(!!sym(new_column) := coalesce(!!sym(column), default_function(!!sym(column))))
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_names(names(transformed_data), must.include = c(column, new_column))
      return(transformed_data)
    },

    #' @title Prepare a date column for analysis by parsing and filling defaults
    #'
    #' @description Parse and fill missing values in a date column then add a DATE column.
    #'
    #' @details This method delegates to prepare_column with date-specific parsing and default functions to transform a raw date column into a standardized DATE column suitable for plotting and analysis.
    #'
    #' @typed data (data.frame) Data frame with the raw date column
    #' @typed date_column (character) Name of the column to parse as date
    #' @typedreturn (data.frame) Data frame with parsed DATE column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(ABGESCHICKT_DATUM = c("01.01.2021",""))
    #' result <- pg$private$prepare_date_column(df, "ABGESCHICKT_DATUM")
    prepare_date_column = function(data, date_column) {
      checkmate::assert_data_frame(data, null.ok = FALSE)
      checkmate::assert_string(date_column, null.ok = FALSE, min.chars = 1)
      data <- private$prepare_column(data,
        column = date_column,
        parse_function = private$get_date,
        default_function = private$get_default_date,
        new_column = "DATE"
      )
      return(data)
    },

    #' @title Prepare a numeric column for analysis by parsing and filling defaults
    #'
    #' @description Coerce and fill missing values in a numeric column then add a NUMBER column.
    #'
    #' @details This method delegates to prepare_column with numeric-specific parsing and default functions to transform a raw numeric column into a standardized NUMBER column suitable for plotting and analysis.
    #'
    #' @typed data (data.frame) Data frame with the raw numeric column
    #' @typed number_column (character) Name of the column to parse as numeric
    #' @typedreturn (data.frame) Data frame with parsed NUMBER column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(ERGEBNIS_STUFE1 = c("5",""))
    #' result <- pg$private$prepare_number_column(df, "ERGEBNIS_STUFE1")
    prepare_number_column = function(data, number_column) {
      checkmate::assert_data_frame(data, null.ok = FALSE)
      checkmate::assert_string(number_column, null.ok = FALSE, min.chars = 1)
      data <- private$prepare_column(data,
        column = number_column,
        parse_function = private$get_number,
        default_function = private$get_default_number,
        new_column = "NUMBER"
      )
      return(data)
    },

    #' @title Prepare two columns and compute their difference using specified functions
    #'
    #' @description Apply a single column prepare function to two columns then compute difference using diff function and add new_column.
    #'
    #' @details This method provides a generic framework for preparing two related columns and computing their difference. It applies the single prepare function to each column individually, then uses the diff function to compute the difference between the prepared columns and stores the result in a new column.
    #'
    #' @typed data (data.frame) Data frame containing the two columns
    #' @typed columns (character) Vector of two column names to prepare and compare
    #' @typed single_prepare_function (function) Function to prepare each column separately
    #' @typed diff_function (function) Function to compute difference between parsed columns
    #' @typed new_column (character) Name for the resulting difference column
    #' @typedreturn (data.frame) Data frame with original columns and difference in new_column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(A = c("01.01.2021",""), B = c("02.01.2021",""))
    #' result <- pg$private$prepare_two_columns(df, c("A","B"), private$prepare_date_column, private$get_date_difference, "NUMBER")
    prepare_two_columns = function(data, columns, single_prepare_function, diff_function, new_column) {
      checkmate::assert_character(columns, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 2, unique = TRUE, min.chars = 1)
      checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = length(columns))
      checkmate::assert_names(names(data), must.include = columns)
      checkmate::assert_function(single_prepare_function, null.ok = FALSE)
      checkmate::assert_function(diff_function, null.ok = FALSE)
      checkmate::assert_string(new_column, null.ok = FALSE, min.chars = 1)
      transformed_data <- data
      for (col in columns) {
        transformed_data <- single_prepare_function(transformed_data, col)
      }
      transformed_data <- transformed_data %>%
        mutate(!!sym(new_column) := diff_function(!!sym(columns[1]), !!sym(columns[2])))
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE, min.cols = 1 + length(columns))
      checkmate::assert_names(names(transformed_data), must.include = c(columns, new_column))
      return(transformed_data)
    },

    #' @title Prepare and compute difference between two date columns
    #'
    #' @description Parse two date columns and compute day difference stored in NUMBER column.
    #'
    #' @details This method delegates to prepare_two_columns with date-specific functions to parse two date columns and compute the difference in days between them. The result is stored in a new NUMBER column for further analysis.
    #'
    #' @typed data (data.frame) Data frame with raw date columns
    #' @typed date_columns (character) Vector of two date column names
    #' @typedreturn (data.frame) Data frame with new NUMBER column of day differences
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(ABGESCHICKT_DATUM = c("01.01.2021",""), ZUL_DATUM = c("05.01.2021",""))
    #' result <- pg$private$prepare_date_columns(df, c("ABGESCHICKT_DATUM","ZUL_DATUM"))
    prepare_date_columns = function(data, date_columns) {
      transformed_data <- private$prepare_two_columns(
        data,
        date_columns,
        private$prepare_date_column,
        private$get_date_difference,
        "NUMBER"
      )
      return(transformed_data)
    },

    #' @title Prepare and compute difference between two numeric columns
    #'
    #' @description Coerce two numeric columns then compute their difference stored in NUMBER column.
    #'
    #' @details This method delegates to prepare_two_columns with numeric-specific functions to parse two numeric columns and compute the arithmetic difference between them. The result is stored in a new NUMBER column for further analysis.
    #'
    #' @typed data (data.frame) Data frame with raw numeric columns
    #' @typed number_columns (character) Vector of two numeric column names
    #' @typedreturn (data.frame) Data frame with new NUMBER column of numeric differences
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(X = c("1",""), Y = c("3",""))
    #' result <- pg$private$prepare_number_columns(df, c("X","Y"))
    prepare_number_columns = function(data, number_columns) {
      transformed_data <- private$prepare_two_columns(
        data,
        number_columns,
        private$prepare_number_column,
        private$get_number_difference,
        "NUMBER"
      )
      return(transformed_data)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3: Create distribution plots">

    # <editor-fold desc="Step 3.1: Create distributions">

    #' @title Generate sequence of dates for distribution analysis
    #'
    #' @description Generate a sequence of dates from start_date to end_date using the specified interval.
    #'
    #' @details This method uses base R's seq.Date function to create a comprehensive sequence of dates between two endpoints. This is essential for ensuring that all possible date values are represented in distribution plots, even if no events occurred on specific dates.
    #'
    #' @typed start_date (Date) Starting date for sequence generation
    #' @typed end_date (Date) Ending date for sequence generation
    #' @typed interval (character) Interval string passed to seq.Date such as "day", "week", etc.
    #' @typedreturn (Date) Vector containing all dates from start_date to end_date using interval
    #' @examples
    #' pg <- plot_generator$new()
    #' dates <- pg$private$get_all_dates(as.Date("2021-01-01"), as.Date("2021-01-10"), "day")
    get_all_dates = function(start_date, end_date, interval) {
      checkmate::assert_date(start_date, null.ok = FALSE)
      checkmate::assert_date(end_date, null.ok = FALSE)
      checkmate::assert_string(interval, null.ok = FALSE, min.chars = 1)
      all_dates <- seq.Date(from = start_date, to = end_date, by = interval)
      checkmate::assert_date(all_dates, null.ok = FALSE)
      return(all_dates)
    },

    #' @title Compute break points for numeric interval grouping
    #'
    #' @description Compute break points between min_number and max_number using specified width for creating intervals.
    #'
    #' @details This method creates appropriate break points for grouping numeric data into intervals. It handles edge cases where the range equals zero (returns min_number ± 0.5) or is smaller than the width (returns just the endpoints). Otherwise, it creates a sequence of break points with the specified width.
    #'
    #' @typed min_number (numeric) Minimum value for grouping
    #' @typed max_number (numeric) Maximum value for grouping  
    #' @typed width (numeric) Width of each interval
    #' @typedreturn (numeric) Vector of break points used for grouping
    #' @examples
    #' pg <- plot_generator$new()
    #' breaks <- pg$private$get_all_break_points(0, 100, 10)
    get_all_break_points = function(min_number, max_number, width) {
      checkmate::assert_numeric(min_number, null.ok = FALSE)
      checkmate::assert_numeric(max_number, null.ok = FALSE)
      checkmate::assert_numeric(width, null.ok = FALSE, lower = 1)
      break_points <- if (max_number == min_number) {
        c(min_number - 0.5, max_number + 0.5)
      } else if ((max_number - min_number) < width) {
        c(min_number, max_number)
      } else {
        c(seq(min_number, max_number - width, by = width), max_number)
      }
      checkmate::assert_numeric(break_points, null.ok = FALSE)
      return(break_points)
    },

    #' @title Generate interval group labels from break points
    #'
    #' @description Generate character labels representing numeric intervals using break points computed from min_number, max_number and width.
    #'
    #' @details This method creates human-readable labels for numeric intervals in the form "lower-upper". It first computes break points using get_all_break_points, then generates appropriate labels for each interval that will be displayed on plots.
    #'
    #' @typed min_number (numeric) Minimum value for grouping
    #' @typed max_number (numeric) Maximum value for grouping
    #' @typed width (numeric) Width of each interval
    #' @typedreturn (character) Vector of interval labels in the form "lower-upper"
    #' @examples
    #' pg <- plot_generator$new()
    #' labels <- pg$private$get_all_groups(0, 100, 10)
    get_all_groups = function(min_number, max_number, width) {
      break_points <- private$get_all_break_points(min_number, max_number, width)
      lower_bounds <- head(break_points, -1)
      upper_bounds <- pmin(lower_bounds + width - 1, max_number - 1)
      all_groups <- paste(lower_bounds, upper_bounds, sep = "-")
      checkmate::assert_character(all_groups, null.ok = FALSE)
      return(all_groups)
    },

    #' @title Create distribution summary with zero-filling for missing groups
    #'
    #' @description Create a distribution summary data frame for a grouping column including all groups with zero fill.
    #'
    #' @details This method counts occurrences by the specified grouping column and ensures that all possible values from complete_values are represented in the output, filling missing groups with zero counts. This is essential for creating complete distribution plots that show the full range of possible values.
    #'
    #' @typed transformed_data (data.frame) Data containing the grouping column
    #' @typed distribution_based_column (symbol) Unquoted column symbol to group by
    #' @typed distribution_based_column_name (character) Name of the grouping column in the output
    #' @typed complete_values (vector) All possible group values used to fill missing groups
    #' @typedreturn (data.frame) Summary data with grouping column and Distribution column
    #' @examples
    #' df <- data.frame(NUMBER = c(1,2,2,3))
    #' pg <- plot_generator$new()
    #' summary <- pg$private$create_distribution(df, NUMBER, "NUMBER", as.character(1:3))
    create_distribution = function(transformed_data, distribution_based_column, distribution_based_column_name, complete_values) {
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_string(distribution_based_column_name, null.ok = FALSE, min.chars = 1)
      if (is.character(complete_values)) {
        checkmate::assert_character(complete_values, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      } else if (inherits(complete_values, "Date")) {
        checkmate::assert_date(complete_values, null.ok = FALSE, any.missing = FALSE, min.len = 1)
      } else {
        abort(glue("complete_values must be a character or Date vector"))
      }
      summary_data <- transformed_data %>%
        filter(!is.na({{ distribution_based_column }})) %>%
        count({{ distribution_based_column }}, name = "Distribution") %>%
        complete({{ distribution_based_column }} := complete_values, fill = list(Distribution = 0)) %>%
        arrange({{ distribution_based_column }})
      checkmate::assert_data_frame(summary_data, null.ok = FALSE, min.cols = 2)
      checkmate::assert_names(
        names(summary_data),
        must.include = c(distribution_based_column_name, "Distribution")
      )
      return(summary_data)
    },

    #' @title Create daily date distribution for temporal analysis
    #'
    #' @description Computes a distribution of events per date within a specified interval.
    #'
    #' @details This method creates a complete date distribution by ensuring all dates in the specified range are represented, even if no events occurred on those dates. It delegates to create_distribution with a complete sequence of dates generated by get_all_dates.
    #'
    #' @typed transformed_data (data.frame) Data frame with DATE column
    #' @typed start_date (Date) Starting date for sequence, defaults to min of data
    #' @typed end_date (Date) Ending date for sequence, defaults to max of data
    #' @typed interval (character) Interval to use for sequence such as "day", defaults to "day"
    #' @typedreturn (data.frame) Summary data frame with DATE and Distribution columns
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE=as.Date(c("2021-01-01","2021-01-02","2021-01-02")))
    #' summary <- pg$private$create_date_distribution(df, as.Date("2021-01-01"), as.Date("2021-01-02"), "day")
    create_date_distribution = function(transformed_data, start_date = NULL, end_date = NULL, interval = "day") {
      if (is.null(start_date)) start_date <- min(transformed_data$DATE, na.rm = TRUE)
      if (is.null(end_date)) end_date <- max(transformed_data$DATE, na.rm = TRUE)
      summary_data <- transformed_data %>%
        private$create_distribution(DATE, "DATE", private$get_all_dates(start_date, end_date, interval))
      return(summary_data)
    },

    #' @title Create numeric distribution summary by grouping into intervals
    #'
    #' @description Generate a distribution summary by grouping numeric values into intervals.
    #'
    #' @details This method groups numeric data into intervals using cut() with break points computed by get_all_break_points and labels from get_all_groups. It ensures all intervals are represented in the output and converts the GROUP column to a factor for proper ordering in plots.
    #'
    #' @typed transformed_data (data.frame) Data frame containing a NUMBER column to group
    #' @typed min_number (numeric) Minimum value for grouping, defaults to min of data
    #' @typed max_number (numeric) Maximum value for grouping, defaults to max of data  
    #' @typed width (numeric) Width of each interval, defaults to 10
    #' @typedreturn (data.frame) Summary data frame with GROUP factor and Distribution counts
    #' @examples
    #' df <- data.frame(NUMBER = c(1,2,2,5,7))
    #' pg <- plot_generator$new()
    #' summary <- pg$private$create_number_distribution(df, 1, 7, 3)
    create_number_distribution = function(transformed_data, min_number = min(transformed_data$NUMBER, na.rm = TRUE), max_number = max(transformed_data$NUMBER, na.rm = TRUE), width = 10) {
      summary_data <- transformed_data %>%
        mutate(GROUP = cut(NUMBER, breaks = private$get_all_break_points(min_number, max_number, width), labels = private$get_all_groups(min_number, max_number, width), include.lowest = TRUE, right = FALSE)) %>%
        private$create_distribution(GROUP, "GROUP", private$get_all_groups(min_number, max_number, width))
      summary_data$GROUP <- as.factor(summary_data$GROUP)
      return(summary_data)
    },

    #' @title Create dual-condition distribution summary split by logical condition
    #'
    #' @description Generate a distribution summary split by a logical condition for each group value.
    #'
    #' @details This method creates a distribution that is split by a logical condition, creating separate counts for TRUE and FALSE values of the condition. It uses complete() to ensure all groups appear for both condition values, enabling comparative analysis between different subsets of the data.
    #'
    #' @typed transformed_data (data.frame) Data frame containing the grouping column
    #' @typed distribution_based_column (symbol) Unquoted column symbol to group by
    #' @typed distribution_based_column_name (character) Name of the grouping column in the output
    #' @typed condition (character) Logical expression string used to split the data
    #' @typed complete_values (vector) All possible group values to include in the summary
    #' @typedreturn (data.frame) Summary with distribution counts for each ConditionGroup
    #' @examples
    #' df <- data.frame(GROUP = c('A','B','A','C'), ZUL = c(TRUE, FALSE, TRUE, FALSE))
    #' pg <- plot_generator$new()
    #' summary <- pg$private$create_dual_distribution(df, GROUP, "GROUP", "ZUL == TRUE", c('A','B','C'))
    create_dual_distribution = function(transformed_data, distribution_based_column, distribution_based_column_name, condition, complete_values) {
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_string(condition, null.ok = FALSE, min.chars = 1, pattern = "^.+\\s*==\\s*.+$")
      if (is.character(complete_values)) {
        checkmate::assert_character(complete_values, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      } else if (inherits(complete_values, "Date")) {
        checkmate::assert_date(complete_values, null.ok = FALSE, any.missing = FALSE, min.len = 1)
      } else {
        abort(glue("complete_values must be a character or Date vector"))
      }
      summary_data <- transformed_data %>%
        filter(!is.na({{ distribution_based_column }})) %>%
        mutate(ConditionGroup = (!!rlang::parse_expr(condition))) %>%
        count({{ distribution_based_column }}, ConditionGroup, name = "Distribution") %>%
        ungroup() %>%
        complete({{ distribution_based_column }} := complete_values, ConditionGroup = c(TRUE, FALSE), fill = list(Distribution = 0)) %>%
        group_by(ConditionGroup) %>%
        arrange({{ distribution_based_column }}, ConditionGroup)
      checkmate::assert_data_frame(summary_data, null.ok = FALSE, min.cols = 3)
      checkmate::assert_names(
        names(summary_data),
        must.include = c(distribution_based_column_name, "ConditionGroup", "Distribution")
      )
      return(summary_data)
    },

    #' @title Create dual date distribution summary split by logical condition
    #'
    #' @description Compute a date-based distribution split by a logical condition over a date sequence.
    #'
    #' @details This method creates a date distribution that is split by a logical condition, enabling comparison between different subsets of date-based events. It delegates to create_dual_distribution with a complete sequence of dates.
    #'
    #' @typed transformed_data (data.frame) Data frame containing a DATE column
    #' @typed condition (character) Logical expression string to split by
    #' @typed start_date (Date) Starting date for sequence, defaults to min of data
    #' @typed end_date (Date) Ending date for sequence, defaults to max of data
    #' @typed interval (character) Interval string passed to seq.Date, defaults to "day"
    #' @typedreturn (data.frame) Summary with DATE, ConditionGroup and Distribution counts
    #' @examples
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c('J','N'))
    #' pg <- plot_generator$new()
    #' summary <- pg$private$create_date_dual_distribution(df, "ZUL == 'J'", as.Date('2021-01-01'), as.Date('2021-01-02'), "day")
    create_date_dual_distribution = function(transformed_data, condition, start_date = min(transformed_data$DATE, na.rm = TRUE), end_date = max(transformed_data$DATE, na.rm = TRUE), interval = "day") {
      summary_data <- private$create_dual_distribution(
        transformed_data,
        DATE,
        "DATE",
        condition,
        private$get_all_dates(start_date, end_date, interval)
      )
      return(summary_data)
    },

    #' @title Create dual numeric distribution summary split by logical condition
    #'
    #' @description Compute a numeric distribution split by a logical condition and grouped into intervals.
    #'
    #' @details This method creates a numeric distribution that is split by a logical condition, enabling comparison between different subsets of numeric data. It uses cut() for grouping into intervals and complete() to ensure both condition groups are represented for all intervals.
    #'
    #' @typed transformed_data (data.frame) Data frame containing a NUMBER column
    #' @typed condition (character) Logical expression string to split the data
    #' @typed min_number (numeric) Minimum value for grouping, defaults to min of data
    #' @typed max_number (numeric) Maximum value for grouping, defaults to max of data
    #' @typed width (numeric) Width of each interval, defaults to 10
    #' @typedreturn (data.frame) Summary data frame with GROUP, ConditionGroup and Distribution counts
    #' @examples
    #' df <- data.frame(NUMBER = 1:5, ZUL = c('J','N','J','N','J'))
    #' pg <- plot_generator$new()
    #' summary <- pg$private$create_number_dual_distribution(df, "ZUL == 'J'", 1, 5, 2)
    create_number_dual_distribution = function(transformed_data, condition, min_number = min(transformed_data$NUMBER, na.rm = TRUE), max_number = max(transformed_data$NUMBER, na.rm = TRUE), width = 10) {
      summary_data <- transformed_data %>%
        mutate(GROUP = cut(NUMBER, breaks = private$get_all_break_points(min_number, max_number, width), labels = private$get_all_groups(min_number, max_number, width), include.lowest = TRUE, right = FALSE)) %>%
        private$create_dual_distribution(GROUP, "GROUP", condition, private$get_all_groups(min_number, max_number, width))
      summary_data$GROUP <- factor(summary_data$GROUP,
        levels = private$get_all_groups(min_number, max_number, width)
      )
      return(summary_data)
    },

    #' @title Create cumulative distribution summary from existing distribution data
    #'
    #' @description Compute cumulative counts and percentages from a distribution summary, handling both grouped and ungrouped data.
    #'
    #' @details This method transforms distribution data into cumulative format by computing running sums and percentage values. For dual distributions with condition groups, it processes each group separately. The method safely handles edge cases like zero total distributions by using a minimum divisor of 1.
    #'
    #' @typed summary_data (data.frame) Summary data frame containing a Distribution column and optionally DATE/GROUP and ConditionGroup columns
    #' @typedreturn (data.frame) Summary data frame with added CumulativeDistribution and CumulativeDistributionPercentage columns
    #' @examples
    #' df_sum <- data.frame(GROUP = c('1,2','3,4'), Distribution = c(10,20))
    #' pg <- plot_generator$new()
    #' result <- pg$private$create_cumulative_distribution(df_sum)
    create_cumulative_distribution = function(summary_data) {
      checkmate::assert_data_frame(summary_data, null.ok = FALSE, min.col = 1)
      checkmate::assert_names(
        names(summary_data),
        must.include = c("Distribution")
      )
      x_col <- if ("DATE" %in% names(summary_data)) "DATE" else "GROUP"
      output <- if ("ConditionGroup" %in% names(summary_data)) {
        summary_data %>%
          group_by(ConditionGroup) %>%
          arrange(.data[[x_col]]) %>%
          mutate(CumulativeDistribution = cumsum(Distribution), CumulativeDistributionPercentage = round(100 * CumulativeDistribution / sum(Distribution, na.rm = TRUE), 2)) %>%
          ungroup()
      } else {
        total <- sum(summary_data$Distribution, na.rm = TRUE)
        safe_total <- if (total == 0) 1 else total
        summary_data %>%
          arrange(.data[[x_col]]) %>%
          mutate(CumulativeDistribution = cumsum(Distribution), CumulativeDistributionPercentage = round(100 * CumulativeDistribution / safe_total, 2)) %>%
          ungroup()
      }
      checkmate::assert_data_frame(output, null.ok = FALSE, min.col = 2)
      checkmate::assert_names(
        names(output),
        must.include = c("CumulativeDistribution", "CumulativeDistributionPercentage")
      )
      return(output)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3.2: Create annotations">

    #' @title Calculate descriptive statistics for annotation display
    #'
    #' @description Calculate descriptive statistics for a numeric vector including mean, standard deviation, and mode for use in plot annotations.
    #'
    #' @details This method computes essential statistical measures that are displayed in plot annotations. It handles edge cases for small datasets by setting standard deviation to zero when there's only one value. The mode is calculated as the most frequent value, with fallback to the first value if calculation fails.
    #'
    #' @typed values (numeric) Vector of numeric values from which to compute statistical metrics
    #' @typedreturn (list) Named list with elements mean (numeric), sd (numeric), and mode (numeric)
    #' @examples
    #' pg <- plot_generator$new()
    #' stats <- pg$private$compute_annotation_stats(c(1,2,2,3,4))
    compute_annotation_stats = function(values) {
      stats <- values %>%
        {
          list(
            mean = mean(., na.rm = TRUE),
            sd = if (length(.) <= 1) 0 else round(sd(., na.rm = TRUE), 2)
          )
        }
      mode_val <- names(sort(table(values), decreasing = TRUE))[1]
      stats$mode <- if (is.na(mode_val) || mode_val == "") values[1] else as.numeric(mode_val)
      return(stats)
    },

    #' @title Format statistical values into annotation string for plotting
    #'
    #' @description Convert computed statistics into a formatted annotation string with appropriate formatting for date or numeric types.
    #'
    #' @details This method takes statistical values and formats them into a multi-line annotation string suitable for display on plots. Date values are formatted as DD.MM.YYYY, while numeric values use two decimal places. The output includes mean (μ), mode (mod), and standard deviation (σ) with appropriate units.
    #'
    #' @typed stats (list) Named list with mean, mode, and sd values from compute_annotation_stats
    #' @typed type (character) One of "date" or "number" determining the formatting style
    #' @typedreturn (character) Multi-line annotation string with formatted statistical information
    #' @examples
    #' pg <- plot_generator$new()
    #' stats <- pg$private$compute_annotation_stats(c(1,2,2,3))
    #' ann_num <- pg$private$format_annotation(stats, "number")
    #' ann_date <- pg$private$format_annotation(stats, "date")
    format_annotation = function(stats, type) {
      fmt <- function(value) {
        if (type == "date") {
          format(as.Date(value, origin = "1970-01-01"), "%d.%m.%Y")
        } else {
          sprintf("%.2f", value)
        }
      }
      mean_str <- fmt(stats$mean)
      mode_str <- fmt(stats$mode)
      sd_str <- paste0(sprintf("%.2f", stats$sd), if (type == "date") " Tage" else "")
      lines <- c(
        paste0("\u03bc: ", mean_str),
        paste0("mod: ", mode_str),
        paste0("\u03c3: ", sd_str)
      )
      annotation <- paste(lines, collapse = "\n")
      return(annotation)
    },

    #' @title Create annotation string for date distribution statistics
    #'
    #' @description Compute descriptive statistics for date values and format them into an annotation string for display on date distribution plots.
    #'
    #' @details This method processes date data to extract statistical information including mean date, mode date, and standard deviation in days. It handles missing values by filtering out zero-distribution dates and returns a formatted annotation string. For empty datasets, it returns "Keine Ereignisse".
    #'
    #' @typed raw_data (data.frame) Data frame containing a DATE column with raw date values
    #' @typed summary_data (data.frame) Summary data frame containing DATE and Distribution columns  
    #' @typedreturn (character) Formatted annotation string with date statistics or "Keine Ereignisse" if no data
    #' @examples
    #' raw <- data.frame(DATE = as.Date(c("2022-03-01","2022-03-02","2022-03-02")))
    #' summary <- data.frame(DATE = as.Date(c("2022-03-01","2022-03-02")), Distribution = c(1, 2))
    #' pg <- plot_generator$new()
    #' annotation <- pg$private$compute_date_annotation(raw, summary)
    compute_date_annotation = function(raw_data, summary_data) {
      total_events <- sum(summary_data$Distribution, na.rm = TRUE)
      if (total_events == 0) {
        return("Keine Ereignisse")
      }
      annotation <- summary_data %>%
        filter(Distribution > 0) %>%
        uncount(Distribution) %>%
        pull(DATE) %>%
        as.numeric() %>%
        {
          private$compute_annotation_stats(.)
        } %>%
        {
          private$format_annotation(., "date")
        }
      return(annotation)
    },

    #' @title Create annotation string for numeric distribution statistics  
    #'
    #' @description Compute descriptive statistics for numeric values and format them into an annotation string for display on numeric distribution plots.
    #'
    #' @details This method processes numeric data to extract statistical information including mean, mode, and standard deviation. It handles missing values by replacing them with the minimum value and returns a formatted annotation string. For empty datasets, it returns "Keine Ereignisse".
    #'
    #' @typed raw_data (data.frame) Data frame containing a NUMBER column with raw numeric values
    #' @typed summary_data (data.frame) Summary data frame containing Distribution column
    #' @typedreturn (character) Formatted annotation string with numeric statistics or "Keine Ereignisse" if no data
    #' @examples
    #' pg <- plot_generator$new()
    #' raw <- data.frame(NUMBER=c(1,2,2,3,NA))
    #' summary <- data.frame(GROUP=c('1-2','3-4'), Distribution=c(3,1))
    #' annotation <- pg$private$compute_number_annotation(raw, summary)
    compute_number_annotation = function(raw_data, summary_data) {
      total_events <- sum(summary_data$Distribution, na.rm = TRUE)
      if (total_events == 0) {
        return("Keine Ereignisse")
      }
      annotation <- raw_data$NUMBER %>%
        replace_na(min(raw_data$NUMBER, na.rm = TRUE)) %>%
        {
          private$compute_annotation_stats(.)
        } %>%
        {
          private$format_annotation(., "number")
        }
      return(annotation)
    },

    #' @title Calculate annotation positioning coordinates for plots
    #'
    #' @description Determine x and y coordinates for placing annotation boxes on distribution plots, supporting both date and numeric axis types.
    #'
    #' @details This method calculates appropriate positioning for annotation boxes based on the plot type and data range. For date plots, it uses the date span to determine positioning. For numeric plots, it uses group indices. The method supports placing annotations on either side of the plot and can adjust text width based on annotation content.
    #'
    #' @typed summary_data (data.frame) Summary data containing either DATE/Distribution or GROUP/Distribution columns
    #' @typed type (character) One of "DATE" or "NUMBER" specifying the axis type
    #' @typed opposite (logical) Whether to place annotation on the opposite side of the plot
    #' @typed annotation_text (character) Optional annotation text to calculate width requirements, defaults to NULL
    #' @typedreturn (list) Named list with x_left, x_right, y_top, axis_range, and text_width for annotation placement
    #' @examples
    #' pg <- plot_generator$new()
    #' summary <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")), Distribution = c(5, 10))
    #' pos <- pg$private$compute_positions(summary, "DATE", FALSE)
    compute_positions = function(summary_data, type = c("DATE", "NUMBER"), opposite = FALSE, annotation_text = NULL) {
      checkmate::assert_data_frame(summary_data, null.ok = FALSE)
      # Determine type and validate column names
      type <- match.arg(type)
      
      # Calculate text width based on longest line if annotation text is provided
      text_width_factor <- if (!is.null(annotation_text)) {
        lines <- strsplit(annotation_text, "\n")[[1]]
        max_chars <- max(nchar(lines))
        # Estimate text width as proportion of axis range (roughly 1% per character)
        max_chars * 0.01
      } else {
        0.15 # Default width factor
      }
      
      if (type == "DATE") {
        checkmate::assert_names(names(summary_data), must.include = c("DATE", "Distribution"))
        coords <- summary_data %>%
          summarise(
            min_date = min(DATE, na.rm = TRUE),
            max_date = max(DATE, na.rm = TRUE),
            y_top = max(Distribution, na.rm = TRUE)
          ) %>%
          mutate(
            span = as.numeric(max_date - min_date, units = "days"),
            axis_range = span,
            text_width = span * text_width_factor,
            x_left = if (!opposite) min_date + span * 0.05 else max_date - span * 0.05 - text_width,
            x_right = if (!opposite) min_date + span * 0.95 else max_date - span * 0.05
          ) %>%
          select(x_left, x_right, y_top, axis_range, text_width) %>%
          as.list()
      } else {
        checkmate::assert_names(names(summary_data), must.include = c("GROUP", "Distribution"))
        coords <- summary_data %>%
          summarise(
            groups = list(as.character(GROUP)),
            y_top = max(Distribution, na.rm = TRUE)
          ) %>%
          mutate(
            min_group = groups[[1]][1],
            max_group = groups[[1]][length(groups[[1]])],
            span = length(groups[[1]]) - 1,
            axis_range = length(groups[[1]]),
            text_width = axis_range * text_width_factor,
            x_left = if (!opposite) min_group else max_group,
            x_right = if (!opposite) max_group else min_group
          ) %>%
          select(x_left, x_right, y_top, axis_range, text_width) %>%
          as.list()
      }
      return(coords)
    },

    #' @title Calculate annotation positioning coordinates for date plots
    #'
    #' @description Get coordinates for placing date annotations on distribution plots using standard positioning.
    #'
    #' @details This method is a convenience wrapper around compute_positions specifically for date-based plots. It calculates appropriate coordinates for annotation placement on the primary side of date distribution plots.
    #'
    #' @typed summary_data (data.frame) Summary data frame with DATE and Distribution columns
    #' @typed annotation_text (character) Optional annotation text to calculate width requirements, defaults to NULL
    #' @typedreturn (list) Named list with coordinates x_left, x_right, y_top, axis_range, and text_width for annotation placement
    #' @examples
    #' pg <- plot_generator$new()
    #' summary <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")), Distribution = c(5, 10))
    #' pos <- pg$private$compute_date_annotation_positions(summary)
    compute_date_annotation_positions = function(summary_data, annotation_text = NULL) {
      positions <- summary_data %>% private$compute_positions("DATE", FALSE, annotation_text)
      return(positions)
    },

    #' @title Calculate annotation positioning coordinates for date plots on opposite side
    #'
    #' @description Get coordinates for placing date annotations on the opposite side of distribution plots.
    #'
    #' @details This method is a convenience wrapper around compute_positions specifically for date-based plots when annotations need to be placed on the opposite side. This is useful for dual distribution plots where annotations for different groups need to be separated.
    #'
    #' @typed summary_data (data.frame) Summary data frame with DATE and Distribution columns
    #' @typed annotation_text (character) Optional annotation text to calculate width requirements, defaults to NULL
    #' @typedreturn (list) Named list with coordinates x_left, x_right, y_top, axis_range, and text_width for annotation placement on opposite side
    #' @examples
    #' pg <- plot_generator$new()
    #' summary <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")), Distribution = c(5, 10))
    #' pos <- pg$private$compute_date_annotation_positions_opposite(summary)
    compute_date_annotation_positions_opposite = function(summary_data, annotation_text = NULL) {
      positions <- summary_data %>% private$compute_positions("DATE", TRUE, annotation_text)
      return(positions)
    },

    #' @title Calculate annotation positioning coordinates for numeric plots
    #'
    #' @description Get coordinates for placing numeric annotations on distribution plots using standard positioning.
    #'
    #' @details This method is a convenience wrapper around compute_positions specifically for numeric-based plots. It calculates appropriate coordinates for annotation placement on the primary side of numeric distribution plots using group indices.
    #'
    #' @typed summary_data (data.frame) Summary data frame with GROUP and Distribution columns
    #' @typed annotation_text (character) Optional annotation text to calculate width requirements, defaults to NULL
    #' @typedreturn (list) Named list with coordinates x_left, x_right, y_top, axis_range, and text_width for annotation placement
    #' @examples
    #' pg <- plot_generator$new()
    #' summary <- data.frame(GROUP = c("0-10", "10-20"), Distribution = c(5, 10))
    #' pos <- pg$private$compute_number_annotation_positions(summary)
    compute_number_annotation_positions = function(summary_data, annotation_text = NULL) {
      positions <- summary_data %>% private$compute_positions("NUMBER", FALSE, annotation_text)
      return(positions)
    },

    #' @title Calculate annotation positioning coordinates for numeric plots on opposite side
    #'
    #' @description Get coordinates for placing numeric annotations on the opposite side of distribution plots.
    #'
    #' @details This method is a convenience wrapper around compute_positions specifically for numeric-based plots when annotations need to be placed on the opposite side. This is useful for dual distribution plots where annotations for different groups need to be separated.
    #'
    #' @typed summary_data (data.frame) Summary data frame with GROUP and Distribution columns
    #' @typed annotation_text (character) Optional annotation text to calculate width requirements, defaults to NULL
    #' @typedreturn (list) Named list with coordinates x_left, x_right, y_top, axis_range, and text_width for annotation placement on opposite side
    #' @examples
    #' pg <- plot_generator$new()
    #' summary <- data.frame(GROUP = c("0-10", "10-20"), Distribution = c(5, 10))
    #' pos <- pg$private$compute_number_annotation_positions_opposite(summary)
    compute_number_annotation_positions_opposite = function(summary_data, annotation_text = NULL) {
      positions <- summary_data %>% private$compute_positions("NUMBER", TRUE, annotation_text)
      return(positions)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3.3: Create plot parts">

    #' @title Create annotation rectangle background for plot annotations
    #'
    #' @description Generate annotation rectangle with proper coordinate handling for both date and numeric plot types.
    #'
    #' @details This method creates a rectangular background element for plot annotations, handling different coordinate systems for date and numeric plots. It calculates appropriate margins and positioning based on annotation constants and supports placing annotations on either side of the plot.
    #'
    #' @typed position (list) Position coordinates containing x_left/x_right, y_top, axis_range, and text_width
    #' @typed constants (list) Annotation constants from get_annotation_constants containing margin specifications
    #' @typed text_height (numeric) Height of the text block that will be placed over the rectangle
    #' @typed y_position (numeric) Y coordinate position for the annotation
    #' @typed type (character) Either "date" or "number" to determine coordinate handling
    #' @typed summary_data (data.frame) Summary data, required for numeric plots to determine group positioning, defaults to NULL
    #' @typed use_right (logical) Whether to use x_right instead of x_left for positioning, defaults to FALSE
    #' @typedreturn (ggplot2::annotation) ggplot2 annotation layer containing the background rectangle
    #' @examples
    #' pg <- plot_generator$new()
    #' pos <- list(x_left = 1, x_right = 5, y_top = 10, axis_range = 10, text_width = 2)
    #' constants <- pg$private$get_annotation_constants()
    #' rect <- pg$private$create_annotation_rect(pos, constants, 1.5, 12, "date")
    create_annotation_rect = function(position, constants, text_height, y_position, type, summary_data = NULL, use_right = FALSE) {
      # Use pre-calculated values from position
      axis_range <- position$axis_range
      text_width <- position$text_width
      
      if (type == "date") {
        x_pos <- if (use_right) position$x_right else position$x_left
      } else {
        groups <- unique(summary_data$GROUP)
        x_pos <- if (use_right) which(groups == position$x_right) else which(groups == position$x_left)
      }
      
      if (use_right) {
        right_margin <- (axis_range * constants$rect_margin_right)
        xmax <- x_pos + right_margin
        xmin <- x_pos + right_margin - text_width
      } else {
        left_margin <- (axis_range * constants$rect_margin_left)
        xmin <- x_pos - left_margin
        xmax <- x_pos - left_margin + text_width
      }
      
      ymin <- y_position - text_height - constants$rect_margin_bottom
      ymax <- y_position + constants$rect_margin_top
      
      annotate("rect",
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = private$get_colors()["annotation_background"],
        colour = private$get_colors()["annotation_border"],
        alpha = 0.9
      )
    },

    #' @title Provide consistent ggplot2 theme for all plot types
    #'
    #' @description Create a minimalistic theme with white background, rotated x-axis text, and consistent styling across all plots.
    #'
    #' @details This method provides standardized theming for all plots generated by the PlotGenerator class. It uses theme_minimal as a base and applies custom colors from the color palette, rotates x-axis labels for better readability, and sets appropriate margins and grid styling.
    #'
    #' @typedreturn (ggplot2::theme) ggplot2 theme object with standardized styling
    #' @examples
    #' pg <- plot_generator$new()
    #' theme <- pg$private$get_common_theme()
    #' # Use in plot: ggplot(data) + geom_point() + theme
    get_common_theme = function() {
      theme <- theme_minimal() +
        theme(
          plot.background = element_rect(fill = private$get_colors()["background"], colour = NA),
          panel.background = element_rect(fill = private$get_colors()["panel_background"], colour = NA),
          panel.grid.major = element_line(colour = private$get_colors()["panel_grid_major"]),
          panel.grid.minor = element_line(colour = private$get_colors()["panel_grid_minor"]),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          plot.margin = margin(20, 5, 5, 5)
        )
      return(theme)
    },

    #' @title Create date scale for x-axis with weekly breaks and formatted labels
    #'
    #' @description Generate a ggplot2 date scale for x-axis with weekly breaks, German date formatting, and appropriate expansion.
    #'
    #' @details This method creates a standardized date scale for use in date-based distribution plots. It sets weekly breaks for readability, uses German date format (DD.MM), and applies 5-10% expansion to provide appropriate margins around the data range.
    #'
    #' @typed min_date (Date) Starting date for the scale limits
    #' @typed max_date (Date) Ending date for the scale limits
    #' @typedreturn (ggplot2::scale) ggplot2 scale object configured for date display
    #' @examples
    #' pg <- plot_generator$new()
    #' scale <- pg$private$get_date_scale(as.Date("2021-01-01"), as.Date("2021-01-31"))
    get_date_scale = function(min_date, max_date) {
      checkmate::assert_date(min_date, null.ok = FALSE)
      checkmate::assert_date(max_date, null.ok = FALSE)
      scale <- scale_x_date(
        limits = c(min_date, max_date),
        date_breaks = "1 week",
        date_labels = "%d.%m",
        expand = expansion(mult = c(0.05, 0.10))
      )
      return(scale)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3.4: Create distribution plots">

    #' @title Generate distribution plot with statistical annotations for any data type
    #'
    #' @description Create a distribution plot with annotation for either date or numeric data types, automatically selecting appropriate geometry and scales.
    #'
    #' @details This method serves as the core plotting function for single distribution visualizations. It automatically determines the appropriate geom layers (line/point for dates, bar for numbers), axis labels, and annotation positioning based on the data type. The method computes statistical annotations, creates background rectangles, and applies consistent theming.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed DATE or NUMBER column
    #' @typed summary_data (data.frame) Summary data frame with either DATE/Distribution or GROUP/Distribution columns
    #' @typed label (character) Descriptive label used for axis labels and plot title
    #' @typed type (character) Either "date" or "number" determining the plot type and formatting
    #' @typedreturn (ggplot) Complete ggplot object with distribution visualization and statistical annotations
    #' @examples
    #' pg <- plot_generator$new()
    #' df_date <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")))
    #' summary_date <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")), Distribution = c(5, 3))
    #' plot <- pg$private$plot_distribution_generic(df_date, summary_date, "Events", "date")
    plot_distribution_generic = function(transformed_data, summary_data, label, type) {
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_data_frame(summary_data, null.ok = FALSE)
      checkmate::assert_string(label, null.ok = FALSE)
      checkmate::assert_choice(type, c("date", "number"))
      constants <- private$get_annotation_constants()
      if (type == "date") {
        annotation <- private$compute_date_annotation(transformed_data, summary_data)
        position <- private$compute_date_annotation_positions(summary_data, annotation)
        geom_layer <- list(geom_line(), geom_point(), private$get_date_scale(min(summary_data$DATE), max(summary_data$DATE)))
        x_lab <- "Tag"
        y_lab <- paste(label, "pro Tag")
        title_suffix <- "Verteilung der"
        aes_mapping <- aes(x = DATE, y = Distribution)
      } else {
        annotation <- private$compute_number_annotation(transformed_data, summary_data)
        position <- private$compute_number_annotation_positions(summary_data, annotation)
        geom_layer <- list(geom_bar(stat = "identity"))
        x_lab <- "Gruppe"
        y_lab <- paste(label, "pro Gruppe")
        title_suffix <- "Verteilung der"
        aes_mapping <- aes(x = GROUP, y = Distribution)
      }
      annotation_lines <- length(strsplit(annotation, "\n")[[1]])
      line_height <- constants$line_height_factor * position$y_top
      text_height <- annotation_lines * line_height
      annotation_rect <- private$create_annotation_rect(
        position, constants, text_height,
        position$y_top * constants$annotation_y_factor,
        type, summary_data,
        use_right = FALSE
      )
      plot <- ggplot(summary_data, aes_mapping) +
        geom_layer +
        labs(title = paste(title_suffix, label), x = x_lab, y = y_lab) +
        annotation_rect +
        annotate("text", x = position$x_left, y = position$y_top * constants$annotation_y_factor, label = annotation, hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
        private$get_common_theme()
      invisible(plot)
    },

    #' @title Generate date-based distribution plot with statistical annotations
    #'
    #' @description Create a line plot showing event distribution over dates with computed statistical annotations.
    #'
    #' @details This method creates date distribution plots by delegating to plot_distribution_generic with the "date" type parameter. It automatically generates line and point plots for temporal data with proper date scaling and annotation positioning.
    #'
    #' @typed transformed_data (data.frame) Data frame containing a DATE column with parsed dates
    #' @typed summary_data (data.frame) Summary data frame containing DATE and Distribution columns
    #' @typed label (character) Descriptive label used for axis labels and plot title
    #' @typedreturn (ggplot) ggplot object with date distribution visualization and statistical annotations
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), Distribution = c(5,3))
    #' plot <- pg$private$plot_date_distribution(df, df, 'Applications')
    plot_date_distribution = function(transformed_data, summary_data, label) {
      plot <- transformed_data %>% private$plot_distribution_generic(summary_data, label, "date")
      invisible(plot)
    },

    #' @title Generate numeric distribution bar plot with statistical annotations
    #'
    #' @description Create a bar chart showing numeric value distribution across groups with computed statistical annotations.
    #'
    #' @details This method creates numeric distribution plots by delegating to plot_distribution_generic with the "number" type parameter. It automatically generates bar charts for grouped numeric data with proper scaling and annotation positioning.
    #'
    #' @typed transformed_data (data.frame) Data frame containing a NUMBER column with raw numeric values
    #' @typed summary_data (data.frame) Summary data frame containing GROUP and Distribution columns
    #' @typed label (character) Descriptive label used for axis labels and plot title
    #' @typedreturn (ggplot) ggplot object with numeric distribution visualization and statistical annotations
    #' @examples
    #' pg <- plot_generator$new()
    #' df_raw <- data.frame(NUMBER = c(1,2,2,3,NA))
    #' df_sum <- data.frame(GROUP = c('1-2','3-4'), Distribution = c(3,1))
    #' plot <- pg$private$plot_number_distribution(df_raw, df_sum, 'Points')
    plot_number_distribution = function(transformed_data, summary_data, label) {
      plot <- transformed_data %>% private$plot_distribution_generic(summary_data, label, "number")
      invisible(plot)
    },

    #' @title Generate dual distribution plot with condition-based splitting and annotations
    #'
    #' @description Create a dual distribution plot for date or numeric data split by a logical condition with separate annotations for each group.
    #'
    #' @details This method creates comparative distribution plots by splitting data based on a logical condition and displaying both groups with distinct colors and annotations. It handles both date and numeric data types, automatically selecting appropriate geometries and positioning annotations on opposite sides for clarity.
    #'
    #' @typed transformed_data (data.frame) Data frame with parsed data and condition columns
    #' @typed summary_data (data.frame) Summary data frame with grouping, ConditionGroup, and Distribution columns
    #' @typed label (character) Descriptive label used for axis labels, plot title, and legend
    #' @typed condition (character) String expression evaluated to split data into TRUE/FALSE groups
    #' @typed type (character) Either "date" or "number" to select appropriate geometry and scale
    #' @typedreturn (ggplot) ggplot object with dual distribution visualization and separate annotations
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c(TRUE, FALSE))
    #' summary <- pg$private$create_date_dual_distribution(df, "ZUL == TRUE", as.Date('2021-01-01'), as.Date('2021-01-02'), "day")
    #' plot <- pg$private$plot_distribution_dual_generic(df, summary, 'Applications', 'ZUL == TRUE', 'date')
    plot_distribution_dual_generic = function(transformed_data, summary_data, label, condition, type) {
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_data_frame(summary_data, null.ok = FALSE)
      checkmate::assert_string(label, null.ok = FALSE)
      checkmate::assert_string(condition, null.ok = FALSE)
      checkmate::assert_choice(type, c("date", "number"))
      expr <- rlang::parse_expr(condition)
      constants <- private$get_annotation_constants()
      data_true <- transformed_data %>% filter(!!expr)
      summary_true <- summary_data %>% filter(ConditionGroup == TRUE)
      ann_true <- if (type == "date") private$compute_date_annotation(data_true, summary_true) else private$compute_number_annotation(data_true, summary_true)
      pos_true <- if (type == "date") private$compute_date_annotation_positions(summary_true, ann_true) else private$compute_number_annotation_positions(summary_true, ann_true)
      
      data_false <- transformed_data %>% filter(!(!!expr))
      summary_false <- summary_data %>% filter(ConditionGroup == FALSE)
      ann_false <- if (type == "date") private$compute_date_annotation(data_false, summary_false) else private$compute_number_annotation(data_false, summary_false)
      pos_false <- if (type == "date") private$compute_date_annotation_positions_opposite(summary_false, ann_false) else private$compute_number_annotation_positions_opposite(summary_false, ann_false)
      y_max <- max(summary_data$Distribution, na.rm = TRUE)
      ann_true_lines <- length(strsplit(ann_true, "\n")[[1]])
      ann_false_lines <- length(strsplit(ann_false, "\n")[[1]])
      line_height <- constants$line_height_factor * y_max
      text_height_true <- ann_true_lines * line_height
      text_height_false <- ann_false_lines * line_height
      annotation_rect_true <- private$create_annotation_rect(
        pos_true, constants, text_height_true,
        y_max * constants$annotation_y_factor,
        type, summary_data,
        use_right = FALSE
      )
      annotation_rect_false <- private$create_annotation_rect(
        pos_false, constants, text_height_false,
        y_max * constants$annotation_y_factor,
        type, summary_data,
        use_right = TRUE
      )
      aes_map <- if (type == "date") aes(x = DATE, y = Distribution, colour = as.factor(ConditionGroup), group = ConditionGroup) else aes(x = GROUP, y = Distribution, fill = as.factor(ConditionGroup), group = ConditionGroup)
      plot <- ggplot(summary_data, aes_map) +
        (if (type == "date") list(geom_line(), geom_point(), private$get_date_scale(min(summary_data$DATE), max(summary_data$DATE))) else geom_bar(stat = "identity", position = position_dodge(), colour = private$get_colors()["bar_outline"])) +
        labs(
          title = paste("Verteilung der", label),
          x = if (type == "date") "Tag" else "Gruppe",
          y = paste(label, if (type == "date") "pro Tag" else "pro Gruppe"),
          colour = label
        ) +
        annotation_rect_true +
        annotate(
          "text",
          x = pos_true$x_left,
          y = y_max * constants$annotation_y_factor,
          label = ann_true,
          hjust = 0,
          vjust = 1,
          size = 3.5,
          fontface = "italic",
          colour = private$get_colors()["TRUE"]
        ) +
        annotation_rect_false +
        annotate(
          "text",
          x = pos_false$x_right,
          y = y_max * constants$annotation_y_factor,
          label = ann_false,
          hjust = 1,
          vjust = 1,
          size = 3.5,
          fontface = "italic",
          colour = private$get_colors()["FALSE"]
        ) +
        (
          if (type == "date") {
            scale_colour_manual(
              name = "Zulassung",
              values = private$get_colors(),
              breaks = c("TRUE", "FALSE"),
              labels = c("Angenommen", "Abgelehnt")
            )
          } else {
            scale_fill_manual(
              name = "Zulassung",
              values = private$get_colors(),
              breaks = c("TRUE", "FALSE"),
              labels = c("Angenommen", "Abgelehnt")
            )
          }) +
        private$get_common_theme()
      invisible(plot)
    },

    #' @title Generate dual date distribution plot with condition-based splitting
    #'
    #' @description Create a dual distribution plot for date data split by a logical condition with separate annotations for each group.
    #'
    #' @details This method creates comparative date distribution plots by delegating to plot_distribution_dual_generic with the "date" type parameter. It automatically generates line and point plots for both groups with distinct colors and annotations positioned on opposite sides.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed DATE column and original condition records
    #' @typed summary_data (data.frame) Summary data frame with DATE, ConditionGroup, and Distribution columns
    #' @typed label (character) Descriptive label used for axis labels, plot title, and legend
    #' @typed condition (character) String expression evaluated to split data into groups
    #' @typedreturn (ggplot) ggplot object with dual date distribution visualization and separate annotations
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c(TRUE, FALSE))
    #' summary <- pg$private$create_date_dual_distribution(df, "ZUL == TRUE", as.Date('2021-01-01'), as.Date('2021-01-02'), "day")
    #' plot <- pg$private$plot_date_dual_distribution(df, summary, "Applications", "ZUL == TRUE")
    plot_date_dual_distribution = function(transformed_data, summary_data, label, condition) {
      plot <- transformed_data %>% private$plot_distribution_dual_generic(summary_data, label, condition, "date")
      invisible(plot)
    },

    #' @title Generate dual numeric distribution plot with condition-based splitting
    #'
    #' @description Create a dual distribution bar plot for numeric data split by a logical condition with separate annotations for each group.
    #'
    #' @details This method creates comparative numeric distribution plots by delegating to plot_distribution_dual_generic with the "number" type parameter. It automatically generates bar charts for both groups with distinct colors and annotations positioned on opposite sides.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed NUMBER column and original condition records
    #' @typed summary_data (data.frame) Summary data frame with GROUP, ConditionGroup, and Distribution columns
    #' @typed label (character) Descriptive label used for axis labels, plot title, and legend
    #' @typed condition (character) String expression evaluated to split data into groups
    #' @typedreturn (ggplot) ggplot object with dual numeric distribution visualization and separate annotations
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(NUMBER = c(1,2,2,3), ZUL = c(TRUE, FALSE, TRUE, FALSE))
    #' summary <- pg$private$create_number_dual_distribution(df, "ZUL == TRUE", 1, 4, 1)
    #' plot <- pg$private$plot_number_dual_distribution(df, summary, "Scores", "ZUL == TRUE")
    plot_number_dual_distribution = function(transformed_data, summary_data, label, condition) {
      plot <- transformed_data %>% private$plot_distribution_dual_generic(summary_data, label, condition, "number")
      invisible(plot)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3.5: Create cumulative distribution plots">

    #' @title Generate cumulative distribution plot for any data type
    #'
    #' @description Create a cumulative distribution plot for date or numeric data with percentage scaling and appropriate geometry.
    #'
    #' @details This method creates cumulative distribution plots by automatically selecting appropriate visualization types based on data type. For date data, it uses line and point plots with date scaling. For numeric data, it uses bar charts. All plots include percentage scaling and consistent theming.
    #'
    #' @typed cumulative_data (data.frame) Summary data with either DATE/CumulativeDistributionPercentage or GROUP/CumulativeDistributionPercentage columns
    #' @typed label (character) Descriptive label used for title and axis label prefix
    #' @typed type (character) Either "date" or "number" determining the visualization type
    #' @typedreturn (ggplot) ggplot object with cumulative distribution visualization and percentage scaling
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), CumulativeDistributionPercentage = c(50,100))
    #' plot <- pg$private$plot_cumulative_distribution_generic(df, 'Applications', 'date')
    plot_cumulative_distribution_generic = function(cumulative_data, label, type) {
      checkmate::assert_data_frame(cumulative_data, null.ok = FALSE)
      if (type == "date") {
        checkmate::assert_names(names(cumulative_data), must.include = c("DATE", "CumulativeDistributionPercentage"))
        plot <- cumulative_data %>%
          ggplot(aes(x = DATE, y = CumulativeDistributionPercentage)) +
          geom_line() +
          geom_point() +
          private$get_date_scale(min(cumulative_data$DATE), max(cumulative_data$DATE)) +
          scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
          labs(title = paste("Kumulative Verteilung der", label), x = "Tag", y = paste0("% der Gesamt-", label, " pro Tag")) +
          private$get_common_theme()
      } else {
        checkmate::assert_names(names(cumulative_data), must.include = c("GROUP", "CumulativeDistributionPercentage"))
        plot <- cumulative_data %>%
          ggplot(aes(x = GROUP, y = CumulativeDistributionPercentage)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Kumulative Verteilung der", label), x = "Gruppe", y = paste0("% der Gesamt-", label, " pro Gruppe")) +
          private$get_common_theme()
      }
      checkmate::assert_class(plot, "ggplot", null.ok = FALSE)
      invisible(plot)
    },

    #' Plot cumulative date distribution
    #'
    #' @title Generate cumulative distribution plot for date data
    #'
    #' @description Create a cumulative distribution line plot for date data showing percentage progression over time.
    #'
    #' @details This method creates cumulative distribution plots for temporal data by delegating to plot_cumulative_distribution_generic with the "date" type parameter. It automatically generates line and point plots for cumulative percentages with proper date scaling and axis formatting.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed DATE column and original records
    #' @typed cumulative_data (data.frame) Summary data with DATE and CumulativeDistributionPercentage columns  
    #' @typed label (character) Title and axis label prefix
    #' @typedreturn (ggplot) ggplot object with cumulative date distribution visualization and percentage scaling
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), CumulativeDistributionPercentage = c(50,100))
    #' plot <- pg$private$plot_date_cumulative_distribution(df, df, 'Applications')
    plot_date_cumulative_distribution = function(transformed_data, cumulative_data, label) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_generic(label, "date")
      invisible(plot)
    },

    #' Plot cumulative numeric distribution
    #'
    #' @title Generate cumulative distribution plot for numeric data
    #'
    #' @description Create a cumulative distribution bar plot for numeric data showing percentage progression across groups.
    #'
    #' @details This method creates cumulative distribution plots for grouped numeric data by delegating to plot_cumulative_distribution_generic with the "number" type parameter. It automatically generates bar charts for cumulative percentages with proper scaling and axis formatting.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed NUMBER column and original records
    #' @typed cumulative_data (data.frame) Summary data with GROUP and CumulativeDistributionPercentage columns
    #' @typed label (character) Title and axis label prefix
    #' @typedreturn (ggplot) ggplot object with cumulative numeric distribution visualization and percentage scaling
    #' @examples
    #' pg <- plot_generator$new()
    #' df_num <- data.frame(GROUP = c('1,2','3,4'), CumulativeDistributionPercentage = c(25,100))
    #' plot <- pg$private$plot_number_cumulative_distribution(df_raw, df_num, 'Scores')
    plot_number_cumulative_distribution = function(transformed_data, cumulative_data, label) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_generic(label, "number")
      invisible(plot)
    },

    #' Generic dual cumulative distribution plotting helper
    #'
    #' @title Generate dual cumulative distribution plot with condition-based splitting for any data type
    #'
    #' @description Create a dual cumulative distribution plot for date or numeric data split by a logical condition with separate series for each group.
    #'
    #' @details This method creates comparative cumulative distribution plots with condition-based grouping. It automatically selects appropriate geometries based on data type (line/point for dates, bars for numbers) and applies distinct colors and legends for each condition group with percentage scaling.
    #'
    #' @typed cumulative_data (data.frame) Summary data with DATE or GROUP, ConditionGroup, and CumulativeDistributionPercentage columns
    #' @typed label (character) Title and axis label prefix
    #' @typed condition (character) Logical expression to split data
    #' @typed type (character) Either "date" or "number"
    #' @typedreturn (ggplot) ggplot object with dual cumulative distribution visualization and separate condition series
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ConditionGroup = c(TRUE, FALSE), CumulativeDistributionPercentage = c(50,100))
    #' plot <- pg$private$plot_cumulative_distribution_dual_generic(df, 'Applications', 'ZUL == TRUE', 'date')
    plot_cumulative_distribution_dual_generic = function(cumulative_data, label, condition, type) {
      checkmate::assert_data_frame(cumulative_data, null.ok = FALSE)
      checkmate::assert_string(label, null.ok = FALSE, min.chars = 1)
      checkmate::assert_string(condition, null.ok = FALSE, min.chars = 1, pattern = "^.+\\s*==\\s*.+$")
      if (type == "date") {
        checkmate::assert_names(names(cumulative_data), must.include = c("DATE", "ConditionGroup", "CumulativeDistributionPercentage"))
        plot <- cumulative_data %>%
          ggplot(aes(x = DATE, y = CumulativeDistributionPercentage, colour = as.factor(ConditionGroup), group = ConditionGroup)) +
          geom_line() +
          geom_point() +
          private$get_date_scale(min(cumulative_data$DATE), max(cumulative_data$DATE)) +
          scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
          labs(title = paste("Kumulative Verteilung der", label), x = "Tag", y = paste0("% der Gesamt-", label, " pro Tag"), colour = label) +
          scale_colour_manual(
            name = "Zulassung",
            values = private$get_colors(),
            breaks = c("TRUE", "FALSE"),
            labels = c("Angenommen", "Abgelehnt")
          ) +
          private$get_common_theme()
      } else {
        checkmate::assert_names(names(cumulative_data), must.include = c("GROUP", "ConditionGroup", "CumulativeDistributionPercentage"))
        plot <- cumulative_data %>%
          ggplot(aes(x = GROUP, y = CumulativeDistributionPercentage, fill = as.factor(ConditionGroup), group = ConditionGroup)) +
          geom_bar(stat = "identity", position = position_dodge(), colour = private$get_colors()["bar_outline"]) +
          labs(title = paste("Kumulative Verteilung der", label), x = "Gruppe", y = paste0("% der Gesamt-", label, " pro Tag"), fill = label) +
          scale_fill_manual(
            name = "Zulassung",
            values = private$get_colors(),
            breaks = c("TRUE", "FALSE"),
            labels = c("Angenommen", "Abgelehnt")
          ) +
          private$get_common_theme()
      }
      checkmate::assert_class(plot, "ggplot", null.ok = FALSE)
      invisible(plot)
    },

    #' Plot dual cumulative date distribution
    #'
    #' @title Generate dual cumulative distribution plot for date data with condition-based splitting
    #'
    #' @description Create a dual cumulative distribution line plot for date data split by a logical condition with separate series for each group.
    #'
    #' @details This method creates comparative cumulative date distribution plots by delegating to plot_cumulative_distribution_dual_generic with the "date" type parameter. It automatically generates line and point plots for both condition groups with distinct colors and cumulative percentage scaling.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed DATE column and original records
    #' @typed cumulative_data (data.frame) Summary data with DATE, ConditionGroup, and CumulativeDistributionPercentage columns
    #' @typed label (character) Title and axis label prefix
    #' @typed condition (character) Logical expression used to split data
    #' @typedreturn (ggplot) ggplot object with dual cumulative date distribution visualization and separate condition series
    #' @examples
    #' pg <- plot_generator$new()
    #' df_raw <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c(TRUE, FALSE))
    #' cum <- pg$private$create_cumulative_distribution(summary_df)
    #' plot <- pg$private$plot_date_dual_cumulative_distribution(df_raw, cum, 'Applications', 'ZUL == TRUE')
    plot_date_dual_cumulative_distribution = function(transformed_data, cumulative_data, label, condition) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_dual_generic(label, condition, "date")
      invisible(plot)
    },

    #' Plot dual cumulative numeric distribution
    #'
    #' @title Generate dual cumulative distribution plot for numeric data with condition-based splitting
    #'  
    #' @description Create a dual cumulative distribution bar plot for numeric data split by a logical condition with separate series for each group.
    #'
    #' @details This method creates comparative cumulative numeric distribution plots by delegating to plot_cumulative_distribution_dual_generic with the "number" type parameter. It automatically generates bar charts for both condition groups with distinct colors and cumulative percentage scaling.
    #'
    #' @typed transformed_data (data.frame) Data frame containing parsed NUMBER column and original records
    #' @typed cumulative_data (data.frame) Summary data with GROUP, ConditionGroup, and CumulativeDistributionPercentage columns
    #' @typed label (character) Title and axis label prefix
    #' @typed condition (character) Logical expression used to split data
    #' @typedreturn (ggplot) ggplot object with dual cumulative numeric distribution visualization and separate condition series
    #' @examples
    #' pg <- plot_generator$new()
    #' df_raw <- data.frame(NUMBER = c(1,2,3), ZUL = c(TRUE, FALSE, TRUE))
    #' cum <- pg$private$create_cumulative_distribution(summary_df)
    #' plot <- pg$private$plot_number_dual_cumulative_distribution(df_raw, cum, 'Scores', 'ZUL == TRUE')
    plot_number_dual_cumulative_distribution = function(transformed_data, cumulative_data, label, condition) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_dual_generic(label, condition, "number")
      invisible(plot)
    },

    # </editor-fold>

    # </editor-fold>

    # <editor-fold desc="Step 4: Create File Name">

    #' @title Create output file name for generated plots
    #'
    #' @description Build a complete file path by combining the target directory with components derived from the source filename and additional identifiers.
    #'
    #' @details This method constructs file names for output plots by extracting the base name from the source path, removing the extension, and combining it with the target directory, infix string, and suffix. It ensures consistent file naming conventions across all generated plots.
    #'
    #' @typed source_path (character) Path to the input file
    #' @typed target_path (character) Directory path for the output file
    #' @typed infix (character) One-element string inserted between the prefix and suffix
    #' @typed suffix (character) One-element string appended as file suffix
    #' @typedreturn (character) Complete file path for the output plot
    #' @examples
    #' pg <- plot_generator$new()
    #' path <- pg$private$create_file_name("data/input.csv", "out", "plot", "png")
    create_file_name = function(source_path, target_path, infix, suffix) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      checkmate::assert_string(infix, null.ok = FALSE, min.chars = 1)
      checkmate::assert_string(suffix, null.ok = FALSE, min.chars = 1)
      prefix <- file_path_sans_ext(basename(source_path))
      path <- file.path(target_path, paste(prefix, infix, suffix, sep = "_"))
      checkmate::assert_string(path, null.ok = FALSE, min.chars = 1)
      invisible(path)
    },

    # </editor-fold>

    # <editor-fold desc="Step 5: Save Plots">

    #' @title Save ggplot object to disk with specified dimensions
    #'
    #' @description Write a ggplot object to file using ggsave with custom width and height specifications.
    #'
    #' @details This method provides a standardized interface for saving generated plots to disk with consistent dimensions. It validates input parameters and uses ggsave's built-in format detection based on the file extension to determine the output format.
    #'
    #' @typed filename (character) File path where the plot will be saved
    #' @typed plot (ggplot) ggplot object to save to disk
    #' @typed width (numeric) Width of the output in inches  
    #' @typed height (numeric) Height of the output in inches
    #' @typedreturn (NULL) Invisibly returns NULL after saving the plot
    #' @examples
    #' pg <- plot_generator$new()
    #' plot <- ggplot(data.frame(x=1:10, y=1:10), aes(x,y)) + geom_point()
    #' pg$private$save_plot("plot.png", plot, 8, 6)
    save_plot = function(filename, plot, width, height) {
      checkmate::assert_string(filename, null.ok = FALSE)
      checkmate::assert_class(plot, "ggplot", null.ok = FALSE)
      checkmate::assert_numeric(width, null.ok = FALSE, lower = 0)
      checkmate::assert_numeric(height, null.ok = FALSE, lower = 0)
      ggsave(filename = filename, plot = plot, width = width, height = height)
      invisible(NULL)
    },

    # </editor-fold>

    # <editor-fold desc="Create Plot">

    # <editor-fold desc="Generic">

    #' @title Generic processing pipeline for generating and saving distribution plots
    #'
    #' @description Load raw data, apply preparation functions, compute summary and cumulative distributions, generate plots, and save output files to disk.
    #'
    #' @details This method provides a centralized workflow for processing CSV files and generating standardized distribution plots. It handles data loading with column type specification, applies custom preparation functions, computes statistical summaries, creates visualizations, generates appropriate file names, and saves plots with consistent dimensions.
    #'
    #' @typed source_path (character) Path to the CSV input file
    #' @typed target_path (character) Directory path for saving output plots
    #' @typed date_columns (character) Vector of column names to parse as dates; may be NULL
    #' @typed number_columns (character) Vector of column names to parse as numeric values; may be NULL
    #' @typed condition_columns (character) Vector of column names for grouping conditions; may be NULL
    #' @typed prepare_function (function) Function transforming raw data.frame to prepared data.frame
    #' @typed distribution_function (function) Function computing summary distribution from prepared data
    #' @typed cumulative_distribution_function (function) Function computing cumulative distribution from summary data
    #' @typed distribution_plot_function (function) Function generating distribution ggplot from prepared and summary data with infix label
    #' @typed cumulative_distribution_plot_function (function) Function generating cumulative distribution ggplot from prepared and cumulative data with infix label
    #' @typed infix (character) Label string used in plot titles and file names
    #' @typedreturn (NULL) Invisibly returns NULL after saving distribution and cumulative distribution plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$process_plot_generic(
    #'   source_path = "data/input.csv",
    #'   target_path = "out",
    #'   date_columns = NULL,
    #'   number_columns = NULL,
    #'   condition_columns = NULL,
    #'   prepare_function = function(df) df,
    #'   distribution_function = function(df) private$create_date_distribution(df),
    #'   cumulative_distribution_function = function(sum) private$create_cumulative_distribution(sum),
    #'   distribution_plot_function = function(df, sum, infix) private$plot_date_distribution(df, sum, infix),
    #'   cumulative_distribution_plot_function = function(df, cum, infix) private$plot_date_cumulative_distribution(df, cum, infix),
    #'   infix = "Test"
    #' )
    process_plot_generic = function(source_path, target_path,
                                    date_columns = NULL, number_columns = NULL, condition_columns = NULL,
                                    prepare_function,
                                    distribution_function, cumulative_distribution_function,
                                    distribution_plot_function, cumulative_distribution_plot_function,
                                    infix) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      checkmate::assert_character(date_columns, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 0, unique = TRUE, min.chars = 1)
      checkmate::assert_character(number_columns, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 0, unique = TRUE, min.chars = 1)
      checkmate::assert_character(condition_columns, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 0, unique = TRUE, min.chars = 1)
      checkmate::assert_function(prepare_function, null.ok = FALSE)
      checkmate::assert_function(distribution_function, null.ok = FALSE)
      checkmate::assert_function(cumulative_distribution_function, null.ok = FALSE)
      checkmate::assert_function(distribution_plot_function, null.ok = FALSE)
      checkmate::assert_function(cumulative_distribution_plot_function, null.ok = FALSE)
      checkmate::assert_string(infix, null.ok = FALSE, min.chars = 1)
      # Step 1: Load the data
      data <- private$load_data(
        source_path = source_path,
        date_columns = date_columns,
        number_columns = number_columns,
        condition_columns = condition_columns
      )

      # Step 2: Prepare the data
      transformed_data <- prepare_function(data)

      # Step 3: Create distribution
      summary_data <- distribution_function(transformed_data)
      cumulative_data <- cumulative_distribution_function(summary_data)

      # Step 4: Create Plots
      distribution_plot <- distribution_plot_function(transformed_data, summary_data, infix)
      cumulative_distribution_plot <- cumulative_distribution_plot_function(transformed_data, cumulative_data, infix)

      # Step 5: Create File Names
      distribution_plot_filename <- private$create_file_name(
        source_path = source_path,
        target_path = target_path,
        infix = infix,
        suffix = "Verteilung.png"
      )
      cumulative_distribution_plot_filename <- private$create_file_name(
        source_path = source_path,
        target_path = target_path,
        infix = infix,
        suffix = "Kumulative_Verteilung.png"
      )

      # Step 6: Save Plots
      private$save_plot(distribution_plot_filename, distribution_plot, 10, 6)
      private$save_plot(cumulative_distribution_plot_filename, cumulative_distribution_plot, 10, 6)
      invisible(NULL)
    },

    #' @title Generic processing pipeline for generating distribution plots and returning objects
    #'
    #' @description Load raw data, apply preparation functions, compute summary and cumulative distributions, generate plots, and return the plot objects as a list.
    #'
    #' @details This method provides a centralized workflow for processing CSV files and generating distribution plots programmatically. Unlike process_plot_generic, this method returns the plot objects directly rather than saving them to disk, enabling programmatic use and integration with other visualization systems.
    #'
    #' @typed source_path (character) Path to the CSV input file
    #' @typed date_columns (character) Vector of column names to parse as dates; may be NULL
    #' @typed number_columns (character) Vector of column names to parse as numeric values; may be NULL
    #' @typed condition_columns (character) Vector of column names for grouping conditions; may be NULL
    #' @typed prepare_function (function) Function transforming raw data.frame to prepared data.frame
    #' @typed distribution_function (function) Function computing summary distribution from prepared data
    #' @typed cumulative_distribution_function (function) Function computing cumulative distribution from summary data
    #' @typed distribution_plot_function (function) Function generating distribution ggplot from prepared and summary data with infix label
    #' @typed cumulative_distribution_plot_function (function) Function generating cumulative distribution ggplot from prepared and cumulative data with infix label
    #' @typed infix (character) Label string used in plot titles and file names
    #' @typedreturn (list) A list containing the distribution and cumulative distribution plots
    #' @examples
    #' pg <- plot_generator$new()
    #' plots <- pg$private$process_plot_generic_output(
    #'   source_path = "data/input.csv",
    #'   date_columns = NULL,
    #'   number_columns = NULL,
    #'   condition_columns = NULL,
    #'   prepare_function = function(df) df,
    #'   distribution_function = function(df) private$create_date_distribution(df),
    #'   cumulative_distribution_function = function(sum) private$create_cumulative_distribution(sum),
    #'   distribution_plot_function = function(df, sum, infix) private$plot_date_distribution(df, sum, infix),
    #'   cumulative_distribution_plot_function = function(df, cum, infix) private$plot_date_cumulative_distribution(df, cum, infix),
    #'   infix = "Test"
    #' )
    process_plot_generic_output = function(source_path,
                                           date_columns = NULL, number_columns = NULL, condition_columns = NULL,
                                           prepare_function,
                                           distribution_function, cumulative_distribution_function,
                                           distribution_plot_function, cumulative_distribution_plot_function,
                                           infix) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_string(infix, null.ok = FALSE, min.chars = 1)

      # Step 1: Load the data
      data <- private$load_data(
        source_path = source_path,
        date_columns = date_columns,
        number_columns = number_columns,
        condition_columns = condition_columns
      )

      # Step 2: Prepare the data
      transformed_data <- prepare_function(data)

      # Step 3: Create distribution
      summary_data <- distribution_function(transformed_data)
      cumulative_data <- cumulative_distribution_function(summary_data)

      # Step 4: Create Plots
      distribution_plot <- distribution_plot_function(transformed_data, summary_data, infix)
      cumulative_distribution_plot <- cumulative_distribution_plot_function(transformed_data, cumulative_data, infix)

      # Return the plots as a list
      return(list(
        distribution_plot = distribution_plot,
        cumulative_distribution_plot = cumulative_distribution_plot
      ))
    },
    # </editor-fold>

    # <editor-fold desc="Application Date">

    #' @title Process application date data and generate distribution plots
    #'
    #' @description Load application date data, compute distribution and cumulative distribution plots, and save the results to disk.
    #'
    #' @details This method processes application date data by loading the CSV file, parsing the ABGESCHICKT_DATUM column, generating daily distribution summaries, creating both distribution and cumulative distribution plots, and saving them with standardized file names. It provides a complete workflow for application timing analysis.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving the plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$process_plot_application_date("data/input.csv", "out")
    process_plot_application_date = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_generic(
        source_path = source_path,
        target_path = target_path,
        date_columns = c("ABGESCHICKT_DATUM"),
        prepare_function = function(data) {
          private$prepare_date_column(data, "ABGESCHICKT_DATUM")
        },
        distribution_function = function(transformed_data) {
          private$create_date_distribution(transformed_data)
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_date_distribution(transformed_data, summary_data, infix)
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_date_cumulative_distribution(transformed_data, cumulative_data, infix)
        },
        infix = "Bewerbungen"
      )
      invisible(NULL)
    },

    #' @title Process application date data and return distribution plot objects
    #'
    #' @description Load application date data, compute distribution and cumulative distribution plots, and return the plot objects as a list.
    #'
    #' @details This method provides the same analysis as process_plot_application_date but returns the plot objects directly for programmatic use rather than saving them to files. It processes the ABGESCHICKT_DATUM column to generate daily distribution summaries and creates both distribution and cumulative distribution visualizations.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application dates
    #' @typedreturn (list) A list containing the distribution and cumulative distribution plots
    #' @examples
    #' pg <- plot_generator$new()
    #' plots <- pg$private$process_plot_application_date_output("data/input.csv")
    process_plot_application_date_output = function(source_path) {
      return(private$process_plot_generic_output(
        source_path = source_path,
        date_columns = c("ABGESCHICKT_DATUM"),
        number_columns = NULL,
        condition_columns = NULL,
        prepare_function = function(data) {
          private$prepare_date_column(data, "ABGESCHICKT_DATUM")
        },
        distribution_function = function(transformed_data) {
          private$create_date_distribution(transformed_data)
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_date_distribution(transformed_data, summary_data, infix)
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_date_cumulative_distribution(transformed_data, cumulative_data, infix)
        },
        infix = "Bewerbungen"
      ))
    },

    # </editor-fold>

    # <editor-fold desc="Admission Date">

    #' @title Process admission date data and generate dual distribution plots  
    #'
    #' @description Load admission date data, compute dual distribution and cumulative distribution plots split by admission status condition, and save the results to disk.
    #'
    #' @details This method processes admission date data by loading the CSV file, parsing the ZUL_DATUM column, creating dual distributions split by the ZUL column (admission status), and generating both distribution and cumulative distribution plots with separate series for accepted and rejected applications.
    #'
    #' @typed source_path (character) Path to the input CSV file containing admission dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving the plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$process_plot_admission_date("data/input.csv", "out")
    process_plot_admission_date = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_generic(
        source_path = source_path,
        target_path = target_path,
        date_columns = c("ZUL_DATUM"),
        condition_columns = c("ZUL"),
        prepare_function = function(data) {
          private$prepare_date_column(data, "ZUL_DATUM")
        },
        distribution_function = function(transformed_data) {
          private$create_date_dual_distribution(transformed_data, "ZUL == 'J'")
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_date_dual_distribution(transformed_data, summary_data, infix, "ZUL == 'J'")
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_date_dual_cumulative_distribution(transformed_data, cumulative_data, infix, "ZUL == 'J'")
        },
        infix = "Zulassungen"
      )
      invisible(NULL)
    },

    #' @title Process admission date data and return dual distribution plot objects
    #'
    #' @description Load admission date data, compute dual distribution and cumulative distribution plots split by admission status condition, and return the plot objects as a list.
    #'
    #' @details This method provides the same analysis as process_plot_admission_date but returns the plot objects directly for programmatic use rather than saving them to files. It processes the ZUL_DATUM column and creates dual distributions split by the ZUL column with separate series for accepted and rejected applications.
    #'
    #' @typed source_path (character) Path to the input CSV file containing admission dates
    #' @typedreturn (list) A list containing the distribution and cumulative distribution plots
    #' @examples
    #' pg <- plot_generator$new()
    #' plots <- pg$private$process_plot_admission_date_output("data/input.csv")
    process_plot_admission_date_output = function(source_path) {
      return(private$process_plot_generic_output(
        source_path = source_path,
        date_columns = c("ZUL_DATUM"),
        number_columns = NULL,
        condition_columns = c("ZUL"),
        prepare_function = function(data) {
          private$prepare_date_column(data, "ZUL_DATUM")
        },
        distribution_function = function(transformed_data) {
          private$create_date_dual_distribution(transformed_data, "ZUL == 'J'")
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_date_dual_distribution(transformed_data, summary_data, infix, "ZUL == 'J'")
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_date_dual_cumulative_distribution(transformed_data, cumulative_data, infix, "ZUL == 'J'")
        },
        infix = "Zulassungen"
      ))
    },

    # </editor-fold>

    # <editor-fold desc="Application Admission Date Difference">

    #' @title Process application-admission date difference data and generate dual distribution plots
    #'
    #' @description Load application and admission date data, compute the day difference between them, and generate dual distribution plots split by admission status.
    #'
    #' @details This method processes both ABGESCHICKT_DATUM and ZUL_DATUM columns to calculate the number of days between application and admission dates. It creates dual distributions split by the ZUL column showing how admission processing time differs between accepted and rejected applications, saving both distribution and cumulative distribution plots.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application and admission dates
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving the plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$process_plot_application_admission_date_difference("data/input.csv", "out")
    process_plot_application_admission_date_difference = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_generic(
        source_path = source_path,
        target_path = target_path,
        date_columns = c("ABGESCHICKT_DATUM", "ZUL_DATUM"),
        condition_columns = c("ZUL"),
        prepare_function = function(data) {
          private$prepare_date_columns(data, c("ABGESCHICKT_DATUM", "ZUL_DATUM"))
        },
        distribution_function = function(transformed_data) {
          private$create_number_dual_distribution(transformed_data, "ZUL == 'J'", min_number = 0)
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_number_dual_distribution(transformed_data, summary_data, infix, "ZUL == 'J'")
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_number_dual_cumulative_distribution(transformed_data, cumulative_data, infix, "ZUL == 'J'")
        },
        infix = "Differenzen"
      )
      invisible(NULL)
    },

    #' @title Process application-admission date difference data and return dual distribution plot objects
    #'
    #' @description Load application and admission date data, compute the day difference between them, and return dual distribution plot objects split by admission status.
    #'
    #' @details This method provides the same analysis as process_plot_application_admission_date_difference but returns the plot objects directly for programmatic use rather than saving them to files. It calculates processing time differences and creates dual distributions showing variations between accepted and rejected applications.
    #'
    #' @typed source_path (character) Path to the input CSV file containing application and admission dates
    #' @typedreturn (list) A list containing the distribution and cumulative distribution plots
    #' @examples
    #' pg <- plot_generator$new()
    #' plots <- pg$private$process_plot_application_admission_date_difference_output("data/input.csv")
    process_plot_application_admission_date_difference_output = function(source_path) {
      return(private$process_plot_generic_output(
        source_path = source_path,
        date_columns = c("ABGESCHICKT_DATUM", "ZUL_DATUM"),
        number_columns = NULL,
        condition_columns = c("ZUL"),
        prepare_function = function(data) {
          private$prepare_date_columns(data, c("ABGESCHICKT_DATUM", "ZUL_DATUM"))
        },
        distribution_function = function(transformed_data) {
          private$create_number_dual_distribution(transformed_data, "ZUL == 'J'", min_number = 0)
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_number_dual_distribution(transformed_data, summary_data, infix, "ZUL == 'J'")
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_number_dual_cumulative_distribution(transformed_data, cumulative_data, infix, "ZUL == 'J'")
        },
        infix = "Differenzen"
      ))
    },

    #' @title Process points step 1 data and generate distribution plots
    #'
    #' @description Load points step 1 data, compute numeric distribution and cumulative distribution plots for score analysis, and save the results to disk.
    #'
    #' @details This method processes points data from the first evaluation step by loading the CSV file, parsing the ERGEBNIS_STUFE1 column, creating numeric distributions grouped into score intervals (0-100), and generating both distribution and cumulative distribution plots showing score distribution patterns.
    #'
    #' @typed source_path (character) Path to the input CSV file containing points step 1 data
    #' @typed target_path (character) Directory path for saving the output plots
    #' @typedreturn (NULL) Invisibly returns NULL after processing and saving the plots
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$process_plot_points_step1("data/input.csv", "out")
    process_plot_points_step1 = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_generic(
        source_path = source_path,
        target_path = target_path,
        number_columns = c("ERGEBNIS_STUFE1"),
        prepare_function = function(data) {
          private$prepare_number_column(data, "ERGEBNIS_STUFE1")
        },
        distribution_function = function(transformed_data) {
          private$create_number_distribution(transformed_data, min_number = 0, max_number = 100)
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_number_distribution(transformed_data, summary_data, infix)
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_number_cumulative_distribution(transformed_data, cumulative_data, infix)
        },
        infix = "Punkte Aus Stufe 1"
      )
      invisible(NULL)
    },

    #' @title Process points step 1 data and return distribution plot objects
    #'
    #' @description Load points step 1 data, compute numeric distribution and cumulative distribution plots for score analysis, and return the plot objects as a list.
    #'
    #' @details This method provides the same analysis as process_plot_points_step1 but returns the plot objects directly for programmatic use rather than saving them to files. It processes the ERGEBNIS_STUFE1 column to create numeric distributions showing score patterns in the first evaluation step.
    #'
    #' @typed source_path (character) Path to the input CSV file containing points step 1 data
    #' @typedreturn (list) A list containing the distribution and cumulative distribution plots
    #' @examples
    #' pg <- plot_generator$new()
    #' plots <- pg$private$process_plot_points_step1_output("data/input.csv")
    process_plot_points_step1_output = function(source_path) {
      return(private$process_plot_generic_output(
        source_path = source_path,
        number_columns = c("ERGEBNIS_STUFE1"),
        prepare_function = function(data) {
          private$prepare_number_column(data, "ERGEBNIS_STUFE1")
        },
        distribution_function = function(transformed_data) {
          private$create_number_distribution(transformed_data, min_number = 0, max_number = 100)
        },
        cumulative_distribution_function = function(summary_data) {
          private$create_cumulative_distribution(summary_data)
        },
        distribution_plot_function = function(transformed_data, summary_data, infix) {
          private$plot_number_distribution(transformed_data, summary_data, infix)
        },
        cumulative_distribution_plot_function = function(transformed_data, cumulative_data, infix) {
          private$plot_number_cumulative_distribution(transformed_data, cumulative_data, infix)
        },
        infix = "Punkte Aus Stufe1"
      ))
    }
    # </editor-fold>

    # </editor-fold>
  )
)
