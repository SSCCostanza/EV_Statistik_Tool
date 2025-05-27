library(R6)
library(tidyverse)
library(lubridate)
library(e1071)
library(rlang)
library(tools)
library(checkmate)
library(glue)

#' Plot Generator R6 Class
#'
#' Provides methods to process application and admission data files and generate
#' distribution, dual-distribution, and cumulative distribution plots
#' for both date and numeric data.
#'
#' @param none NULL No parameters required for initialization.
#' @return [R6Class] Generator for creating `plot_generator` objects,
#'   each of which has public methods for file processing and plotting
#'   and private helper methods for data transformation and annotation.
#' @examples
#' pg <- plot_generator$new()
#' # process a single CSV and save plots
#' pg$process_file_application_date("data/app.csv", "out")
#' # create and render a date distribution plot
#' df <- data.frame(DATE = as.Date(c("2021-01-01", "2021-01-02")))
#' summary <- pg$private$create_date_distribution(df, as.Date("2021-01-01"), as.Date("2021-01-02"), "day")
#' pg$plot_date_distribution(df, summary, "Applications")
#' @family file-processing, plotting
#' @keywords internal
#' @concept interface
#' @note Delegates low-level work to private methods like
#'   `process_plot_application_date`, `plot_distribution_generic`, etc.
#' @seealso R6::R6Class, private$process_plot_application_date,
#'   private$plot_distribution_generic
plot_generator <- R6Class(
  "plot_generator",
  public = list(
    # <editor-fold desc="Single Files">

    #' Process a single application date file
    #'
    #' Validate the input CSV file and output directory, then generate application date distribution and cumulative distribution plots.
    #'
    #' @param source_path (character) Path to the input CSV file containing application dates.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing and saving plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_file_application_date("data/app.csv", "out")
    #' @family file-processing
    #' @keywords internal single-file
    #' @concept interface
    #' @note Delegates to private$process_plot_application_date
    #' @seealso private$process_plot_application_date, process_batch_application_date
    process_file_application_date = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_application_date(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)  # Added explicit return value
    },

    process_file_application_date_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_application_date_output(
        source_path = source_path
      )
      return(plot)
    },

    #' Process a single admission date file
    #'
    #' Validate the input CSV file and output directory, then generate admission date distribution and cumulative distribution plots.
    #'
    #' @param source_path (character) Path to the input CSV file containing admission dates.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing and saving plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_file_admission_date("data/admit.csv", "out")
    #' @family file-processing
    #' @keywords internal single-file
    #' @concept interface
    #' @note Delegates to private$process_plot_admission_date
    #' @seealso private$process_plot_admission_date, process_batch_admission_date
    process_file_admission_date = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_admission_date(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)
    },

    process_file_admission_date_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_admission_date_output(
        source_path = source_path
      )
      return(plot)
    },

    #' Process a single application-admission date difference file
    #'
    #' Validate the input CSV file and output directory, then generate difference distribution and cumulative distribution plots.
    #'
    #' @param source_path (character) Path to the input CSV file containing application and admission dates.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing and saving plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_file_application_admission_date_difference("data/diff.csv", "out")
    #' @family file-processing
    #' @keywords internal single-file
    #' @concept interface
    #' @note Delegates to private$process_plot_application_admission_date_difference
    #' @seealso private$process_plot_application_admission_date_difference, process_batch_application_admission_date_difference
    process_file_application_admission_date_difference = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_application_admission_date_difference(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)
    },

    process_file_application_admission_date_difference_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_application_admission_date_difference_output(
        source_path = source_path
      )
      return(plot)
    },

    #' Process a single points step1 file
    #'
    #' Validate the input CSV file and output directory, then generate points step1 distribution and cumulative distribution plots.
    #'
    #' @param source_path (character) Path to the input CSV file containing points step1 data.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing and saving plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_file_points_step1("data/points1.csv", "out")
    #' @family file-processing
    #' @keywords internal single-file
    #' @concept interface
    #' @note Delegates to private$process_plot_points_step1
    #' @seealso private$process_plot_points_step1, process_batch_points_step1
    process_file_points_step1 = function(source_path, target_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      checkmate::assert_directory(target_path, access = "w")
      private$process_plot_points_step1(
        source_path = source_path,
        target_path = target_path
      )
      invisible(NULL)
    },

    process_file_points_step1_output = function(source_path) {
      checkmate::assert_file(source_path, access = "r", extension = "csv")
      plot <- private$process_plot_points_step1_output(
        source_path = source_path
      )
      return(plot)
    },

    # </editor-fold>
    # <editor-fold desc="Multiple Files">

    #' Process a batch of application date files
    #'
    #' Validate multiple input CSV files and output directory, then process each file for application date plots.
    #'
    #' @param source_paths (character) Vector of paths to input CSV files containing application dates.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing all files.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_batch_application_date(c("app1.csv","app2.csv"), "out")
    #' @family file-processing
    #' @keywords internal batch-file
    #' @concept interface
    #' @note Iterates over source_paths calling process_file_application_date
    #' @seealso process_file_application_date
    process_batch_application_date = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_application_date(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    },

    #' Process a batch of admission date files
    #'
    #' Validate multiple input CSV files and output directory, then process each file for admission date plots.
    #'
    #' @param source_paths (character) Vector of paths to input CSV files containing admission dates.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing all files.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_batch_admission_date(c("ad1.csv","ad2.csv"), "out")
    #' @family file-processing
    #' @keywords internal batch-file
    #' @concept interface
    #' @note Iterates over source_paths calling process_file_admission_date
    #' @seealso process_file_admission_date
    process_batch_admission_date = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_admission_date(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    },

    #' Process a batch of application-admission date difference files
    #'
    #' Validate multiple input CSV files and output directory, then process each file for date difference plots.
    #'
    #' @param source_paths (character) Vector of paths to input CSV files containing application and admission dates.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing all files.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_batch_application_admission_date_difference(c("d1.csv","d2.csv"), "out")
    #' @family file-processing
    #' @keywords internal batch-file
    #' @concept interface
    #' @note Iterates over source_paths calling process_file_application_admission_date_difference
    #' @seealso process_file_application_admission_date_difference
    process_batch_application_admission_date_difference = function(source_paths, target_path) {
      checkmate::assert_character(source_paths, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE, min.len = 1, unique = TRUE, min.chars = 1)
      checkmate::assert_directory(target_path, access = "w")
      purrr::walk(
        source_paths,
        ~ self$process_file_application_admission_date_difference(source_path = .x, target_path = target_path)
      )
      invisible(NULL)
    },

    #' Process a batch of points step1 files
    #'
    #' Validate multiple input CSV files and output directory, then process each file for points step1 plots.
    #'
    #' @param source_paths (character) Vector of paths to input CSV files containing points step1 data.
    #' @param target_path (character) Directory path for saving the output plots.
    #' @return NULL Invisibly returns NULL after processing all files.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_batch_points_step1(c("p1.csv","p2.csv"), "out")
    #' @family file-processing
    #' @keywords internal batch-file
    #' @concept interface
    #' @note Iterates over source_paths calling process_file_points_step1
    #' @seealso process_file_points_step1
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

    #' Retrieve all reusable color definitions
    #'
    #' Return a named vector of hexadecimal color codes for use in plots and themes.
    #'
    #' @param none NULL No parameters.
    #' @return character Named vector of colors keyed by logical and theme element names.
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_colors()
    #' @family theming
    #' @keywords internal
    #' @concept colors
    #' @note Standardizes color usage across all plot types and condition groups.
    #' @seealso private$get_common_theme
    get_colors = function() {
      c(
        "TRUE"              = "#006400",
        "FALSE"             = "#FF0000",
        "background"        = "white",
        "panel_background"  = "white",
        "panel_grid_major"  = "grey80",
        "panel_grid_minor"  = "grey90",
        "bar_outline"       = "black"
      )
    },

    # <editor-fold desc="Step 1: Load the data">

    #' Load CSV data from a file and validate required columns
    #'
    #' Read a CSV file into a data frame and ensure that required date, numeric, and condition columns are present.
    #'
    #' @param source_path (character) Path to the CSV file to read.
    #' @param date_columns (character) Character vector of column names to parse as dates. Defaults to empty.
    #' @param number_columns (character) Character vector of column names to parse as numeric. Defaults to empty.
    #' @param condition_columns (character) Character vector of column names for condition checks. Defaults to empty.
    #' @return data.frame The data frame read from the CSV file after validation.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' data <- pg$private$load_data("path/to/file.csv", c("ABGESCHICKT_DATUM"), c("ERGEBNIS_STUFE1"), c("ZUL"))
    #' @family data-loading
    #' @keywords internal
    #' @concept data-import
    #' @note Aborts if the CSV file cannot be read or required columns are missing.
    #' @seealso private$validate_columns, readr::read_csv
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

    #' Parse date time string to Date
    #'
    #' Convert a date time string in dmy_hms format into a Date object
    #'
    #' @param string character One element vector containing the date time string in day month year hours minutes seconds format
    #' @return Date The parsed Date object
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_date("01.02.2021 14:30:00")
    #' @family data preparation
    #' @keywords internal
    #' @concept date parsing
    #' @note Uses lubridate::dmy_hms for parsing, aborts if conversion fails
    #' @seealso lubridate::dmy_hms
    get_date = function(string) {
      checkmate::assert_character(string, null.ok = FALSE, any.missing = TRUE, all.missing = FALSE, min.len = 1, unique = FALSE)
      date_value <- as.Date(dmy_hms(string))
      checkmate::assert_date(date_value, null.ok = FALSE)
      return(date_value)
    },

    #' Compute default placeholder date
    #'
    #' Select a default date placeholder based on observed interval in a date vector, choosing March first or October first of the latest year
    #'
    #' @param date_column Date Vector of dates to analyze for interval
    #' @return Date A single Date object serving as placeholder for missing values
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_default_date(as.Date(c("2022-03-02","2022-05-01")))
    #' @family data preparation
    #' @keywords internal
    #' @concept date handling
    #' @note Assumes all dates occur within one calendar year, prefilter if not
    #' @seealso private$get_date, lubridate::ymd, lubridate::interval
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

    #' Calculate difference in days between two dates
    #'
    #' Compute the number of days from one Date to another
    #'
    #' @param date1 Date Starting date
    #' @param date2 Date Ending date
    #' @return numeric Integer number of days between date1 and date2
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_date_difference(as.Date("2021-01-01"), as.Date("2021-01-10"))
    #' @family data preparation
    #' @keywords internal
    #' @concept date arithmetic
    #' @note Returns negative values if date2 is before date1
    #' @seealso as.numeric, difftime
    get_date_difference = function(date1, date2) {
      checkmate::assert_date(date1, null.ok = FALSE)
      checkmate::assert_date(date2, null.ok = FALSE)
      date_diff <- as.numeric(date2 - date1, units = "days")
      checkmate::assert_numeric(date_diff, null.ok = FALSE)
      return(date_diff)
    },

    #' Coerce character or numeric input to numeric
    #'
    #' Convert a numeric string or numeric vector to numeric type
    #'
    #' @param string character or numeric Input value to convert
    #' @return numeric Numeric vector representing input values
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_number("42")
    #' pg$private$get_number(3.14)
    #' @family data preparation
    #' @keywords internal
    #' @concept number parsing
    #' @note Character input is cast via as.numeric, aborts for unsupported types
    #' @seealso as.numeric
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

    #' Provide default numeric placeholder
    #'
    #' Return an NA_real_ placeholder for missing numeric data
    #'
    #' @param number_vector numeric Numeric vector to inspect for missing values
    #' @return numeric Single NA_real_ value as placeholder
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_default_number(c(1,2,NA))
    #' @family data preparation
    #' @keywords internal
    #' @concept number handling
    #' @note Always returns NA_real_ regardless of input
    #' @seealso is.na, NA_real_
    get_default_number = function(number_vector) {
      checkmate::assert_numeric(number_vector, null.ok = FALSE, any.missing = TRUE, all.missing = TRUE, min.len = 0, unique = FALSE)
      default_number <- NA_real_
      checkmate::assert_numeric(default_number, null.ok = FALSE)
      return(default_number)
    },

    #' Compute difference between two numeric values
    #'
    #' Subtract number1 from number2 to yield a numeric difference
    #'
    #' @param number1 numeric First numeric value
    #' @param number2 numeric Second numeric value
    #' @return numeric Result of number2 minus number1
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_number_difference(10, 15)
    #' @family data preparation
    #' @keywords internal
    #' @concept number arithmetic
    #' @note Supports vectors, returns elementwise difference
    #' @seealso `-` operator
    get_number_difference = function(number1, number2) {
      checkmate::assert_numeric(number1, null.ok = FALSE)
      checkmate::assert_numeric(number2, null.ok = FALSE)
      number_diff <- number2 - number1
      checkmate::assert_numeric(number_diff, null.ok = FALSE)
      return(number_diff)
    },

    #' Prepare a single column with parsing and default filling
    #'
    #' Apply a parse function to transform a column, fill missing values using default function, and store results in a new column
    #'
    #' @param data data.frame Data frame containing the original column
    #' @param column character Name of the column to parse
    #' @param parse_function function Function applied to each value to parse
    #' @param default_function function Function applied to column to compute default value for missing entries
    #' @param new_column character Name for the newly created column with parsed or default values
    #' @return data.frame Data frame augmented with new_column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(date_str = c("01.01.2021",""))
    #' pg$private$prepare_column(df, "date_str", private$get_date, private$get_default_date, "DATE")
    #' @family data preparation
    #' @keywords internal
    #' @concept column transformation
    #' @note Uses dplyr::mutate and rlang::sym for evaluation
    #' @seealso dplyr::mutate, rlang::sym, coalesce
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

    #' Prepare a date column for analysis
    #'
    #' Parse and fill missing values in a date column then add a DATE column
    #'
    #' @param data data.frame Data frame with the raw date column
    #' @param date_column character Name of the column to parse as date
    #' @return data.frame Data frame with parsed DATE column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(ABGESCHICKT_DATUM = c("01.01.2021",""))
    #' pg$private$prepare_date_column(df, "ABGESCHICKT_DATUM")
    #' @family data preparation
    #' @keywords internal
    #' @concept date handling
    #' @note Delegates to private$prepare_column with get_date and get_default_date
    #' @seealso private$prepare_column, private$get_date
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

    #' Prepare a numeric column for analysis
    #'
    #' Coerce and fill missing values in a numeric column then add a NUMBER column
    #'
    #' @param data data.frame Data frame with the raw numeric column
    #' @param number_column character Name of the column to parse as numeric
    #' @return data.frame Data frame with parsed NUMBER column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(ERGEBNIS_STUFE1 = c("5",""))
    #' pg$private$prepare_number_column(df, "ERGEBNIS_STUFE1")
    #' @family data preparation
    #' @keywords internal
    #' @concept number handling
    #' @note Delegates to private$prepare_column with get_number and get_default_number
    #' @seealso private$prepare_column, private$get_number
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

    #' Prepare two columns and compute their difference
    #'
    #' Apply a single column prepare function to two columns then compute difference using diff function and add new_column
    #'
    #' @param data data.frame Data frame containing the two columns
    #' @param columns character Vector of two column names to prepare and compare
    #' @param single_prepare_function function Function to prepare each column separately
    #' @param diff_function function Function to compute difference between parsed columns
    #' @param new_column character Name for the resulting difference column
    #' @return data.frame Data frame with original columns and difference in new_column
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(A = c("01.01.2021",""), B = c("02.01.2021",""))
    #' pg$private$prepare_two_columns(df, c("A","B"), private$prepare_date_column, private$get_date_difference, "NUMBER")
    #' @family data preparation
    #' @keywords internal
    #' @concept column transformation
    #' @note Uses loop to apply prepare function then mutate for difference
    #' @seealso private$prepare_column, private$get_date_difference
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

    #' Prepare and differ date columns
    #'
    #' Parse two date columns and compute day difference stored in NUMBER column
    #'
    #' @param data data.frame Data frame with raw date columns
    #' @param date_columns character Vector of two date column names
    #' @return data.frame Data frame with new NUMBER column of day differences
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(ABGESCHICKT_DATUM = c("01.01.2021",""), ZUL_DATUM = c("05.01.2021",""))
    #' pg$private$prepare_date_columns(df, c("ABGESCHICKT_DATUM","ZUL_DATUM"))
    #' @family data preparation
    #' @keywords internal
    #' @concept date difference
    #' @note Delegates to private$prepare_two_columns
    #' @seealso private$prepare_two_columns, private$get_date
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

    #' Prepare and differ numeric columns
    #'
    #' Coerce two numeric columns then compute their difference stored in NUMBER column
    #'
    #' @param data data.frame Data frame with raw numeric columns
    #' @param number_columns character Vector of two numeric column names
    #' @return data.frame Data frame with new NUMBER column of numeric differences
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(X = c("1",""), Y = c("3",""))
    #' pg$private$prepare_number_columns(df, c("X","Y"))
    #' @family data preparation
    #' @keywords internal
    #' @concept number difference
    #' @note Delegates to private$prepare_two_columns
    #' @seealso private$prepare_two_columns, private$get_number
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

    #' Generate sequence of dates
    #'
    #' Generate a sequence of dates from start_date to end_date using the specified interval
    #'
    #' @param start_date Date starting date for sequence generation
    #' @param end_date Date ending date for sequence generation
    #' @param interval character interval string passed to seq.Date
    #' @return Date vector containing all dates from start_date to end_date using interval
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_all_dates(as.Date("2021-01-01"), as.Date("2021-01-10"), "day")
    #' @family distribution helpers
    #' @keywords internal
    #' @concept sequence
    #' @note uses base::seq.Date for date sequence creation
    #' @seealso seq.Date
    get_all_dates = function(start_date, end_date, interval) {
      checkmate::assert_date(start_date, null.ok = FALSE)
      checkmate::assert_date(end_date, null.ok = FALSE)
      checkmate::assert_string(interval, null.ok = FALSE, min.chars = 1)
      all_dates <- seq.Date(from = start_date, to = end_date, by = interval)
      checkmate::assert_date(all_dates, null.ok = FALSE)
      return(all_dates)
    },

    #' Compute break points for numeric grouping
    #'
    #' Compute break points between min_number and max_number using specified width.
    #' If range equals zero returns min_number minus 0.5 and max_number plus 0.5,
    #' if range smaller than width returns c(min_number, max_number), otherwise returns a sequence.
    #'
    #' @param min_number numeric minimum value for grouping
    #' @param max_number numeric maximum value for grouping
    #' @param width numeric width of each interval
    #' @return numeric vector of break points used for grouping
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_all_break_points(0, 100, 10)
    #' @family distribution helpers
    #' @keywords internal
    #' @concept grouping
    #' @note ensures at least two break points even when range is zero
    #' @seealso get_all_dates, cut
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

    #' Generate group labels from break points
    #'
    #' Generate character labels representing numeric intervals using break points
    #' computed from min_number max_number and width.
    #'
    #' @param min_number numeric minimum value for grouping
    #' @param max_number numeric maximum value for grouping
    #' @param width numeric width of each interval
    #' @return character vector of interval labels in the form "lower,upper"
    #' @examples
    #' pg <- plot_generator$new()
    #' pg$private$get_all_groups(0, 100, 10)
    #' @family distribution helpers
    #' @keywords internal
    #' @concept grouping_labels
    #' @note relies on get_all_break_points to compute break points
    #' @seealso get_all_break_points
    get_all_groups = function(min_number, max_number, width) {
      break_points <- private$get_all_break_points(min_number, max_number, width)
      lower_bounds <- head(break_points, -1)
      upper_bounds <- pmin(lower_bounds + width - 1, max_number - 1)
      all_groups <- paste(lower_bounds, upper_bounds, sep = ",")
      checkmate::assert_character(all_groups, null.ok = FALSE)
      return(all_groups)
    },

    #' Create distribution summary
    #'
    #' Create a distribution summary data frame for a grouping column including all groups with zero fill
    #'
    #' @param transformed_data data.frame data containing the grouping column
    #' @param distribution_based_column symbol unquoted column symbol to group by
    #' @param distribution_based_column_name character name of the grouping column in the output
    #' @param complete_values vector of all possible group values used to fill missing groups
    #' @return data.frame summary data with grouping column and Distribution column
    #' @examples
    #' df <- data.frame(NUMBER = c(1,2,2,3))
    #' pg <- plot_generator$new()
    #' pg$private$create_distribution(df, NUMBER, "NUMBER", as.character(1:3))
    #' @family distribution helpers
    #' @keywords internal
    #' @concept distribution_summary
    #' @note uses dplyr::count and tidyr::complete to include groups with zero occurrences
    #' @seealso dplyr::count, tidyr::complete
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

    #' Create daily date distribution
    #'
    #' Computes a distribution of events per date within a specified interval.
    #'
    #' @param transformed_data data.frame Data frame with DATE column
    #' @param start_date Date Starting date for sequence
    #' @param end_date Date Ending date for sequence
    #' @param interval character Interval to use for sequence (e.g., "day")
    #' @return data.frame Summary data frame with DATE and Distribution columns
    #' @examples
    #' pg <- PlotGenerator$new()
    #' df <- data.frame(DATE=as.Date(c("2021-01-01","2021-01-02","2021-01-02")))
    #' pg$private$create_date_distribution(df, as.Date("2021-01-01"), as.Date("2021-01-02"), "day")
    #' @family data-preparation
    #' @keywords internal
    #' @concept distribution
    #' @note Aborts if DATE column absent
    #' @seealso private$create_number_distribution, private$create_cumulative_distribution
    create_date_distribution = function(transformed_data, start_date = NULL, end_date = NULL, interval = "day") {
      if (is.null(start_date)) start_date <- min(transformed_data$DATE, na.rm = TRUE)
      if (is.null(end_date)) end_date <- max(transformed_data$DATE, na.rm = TRUE)
      summary_data <- transformed_data %>%
        private$create_distribution(DATE, "DATE", private$get_all_dates(start_date, end_date, interval))
      return(summary_data)
    },

    #' Create numeric distribution summary
    #'
    #' Generate a distribution summary by grouping numeric values into intervals.
    #'
    #' @param transformed_data data.frame Data frame containing a NUMBER column to group
    #' @param min_number numeric Minimum value for grouping, defaults to min(transformed_data$NUMBER, na.rm = TRUE)
    #' @param max_number numeric Maximum value for grouping, defaults to max(transformed_data$NUMBER, na.rm = TRUE)
    #' @param width numeric Width of each interval, defaults to 10
    #' @return data.frame Summary data frame with GROUP factor and Distribution counts
    #' @examples
    #' df <- data.frame(NUMBER = c(1,2,2,5,7))
    #' pg <- plot_generator$new()
    #' pg$private$create_number_distribution(df, 1, 7, 3)
    #' @family distribution helpers
    #' @keywords internal
    #' @concept distribution_numeric
    #' @note Uses cut() with breaks from private$get_all_break_points and labels from private$get_all_groups
    #' @seealso private$get_all_break_points, private$get_all_groups, private$create_distribution
    create_number_distribution = function(transformed_data, min_number = min(transformed_data$NUMBER, na.rm = TRUE), max_number = max(transformed_data$NUMBER, na.rm = TRUE), width = 10) {
      summary_data <- transformed_data %>%
        mutate(GROUP = cut(NUMBER, breaks = private$get_all_break_points(min_number, max_number, width), labels = private$get_all_groups(min_number, max_number, width), include.lowest = TRUE, right = FALSE)) %>%
        private$create_distribution(GROUP, "GROUP", private$get_all_groups(min_number, max_number, width))
      summary_data$GROUP <- as.factor(summary_data$GROUP)
      return(summary_data)
    },

    #' Create dual-condition distribution summary
    #'
    #' Generate a distribution summary split by a logical condition for each group value.
    #'
    #' @param transformed_data data.frame Data frame containing the grouping column
    #' @param distribution_based_column symbol Unquoted column symbol to group by
    #' @param distribution_based_column_name character Name of the grouping column in the output
    #' @param condition character Logical expression string used to split the data
    #' @param complete_values vector All possible group values to include in the summary
    #' @return data.frame Summary with distribution counts for each ConditionGroup
    #' @examples
    #' df <- data.frame(GROUP = c('A','B','A','C'), ZUL = c(TRUE, FALSE, TRUE, FALSE))
    #' pg <- plot_generator$new()
    #' pg$private$create_dual_distribution(df, GROUP, "GROUP", "ZUL == TRUE", c('A','B','C'))
    #' @family distribution helpers
    #' @keywords internal
    #' @concept distribution_dual
    #' @note Uses complete() to ensure all groups appear for both condition values
    #' @seealso private$create_distribution, tidyr::complete
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

    #' Create dual date distribution summary
    #'
    #' Compute a date-based distribution split by a logical condition over a date sequence.
    #'
    #' @param transformed_data data.frame Data frame containing a DATE column
    #' @param condition character Logical expression string to split by
    #' @param start_date Date Starting date for sequence, defaults to min(transformed_data$DATE)
    #' @param end_date Date Ending date for sequence, defaults to max(transformed_data$DATE)
    #' @param interval character Interval string passed to seq.Date, defaults to "day"
    #' @return data.frame Summary with DATE, ConditionGroup and Distribution counts
    #' @examples
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c('J','N'))
    #' pg <- plot_generator$new()
    #' pg$private$create_date_dual_distribution(df, "ZUL == 'J'", as.Date('2021-01-01'), as.Date('2021-01-02'), "day")
    #' @family distribution helpers
    #' @keywords internal
    #' @concept distribution_date_dual
    #' @note Relies on private$create_dual_distribution with date sequence values
    #' @seealso private$get_all_dates, private$create_dual_distribution
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

    #' Create dual numeric distribution summary
    #'
    #' Compute a numeric distribution split by a logical condition and grouped into intervals.
    #'
    #' @param transformed_data data.frame Data frame containing a NUMBER column
    #' @param condition character Logical expression string to split the data
    #' @param min_number numeric Minimum value for grouping, defaults to min(transformed_data$NUMBER)
    #' @param max_number numeric Maximum value for grouping, defaults to max(transformed_data$NUMBER)
    #' @param width numeric Width of each interval, defaults to 10
    #' @return data.frame Summary data frame with GROUP, ConditionGroup and Distribution counts
    #' @examples
    #' df <- data.frame(NUMBER = 1:5, ZUL = c('J','N','J','N','J'))
    #' pg <- plot_generator$new()
    #' pg$private$create_number_dual_distribution(df, "ZUL == 'J'", 1, 5, 2)
    #' @family distribution helpers
    #' @keywords internal
    #' @concept distribution_numeric_dual
    #' @note Uses cut() for grouping and complete() for both condition groups
    #' @seealso private$get_all_break_points, private$get_all_groups, private$create_dual_distribution
    create_number_dual_distribution = function(transformed_data, condition, min_number = min(transformed_data$NUMBER, na.rm = TRUE), max_number = max(transformed_data$NUMBER, na.rm = TRUE), width = 10) {
      summary_data <- transformed_data %>%
        mutate(GROUP = cut(NUMBER, breaks = private$get_all_break_points(min_number, max_number, width), labels = private$get_all_groups(min_number, max_number, width), include.lowest = TRUE, right = FALSE)) %>%
        private$create_dual_distribution(GROUP, "GROUP", condition, private$get_all_groups(min_number, max_number, width))
      summary_data$GROUP <- factor(summary_data$GROUP,
        levels = private$get_all_groups(min_number, max_number, width)
      )
      return(summary_data)
    },

    #' Create cumulative distribution summary
    #'
    #' Compute cumulative counts and percentages from a distribution summary.
    #'
    #' @param summary_data data.frame Summary data frame containing a Distribution column
    #' @return data.frame Summary with CumulativeDistribution and CumulativeDistributionPercentage columns
    #' @examples
    #' df_sum <- data.frame(GROUP = c('1,2','3,4'), Distribution = c(10,20))
    #' pg <- plot_generator$new()
    #' pg$private$create_cumulative_distribution(df_sum)
    #' @family distribution helpers
    #' @keywords internal
    #' @concept distribution_cumulative
    #' @note Handles zero total distribution by using a safe total of one
    #' @seealso private$create_distribution, dplyr::cumsum
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

    #' Compute annotation statistics
    #'
    #' Calculate descriptive statistics for a numeric vector including mean, standard deviation, skewness, kurtosis, and mode.
    #'
    #' @param values numeric vector Values from which to compute statistical metrics.
    #' @return list Named list with elements:
    #'   - mean: numeric arithmetic mean
    #'   - sd: numeric standard deviation (0 if length(values) <= 1)
    #'   - skew: numeric skewness (0 if length(values) <= 1)
    #'   - kurt: numeric kurtosis (0 if length(values) <= 1)
    #'   - mode: numeric most frequent value
    #' @examples
    #' pg <- plot_generator$new()
    #' stats <- pg$private$compute_annotation_stats(c(1,2,2,3,4))
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_stats
    #' @note Requires at least one value; standard deviation, skewness, and kurtosis default to zero for small vectors.
    #' @seealso private$format_annotation, private$compute_date_annotation, private$compute_number_annotation
    compute_annotation_stats = function(values) {
      stats <- values %>%
        {
          list(
            mean = mean(., na.rm = TRUE),
            sd = if (length(.) <= 1) 0 else round(sd(., na.rm = TRUE), 2),
            skew = if (length(.) <= 1) 0 else round(skewness(., na.rm = TRUE), 2),
            kurt = if (length(.) <= 1) 0 else round(kurtosis(., na.rm = TRUE), 2)
          )
        }
      mode_val <- names(sort(table(values), decreasing = TRUE))[1]
      stats$mode <- if (is.na(mode_val) || mode_val == "") values[1] else as.numeric(mode_val)
      return(stats)
    },

    #' Format annotation string
    #'
    #' Convert computed statistics into a formatted annotation string for plotting.
    #'
    #' @param stats list Named list with mean, mode, sd, skew, and kurt values.
    #' @param type character One of "date" or "number" determining format.
    #' @return character Annotation string with lines for μ (mean), mod (mode), σ (sd), skew, and kurt.
    #' @examples
    #' pg <- plot_generator$new()
    #' stats <- pg$private$compute_annotation_stats(c(1,2,2,3))
    #' pg$private$format_annotation(stats, "number")
    #' pg$private$format_annotation(stats, "date")
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_format
    #' @note Dates are formatted as DD.MM.YYYY; numeric values use two decimal places.
    #' @seealso private$compute_annotation_stats
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
      lines <- c(
        paste0("\u03bc: ", mean_str),
        paste0("mod: ", mode_str),
        paste0("\u03c3: ", sprintf("%.2f", stats$sd), if (type == "date") " Tage" else ""),
        paste0("skew: ", sprintf("%.2f", stats$skew)),
        paste0("kurt: ", sprintf("%.2f", stats$kurt))
      )
      annotation <- paste(lines, collapse = "\n")
      return(annotation)
    },

    #' Compute descriptive statistics annotation for date distributions
    #'
    #' Computes descriptive statistics for date values including mean, mode, standard deviation, skewness, and kurtosis. Missing dates are replaced with the earliest date and an annotation string is formatted in German date and numeric format.
    #'
    #' @param raw_data data.frame Data frame containing a DATE column with raw date values.
    #' @param summary_data data.frame Summary data frame containing DATE and Distribution columns.
    #' @return character A formatted annotation string with μ (mean date), mod (mode date), σ (standard deviation in days), skew, and kurtosis; or "Keine Ereignisse" if there are no events.
    #' @examples
    #' raw <- data.frame(DATE = as.Date(c("2022-03-01","2022-03-02","2022-03-02")))
    #' summary <- data.frame(DATE = as.Date(c("2022-03-01","2022-03-02")), Distribution = c(1, 2))
    #' pg <- PlotGenerator$new()
    #' pg$private$compute_date_annotation(raw, summary)
    #' @family annotation
    #' @keywords internal date annotation
    #' @concept annotation date
    #' @note If only a single or no event is present, standard deviation, skewness, and kurtosis default to 0. Returns "Keine Ereignisse" when total events equals zero.
    #' @seealso private$compute_number_annotation, private$compute_date_annotation_positions
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

    #' Create numeric annotation
    #'
    #' Computes descriptive statistics for numeric values including mean, mode, standard deviation, skewness, and kurtosis.
    #'
    #' @param raw_data data.frame Data frame with NUMBER column
    #' @param summary_data data.frame Summary data frame with Distribution column
    #' @return character Annotation string with statistical metrics
    #' @examples
    #' pg <- PlotGenerator$new()
    #' raw <- data.frame(NUMBER=c(1,2,2,3,NA))
    #' summary <- data.frame(GROUP=c('1-2','3-4'), Distribution=c(3,1))
    #' pg$private$compute_number_annotation(raw, summary)
    #' @family annotation
    #' @keywords internal
    #' @concept annotation
    #' @note Aborts if NUMBER column is missing or raw_data lacks required structure
    #' @seealso private$compute_date_annotation
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

    #' Compute annotation positions
    #'
    #' Determine x and y coordinates for placing annotations on distribution plots.
    #'
    #' @param summary_data data.frame Summary data containing grouping and Distribution columns.
    #' @param type character One of "DATE" or "NUMBER" specifying axis type.
    #' @param opposite logical Whether to place annotation on the opposite side.
    #' @return list List with elements x_left, x_right, and y_top for annotation placement.
    #' @examples
    #' pg <- plot_generator$new()
    #' pos <- pg$private$compute_positions(summary_data, "DATE", FALSE)
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_positions
    #' @note For DATE, calculates span in days; for NUMBER, uses first and last group.
    #' @seealso private$compute_date_annotation_positions, private$compute_number_annotation_positions
    compute_positions = function(summary_data, type = c("DATE", "NUMBER"), opposite = FALSE) {
      checkmate::assert_data_frame(summary_data, null.ok = FALSE)
      # Determine type and validate column names
      type <- match.arg(type)
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
            x_left = if (!opposite) min_date + span * 0.1 else max_date - span * 0.9,
            x_right = if (!opposite) min_date + span * 0.9 else max_date - span * 0.1
          ) %>%
          select(x_left, x_right, y_top) %>%
          as.list()
      } else {
        checkmate::assert_names(names(summary_data), must.include = c("GROUP", "Distribution"))
        coords <- summary_data %>%
          summarise(
            groups = list(as.character(GROUP)),
            y_top = max(Distribution, na.rm = TRUE)
          ) %>%
          mutate(
            x_left = groups[[1]][1],
            x_right = groups[[1]][length(groups[[1]])]
          ) %>%
          select(x_left, x_right, y_top) %>%
          as.list()
      }
      return(coords)
    },

    #' Compute date annotation positions
    #'
    #' Get coordinates for placing date annotations on distribution plots.
    #'
    #' @param summary_data data.frame Summary with DATE and Distribution columns.
    #' @return list Coordinates (x_left, x_right, y_top) for annotation.
    #' @examples
    #' pg <- plot_generator$new()
    #' pos <- pg$private$compute_date_annotation_positions(summary_data)
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_positions_date
    #' @note Calls compute_positions with type="DATE", opposite=FALSE.
    #' @seealso private$compute_positions, private$compute_date_annotation_positions_opposite
    compute_date_annotation_positions = function(summary_data) {
      positions <- summary_data %>% private$compute_positions("DATE", FALSE)
      return(positions)
    },

    #' Compute date annotation positions (opposite)
    #'
    #' Get coordinates for placing date annotations on the opposite side of plots.
    #'
    #' @param summary_data data.frame Summary with DATE and Distribution columns.
    #' @return list Coordinates (x_left, x_right, y_top) for annotation on opposite side.
    #' @examples
    #' pg <- plot_generator$new()
    #' pos <- pg$private$compute_date_annotation_positions_opposite(summary_data)
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_positions_date
    #' @note Calls compute_positions with type="DATE", opposite=TRUE.
    #' @seealso private$compute_positions, private$compute_date_annotation_positions
    compute_date_annotation_positions_opposite = function(summary_data) {
      positions <- summary_data %>% private$compute_positions("DATE", TRUE)
      return(positions)
    },

    #' Compute numeric annotation positions
    #'
    #' Get coordinates for placing numeric annotations on distribution plots.
    #'
    #' @param summary_data data.frame Summary with GROUP and Distribution columns.
    #' @return list Coordinates (x_left, x_right, y_top) for annotation.
    #' @examples
    #' pg <- plot_generator$new()
    #' pos <- pg$private$compute_number_annotation_positions(summary_data)
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_positions_number
    #' @note Calls compute_positions with type="NUMBER", opposite=FALSE.
    #' @seealso private$compute_positions, private$compute_number_annotation_positions_opposite
    compute_number_annotation_positions = function(summary_data) {
      positions <- summary_data %>% private$compute_positions("NUMBER", FALSE)
      return(positions)
    },

    #' Compute numeric annotation positions (opposite)
    #'
    #' Get coordinates for placing numeric annotations on the opposite side of plots.
    #'
    #' @param summary_data data.frame Summary with GROUP and Distribution columns.
    #' @return list Coordinates (x_left, x_right, y_top) for annotation on opposite side.
    #' @examples
    #' pg <- plot_generator$new()
    #' pos <- pg$private$compute_number_annotation_positions_opposite(summary_data)
    #' @family annotation
    #' @keywords internal
    #' @concept annotation_positions_number
    #' @note Calls compute_positions with type="NUMBER", opposite=TRUE.
    #' @seealso private$compute_positions, private$compute_number_annotation_positions
    compute_number_annotation_positions_opposite = function(summary_data) {
      positions <- summary_data %>% private$compute_positions("NUMBER", TRUE)
      return(positions)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3.3: Create plot parts">

    #' Common ggplot2 theme for all plots
    #'
    #' Provides a minimalistic theme with white background and rotated x-axis text.
    #'
    #' @param none NULL No parameters.
    #' @return ggplot2 theme object
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$private$get_common_theme()
    #' @family theming
    #' @keywords internal
    #' @concept theming
    #' @note Applies consistent styling across all plots
    #' @seealso ggplot2::theme_minimal, private$get_date_scale
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

    #' Date scale for x-axis
    #'
    #' Creates a date scale for ggplot2 x-axis with weekly breaks and formatted labels.
    #'
    #' @param min_date Date Starting date for scale limits
    #' @param max_date Date Ending date for scale limits
    #' @return ggplot2 scale object
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$private$get_date_scale(as.Date("2021-01-01"),as.Date("2021-01-31"))
    #' @family scales
    #' @keywords internal
    #' @concept scaling
    #' @note Uses 1-week breaks and 5%-10% expansion
    #' @seealso ggplot2::scale_x_date, private$get_common_theme
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

    #' Generic distribution plotting helper
    #'
    #' Generate a distribution plot with annotation for date or numeric types.
    #'
    #' @param transformed_data data.frame Data frame with parsed data.
    #' @param summary_data data.frame Summary data frame with grouping and Distribution columns.
    #' @param label character Plot label string used for axis labels and plot title.
    #' @param type character Either "date" or "number" to select geometry and scale.
    #' @return ggplot Invisibly returns the ggplot object created.
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), Distribution = c(5,3))
    #' pg$private$plot_distribution_generic(df, df, 'Test', 'date')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_generic
    #' @note Internally chooses layers and scales based on type.
    #' @seealso private$get_common_theme, private$compute_date_annotation_positions
    plot_distribution_generic = function(transformed_data, summary_data, label, type) {
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_data_frame(summary_data, null.ok = FALSE)
      checkmate::assert_string(label, null.ok = FALSE)
      checkmate::assert_choice(type, c("date", "number"))
      # compute annotation and position
      if (type == "date") {
        position <- private$compute_date_annotation_positions(summary_data)
        annotation <- private$compute_date_annotation(transformed_data, summary_data)
        geom_layer <- list(geom_line(), geom_point(), private$get_date_scale(min(summary_data$DATE), max(summary_data$DATE)))
        x_lab <- "Tag"
        y_lab <- paste(label, "pro Tag")
        title_suffix <- "Verteilung"
        x <- summary_data$DATE
        aes_mapping <- aes(x = DATE, y = Distribution)
      } else {
        position <- private$compute_number_annotation_positions(summary_data)
        annotation <- private$compute_number_annotation(transformed_data, summary_data)
        geom_layer <- list(geom_bar(stat = "identity"))
        x_lab <- "Gruppe"
        y_lab <- paste(label, "pro Gruppe")
        title_suffix <- "Verteilung"
        aes_mapping <- aes(x = GROUP, y = Distribution)
      }
      plot <- ggplot(summary_data, aes_mapping) +
        geom_layer +
        labs(title = paste(label, title_suffix), x = x_lab, y = y_lab) +
        annotate("text", x = position$x_left, y = position$y_top * 1.4, label = annotation, hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
        private$get_common_theme()
      return(invisible(plot))
    },

    #' Generate a date-based distribution plot with annotation
    #'
    #' Parses and plots event distribution over dates with statistical annotation.
    #' @param transformed_data data.frame Data frame containing a DATE column and parsed dates
    #' @param summary_data data.frame Summary data frame containing DATE and Distribution columns
    #' @param label character One-element character vector used for axis labels and plot title
    #' @return ggplot Invi­sibly returns the ggplot object
    #' @examples
    #' pg <- PlotGenerator$new()
    #' df <- data.frame(DATE=as.Date(c('2021-01-01','2021-01-02')), Distribution=c(5,3))
    #' pg$private$plot_date_distribution(df, df, 'Test')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution
    #' @note Uses private methods for theme and date scaling
    #' @seealso private$compute_date_annotation, private$get_common_theme
    plot_date_distribution = function(transformed_data, summary_data, label) {
      plot <- transformed_data %>% private$plot_distribution_generic(summary_data, label, "date")
      invisible(plot)
    },

    #' Generate a numeric distribution bar plot with annotation
    #'
    #' Plots numeric value distribution across groups with statistical annotation.
    #' @param transformed_data data.frame Data frame containing a NUMBER column
    #' @param summary_data data.frame Summary data frame containing GROUP and Distribution columns
    #' @param label character One-element character vector used for axis labels and plot title
    #' @return ggplot Invi­sibly returns the ggplot object
    #' @examples
    #' pg <- PlotGenerator$new()
    #' df_raw <- data.frame(NUMBER = c(1,2,2,3,NA))
    #' df_sum <- data.frame(GROUP=c('1,2','3,3'), Distribution=c(3,1))
    #' pg$private$plot_number_distribution(df_raw, df_sum, 'Points')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution
    #' @note Uses default bar fill and theme
    #' @seealso private$compute_number_annotation, private$get_common_theme
    plot_number_distribution = function(transformed_data, summary_data, label) {
      plot <- transformed_data %>% private$plot_distribution_generic(summary_data, label, "number")
      invisible(plot)
    },

    #' Generic dual distribution plotting helper
    #'
    #' Generate a dual distribution plot for date or numeric data split by a logical condition with annotations.
    #'
    #' @param transformed_data data.frame Data frame with parsed data and condition column.
    #' @param summary_data data.frame Summary data frame with grouping, ConditionGroup, and Distribution columns.
    #' @param label character Plot label string used for axis labels and plot title.
    #' @param condition character String expression evaluated to split data.
    #' @param type character Either "date" or "number" to select geometry and scale.
    #' @return ggplot Invisibly returns the ggplot object created.
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c(TRUE, FALSE))
    #' summary <- pg$private$create_date_dual_distribution(df, "ZUL == TRUE", as.Date('2021-01-01'), as.Date('2021-01-02'), "day")
    #' pg$private$plot_distribution_dual_generic(df, summary, 'Applications', 'ZUL == TRUE', 'date')
    #' @family plotting
    #' @keywords internal
    #' @concept dual_distribution_generic
    #' @note Internally splits data by condition and adds separate annotations.
    #' @seealso private$plot_date_dual_distribution, private$plot_number_dual_distribution
    plot_distribution_dual_generic = function(transformed_data, summary_data, label, condition, type) {
      checkmate::assert_data_frame(transformed_data, null.ok = FALSE)
      checkmate::assert_data_frame(summary_data, null.ok = FALSE)
      checkmate::assert_string(label, null.ok = FALSE)
      checkmate::assert_string(condition, null.ok = FALSE)
      checkmate::assert_choice(type, c("date", "number"))
      expr <- rlang::parse_expr(condition)
      # split data for annotation
      data_true <- transformed_data %>% filter(!!expr)
      summary_true <- summary_data %>% filter(ConditionGroup == TRUE)
      pos_true <- if (type == "date") private$compute_date_annotation_positions(summary_true) else private$compute_number_annotation_positions(summary_true)
      ann_true <- if (type == "date") private$compute_date_annotation(data_true, summary_true) else private$compute_number_annotation(data_true, summary_true)
      data_false <- transformed_data %>% filter(!(!!expr))
      summary_false <- summary_data %>% filter(ConditionGroup == FALSE)
      pos_false <- if (type == "date") private$compute_date_annotation_positions_opposite(summary_false) else private$compute_number_annotation_positions_opposite(summary_false)
      ann_false <- if (type == "date") private$compute_date_annotation(data_false, summary_false) else private$compute_number_annotation(data_false, summary_false)
      # ensure both annotations share the same vertical position
      y_max <- max(summary_data$Distribution, na.rm = TRUE)
      # base mapping
      aes_map <- if (type == "date") aes(x = DATE, y = Distribution, colour = as.factor(ConditionGroup), group = ConditionGroup) else aes(x = GROUP, y = Distribution, fill = as.factor(ConditionGroup), group = ConditionGroup)
      plot <- ggplot(summary_data, aes_map) +
        (if (type == "date") list(geom_line(), geom_point(), private$get_date_scale(min(summary_data$DATE), max(summary_data$DATE))) else geom_bar(stat = "identity", position = position_dodge(), colour = private$get_colors()["bar_outline"])) +
        labs(
          title = paste(label, "Verteilung"),
          x = if (type == "date") "Tag" else "Gruppe",
          y = paste(label, if (type == "date") "pro Tag" else "pro Gruppe"),
          colour = label
        ) +
        annotate(
          "text",
          x = pos_true$x_left,
          y = y_max * 1.4,
          label = ann_true,
          hjust = 0,
          vjust = 1,
          size = 3.5,
          fontface = "italic",
          colour = private$get_colors()["TRUE"]
        ) +
        annotate(
          "text",
          x = pos_false$x_right,
          y = y_max * 1.4,
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
      return(invisible(plot))
    },

    #' Plot dual date distribution
    #'
    #' Generate a dual distribution plot for date data split by a logical condition with annotations.
    #'
    #' @param transformed_data data.frame Data frame containing parsed DATE column and original records
    #' @param summary_data data.frame Summary data frame with DATE, ConditionGroup, and Distribution columns
    #' @param label character One-element string used for axis labels and plot title
    #' @param condition character String expression used to filter and group data
    #' @return ggplot Invisibly returns the ggplot object of the dual date distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c(TRUE, FALSE))
    #' summary <- pg$private$create_date_dual_distribution(df, "ZUL == TRUE", as.Date('2021-01-01'), as.Date('2021-01-02'), "day")
    #' pg$private$plot_date_dual_distribution(df, summary, "Applications", "ZUL == TRUE")
    #' @family plotting
    #' @keywords internal
    #' @concept dual_distribution_date
    #' @note Uses private$plot_distribution_dual_generic with type set to "date"
    #' @seealso private$plot_distribution_dual_generic, private$create_date_dual_distribution
    plot_date_dual_distribution = function(transformed_data, summary_data, label, condition) {
      plot <- transformed_data %>% private$plot_distribution_dual_generic(summary_data, label, condition, "date")
      invisible(plot)
    },

    #' Plot dual numeric distribution
    #'
    #' Generate a dual distribution bar plot for numeric data split by a logical condition with annotations.
    #'
    #' @param transformed_data data.frame Data frame containing parsed NUMBER column and original records
    #' @param summary_data data.frame Summary data frame with GROUP, ConditionGroup, and Distribution columns
    #' @param label character One-element string used for axis labels and plot title
    #' @param condition character String expression used to filter and group data
    #' @return ggplot Invisibly returns the ggplot object of the dual numeric distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(NUMBER = c(1,2,2,3), ZUL = c(TRUE, FALSE, TRUE, FALSE))
    #' summary <- pg$private$create_number_dual_distribution(df, "ZUL == TRUE", 1, 4, 1)
    #' pg$private$plot_number_dual_distribution(df, summary, "Scores", "ZUL == TRUE")
    #' @family plotting
    #' @keywords internal
    #' @concept dual_distribution_numeric
    #' @note Uses private$plot_distribution_dual_generic with type set to "number"
    #' @seealso private$plot_distribution_dual_generic, private$create_number_dual_distribution
    plot_number_dual_distribution = function(transformed_data, summary_data, label, condition) {
      plot <- transformed_data %>% private$plot_distribution_dual_generic(summary_data, label, condition, "number")
      invisible(plot)
    },

    # </editor-fold>

    # <editor-fold desc="Step 3.5: Create cumulative distribution plots">

    #' Generic cumulative distribution plotting helper
    #'
    #' Generate a cumulative distribution plot for date or numeric data.
    #'
    #' @param cumulative_data data.frame Summary data with DATE or GROUP and CumulativeDistributionPercentage columns
    #' @param label character Title and axis label prefix
    #' @param type character Either "date" or "number"
    #' @return ggplot Invisibly returns the ggplot object of the cumulative distribution plot
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), CumulativeDistributionPercentage = c(50,100))
    #' pg$private$plot_cumulative_distribution_generic(df, 'Applications', 'date')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_cumulative_generic
    #' @note Uses geom_line for date type, geom_bar for numeric type, and ensures proper y-axis scale and labels
    #' @seealso private$plot_date_cumulative_distribution, private$plot_number_cumulative_distribution
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
          labs(title = paste(label, "Kumulative Verteilung"), x = "Tag", y = paste0("% der Gesamt-", label, " pro Tag")) +
          private$get_common_theme()
      } else {
        checkmate::assert_names(names(cumulative_data), must.include = c("GROUP", "CumulativeDistributionPercentage"))
        plot <- cumulative_data %>%
          ggplot(aes(x = GROUP, y = CumulativeDistributionPercentage)) +
          geom_bar(stat = "identity") +
          labs(title = paste(label, "Kumulative Verteilung"), x = "Gruppe", y = paste0("% der Gesamt-", label, " pro Gruppe")) +
          private$get_common_theme()
      }
      checkmate::assert_class(plot, "ggplot", null.ok = FALSE)
      invisible(plot)
    },

    #' Plot cumulative date distribution
    #'
    #' Generate a cumulative distribution plot for date data.
    #'
    #' @param transformed_data data.frame Data frame containing parsed DATE column and original records
    #' @param cumulative_data data.frame Summary data with DATE and CumulativeDistributionPercentage columns
    #' @param label character Title and axis label prefix
    #' @return ggplot Invisibly returns the ggplot object of the cumulative date distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), CumulativeDistributionPercentage = c(50,100))
    #' pg$private$plot_date_cumulative_distribution(df, df, 'Applications')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_cumulative_date
    #' @note Calls private$plot_cumulative_distribution_generic with type set to "date"
    #' @seealso private$plot_cumulative_distribution_generic
    plot_date_cumulative_distribution = function(transformed_data, cumulative_data, label) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_generic(label, "date")
      invisible(plot)
    },

    #' Plot cumulative numeric distribution
    #'
    #' Generate a cumulative distribution bar plot for numeric data.
    #'
    #' @param transformed_data data.frame Data frame containing parsed NUMBER column and original records
    #' @param cumulative_data data.frame Summary data with GROUP and CumulativeDistributionPercentage columns
    #' @param label character Title and axis label prefix
    #' @return ggplot Invisibly returns the ggplot object of the cumulative numeric distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' df_num <- data.frame(GROUP = c('1,2','3,4'), CumulativeDistributionPercentage = c(25,100))
    #' pg$private$plot_number_cumulative_distribution(df_raw, df_num, 'Scores')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_cumulative_numeric
    #' @note Calls private$plot_cumulative_distribution_generic with type set to "number"
    #' @seealso private$plot_cumulative_distribution_generic
    plot_number_cumulative_distribution = function(transformed_data, cumulative_data, label) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_generic(label, "number")
      invisible(plot)
    },

    #' Generic dual cumulative distribution plotting helper
    #'
    #' Generate a dual cumulative distribution plot for date or numeric data split by a logical condition.
    #'
    #' @param cumulative_data data.frame Summary data with DATE or GROUP, ConditionGroup, and CumulativeDistributionPercentage columns
    #' @param label character Title and axis label prefix
    #' @param condition character Logical expression to split data
    #' @param type character Either "date" or "number"
    #' @return ggplot Invisibly returns the ggplot object of the dual cumulative distribution plot
    #' @examples
    #' pg <- plot_generator$new()
    #' df <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ConditionGroup = c(TRUE, FALSE), CumulativeDistributionPercentage = c(50,100))
    #' pg$private$plot_cumulative_distribution_dual_generic(df, 'Applications', 'ZUL == TRUE', 'date')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_dual_cumulative_generic
    #' @note Uses manual scales and geoms to plot separate series for each condition group
    #' @seealso private$plot_date_dual_cumulative_distribution, private$plot_number_dual_cumulative_distribution
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
          labs(title = paste(label, "Kumulative Verteilung"), x = "Tag", y = paste0("% der Gesamt-", label, " pro Tag"), colour = label) +
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
          labs(title = paste(label, "Kumulative Verteilung"), x = "Gruppe", y = paste0("% der Gesamt-", label, " pro Tag"), fill = label) +
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
    #' Generate a dual cumulative distribution plot for date data split by a logical condition.
    #'
    #' @param transformed_data data.frame Data frame containing parsed DATE column and original records
    #' @param cumulative_data data.frame Summary data with DATE, ConditionGroup, and CumulativeDistributionPercentage columns
    #' @param label character Title and axis label prefix
    #' @param condition character Logical expression used to split data
    #' @return ggplot Invisibly returns the ggplot object of the dual cumulative date distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' df_raw <- data.frame(DATE = as.Date(c('2021-01-01','2021-01-02')), ZUL = c(TRUE, FALSE))
    #' cum <- pg$private$create_cumulative_distribution(summary_df)
    #' pg$private$plot_date_dual_cumulative_distribution(df_raw, cum, 'Applications', 'ZUL == TRUE')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_dual_cumulative_date
    #' @note Calls private$plot_cumulative_distribution_dual_generic with type set to "date"
    #' @seealso private$plot_cumulative_distribution_dual_generic
    plot_date_dual_cumulative_distribution = function(transformed_data, cumulative_data, label, condition) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_dual_generic(label, condition, "date")
      invisible(plot)
    },

    #' Plot dual cumulative numeric distribution
    #'
    #' Generate a dual cumulative distribution bar plot for numeric data split by a logical condition.
    #'
    #' @param transformed_data data.frame Data frame containing parsed NUMBER column and original records
    #' @param cumulative_data data.frame Summary data with GROUP, ConditionGroup, and CumulativeDistributionPercentage columns
    #' @param label character Title and axis label prefix
    #' @param condition character Logical expression used to split data
    #' @return ggplot Invisibly returns the ggplot object of the dual cumulative numeric distribution
    #' @examples
    #' pg <- plot_generator$new()
    #' df_raw <- data.frame(NUMBER = c(1,2,3), ZUL = c(TRUE, FALSE, TRUE))
    #' cum <- pg$private$create_cumulative_distribution(summary_df)
    #' pg$private$plot_number_dual_cumulative_distribution(df_raw, cum, 'Scores', 'ZUL == TRUE')
    #' @family plotting
    #' @keywords internal
    #' @concept distribution_dual_cumulative_numeric
    #' @note Calls private$plot_cumulative_distribution_dual_generic with type set to "number"
    #' @seealso private$plot_cumulative_distribution_dual_generic
    plot_number_dual_cumulative_distribution = function(transformed_data, cumulative_data, label, condition) {
      plot <- cumulative_data %>% private$plot_cumulative_distribution_dual_generic(label, condition, "number")
      invisible(plot)
    },

    # </editor-fold>

    # </editor-fold>

    # <editor-fold desc="Step 4: Create File Name">

    #' Create file name for output plots
    #'
    #' Builds a filepath by combining the target directory, a prefix derived from the source filename, an infix, and a suffix.
    #' @param source_path character Path to the input file
    #' @param target_path character Directory path for the output file
    #' @param infix character One-element string inserted between the prefix and suffix
    #' @param suffix character One-element string appended as file suffix
    #' @return character Invisibly returns the constructed file path
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$private$create_file_name("data/input.csv", "out", "plot", "png")
    #' @family file-handling
    #' @keywords internal
    #' @concept io
    #' @note Relies on tools::file_path_sans_ext and base::file.path
    #' @seealso basename, file_path_sans_ext, file.path
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

    #' Save a ggplot object to file
    #'
    #' Uses ggsave to write the plot to disk with specified dimensions.
    #' @param filename character File path where the plot will be saved
    #' @param plot ggplot ggplot object to save
    #' @param width numeric Width of the output in inches
    #' @param height numeric Height of the output in inches
    #' @return NULL Invisibly returns NULL after saving
    #' @examples
    #' pg <- PlotGenerator$new()
    #' plot <- ggplot(data.frame(x=1:10, y=1:10), aes(x,y)) + geom_point()
    #' pg$private$save_plot("plot.png", p, 8, 6)
    #' @family io
    #' @keywords internal
    #' @concept file-output
    #' @note Relies on ggplot2::ggsave
    #' @seealso ggplot2::ggsave
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

    #' Generic processing pipeline for plotting distributions and cumulative distributions
    #'
    #' Loads raw data, applies preparation, computes summary and cumulative distributions, generates plots, and saves output files.
    #'
    #' @param source_path character Path to the CSV input file
    #' @param target_path character Directory path for saving output plots
    #' @param date_columns character Vector of column names to parse as dates; may be NULL
    #' @param number_columns character Vector of column names to parse as numeric values; may be NULL
    #' @param condition_columns character Vector of column names for grouping conditions; may be NULL
    #' @param prepare_function function Function transforming raw data.frame to prepared data.frame
    #' @param distribution_function function Function computing summary distribution from prepared data
    #' @param cumulative_distribution_function function Function computing cumulative distribution from summary data
    #' @param distribution_plot_function function Function generating distribution ggplot from prepared and summary data with infix label
    #' @param cumulative_distribution_plot_function function Function generating cumulative distribution ggplot from prepared and cumulative data with infix label
    #' @param infix character Label string used in plot titles and file names
    #' @return NULL Invisibly returns NULL after saving distribution and cumulative distribution plots
    #' @examples
    #' pg <- PlotGenerator$new()
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
    #' @family pipeline
    #' @keywords internal generic workflow
    #' @concept pipeline processing
    #' @note Centralizes common steps for diverse plot types, ensuring consistency in loading, processing, plotting, and saving
    #' @seealso private$load_data, private$create_file_name, private$save_plot
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

    #' Generic processing pipeline for plotting distributions and cumulative distributions
    #'
    #' Loads raw data, applies preparation, computes summary and cumulative distributions, generates plots, and returns the results as a list.
    #'
    #' @param source_path character Path to the CSV input file
    #' @param date_columns character Vector of column names to parse as dates; may be NULL
    #' @param number_columns character Vector of column names to parse as numeric values; may be NULL
    #' @param condition_columns character Vector of column names for grouping conditions; may be NULL
    #' @param prepare_function function Function transforming raw data.frame to prepared data.frame
    #' @param distribution_function function Function computing summary distribution from prepared data
    #' @param cumulative_distribution_function function Function computing cumulative distribution from summary data
    #' @param distribution_plot_function function Function generating distribution ggplot from prepared and summary data with infix label
    #' @param cumulative_distribution_plot_function function Function generating cumulative distribution ggplot from prepared and cumulative data with infix label
    #' @param infix character Label string used in plot titles and file names
    #' @return list A list containing the distribution and cumulative distribution plots
    #' @examples
    #' pg <- PlotGenerator$new()
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
    #' @family pipeline
    #' @keywords internal generic workflow
    #' @concept pipeline processing
    #' @note Centralizes common steps for diverse plot types, ensuring consistency in loading, processing, plotting, and returning results
    #' @seealso private$load_data, private$create_file_name, private$save_plot
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

    #' Process application date plot
    #'
    #' Load application date data, compute distribution and cumulative distribution plots, and save the results.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @param target_path (character) Directory path for the output files.
    #' @return NULL Invisibly returns NULL after processing and saving the plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_plot_application_date("data/input.csv", "out")
    #' @family pipeline
    #' @keywords internal application date workflow
    #' @concept pipeline processing
    #' @note Uses the ABGESCHICKT_DATUM column for dates
    #' @seealso private$process_plot_generic
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

    #' Process application date plot (output)
    #'
    #' Load application date data, compute distribution and cumulative distribution plots, and return the results as a list.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @return list A list containing the distribution and cumulative distribution plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' plots <- pg$process_plot_application_date_output("data/input.csv")
    #' @family pipeline
    #' @keywords internal application date workflow
    #' @concept pipeline processing
    #' @note Uses the ABGESCHICKT_DATUM column for dates
    #' @seealso private$process_plot_generic_output
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

    #' Process admission date plot
    #'
    #' Load admission date data, compute dual distribution and cumulative distribution plots split by condition, and save the results.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @param target_path (character) Directory path for the output files.
    #' @return NULL Invisibly returns NULL after processing and saving the plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_plot_admission_date("data/input.csv", "out")
    #' @family pipeline
    #' @keywords internal admission date workflow
    #' @concept pipeline processing
    #' @note Uses the ZUL_DATUM column for dates and ZUL for grouping
    #' @seealso private$process_plot_generic
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

    #' Process admission date plot (output)
    #'
    #' Load admission date data, compute dual distribution and cumulative distribution plots split by condition, and return the results as a list.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @return list A list containing the distribution and cumulative distribution plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' plots <- pg$process_plot_admission_date_output("data/input.csv")
    #' @family pipeline
    #' @keywords internal admission date workflow
    #' @concept pipeline processing
    #' @note Uses the ZUL_DATUM column for dates and ZUL for grouping
    #' @seealso private$process_plot_generic_output
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

    #' Process application-admission date difference plot
    #'
    #' Load application and admission date data, compute the difference distribution and cumulative distribution plots, and save the results.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @param target_path (character) Directory path for the output files.
    #' @return NULL Invisibly returns NULL after processing and saving the plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_plot_application_admission_date_difference("data/input.csv", "out")
    #' @family pipeline
    #' @keywords internal date difference workflow
    #' @concept pipeline processing
    #' @note Computes day differences between ABGESCHICKT_DATUM and ZUL_DATUM, split by ZUL
    #' @seealso private$process_plot_generic
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

    #' Process application-admission date difference plot (output)
    #'
    #' Load application and admission date data, compute the difference distribution and cumulative distribution plots, and return the results as a list.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @return list A list containing the distribution and cumulative distribution plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' plots <- pg$process_plot_application_admission_date_difference_output("data/input.csv")
    #' @family pipeline
    #' @keywords internal date difference workflow
    #' @concept pipeline processing
    #' @note Computes day differences between ABGESCHICKT_DATUM and ZUL_DATUM, split by ZUL
    #' @seealso private$process_plot_generic_output
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

    #' Process points step 1 plot
    #'
    #' Load points step1 data, compute distribution and cumulative distribution plots, and save the results.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @param target_path (character) Directory path for the output files.
    #' @return NULL Invisibly returns NULL after processing and saving the plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' pg$process_plot_points_step1("data/input.csv", "out")
    #' @family pipeline
    #' @keywords internal points workflow
    #' @concept pipeline processing
    #' @note Uses the ERGEBNIS_STUFE1 column for numeric values
    #' @seealso private$process_plot_generic
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
        infix = "PunkteAusStufe1"
      )
      invisible(NULL)
    },

    #' Process points step 1 plot (output)
    #'
    #' Load points step1 data, compute distribution and cumulative distribution plots, and return the results as a list.
    #'
    #' @param source_path (character) Path to the input CSV file.
    #' @return list A list containing the distribution and cumulative distribution plots.
    #' @examples
    #' pg <- PlotGenerator$new()
    #' plots <- pg$process_plot_points_step1_output("data/input.csv")
    #' @family pipeline
    #' @keywords internal points workflow
    #' @concept pipeline processing
    #' @note Uses the ERGEBNIS_STUFE1 column for numeric values
    #' @seealso private$process_plot_generic_output
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
        infix = "PunkteAusStufe1"
      ))
    }
    # </editor-fold>
    # </editor-fold>
  )
)
