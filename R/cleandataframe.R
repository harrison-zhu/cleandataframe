# check is an empty data.frame
# check column names and format column names
# check  rown names and format row names
# check columns and format if needded
# check records and operate  records if needed

#' @import snakecase
#' @import stringdist

#' @title
#' Clean and Validate a Data Frame
#'
#' @description
#' Cleans and validates a data.frame or tibble according to user-specified rules for column names, row names, columns, and records.
#' This function can:
#' \itemize{
#'   \item Check if the data.frame is empty.
#'   \item Validate and optionally format column names (case style, regex pattern).
#'   \item Validate and optionally format row names (if present).
#'   \item Clean and validate each column using `clean_vector()`.
#'   \item Optionally check and operate on records (rows), e.g., remove duplicates or empty rows.
#' }
#'
#' @param df data.frame or tibble. The input data frame to clean.
#' @param colname_case_style character. Case style for column names (e.g., "snake", "lower", etc.). Default is NULL (no change).
#' @param colname_pattern character. Regex pattern for column names. Default is NULL (no check).
#' @param rowname_case_style character. Case style for row names. Default is NULL (no change).
#' @param rowname_pattern character. Regex pattern for row names. Default is NULL (no check).
#' @param clean_columns logical. Whether to clean columns using clean_vector(). Default is TRUE.
#' @param clean_columns_args list. Additional arguments passed to clean_vector() for columns. Default is list().
#' @param check_column logical. Whether to check and operate on columns. Default is FALSE.
#' @param column_operation character. Vector of operations for columns: "keep", "remove_empty", "remove_duplicates", "replace_duplicates", "remove_no_name", "replace_no_name". Default is "keep".
#' @param check_records logical. Whether to check and operate on records (rows). Default is FALSE.
#' @param record_operation character. Vector of operations for records: "keep", "remove_empty", "remove_duplicates", "replace_duplicates", "remove_no_name", "replace_no_name". Default is "keep".
#' @param replace_whitespace logical. Whether to replace whitespace with NA or not. Default is \code{TRUE}.
#' @return A cleaned data.frame or tibble.
#' @examples
#' # Example: Converts the column titles of the data frame to the correct case,
#' # converts each column to be characters, and replaces empty and duplicate
#' # values in each column with NA.
#' test_df <- data.frame(
#'   Name = c("Alice Smith", "bob jones", "Alice Smith", "", NA),
#'   Age = c(30, 25, 30, NA, 40),
#'   stringsAsFactors = FALSE
#' )
#' clean_dataframe(
#'   test_df,
#'   colname_case_style = "snake",
#'   clean_columns_args = list(
#'     class_convert = "character",
#'     duplicate_operation = "replace",
#'     validate_non_duplicate_value = TRUE,
#'     empty_operation = "replace",
#'     validate_nonempty_value = TRUE
#'   )
#' )
#'
#' # Example: Converts the column titles to be uppercase and checks that the names are
#' # only letters. Removes any duplicate columns with matching titles and values.
#' duplicate_df <- data.frame(
#'   a = c("Alice", "Bob", "Cole", "Damien"),
#'   b = c(10, 20, 30, 40),
#'   c = c("orange", "purple", "yellow", "red"),
#'   d = c("Alice", "Billy", "Collin", "Dan"),
#'   e = c(10, 20, 30, 40)
#' )
#'
#' # Change the column names to create duplicates for the test.
#' names(duplicate_df)[4] <- "a"
#' names(duplicate_df)[5] <- "b"
#'
#' clean_dataframe(
#'   duplicate_df,
#'   colname_case_style = "upper_lower",
#'   colname_pattern = "^[A-Za-z]+$",
#'   check_column = TRUE,
#'   column_operation = "remove_duplicates"
#' )
#'
#' # Example: A data frame containing duplicate and empty entries is given.
#' # Column names are changed to be in uppercase before removing duplicates
#' # and empty records from the data frame.
#'
#' record_df <- data.frame(
#'   fruit = c("apple", "banana", "strawberry", NA, "orange", "pear", "apple"),
#'   color = c("red", "yellow", "red", NA, "orange", "green", "red"),
#'   count = c(3, 2, 10, NA, 3, 4, 3)
#' )
#'
#' clean_dataframe(
#'   record_df,
#'   colname_case_style = "upper_lower",
#'   check_records = TRUE,
#'   record_operation = c("remove_duplicates", "remove_empty")
#' )
#'
#' @export
clean_dataframe <- function(
  df,
  colname_case_style = NULL,
  colname_pattern = NULL,
  rowname_case_style = NULL,
  rowname_pattern = NULL,
  clean_columns = TRUE,
  clean_columns_args = list(),
  check_column = FALSE,
  column_operation = "keep",
  check_records = FALSE,
  record_operation = "keep",
  replace_whitespace = TRUE
) {
  # Check if input is a data.frame or tibble
  if (!inherits(df, "data.frame")) stop("Input must be a data.frame or tibble.")
  # Check if empty
  if (nrow(df) == 0 || ncol(df) == 0) stop("Input data.frame is empty.")

  # Clean column names
  coln <- colnames(df)
  if (!is.null(colname_case_style) || !is.null(colname_pattern)) {
    if (!requireNamespace("snakecase", quietly = TRUE)) {
      stop("The 'snakecase' package is required for column name formatting. Please install it.")
    }
    if (!is.null(colname_case_style)) {
      coln <- snakecase::to_any_case(coln, case = colname_case_style)
    }
    if (!is.null(colname_pattern)) {
      # Replace non-matching names with NA
      coln[!grepl(colname_pattern, coln)] <- NA
      if (any(is.na(coln))) stop("Some column names do not match the required pattern.")
    }
    colnames(df) <- coln
  }

  # Clean row names if present
  if (!is.null(rownames(df))) {
    rn <- rownames(df)
    if (!is.null(rowname_case_style) || !is.null(rowname_pattern)) {
      if (!requireNamespace("snakecase", quietly = TRUE)) {
        stop("The 'snakecase' package is required for row name formatting. Please install it.")
      }
      if (!is.null(rowname_case_style)) {
        rn <- snakecase::to_any_case(rn, case = rowname_case_style)
      }
      if (!is.null(rowname_pattern)) {
        rn[!grepl(rowname_pattern, rn)] <- NA
        if (any(is.na(rn))) stop("Some row names do not match the required pattern.")
      }
      rownames(df) <- rn
    }
  }

  # Clean columns
  if (clean_columns) {
    for (col in seq_along(df)) {
      df[[col]] <- do.call(clean_vector, c(list(df[[col]]), clean_columns_args))
    }
  }

  # Check and operate on columns
  col_op <- match.arg(column_operation, choices = c("keep", "remove_empty", "remove_duplicates", "replace_duplicates", "remove_no_name", "remove_incomplete", "replace_no_name"), several.ok = TRUE)
  if (check_column) {
    if ("keep" %in% col_op) {
      if (any(duplicated(t(df)))) {
        cat(paste0("There are some duplicate columns that are present.\n"))
      }
    }

    if ("replace_duplicates" %in% col_op) {
      df[, duplicated(t(df))] <- NA
    }

    if ("remove_duplicates" %in% col_op) {
      df <- df[, !duplicated(t(df)), drop = FALSE]
    }

    if ("replace_no_name" %in% col_op) {
      no_name <- is.na(names(df)) | grepl("^\\s*$", names(df))
      df[, no_name] <- NA
    }

    if ("remove_no_name" %in% col_op) {
      no_name <- is.na(names(df)) | grepl("^\\s*$", names(df))
      df <- df[, !no_name, drop = FALSE]
    }

    if ("remove_incomplete" %in% col_op) {
      empty_cols <- apply(df, 2, function(col) any(is.na(col) | grepl("^\\s*$", col)))
      df <- df[, !empty_cols, drop = FALSE]
    }

    if ("remove_empty" %in% col_op) {
      empty_cols <- apply(df, 2, function(col) all(is.na(col) | grepl("^\\s*$", col)))
      df <- df[, !empty_cols, drop = FALSE]
    }
  }

  # Check and operate on records (rows)
  rec_op <- match.arg(record_operation, choices = c("keep", "remove_empty", "remove_duplicates", "replace_duplicates", "remove_no_name", "remove_incomplete", "replace_no_name"), several.ok = TRUE)
  if (check_records) {
    if ("keep" %in% rec_op) {
      cat(paste0("There are some duplicate records that are present.\n"))
    }
    
    if ("replace_duplicates" %in% rec_op) {
      df[duplicated(df), ] <- NA
    }

    if ("remove_duplicates" %in% rec_op) {
      df <- df[!duplicated(df), , drop = FALSE]
    }
    
    if ("replace_no_name" %in% rec_op) {
      no_name <- grepl("^\\s*$", names(df))
      df[, no_name] <- NA
    }

    if ("remove_no_name" %in% rec_op) {
      no_name <- grepl("^\\s*$", names(df))
      df <- df[, !no_name, drop = FALSE]
    }

    if ("remove_incomplete" %in% rec_op) {
      empty_rows <- apply(df, 1, function(row) any(is.na(row) | grepl("^\\s*$", col)))
      df <- df[!empty_rows, , drop = FALSE]
    }

    if ("remove_empty" %in% rec_op) {
      empty_rows <- apply(df, 1, function(row) all(is.na(row) | grepl("^\\s*$", col)))
      df <- df[!empty_rows, , drop = FALSE]
    }
  }

  if (replace_whitespace) {
    for (i in seq_len(nrow(df))) {
      for (j in seq_len(ncol(df))) {
        if (grepl("^\\s*$", df[i, j])) {
          df[i, j] <- NA
        }
      }
    }
  }

  return(df)
}
