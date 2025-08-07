#' @import snakecase
#' @import stringdist

#' @title
#' Checks whether a vector is from a specified class.
#'
#' @description
#' Assert that an object inherits from a specified class.
#'
#' @param vector The object to check.
#' @param required_class character. A string specifying the class the object should inherit from.
#'
#' @return logical. Returns \code{TRUE} if the object is of the required class; otherwise, prints a warning and returns \code{FALSE}.
#'
#' @examples
#' # Example: Passes and returns TRUE because 1:3 is of class "integer"
#' assert_class(1:3, "integer")
#'
#' # Example: Prints warning and returns FALSE because 1:3 is not of the
#' # class "character"
#' assert_class(1:3, "character")
#'
#' @export
assert_class <- function(vector, required_class) {
  # Paste a message if the vector is not of the required class
  if (!inherits(vector, required_class)) {
    cat(paste0("The object is not of class ", required_class, "\n"))
  }

  return(inherits(vector, required_class))
}

#' @title
#' Convert an object to a different class.
#'
#' @description
#' Convert an object to a specific built-in class if it is not already of that class.
#' If a class is not built-in and no custom function is provided, an error will be thrown.
#' Convert an object to a specified class outside of the built-in types using a custom function.
#' If the conversion fails and the resulting vector is not of the desired class, and error will be thrown.
#'
#' @param vector An object to be coerced.
#' @param required_class character. A string specifying the target class.
#' @param custom_transform_function A custom function to transform the vector to the required class. Default is \code{NULL}.
#'
#' @return vector. coerced to the required class if necessary.
#' @examples
#' # Example: Coerce numeric to character
#' convert_class(1:3, "character")
#'
#' # Example: No coercion needed (already integer)
#' convert_class(1:3, "integer")
#'
#' # Example: Attempt to coerce character to numeric
#' convert_class(c("1", "2", "3"), "numeric")
#'
#' # Example: Attempt to coerce numerical vector into list.
#' convert_class(1:3, "list", as.list)
#' @export
convert_class <- function(vector, required_class, custom_transform_function = NULL) {
  if (class(vector) != required_class) {
    vector <- tryCatch({
      if (is.null(custom_transform_function)) {
        switch(required_class,
          "character" = as.character(vector),
          "numeric"   = as.numeric(vector),
          "integer"   = as.integer(vector),
          "factor"    = as.factor(vector),
          "logical"   = as.logical(vector),
          "Date"      = as.Date(vector),
          stop(paste0("No coercion implemented for class ", required_class))
        )
      } else {
        new_vector <- custom_transform_function(vector)
        if (class(new_vector) != required_class) {
          stop(paste0("The custom_transform_function did not return a vector of class ", required_class))
        }
        new_vector
      }
    }, error = function(e) {
      stop(paste0("Cannot coerce vector to class ", required_class, ": ", e$message))
    })
  }
  return(vector)
}

#' @title
#' Checks whether a character vector has the specified encoding.
#'
#' @description
#' Assert that all elements of a character vector have the required encoding.
#' Any non-character or factor vectors will cause for an error to be thrown.
#'
#' @param vector A character vector to check.
#' @param required_encode character. A string specifying the required encoding (e.g., "UTF-8", "latin1").
#'
#' @return logical. TRUE if all elements have the required encoding, FALSE otherwise.
#' @examples
#' # All elements are UTF-8 encoded
#' assert_encode(c("a", "b", "c"), "UTF-8")
#'
#' # Mixed encodings (should return FALSE)
#' x <- c("apple", iconv("café", to = "latin1"))
#' assert_encode(x, "UTF-8")
#' @export
assert_encode <- function(vector, required_encode) {
  if (!is.character(vector) || is.factor(vector)) {
    stop(paste0("Cannot check encoding: Vector is not of the character or factor class."))
  }
  encs <- Encoding(vector)
  # Treat 'unknown' as matching required_encode
  return(all(encs == required_encode | encs == "unknown"))
}

#' @title
#' Convert the encoding of all elements in a character vector to the specified encoding.
#'
#' @description
#' Checks if all elements of the input character vector have the specified encoding.
#' If not, try to convert all elements to the required encoding using \code{iconv}.
#' Throws an error if the conversion is unsuccessful.
#'
#' @param vector A character vector to check and convert.
#' @param required_encode character. A string specifying the required encoding (e.g., "UTF-8", "latin1").
#' @param from_encode character. A string specifying the encoding to convert from. Default is \code{""} or no pattern.
#' @param ignore_na logical. Whether to throw an error if NA values are present or not. Default is \code{TRUE}.
#'
#' @return A character vector with the required encoding if conversion is successful; otherwise, prints an error and returns \code{NULL}.
#' @examples
#' # All elements are already UTF-8 encoded
#' convert_encode(c("a", "b", "c"), "UTF-8")
#'
#' # Convert to latin1 encoding
#' x <- c("a", "\u20ac") # Euro sign may not convert into latin1
#' convert_encode(x, "latin1")
#'
#' # Convert from latin1 to UTF-8
#' x <- iconv(c("café", "über"), to = "latin1")
#' convert_encode(x, required_encode = "UTF-8", from_encode = "latin1")
#'
#' # Convert from latin1 to UTF-8 with an NA will throw an error if NA's are not ignored.
#' \dontrun{
#'   x <- iconv(c("café", "über", NA), to = "latin1")
#'   convert_encode(x, required_encode = "UTF-8", from_encode = "latin1", ignore_na = FALSE)
#' }
#'
#' @export
convert_encode <- function(vector, required_encode, from_encode = "", ignore_na = TRUE) {
  encs <- Encoding(vector)
  if (all(encs == required_encode | encs == "unknown")) {
    return(vector)
  } else {
    result <- tryCatch(
      {
        converted <- iconv(vector, from = from_encode, to = required_encode)
        if (any(is.na(converted)) & !ignore_na) {
          stop("Some elements could not be converted to the required encoding.")
        }
        converted
      },
      error = function(e) {
        message(paste0("Error in encoding conversion: ", e$message))
        return(NULL)
      }
    )
    return(result)
  }
}

#' @title
#' Assert that a vector matches a specific format, case style, or date/time format.
#'
#' @description
#' Checks whether every element in the vector matches the specific format, styling, and other parameters.
#' For character or factor vectors, checks if all elements match a specified pattern or case style
#' (e.g., "snake", "small_camel", "big_camel", etc. as supported by snakecase::to_any_case).
#' For numeric vectors, checks if all elements match a decimal digit requirement (e.g., number of decimal places).
#' For Date, POSIXct, or POSIXlt vectors, checks if all elements match a specified date/time format.
#'
#' @param vector The vector to check for formatting.
#' @param regex_pattern character. A regular expression pattern (character string starting with "^" or ending with "$") to match for character/factor vectors.
#' @param ignore_na logical. For character/factor vectors, whether to ignore NA values when checking the format. Default is TRUE.
#' @param case_style character. A cinteger.ase style supported by snakecase::to_any_case (e.g., "snake", "small_camel", etc.) for character/factor vectors.
#' @param decimal_digits integer. For numeric vectors, an integer specifying the required number of decimal digits.
#' @param require_integer logical. For numeric vectors, whether to require all elements to be whole numbers.
#' @param date_format character. For Date or POSIXt vectors, a format string as in \code{format.Date} or \code{format.POSIXct} to check string representation.
#' @param na_value character. For character/factor vectors, a value to replace NA values with. Default is NA.
#' @param ... list. Additional arguments passed to snakecase::to_any_case if used.
#'
#' @return logical. TRUE if all elements match the format, otherwise FALSE.
#' @examples
#' assert_format(c("my_var", "another_var"), case_style = "snake")
#' assert_format(c("myVar", "anotherVar"), regex_pattern = "^[A-Za-z]*$", case_style = "small_camel")
#' assert_format(c(1.23, 4.56), decimal_digits = 2)
#' assert_format(c(1, 2, 3), require_integer = TRUE)
#' assert_format(c("foo", "bar"), regex_pattern = "^f")
#' assert_format(c("foo", "fall", NA), regex_pattern = "^f", ignore_na = FALSE)
#' assert_format(c("foo", "fall", "na"), regex_pattern = "^f", na_value = "na")
#' assert_format(as.Date(c("2020-01-01", "2020-01-02")), date_format = "%Y-%m-%d")
#' assert_format(as.POSIXct(c("2020-01-01 12:00:00", "2020-01-02 13:00:00")), date_format = "%Y-%m-%d %H:%M:%S")
#' @export
assert_format <- function(
  vector,
  regex_pattern = NULL,
  ignore_na = TRUE,
  case_style = NULL,
  decimal_digits = NULL,
  require_integer = NULL,
  date_format = NULL,
  na_value = NA,
  ...
) {
  # Handle factor as character
  if (is.factor(vector)) {
    vector <- tryCatch(
      as.character(vector),
      error = function(e) {
        stop("Failed to convert factor to character: ", e$message)
      }
    )
  }
  # Character/factor logic
  if (is.character(vector)) {
    if (!is.null(regex_pattern)) {
      if (ignore_na) {
        if (is.na(na_value)) {
          na_removed <- vector[!is.na(vector)]
        } else {
          na_removed <- vector[vector != na_value & !is.na(vector)]
        }
        return(all(grepl(regex_pattern, na_removed, perl = TRUE)))
      } else {
        na_replaced <- vector
        na_replaced[na_replaced == na_value] <- NA
        return(all(grepl(regex_pattern, na_replaced, perl = TRUE)))
      }
    }
    if (!is.null(case_style)) {
      if (!requireNamespace("snakecase", quietly = TRUE)) {
        stop("snakecase package is required for case style checking. Please install it.")
      }
      converted <- snakecase::to_any_case(vector, case = case_style, ...)
      return(all(vector == converted))
    }
    stop("For character/factor vectors, provide either regex_pattern or case_style.")
  }
  # Numeric logic
  else if (is.numeric(vector)) {
    if (!is.null(require_integer) && isTRUE(require_integer)) {
      return(all(vector == as.integer(vector)))
    }
    if (!is.null(decimal_digits)) {
      dec_digits <- function(x) {
        if (is.na(x) || is.infinite(x)) return(NA_integer_)
        # Remove sign, integer part, and possible scientific notation
        x_str <- sub("^-?\\d+\\.?|e[+-]?\\d+$", "", format(x, scientific = FALSE))
        nchar(x_str)
      }
      return(all(sapply(vector, dec_digits) == decimal_digits))
    }
    stop("For numeric vectors, provide either require_integer = TRUE or decimal_digits = <integer>.")
  }
  # Date/time logic
  else if (inherits(vector, "Date") || inherits(vector, "POSIXct") || inherits(vector, "POSIXlt")) {
    if (!is.null(date_format)) {
      formatted <- format(vector, format = date_format)
      # Check if formatting and parsing back yields the same value (for all non-NA)
      parsed <- tryCatch(
        {
          if (inherits(vector, "Date")) {
            as.Date(formatted, format = date_format)
          } else {
            as.POSIXct(formatted, format = date_format, tz = attr(vector, "tzone"))
          }
        },
        error = function(e) rep(NA, length(formatted))
      )
      # Only compare non-NA elements
      non_na_idx <- which(!is.na(vector))
      if (inherits(vector, "Date")) {
        return(all(vector[non_na_idx] == parsed[non_na_idx]))
      } else {
        # For POSIXct/POSIXlt, compare as numeric (seconds since epoch)
        return(all(as.numeric(vector[non_na_idx]) == as.numeric(parsed[non_na_idx])))
      }
    } else {
      cat("For Date/POSIXct/POSIXlt vectors, provide date_format.")
    }
  }
  # Not supported
  else {
    stop("assert_format only supports character, factor, numeric, Date, or POSIXct/POSIXlt vectors.")
  }
}

#' @title
#' Check for and correct typos in a vector.
#'
#' @description
#' Identifies typos in a vector by clustering similar elements together.
#' A list of the correct spelling can be provided. Otherwise the "correct" spelling is determined
#' by the variation of the typo that occurs most frequently.
#' Any typos deemed not similar enough to the given correct spellings will be treated as separate words,
#' meaning the correct spelling of these typos is assumed to be a word not provided in the correct spellings.
#' Typos can be kept, removed, replaced with the most correct spelling, or cause for an error to be thrown.
#' Any NA or whitespace elements in character vectors are ignored.
#'
#' @param vector A character vector to check for typos.
#' @param distance_method character. A string specifying the distance method to use. Default is "cosine".
#' @param clustering_method character. A string specifying the clustering method to use. Default is "complete".
#' @param cut_height numeric. A value specifying the height at which to cut the dendrogram. Default is 0.5.
#' @param max_distance numeric. A value specifying the maximum distance between elements to be considered similar. Default is 0.5.
#' @param correct logical. A value specifying whether to correct typos. Default is TRUE.
#' @param correct_method character. A string specifying the method to use for correction. Default is "replace".
#' @param correct_spelling character. A vector containing a list of valid spellings for the vector. Default is \code{NULL}.
#' @param similarity_cut numeric. Value between 0 and 1 that determines similarity score needed for two strings to be considered typos. Default is \code{0.25}.
#' The different methods are "replace", "remove", "error", and "keep".
#' @param ... Additional arguments passed to stringdist::stringdistmatrix.
#'
#' @return A character vector with typos corrected if chosen.
#' @examples
#' # Example: Vector with no typos is returned as is.
#' check_typo(c("apple", "banana", "cherry"))
#'
#' # Example: Vector with no typos and spelling provided is returned as is.
#' # "cherry" is not part of the correct spelling, but different from "apple"
#' # and "banana", so it is not replaced.
#' check_typo(
#'   c("apple", "banana", "cherry"),
#'   correct_spelling = c("apple", "banana")
#' )
#'
#' # Example: Default operation is to replace each typo with its most variation
#' # of the correct spelling.
#' check_typo(
#'   c("apple", "aple", "appel", "aple","apple",
#'   "apple","ap ple","Back","Back","b_eck")
#' )
#'
#' # Example: Correct spellings are provided, and typos are matched to
#' # different spellings.
#' # provided that they are similar enough, and then replaced.
#' check_typo(
#'   c("applu", "ap pl", "aple", "banan", "anana", "banbanas"),
#'   correct_spelling = c("apple", "banana")
#' )
#'
#' # Example: Words are matched to correct spelling and replaced if similar.
#' # Words that are not similar to the correct spellings are clustered
#' # separately and matched together among themselves.
#' check_typo(
#'   c("applu", "aple", "anana", "banbanas", "durian", "durian", "duria"),
#'   correct_spelling = c("apple", "banana")
#' )
#'
#' # Example: Remove typos while keeping the "correct" spelling,
#' # which is the most common occurrence
#' check_typo(
#'   c("cow", "cowa", "cowch", "cow", "moose", "meese", "meese"),
#'   correct_method = "remove"
#' )
#'
#' # Example: Check if there are anyt typos and keep them.
#' check_typo(
#'   c("apple", "ap pl", "banana", "orange", "orenge", "apple"),
#'   correct_method = "keep"
#' )
#'
#' # Example: Replaces typos with the most common occurrence, which is
#' # not "apple" in this case
#' # and automatically ignores NA and empty or whitespace strings
#' check_typo(c("apple", "ap pl", NA, "apple", "ap pl", "ap pl", " ", "aple"))
#'
#' # Example: Throw an error if there are any typos present.
#' \dontrun{
#' check_typo(c("apple", "apple", "ap pl"), correct_method = "error")
#' }
#' @importFrom stats as.dist cutree hclust setNames
#' @export
check_typo <- function(
  vector,
  distance_method = "cosine",
  clustering_method = "complete",
  cut_height = 0.5,
  max_distance = 0.5,
  correct = TRUE,
  correct_method = "replace",
  correct_spelling = NULL,
  similarity_cut = 0.25,
  ...
) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("The 'stringdist' package is required. Please install it with install.packages('stringdist').")
  }

  # Check for valid typo operation
  correct_method <- match.arg(correct_method, c("replace", "remove", "error", "keep"))

  # Keep copy of vector
  copy_vector <- vector
  non_empty <- !is.na(vector) & !grepl("^\\s*$", vector)
  corrected <- non_empty
  any_typos <- FALSE

  # Replace corresponding words with correct spelling.
  if (!is.null(correct_spelling)) {
    correct_matrix <- stringdist::stringdistmatrix(
      vector,
      correct_spelling,
      method = distance_method,
      ...
    )
    for (i in seq_len(nrow(correct_matrix))) {
      if (!is.na(vector[i]) && !grepl("^\\s*$", vector[i])) {
        # Smallest similarity score should be checked
        row <- correct_matrix[i, ]
        vector[i] <- (
          if (min(row) <= similarity_cut) {
            curr_typo <- correct_spelling[which.min(row)] != vector[i]
            any_typos <- any_typos || curr_typo
            if (correct_method == "replace") {
              correct_spelling[which.min(row)]
            } else if (correct_method == "remove" && curr_typo) {
              NA
            } else if (correct_method == "error" && curr_typo) {
              stop("There are typos!")
            } else {
              vector[i]
            }
          } else {
            corrected[i] <- FALSE
            NA
          }
        )
      }
    }
  } else {
    corrected <- FALSE
  }

  # Remove empty strings from clustering
  checked <- vector
  if (is.null(correct_spelling)) {
    non_empty_elements <- vector[non_empty]
  } else {
    non_empty_elements <- copy_vector[is.na(vector) & non_empty]
    vector[!corrected] <- copy_vector[!corrected]
  }

  if (length(unique(non_empty_elements)) <= 1) {
    if (any_typos) cat("There are typos!\n")
    return(vector)
  }

  # Get the indices of empty strings
  empty_id <- (grepl("^\\s*$", vector))

  # Calculate frequency for each element
  freq <- sort(table(non_empty_elements), decreasing = TRUE)
  elements_ordered <- names(freq)
  elements_modified <- gsub("[^[:alnum:]]", "", elements_ordered)
  elements_modified <- tolower(elements_modified)

  # Compute string distance matrix
  dist_mat <- stringdist::stringdistmatrix(elements_modified, elements_modified, method = distance_method,...)
  # Hierarchical clustering
  hc <- hclust(as.dist(dist_mat), method = clustering_method)
  # Cut into clusters
  clusters <- cutree(hc, h = cut_height)
  names(clusters) <- elements_ordered
  any_typos <- any_typos || any(duplicated(clusters))

  if (!correct) {
    if (any_typos) {
      cat("There are typos!\n")
      return(vector)
    }
  } else {
    if (any_typos) {
      cat("There are typos!\n")
      if (correct_method == "replace"){
        converted_elements <- elements_ordered
        for (cl in unique(clusters)) {
          idx <- which(clusters == cl)
          first_value_idx <- idx[1]
          converted_elements[idx] <- converted_elements[first_value_idx]
        }
        element_dict <- setNames(converted_elements, elements_ordered)
        converted_vector <- element_dict[vector]
      }
      if (correct_method == "remove") {
        correct_spelling <- c()
        for (cl in unique(clusters)) {
          idx <- which(clusters == cl)
          counts <- table(vector[vector %in% names(clusters)[idx]])
          counts <- sort(counts, decreasing = TRUE)
          correct_spelling <- c(correct_spelling, names(counts)[1])
        }
        vector[!vector %in% correct_spelling] <- NA
        converted_vector <- vector
      }
      if (correct_method == "error") {
        stop("There are typos!")
      }
      if (correct_method == "keep") {
        converted_vector <- vector
      }

      converted_vector[empty_id] <- copy_vector[empty_id]
      converted_vector[corrected] <- checked[corrected]
      return(unname(converted_vector))
    } else {
      return(vector)
    }
  }
}

#' @title
#' Convert a vector to a required format
#'
#' @description
#' Attempts to convert a vector to match a specified format, such as a regular expression pattern for character/factor vectors,
#' integer or decimal requirements for numeric vectors, or a date format for date/time vectors.
#' If the conversion fail or is unable to finish, an error will be thrown instead.
#'
#' @param vector A vector to be converted.
#' @param regex_pattern character. Regular expression pattern to match for character/factor vectors. Default is \code{NULL}.
#' @param ignore_na logical. For character/factor vectors, whether to ignore NA values when checking and altering the format. Default is TRUE.
#' @param case_style character. Case style to convert to ("lower", "upper", "title", etc.) for character/factor vectors. Default is \code{NULL}.
#' @param sep_in character. Regular expression for input word separation (used with case_style). Default is \code{"[^[:alnum:]]"}.
#' @param sep_out character. Separator to use for output (used with case_style). Default is \code{"_"}.
#' @param decimal_digits integer. Number of decimal digits required for numeric vectors. Default is \code{NULL}.
#' @param require_integer logical. If \code{TRUE}, require integer values for numeric vectors. Default is \code{NULL}.
#' @param date_format character. Date format string for date/time vectors. Default is \code{NULL}.
#' @param na_value character. Value to use for replacement of non-matching or failed conversions of character vector. Default is \code{NA}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return The converted vector if conversion is possible; otherwise, an error is thrown.
#'
#' @examples
#' # Character pattern conversion
#' convert_format(c("abc", "123", "a1b2"), regex_pattern = "^[a-z]+$")
#' convert_format(c("abc", "123", "a1b2"), regex_pattern = "^[a-z]+$", na_value = "NA")
#'
#' # Throw an error if conversion is not possible when NA is present and not ignored.
#' \dontrun{
#'  convert_format(c("abc", "a1b2", "123"), regex_pattern = "^[a-z]+$", ignore_na = FALSE)
#'  convert_format(c("abc", "def", NA), regex_pattern = "^[a-z]+$", ignore_na = FALSE)
#' }
#'
#' # Character case style conversion
#' convert_format(c("abc", "DEF", "Ghi"), case_style = "upper_lower")
#'
#' # Numeric to integer conversion
#' convert_format(c(1, 2, 3), require_integer = TRUE)
#'
#' # Numeric rounding to decimal digits
#' convert_format(c(1.234, 2.345), decimal_digits = 2)
#'
#' # Date format conversion
#' convert_format(as.Date(c("2020-01-01", "2020-02-01")), date_format = "%Y-%m-%d")
#' @export
convert_format <- function(
  vector,
  regex_pattern = NULL,
  ignore_na = TRUE,
  case_style = NULL,
  sep_in = "[^\\p{L}\\]{N}]",
  sep_out = "_",
  decimal_digits = NULL,
  require_integer = NULL,
  date_format = NULL,
  na_value = NA,
  ...
) {
  # Check if vector already matches the required format
  is_valid <- tryCatch(
    assert_format(
      vector,
      regex_pattern = regex_pattern,
      ignore_na = ignore_na,
      case_style = case_style,
      require_integer = require_integer,
      decimal_digits = decimal_digits,
      date_format = date_format,
      na_value = na_value
    ),
    error = function(e) FALSE
  )
  if (is_valid) {
    if (is.character(vector) || is.factor(vector)) {
      vector <- as.character(vector)
      vector[is.na(vector)] <- na_value
    }
    return(vector)
  }

  # Try to convert vector to required format
  converted <- vector

  # Character/factor logic
  if (is.character(vector) || is.factor(vector)) {
    converted <- as.character(vector)
    # Case style conversion
    if (!is.null(case_style)) {
      if (!requireNamespace("snakecase", quietly = TRUE)) {
        stop("The 'snakecase' package is required for case_style conversion. Please install it.")
      }
      converted <- snakecase::to_any_case(converted, case = case_style, sep_in = sep_in, sep_out = sep_out)
    }
    # Pattern conversion
    if (!is.null(regex_pattern)) {
      # Replace non-matching elements with na_value
      converted[!grepl(regex_pattern, converted, perl = TRUE)] <- na_value
      # Check if all non-NA elements now match
      if (!assert_format(converted, regex_pattern = regex_pattern, ignore_na = ignore_na, na_value = na_value)) {
        stop("Could not convert all elements to match the required pattern.")
      }
      return(converted)
    }
    # If only case_style was requested, return result
    if (!is.null(case_style)) {
      return(converted)
    }
    stop("For character/factor vectors, provide pattern or case_style for conversion.")
  }
  # Numeric logic
  else if (is.numeric(vector)) {
    if (!is.null(require_integer) && isTRUE(require_integer)) {
      # Try to coerce to integer if possible
      if (all(vector == as.integer(vector), na.rm = TRUE)) {
        converted <- as.integer(vector)
      } else {
        stop("Cannot convert all elements to integer without loss.")
      }
      if (!assert_format(converted, require_integer = TRUE)) {
        stop("Could not convert all elements to integer format.")
      }
      return(converted)
    }
    if (!is.null(decimal_digits)) {
      # Round to required decimal digits
      converted <- round(vector, decimal_digits)
      # Check if all elements now have required decimal digits
      if (!assert_format(converted, decimal_digits = decimal_digits)) {
        stop("Could not convert all elements to required decimal digits.")
      }
      return(converted)
    }
    stop("For numeric vectors, provide either require_integer = TRUE or decimal_digits = <integer> for conversion.")
  }
  # Date/time logic
  else if (inherits(vector, "Date") || inherits(vector, "POSIXct") || inherits(vector, "POSIXlt")) {
    if (!is.null(date_format)) {
      # Try to format and parse back
      formatted <- format(vector, format = date_format)
      if (inherits(vector, "Date")) {
        converted <- as.Date(formatted, format = date_format)
      } else {
        converted <- as.POSIXct(formatted, format = date_format, tz = attr(vector, "tzone"))
      }
      if (!assert_format(converted, date_format = date_format)) {
        stop("Could not convert all elements to required date/time format.")
      }
      return(converted)
    } else {
      stop("For Date/POSIXct/POSIXlt vectors, provide date_format for conversion.")
    }
  }
  # Not supported
  else {
    stop("convert_format only supports character, factor, numeric, Date, or POSIXct/POSIXlt vectors.")
  }
}



#' @title
#' Clean and Validate a Vector
#'
#' @description
#' Cleans and validates a vector according to user-specified rules for class, encoding, duplicates, empty values, and format.
#' This function can:
#' \itemize{
#'   \item Validate and optionally convert the class of the vector.
#'   \item Validate and optionally convert the encoding of the vector (for character vectors).
#'   \item Check for and handle duplicate values according to the specified operation.
#'   \item Check for and handle empty values (\code{NA} or \code{""}) according to the specified operation.
#'   \item check for and optionally correct typos.
#'   \item Validate and optionally convert the format of the vector (e.g., regex pattern, case style, decimal digits, or date format).
#' }
#'
#' @param vector A vector to be cleaned and validated.
#' @param assert_stop logical. Throws an error if \code{TRUE} when an assertion returns \code{FALSE}. Otherwise, pastes a message and continues.
#' @param class_required character. The required class for the vector if it needs to be checked (e.g., "character", "numeric"). Default is \code{NULL} for no validation done.
#' @param class_convert character. Which class to convert the vector to. Default is \code{NULL} for no class conversion.
#' @param custom_transform_function A custom function to transform the vector to the required class. Default is \code{NULL}.
#' @param encode_required character. The required encoding (e.g., "UTF-8", "latin1"). Default is `NULL` for no encoding check.
#' @param encode_convert character. Which encoding to convert the vector to. Default is `NULL` for no encoding conversion.
#' @param from_encode character. The encoding to convert from. Default is \code{"UTF-8"}.
#' @param validate_non_duplicate_value logical. Whether to check for duplicate values. Default is \code{TRUE}.
#' @param duplicate_operation character. Operation to perform if duplicates are found. One of \code{"keep"}, \code{"keep_first"}, \code{"keep_last"}, \code{"replace"}, or \code{"error"}. Default is \code{"keep"}.
#' @param validate_nonempty_value logical. Whether to check for empty values (\code{NA} or \code{""}). Default is \code{TRUE}.
#' @param empty_operation character. Operation to perform if empty values are found. One of \code{"keep"}, \code{"remove"}, \code{"replace"}, or \code{"error"}. Default is \code{"keep"}.
#' @param check_typo logical. Whether to check for typos in character/factor vectors. Default is \code{TRUE}.
#' @param correct_typo logical. Whether to correct typos if found. Default is \code{FALSE}.
#' @param typo_operation character. Operation to perform if typos are found. One of \code{"keep"}, \code{"remove"}, \code{"replace"}, or \code{"error"}. Default is \code{"keep"}.
#' @param correct_spelling character. A vector containing a list of valid spellings for the vector. Default is \code{NULL}.
#' @param sep_in character. Regular expression for splitting words when converting case style. Default is \code{"[^\\p{L}\\]{N}]"}.
#' @param squeeze_continuous_sep_in logical. Whether to squeeze multiple consecutive separators in \code{sep_in} to a single separator. Default is \code{TRUE}.
#' @param remove_initial_and_end_sep_in logical. Whether to remove separators at the start or end of the string when converting case style. Default is \code{TRUE}.
#' @param validate_format logical. Whether to validate the format of the vector (e.g., regex for character, decimal digits for numeric, date format for dates). Default is `FALSE`.
#' @param convert_format logical. Whether to convert the format to the vector to the specified parameters. Default is `FALSE`.
#' @param format_pattern character. Regular expression pattern to match for character/factor vectors. Default is \code{NULL}.
#' @param format_case_style character. String specifying the case style to enforce or convert to (e.g., "lower", "upper", "title", "snake", etc.). Default is \code{NULL}.
#' @param sep_out character. Separator to use when joining words after case conversion. Default is \code{"_"}.
#' @param format_decimal_digits integer. Number of decimal digits required for numeric vectors. Default is \code{NULL}.
#' @param format_require_integer logical. If \code{TRUE}, require integer values for numeric vectors. Default is \code{NULL}.
#' @param format_date_format character string specifying the date format for date/time vectors. Default is \code{NULL}.
#' @param ignore_na logical. For character/factor vectors, whether to ignore NA values. Default is \code{TRUE}.
#' @param na_value character. Value to use for replacement of non-matching or failed conversions. Default is \code{NA}.
#' @param ... Additional arguments passed to \code{assert_format} and \code{convert_format}.
#'
#' @return The cleaned and validated vector, with class, encoding, duplicates, empty values, and format handled as specified.
#'
#' @examples
#' # Checks if the vector is a character vector, and shows an error if it is not.
#' \dontrun{
#' assert_error <- c(1, 2, 3)
#' clean_vector(assert_error, assert_stop = TRUE)
#' }
#' 
#' # Remove duplicates and empty values from a character vector
#' x <- c("a", "b", "a", "", NA, "c")
#' clean_vector(
#'   x,
#'   class_required = "character",
#'   validate_non_duplicate_value = TRUE,
#'   duplicate_operation = "keep_first",
#'   validate_nonempty_value = TRUE,
#'   empty_operation = "remove"
#' )
#'
#' # Convert numeric vector to character and replace duplicates with NA
#' y <- c(1, 2, 2, 3)
#' clean_vector(
#'   y,
#'   class_convert = "character",
#'   validate_non_duplicate_value = TRUE,
#'   duplicate_operation = "replace"
#' )
#'
#' # Error if empty values are present
#' z <- c("foo", "", "bar")
#' \dontrun{
#' clean_vector(z, class_required = "character", empty_operation = "error")
#' }
#'
#' # Validate and convert format (e.g., enforce lower case and pattern)
#' w <- c("Abc", "def", "GHI")
#' clean_vector(
#'   w,
#'   class_required = "character",
#'   validate_format = TRUE,
#'   convert_format = TRUE,
#'   format_case_style = "lower_upper",
#'   format_pattern = "^[a-z]+$"
#' )
#'
#' # Check if a vector is of the character class,
#' # replace typos and remove empty values.
#' typo_check <- c("apple", "ap pl", "appl", NA, "applr", "appe", "banana", "bananana", NA, "", "apple", "\n")
#' clean_vector(
#'   vector = typo_check,
#'   class_required = "character",
#'   validate_nonempty_value = TRUE,
#'   empty_operation = "remove",
#'   validate_non_duplicate_value = TRUE,
#'   duplicate_operation = "keep",
#'   check_typo = TRUE, correct_typo = TRUE,
#'   validate_format = TRUE,
#'   format_pattern = "a|b"
#' )
#'
#' # Check if a vector is of the character class, and attempt to convert
#' # it to the correct regex pattern and replacing with a custom NA value.
#' # Then remove duplicates, including the custom NA value.
#' convert_format_check <- c("apple", "bee", "crab", "apple", "a1b2", "dune", "eel2")
#' clean_vector(
#'   vector = convert_format_check,
#'   class_required = "character",
#'   convert_format = TRUE,
#'   format_pattern = "^[a-z]+$",
#'   validate_non_duplicate_value = TRUE,
#'   duplicate_operation = "keep_first",
#'   na_value = "na"
#' )
#' @importFrom stats as.dist cutree hclust setNames
#' @export
clean_vector <- function(
  vector,
  assert_stop = FALSE,
  class_required = NULL,
  class_convert = NULL,
  custom_transform_function = NULL,
  encode_required = NULL,
  encode_convert = NULL,
  from_encode = "",
  validate_format = FALSE,
  convert_format = FALSE,
  format_pattern = NULL,
  format_case_style = NULL,
  format_decimal_digits = NULL,
  format_require_integer = NULL,
  format_date_format = NULL,
  check_typo = FALSE,
  correct_typo = FALSE,
  correct_spelling = NULL,
  sep_in = "[^\\p{L}\\p{N}]",
  squeeze_continuous_sep_in = TRUE,
  remove_initial_and_end_sep_in = TRUE,
  sep_out = "_",
  typo_operation = "replace",
  validate_non_duplicate_value = FALSE,
  duplicate_operation = "keep",
  validate_nonempty_value = FALSE,
  empty_operation = "keep",
  ignore_na = TRUE,
  na_value = NA,
  ...
) {
  # Check if vector is of the required class
  if (!is.null(class_required) && !assert_class(vector, class_required)) {
    if (assert_stop) {
      stop(paste0("Vector is not of required class: ", class_required))
    } else {
      cat(paste0("Vector is not of required class: ", class_required, "\n"))
    }
  }

  # Convert vector to desired class
  if (!is.null(class_convert)) {
    vector <- convert_class(vector, class_convert, custom_transform_function)
  }

  # Check if vector is of the required encoding
  if (!is.null(encode_required) && !assert_encode(vector, encode_required)) {
    if (assert_stop) {
      stop(paste0("Vector is not of required encoding: ", encode_required))
    } else {
      cat(paste0("Vector is not of required encoding: ", encode_required, "\n"))
    }
  }

  # Convert vector to desired encoding and check if it was done successfully
  if (!is.null(encode_convert)) {
    vector <- convert_encode(vector, encode_required, from_encode, ignore_na = ignore_na)
    if (is.null(vector)) {
      stop(paste0("Vector could not be converted to required encoding: ", encode_required))
    }
  }

  # Check if the vector follows the formatting
  if (validate_format) {
    is_valid <- tryCatch(
      assert_format(
        vector,
        regex_pattern = format_pattern,
        ignore_na = ignore_na,
        case_style = format_case_style,
        require_integer = format_require_integer,
        decimal_digits = format_decimal_digits,
        date_format = format_date_format,
        na_value = na_value,
        ...
      ),
      error = function(e) FALSE
    )
    if (!is_valid) {
      if (assert_stop) {
        stop(paste0("Not all elements in the vector follow the specified format."))
      } else {
        cat(paste0("Not all elements in the vector follow the specified format.\n"))
      }
    }
  }

  # Convert the vector to the correct formatting
  if (convert_format) {
    vector <- convert_format(
      vector,
      regex_pattern = format_pattern,
      ignore_na = ignore_na,
      case_style = format_case_style,
      sep_in = sep_in,
      sep_out = sep_out,
      decimal_digits = format_decimal_digits,
      require_integer = format_require_integer,
      date_format = format_date_format,
      na_value = na_value,
      ...
    )
  }

  if((is.character(vector) || is.factor(vector)) && check_typo){
    if (is.factor(vector)) {
      vector <- as.character(vector)
    }
    typo_op <- match.arg(typo_operation, choices = c("replace", "remove", "error", "keep"))
    vector <- check_typo(
      vector,
      correct = correct_typo,
      correct_method = typo_op,
      correct_spelling = correct_spelling
    )
  }

  # For character or factor vector, if squeeze_continuous_sep_in is TRUE, squeeze continuous separators into one
  if ((is.character(vector) || is.factor(vector)) && isTRUE(squeeze_continuous_sep_in)) {
    # If factor, convert to character first
    if (is.factor(vector)) {
      vector <- as.character(vector)
    }
    # Squeeze continuous separators (sep_in is a regex for the separator)
    # Replace runs of sep_in with a single instance (use perl=TRUE for advanced regex)
    vector <- gsub(paste0("(", sep_in, ")+"), "\\1", vector, perl = TRUE)
  }

  # For character or factor vector, remove initial or last separators according to sep_in
  if ((is.character(vector) || is.factor(vector)) && isTRUE(remove_initial_and_end_sep_in)) {
    # If factor, convert to character first
    if (is.factor(vector)) {
      vector <- as.character(vector)
    }
    # Remove initial separators
    vector <- gsub(paste0("^", sep_in, "*"), "", vector, perl = TRUE)
    # Remove trailing separators
    vector <- gsub(paste0(sep_in, "*$"), "", vector, perl = TRUE)
  }

  # Check for duplicate values and perform the corresponding operation on them
  if (validate_non_duplicate_value) {
    dup_op <- match.arg(duplicate_operation, choices = c("keep", "keep_first", "keep_last", "replace", "error"))
    dup_idx <- duplicated(vector)
    if (any(dup_idx)) {
      if (dup_op == "keep") {
        cat("Vector has duplicates, but keeping all values including duplicates.\n")
        # do nothing, keep all values including duplicates
      } else if (dup_op == "keep_first") {
        vector <- vector[!dup_idx]
      } else if (dup_op == "keep_last") {
        dup_idx_last <- duplicated(vector, fromLast = TRUE)
        vector <- vector[!dup_idx_last]
      } else if (dup_op == "replace") {
        vector[dup_idx] <- na_value
      } else if (dup_op == "error") {
        stop("Duplicate values found in vector.")
      }
    }
  }

  # Check for empty values and perform the corresponding operation on them
  if (validate_nonempty_value) {
    empty_op <- match.arg(empty_operation, choices = c("keep", "remove", "replace", "error"))
    if (is.character(vector) || is.factor(vector)) {
      empty_idx <- is.na(vector) | grepl("^\\s*$", vector) | (!is.na(na_value) & vector == na_value)
    } else {
      empty_idx <- is.na(vector)
    }
    if (any(empty_idx, na.rm = TRUE)) {
      if (empty_op == "keep") {
        cat("Vector has empty values, but keeping all values including empties.\n")
        # do nothing
      } else if (empty_op == "remove") {
        vector <- vector[!empty_idx]
      } else if (empty_op == "replace") {
        vector[empty_idx] <- na_value
      } else if (empty_op == "error") {
        stop("Empty values found in vector.")
      }
    }
  }

  return(vector)
}

# End of cleanvector.R
