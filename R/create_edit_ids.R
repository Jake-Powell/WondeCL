#' @title Add Hexadecimal Representation of Numeric IDs
#'
#' @description
#' Adds a hexadecimal representation of an existing numeric or numeric-like
#' identifier column to a data frame.  
#' This can be useful for anonymization, compact storage, cross-system encoding,
#' or debugging.
#'
#' @param data A data frame containing a column with numeric or numeric-like IDs.
#' @param ID_column Character string; name of the column containing the IDs to convert
#'   (default: `"ID"`).
#' @param hex_column Character string; name of the new column to store hexadecimal
#'   values (default: `"HexId"`).
#'
#' @return
#' The input data frame with an additional column containing hexadecimal string
#' representations of the IDs.
#'
#' @details
#' The function converts the specified ID column to numeric, applies `as.hexmode()`,
#' and coerces the result back to character.  
#' This ensures compatibility with external file formats and tidy workflows.
#' 
#' Non-numeric or missing values are converted to `NA` in the hexadecimal column.
#'
#' @examples
#' df <- data.frame(
#'   ID = c(101, 255, 4096, 123456),
#'   Name = c("Alice", "Bob", "Carol", "Dave"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Add a hexadecimal column
#' add_hex_id(df)
#'
#' # Custom column names
#' add_hex_id(df, ID_column = "ID", hex_column = "HexadecimalCode")
#'
#' @export
add_hex_id <- function(data, ID_column = "ID", hex_column = "HexId") {
  # ---- Validate inputs ----
  if (!ID_column %in% names(data)) {
    stop(paste0("Column '", ID_column, "' not found in data."))
  }
  
  # ---- Convert IDs to hexadecimal safely ----
  suppressWarnings({
    numeric_ids <- suppressWarnings(as.numeric(data[[ID_column]]))
    hex_vals <- ifelse(
      is.na(numeric_ids),
      NA_character_,
      as.character(as.hexmode(numeric_ids))
    )
  })
  
  # ---- Assign to new column ----
  data[[hex_column]] <- hex_vals
  
  return(data)
}
