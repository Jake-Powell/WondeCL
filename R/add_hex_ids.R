
#' @title Add Hexadecimal Representation of Numeric IDs
#'
#' @description
#' Adds a hexadecimal representation of an existing numeric or
#' numeric-like identifier column to a data frame.
#'
#' Both the default source ID (`ome_id`) and hexadecimal output
#' column (`hex_id`) use the package's column configuration.
#'
#' @param data A data frame containing numeric or numeric-like IDs.
#' @param ID_column Character string identifying the source ID.
#'   Defaults to `"ome_id"`. Accepts an internal column key,
#'   configured column name, or another column present in the data.
#'
#' @return The input data frame with an additional hexadecimal
#'   column named according to `get_column_names()[["hex_id"]]`.
#'   Existing column names and order are preserved.
#'
#' @details
#' Non-numeric and missing values are converted to NA.
#'
#' @examples
#' \dontrun{
#' add_hex_id(class_list)
#' add_hex_id(df, ID_column = "ID")
#' add_hex_id(df, ID_column = "print_id")
#' }
#'
#' @export
add_hex_id <- function(data, ID_column = "ome_id") {
  
  # ------------------------------------------------------------
  # 1. Validate arguments
  # ------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  
  if (!is.character(ID_column) ||
      length(ID_column) != 1L ||
      is.na(ID_column) ||
      !nzchar(ID_column)) {
    
    stop(
      "ID_column must be a single column name.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 2. Retrieve configured column names
  # ------------------------------------------------------------
  column_names <- get_column_names()
  
  hex_column <- column_names[["hex_id"]]
  
  if (is.null(hex_column) ||
      is.na(hex_column) ||
      !nzchar(hex_column)) {
    
    stop(
      "The column configuration must contain a valid hex_id.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Resolve source ID column
  # ------------------------------------------------------------
  if (ID_column %in% names(data)) {
    
    id_col <- ID_column
    
  } else if (ID_column %in% names(column_names)) {
    
    id_col <- column_names[[ID_column]]
    
  } else {
    
    id_col <- ID_column
  }
  
  if (!id_col %in% names(data)) {
    stop(
      "Column '", ID_column, "' not found in data.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Convert IDs to hexadecimal
  # ------------------------------------------------------------
  suppressWarnings({
    
    numeric_ids <- as.numeric(data[[id_col]])
    
    hex_vals <- rep(
      NA_character_,
      length(numeric_ids)
    )
    
    valid <- !is.na(numeric_ids)
    
    if (any(valid)) {
      hex_vals[valid] <- as.character(
        as.hexmode(numeric_ids[valid])
      )
    }
  })
  
  # ------------------------------------------------------------
  # 5. Assign using configured hex_id column
  # ------------------------------------------------------------
  data[[hex_column]] <- hex_vals
  
  data
}
