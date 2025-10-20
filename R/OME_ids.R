#' @title Add Internal OME IDs to Class List
#'
#' @description
#' Assigns new internal OME IDs to individuals (students or teachers) in a class list
#' who do not already have one. The OME ID is constructed using the format:
#' \preformatted{
#'   PersonType + SchoolType + EstablishmentID + 5-digit unique numeric suffix
#' }
#'
#' The resulting OME_ID always has a total length of 10 characters.
#'
#' where:
#' \itemize{
#'   \item `PersonType` = 1 for students, 2 for teachers.
#'   \item `SchoolType` = numeric school type (1, 2, 3, or 4).
#'   \item `EstablishmentID` = a 3-digit identifier for the school.
#'   \item The final 5-digit suffix comes from the supplied `available_ids` vector.
#' }
#'
#' Only individuals missing an OME_ID (NA or empty string) are assigned a new ID.
#'
#' @param class_list A data frame containing the columns:
#'   "EstablishmentID", "SchoolType", and "OME_ID".
#'   The `EstablishmentID` must be exactly 3 digits.
#' @param available_ids A character vector of available 5-digit suffixes
#'   (e.g., c("00001", "00002", "00003")) to be used for new IDs.
#' @param person_type Either "student" or "teacher". Determines the first digit of the ID
#'   (1 for students, 2 for teachers).
#'
#' @return A list containing:
#' \itemize{
#'   \item `class_list`: The input data frame with the "OME_ID" column updated
#'         to include new IDs for individuals who were missing one.
#'   \item `new_ids`: A data frame containing only the rows where new OME_IDs were assigned,
#'         including key identifying columns.
#' }
#'
#' @examples
#' \dontrun{
#' class_list <- data.frame(
#'   EstablishmentID = c("123", "123", "456", "456"),
#'   SchoolType = c(1, 1, 2, 4),
#'   OME_ID = c(NA, "", "21245600001", ""),
#'   stringsAsFactors = FALSE
#' )
#'
#' available_ids <- sprintf("%05d", 1:100)
#'
#' # Add new IDs for students
#' result <- add_OME_id(class_list, available_ids, person_type = "student")
#' result$class_list
#' result$new_ids
#'
#' # Add new IDs for teachers
#' add_OME_id(class_list, available_ids, person_type = "teacher")
#' }
#'
#' @export
add_OME_id <- function(class_list, available_ids, person_type = "student") {
  
  # ---- Validate inputs ----
  required_cols <- c("EstablishmentID", "SchoolType", "OME_ID")
  missing_cols <- setdiff(required_cols, names(class_list))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in class_list: ",
         paste(missing_cols, collapse = ", "))
  }
  
  if (!is.character(available_ids)) {
    stop("`available_ids` must be a character vector of 5-digit strings (e.g. '00001').")
  }
  
  if (!person_type %in% c("student", "teacher")) {
    stop("`person_type` must be either 'student' or 'teacher'.")
  }
  
  # Person type prefix: 1 = student, 2 = teacher
  person_prefix <- ifelse(person_type == "student", "1", "2")
  
  # Validate EstablishmentID format (must be exactly 3 digits)
  if (any(!grepl("^[0-9]{3}$", class_list$EstablishmentID))) {
    stop("All EstablishmentID values must be exactly 3 digits (e.g., '123').")
  }
  
  # ---- Identify individuals missing OME_ID ----
  missing_index <- which(is.na(class_list$OME_ID) | class_list$OME_ID == "")
  if (length(missing_index) == 0) {
    cli::cli_inform("All individuals already have an OME_ID. No new IDs created.")
    return(list(class_list = class_list, new_ids = data.frame()))
  }
  
  if (length(available_ids) < length(missing_index)) {
    stop("Not enough available IDs supplied (need ", length(missing_index),
         ", have ", length(available_ids), ").")
  }
  
  # ---- Assign new IDs ----
  id_counter <- 1
  new_records <- list()
  
  for (i in missing_index) {
    estab_id <- class_list$EstablishmentID[i]
    school_type <- class_list$SchoolType[i]
    
    if (is.na(estab_id) || estab_id == "") {
      stop(paste0("Missing EstablishmentID for row ", i, "."))
    }
    if (is.na(school_type) || !school_type %in% c(1, 2, 3, 4)) {
      stop(paste0("Invalid SchoolType for row ", i, ". Must be 1, 2, 3, or 4."))
    }
    
    suffix <- available_ids[id_counter]
    id_counter <- id_counter + 1
    
    new_id <- paste0(person_prefix, school_type, estab_id, suffix)
    
    # Validate total OME_ID length
    if (nchar(new_id) != 10) {
      stop(paste0("Generated OME_ID (", new_id, ") does not have 10 characters."))
    }
    
    class_list$OME_ID[i] <- new_id
    new_records[[length(new_records) + 1]] <- data.frame(
      Row = i,
      EstablishmentID = estab_id,
      SchoolType = school_type,
      OME_ID = new_id,
      stringsAsFactors = FALSE
    )
  }
  
  new_ids_df <- do.call(rbind, new_records)
  
  cli::cli_inform(c(
    "v" = paste0(
      nrow(new_ids_df), " new OME_IDs created successfully (person type: ",
      person_type, ")."
    )
  ))
  
  return(list(class_list = class_list, new_ids = new_ids_df))
}


#' @title Add Hexadecimal Representation of OME_ID
#'
#' @description
#' Adds a hexadecimal version of an existing numeric identifier (such as an OME_ID)
#' to the data frame. The hexadecimal form can be useful for anonymization,
#' compact encoding, or debugging purposes.
#'
#' @param data A data frame containing a column with numeric or numeric-like IDs.
#' @param ID_column Character string; name of the column containing the IDs to convert
#' (default: `"OME_ID"`).
#' @param hex_column Character string; name of the new column to store hexadecimal
#' values (default: `"HexId"`).
#'
#' @return
#' The input data frame with an additional column containing hexadecimal string
#' representations of the IDs.
#'
#' @details
#' The function converts the specified ID column to character,
#' coerces it into hexadecimal via `as.hexmode()`,
#' and returns the resulting hex codes as character strings
#' (not as a `hexmode` object).  
#' This ensures compatibility with external file formats and tidy workflows.
#'
#' @examples
#' \dontrun{
#' class_list <- data.frame(
#'   OME_ID = c("1112300001", "1112300002", "2124560001"),
#'   stringsAsFactors = FALSE
#' )
#'
#' add_hex_id(class_list)
#'
#' # Custom column names
#' add_hex_id(class_list, ID_column = "OME_ID", hex_column = "HexadecimalCode")
#' }
#'
#' @export
add_hex_id <- function(data, ID_column = "OME_ID", hex_column = "HexId") {
  # ---- Validate inputs ----
  if (!ID_column %in% names(data)) {
    stop(paste0("Column '", ID_column, "' not found in data."))
  }
  
  # ---- Convert IDs to hexadecimal safely ----
  hex_vals <- as.character(as.hexmode(data[[ID_column]] |> as.numeric()))
  
  # ---- Assign to new column ----
  data[[hex_column]] <- hex_vals
  
  return(data)
}
