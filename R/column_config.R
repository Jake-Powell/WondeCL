# Default mapping
.default_column_names <- c(
  urn = "URN",
  dob = "DOB", 
  est_id = "EST_ID",
  est_name = "EST_NAME",
  est_wonde_id = "EST_WONDE_ID",
  est_wonde_name = "EST_WONDE_NAME",
  est_wonde_estno = "EST_WONDE_EST_NO",
  est_wonde_urn = "EST_WONDE_URN",
  s_wonde_id = "STUDENT_WONDE_ID",
  sfn = "PUPIL_FIRST_NAME",
  sln = "PUPIL_LAST_NAME",
  upn = "UPN",
  c_wonde_id = "CLASS_WONDE_ID",
  c_wonde_name = "CLASS_WONDE_NAME",
  t_wonde_ID = "TEACHER_WONDE_ID",
  tfn = "TEACHER_FIRST_NAME",
  tln = "TEACHER_LAST_NAME",
  t_main = "TEACHER_MAIN",
  t_full = "TEACHER_FULL_NAME",
  class_id = "CLASS_ID",
  class_name = "CLASS_NAME",
  withdrawn = "WITHDRAWN",
  withdrawal_date = "WITHDRAWAL_DATE",
  spare = "SPARE",
  hex_id = "PRINT_ID",
  ome_id = "OME_ID"
)

.column_config <- new.env(parent = emptyenv())
.column_config$names <- .default_column_names

#' Set Column Names
#'
#' Updates the column names used internally by the package.
#' Any names not supplied retain their current values.
#'
#' @param ... Named arguments specifying column names to update.
#'
#' @return Invisibly returns the updated column-name mapping.
#'
#' @export
#'
#' @examples
#' set_column_names(urn = "URN", dob = "DOB")
set_column_names <- function(...) {
  
  new_names <- c(...)
  
  if (length(new_names) == 0) {
    return(invisible(.column_config$names))
  }
  
  if (is.null(names(new_names)) ||
      anyNA(names(new_names)) ||
      any(!nzchar(names(new_names)))) {
    stop("All column names must be supplied as named arguments.")
  }
  
  if (anyDuplicated(names(new_names))) {
    stop("Each column identifier must be supplied only once.")
  }
  
  if (!is.character(new_names) ||
      anyNA(new_names) ||
      any(!nzchar(new_names))) {
    stop("Column names must be non-empty character strings.")
  }
  
  unknown <- setdiff(
    names(new_names),
    names(.column_config$names)
  )
  
  if (length(unknown) > 0) {
    stop(
      "Unknown column identifiers: ",
      paste(unknown, collapse = ", ")
    )
  }
  
  # Create updated mapping without changing existing configuration
  updated <- .column_config$names
  updated[names(new_names)] <- new_names
  
  # Check that all configured column names are unique
  if (anyDuplicated(unname(updated))) {
    stop(
      "Configured column names must be unique.",
      call. = FALSE
    )
  }
  
  .column_config$names[names(new_names)] <- new_names
  
  invisible(.column_config$names)
}

#' Get Column Names
#'
#' Returns the currently configured column names.
#'
#' @param name Optional character vector of internal column identifiers.
#'   If NULL, returns all configured column names.
#'
#' @return A named character vector of column names.
#'
#' @export
#'
#' @examples
#' get_column_names()
#' get_column_names("urn")
#' get_column_names(c("urn", "dob"))
get_column_names <- function(name = NULL) {
  
  cols <- .column_config$names
  
  if (is.null(name)) {
    return(cols)
  }
  
  unknown <- setdiff(name, names(cols))
  
  if (length(unknown) > 0) {
    stop(
      "Unknown column identifiers: ",
      paste(unknown, collapse = ", ")
    )
  }
  
  cols[name]
}




#' Reset Column Names
#'
#' Restores the package's default column-name configuration.
#'
#' @return Invisibly returns the default column-name mapping.
#'
#' @export
reset_column_names <- function() {
  
  .column_config$names <- .default_column_names
  
  invisible(.column_config$names)
  
}

#' Rename Columns to Configured Names
#'
#' Renames dataframe columns from internal identifiers to the
#' configured external column names.
#' Columns not included in the lookup are left unchanged.
#'
#' @param df A dataframe.
#'
#' @return A dataframe with configured column names.
#'
rename_to_configured_names <- function(df) {
  
  cols <- get_column_names()
  
  old_names <- names(df)
  matched <- old_names %in% names(cols)
  
  new_names <- old_names
  new_names[matched] <- unname(cols[old_names[matched]])
  
  if (anyDuplicated(new_names)) {
    stop(
      "Renaming would produce duplicate column names: ",
      paste(unique(new_names[duplicated(new_names)]),
            collapse = ", "),
      call. = FALSE
    )
  }
  
  names(df) <- new_names
  
  df
}


#' Rename Columns to Internal Names
#'
#' Renames dataframe columns from the configured external names
#' back to the internal identifiers used by the package.
#' Columns not included in the lookup are left unchanged.
#'
#' @param df A dataframe.
#'
#' @return A dataframe with internal column names.
#'
rename_to_internal_names <- function(df) {
  
  cols <- get_column_names()
  
  old_names <- names(df)
  matched <- old_names %in% unname(cols)
  
  new_names <- old_names
  new_names[matched] <- names(cols)[match(
    old_names[matched],
    unname(cols)
  )]
  
  if (anyDuplicated(new_names)) {
    stop(
      "Renaming would produce duplicate column names: ",
      paste(unique(new_names[duplicated(new_names)]),
            collapse = ", "),
      call. = FALSE
    )
  }
  
  names(df) <- new_names
  
  df
}
