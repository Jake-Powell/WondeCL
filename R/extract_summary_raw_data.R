#' Extract distinct values across schools for a specified field
#'
#' This general-purpose function scans through a list of Wonde school datasets (`WondeData`)
#' and compiles a summary of all distinct values found for a user-specified field (column)
#' within each school's dataset.
#'
#' For example, it can be used to summarise:
#' * Year codes - from `student$students.data__year.data.code`
#' * Group names - from `students_group$groups.data__name`
#'
#' @param WondeData A named list of school datasets, typically created by data retrieval
#'   functions such as \code{\link{get_primary_school_student_data}} or
#'   \code{\link{get_secondary_school_student_data}}.
#' @param field_path Character. The path to the column of interest within each Wonde dataset,
#'   using `$` notation (e.g., "student$students.data__year.data.code").
#' @param value_name Character. Optional. The name to give the column containing the
#'   distinct field values in the output (default = "value").
#' @param verbose Logical. If TRUE (default), prints messages about schools that were
#'   skipped due to missing data or fields.
#'
#' @return A data frame with:
#' \describe{
#'   \item{value_name}{The distinct values extracted across all schools.}
#'   \item{count}{The total number of records across all schools for that value.}
#'   \item{school_ids}{Comma-separated list of school IDs where that value appears.}
#' }
#'
#' @examples
#' \dontrun{
#' extract_field_across_schools(WondeData, "student$students.data__year.data.code")
#' extract_field_across_schools(WondeData, "students_group$groups.data__name", "group_name", verbose = FALSE)
#' }
#'
#' @export
extract_field_across_schools <- function(WondeData, field_path, value_name = "value", verbose = TRUE) {
  stopifnot(is.list(WondeData), length(WondeData) > 0)
  if (!is.character(field_path) || length(field_path) != 1)
    stop("`field_path` must be a single character string, e.g., 'student$year.data.code'")
  
  # Step 1: collect results and skipped schools together
  results <- lapply(names(WondeData), function(school_id) {
    x <- WondeData[[school_id]]
    
    value <- tryCatch(
      eval(parse(text = paste0("x$", field_path))),
      error = function(e) NULL
    )
    
    if (is.null(value) || all(is.na(value) | value == "")) {
      return(list(data = NULL, skipped = school_id))
    }
    
    value <- value[!is.na(value) & value != ""]
    counts <- table(value)
    df <- data.frame(
      value = names(counts),
      count = as.integer(counts),
      WondeId = school_id,
      stringsAsFactors = FALSE
    )
    
    list(data = df, skipped = NULL)
  })
  
  # Separate successful data and skipped schools
  data_list <- lapply(results, `[[`, "data")
  skipped <- unlist(lapply(results, `[[`, "skipped"))
  
  # Step 2: combine and handle empty cases
  df <- do.call(rbind, data_list)
  
  if (length(skipped) > 0 && verbose) {
    message(length(unique(skipped)), " school(s) skipped: field not found or empty for ",
            paste(unique(skipped), collapse = ", "))
  }
  
  if (is.null(df) || nrow(df) == 0) {
    if (verbose) message("No data extracted - returning empty summary.")
    empty_df <- data.frame(
      value = character(0),
      count = integer(0),
      school_ids = character(0),
      stringsAsFactors = FALSE
    )
    names(empty_df)[1] <- value_name
    return(empty_df)
  }
  
  # Step 3: aggregate across schools
  summary_df <- aggregate(count ~ value, data = df, sum)
  
  # Step 4: list schools that have each value
  school_list <- tapply(df$WondeId, df$value, function(ids)
    paste(unique(sort(ids)), collapse = ", "))
  
  summary_df$school_ids <- school_list[match(summary_df$value, names(school_list))]
  
  # Step 5: rename and tidy
  names(summary_df)[1] <- value_name
  summary_df <- summary_df[order(summary_df[[value_name]]), ]
  rownames(summary_df) <- NULL
  
  summary_df
}


#' Extract unique year codes across schools
#'
#' Wrapper for \code{\link{extract_field_across_schools}} that summarises
#' year codes across all schools.
#'
#' @inheritParams extract_field_across_schools
#' @export
extract_year_codes_across_schools <- function(WondeData, verbose = TRUE) {
  extract_field_across_schools(WondeData,
                               field_path = "student$year.data.code",
                               value_name = "year_code",
                               verbose = verbose)
}

#' Extract unique group names across schools
#'
#' Wrapper for \code{\link{extract_field_across_schools}} that summarises
#' group names across all schools.
#'
#' @inheritParams extract_field_across_schools
#' @export
extract_group_names_across_schools <- function(WondeData, verbose = TRUE) {
  extract_field_across_schools(WondeData,
                               "students_group$groups.data__name",
                               value_name = "group_name",
                               verbose = verbose)
}

#' Extract unique group types across schools
#'
#' Wrapper for \code{\link{extract_field_across_schools}} that summarises
#' group types across all schools.
#'
#' @inheritParams extract_field_across_schools
#' @export
extract_group_types_across_schools <- function(WondeData, verbose = TRUE) {
  extract_field_across_schools(WondeData,
                               "students_group$groups.data__type",
                               value_name = "group_type",
                               verbose = verbose)
}
