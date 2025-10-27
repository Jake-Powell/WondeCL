#' Filter Wonde raw data
#'
#' @description
#' Restricts raw Wonde data to only those students, teachers, and classes
#' that meet user-defined filtering criteria such as year group or subject.
#' Designed to generalise across both primary and secondary school datasets.
#'
#' @param WondeData A list of raw Wonde data, where each element represents
#'   a single school's extracted dataset from Wonde.
#' @param student_filter Optional function applied to the `education_details`
#'   element of each school list, returning the student IDs to keep.
#'   The function should return a character vector of IDs.
#' @param subject_filter Optional function applied to the `subjects` element
#'   of each school list, returning either a logical vector or IDs to keep.
#'   Use this to retain data for specific subjects, such as Mathematics.
#' @param year_fallback_column A character string naming the column in
#'   the `student` data that contains year or code information. Used to
#'   identify students when `education_details` IDs do not match. Defaults
#'   to `"year.data.code"`.
#' @param fallback_codes Optional character vector of values used for matching
#'   in `year_fallback_column` when no students are returned by
#'   `student_filter`. For example, `c("R", "Reception")`.
#' @param verbose Logical; if `TRUE`, displays progress messages while filtering.
#'
#' @return
#' A list of filtered Wonde data objects. Each element will contain only
#' the relevant students, teachers, and classes. The returned list also
#' includes an attribute named `"issue_list"`, which records any schools
#' where fallback rules were applied.
#'
#' @export
filter_WondeData <- function(
    WondeData,
    student_filter = NULL,
    subject_filter = NULL,
    year_fallback_column = "year.data.code",
    fallback_codes = NULL,
    verbose = TRUE
) {
  issue_list <- data.frame(
    Index = integer(),
    URN = character(),
    SchoolName = character(),
    Issue = character(),
    stringsAsFactors = FALSE
  )
  
  filtered_list <- pbapply::pblapply(seq_along(WondeData), function(i) {
    school_data <- WondeData[[i]]
    school_name <- tryCatch(school_data$student$school_name[1], error = function(e) NA)
    urn <- tryCatch(school_data$student$URN[1], error = function(e) NA)
    
    if (verbose && !is.na(school_name))
      message("Processing [", i, "] ", school_name)
    
    # ---- Student selection ----
    student_to_keep <- NULL
    try({
      if (!is.null(student_filter) && "education_details" %in% names(school_data))
        student_to_keep <- student_filter(school_data$education_details)
    }, silent = TRUE)
    
    # ---- Fallback for mismatched IDs ----
    if (is.null(student_to_keep) ||
        sum(school_data$student$id %in% student_to_keep) == 0) {
      issue_list[nrow(issue_list) + 1, ] <- list(
        i, urn, school_name,
        "education_details IDs did not match student IDs; fallback applied"
      )
      
      if (!is.null(fallback_codes) &&
          year_fallback_column %in% names(school_data$student)) {
        fallback_ids <- school_data$student$id[
          school_data$student[[year_fallback_column]] %in% fallback_codes
        ]
        student_to_keep <- unique(c(student_to_keep, fallback_ids))
      }
    }
    
    # ---- Subject filter ----
    subject_ids <- NULL
    if (!is.null(subject_filter) && "subjects" %in% names(school_data)) {
      keep_subjects <- tryCatch(subject_filter(school_data$subjects), error = function(e) NULL)
      if (is.logical(keep_subjects)) keep_subjects <- which(keep_subjects)
      subject_ids <- school_data$subjects$id[keep_subjects]
      school_data$subjects <- school_data$subjects[
        school_data$subjects$id %in% subject_ids, , drop = FALSE
      ]
    }
    
    # ---- Classes and teacher linkage ----
    if ("students_classes" %in% names(school_data)) {
      if (!is.null(subject_ids)) {
        school_data$students_classes <- school_data$students_classes[
          school_data$students_classes$classes.data__subject %in% subject_ids, , drop = FALSE
        ]
      }
      school_data$students_classes <- school_data$students_classes[
        school_data$students_classes$id %in% student_to_keep, , drop = FALSE
      ]
      teacher_to_keep <- unique(school_data$students_classes$classes.data__employees.data__id)
    } else {
      teacher_to_keep <- character()
    }
    
    # ---- Restrict all relevant data frames ----
    keep_ids <- unique(c(student_to_keep, teacher_to_keep))
    school_data <- lapply(names(school_data), function(name) {
      df <- school_data[[name]]
      # Skip subsetting for subjects (already filtered separately)
      if (name == "subjects" || !("id" %in% names(df))) return(df)
      df[df$id %in% keep_ids, , drop = FALSE]
    }) |> setNames(names(school_data))
    
    # ---- Return reduced dataset ----
    attr(school_data, "Index") <- i
    attr(school_data, "SchoolName") <- school_name
    attr(school_data, "URN") <- urn
    school_data
  })
  
  attr(filtered_list, "issue_list") <- issue_list
  filtered_list
}




#' Filter Primary Wonde Data by Year
#'
#' @description
#' Filters primary Wonde raw data to only include students, classes, and teachers
#' belonging to a selected National Curriculum year (for example Reception or Year 1).
#'
#' @param WondeData A list of raw Wonde data (one element per school).
#' @param years Character vector of year identifiers to filter for,
#'   for example `c("0", "R", "Reception")`.
#' @param fallback_codes Optional vector of fallback year codes to use when IDs do not align.
#'   If `NULL`, defaults to the same as `years`.
#' @param verbose Logical; whether to print progress messages.
#'
#' @return
#' A filtered list of Wonde data, with an `"issue_list"` attribute noting
#' schools where fallbacks were used.
#'
#' @examples
#' \dontrun{
#' reception_data <- filter_primary_WondeData(
#'   primary_data_raw,
#'   years = c("0", "R", "Reception")
#' )
#' }
#'
#' @export
filter_primary_WondeData <- function(WondeData,
                                     years = c("0", "R", "Reception"),
                                     fallback_codes = NULL,
                                     verbose = TRUE) {
  if (is.null(fallback_codes)) fallback_codes <- years
  
  filter_WondeData(
    WondeData = WondeData,
    student_filter = function(ed) {
      ed$id[ed$education_details.data.current_nc_year %in% years]
    },
    fallback_codes = fallback_codes,
    verbose = verbose
  )
}


#' Filter Secondary Wonde Data by Year and Subject
#'
#' @description
#' Filters secondary Wonde raw data to only include students and teachers
#' for selected years and (optionally) a given subject area.
#'
#' @param WondeData A list of raw Wonde data (one element per school).
#' @param years Character vector of year identifiers to filter for
#'   (for example `c("7","12")`).
#' @param subject_names Optional character vector of subject names to match
#'   (case-insensitive).
#' @param subject_desc Optional character vector of subject descriptions to match
#'   if names fail.
#' @param fallback_codes Optional fallback year codes for mismatched ID cases.
#'   If `NULL`, defaults to the same as `years`.
#' @param verbose Logical; whether to print progress messages.
#'
#' @return
#' A filtered list of Wonde data, with an `"issue_list"` attribute noting
#' schools where fallbacks were used.
#'
#' @examples
#' \dontrun{
#' maths_data <- filter_secondary_WondeData(
#'   secondary_data_raw,
#'   years = c("7", "12"),
#'   subject_names = c("Maths", "Mathematics", "Ma")
#' )
#' }
#'
#' @export
filter_secondary_WondeData <- function(WondeData,
                                       years = c("7", "12"),
                                       subject_names = NULL,
                                       subject_desc = NULL,
                                       fallback_codes = NULL,
                                       verbose = TRUE) {
  if (is.null(fallback_codes)) fallback_codes <- years
  
  # Create subject filter (case-insensitive matching)
  subject_filter <- NULL
  if (!is.null(subject_names)) {
    subject_filter <- function(sub_df) {
      subject_names_upper <- toupper(subject_names)
      match_name <- toupper(sub_df$name) |> stringr::str_trim()
      match_desc_col <- if ("description" %in% names(sub_df)) {
        toupper(sub_df$description)
      } else {
        rep("", nrow(sub_df))
      }
      
      matched <- match_name %in% subject_names_upper
      if (any(matched)) {
        matched
      } else if (!is.null(subject_desc)) {
        desc_upper <- toupper(subject_desc)
        stringr::str_detect(match_desc_col, paste(desc_upper, collapse = "|"))
      } else {
        rep(FALSE, nrow(sub_df))
      }
    }
  }
  
  filter_WondeData(
    WondeData = WondeData,
    student_filter = function(ed) {
      ed$id[ed$education_details.data.current_nc_year %in% years]
    },
    subject_filter = subject_filter,
    fallback_codes = fallback_codes,
    verbose = verbose
  )
}
