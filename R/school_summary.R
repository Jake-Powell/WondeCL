#' Summarise school-level data extracted from Wonde API
#'
#' Processes a list of Wonde school extracts and produces a summary table showing
#' year group availability, subject detection, student counts, and data part coverage
#' for each school. Designed to work with data returned by
#' \code{get_secondary_school_student_data()}.
#'
#' @param schools Data frame containing school metadata (must include at least
#'   \code{URN}, \code{id}, and \code{name} columns).
#' @param WondeData Named list of school data extracts (as returned from
#'   \code{get_secondary_school_student_data()}).
#' @param year_codes_target Character vector of year codes used to identify the
#'   target year group (default includes multiple Year 7 code variations).
#' @param subject_keywords Character vector of subject name patterns to identify
#'   relevant subjects (default identifies Maths-related names).
#'
#' @return
#' A data frame containing school-level summaries including:
#' \itemize{
#'   \item \code{year_code_values}: available year codes in the extract
#'   \item \code{HasTargetYear}: whether the target year group is present
#'   \item \code{target_year_subjects}: subjects available for the target year
#'   \item \code{GetSubjectByCode}: detection of target subject by subject code
#'   \item \code{class_desc_target_year}: available class descriptions for the target year
#'   \item \code{GetSubjectByClassDescription}: detection of subject by class description
#'   \item \code{total_students}: total number of students in the school
#'   \item \code{total_target_year_students}: total number of students in the target year group
#'   \item Logical indicators for which data parts were successfully extracted
#'     (e.g. \code{student}, \code{subjects}, \code{staff}, etc.)
#' }
#'
#' @details
#' The function checks the structure of each school's data extract, detects the
#' presence of the user-specified year group and subjects, and identifies whether
#' relevant Wonde endpoints were available. It is robust to missing or incomplete
#' data and merges all derived summaries back into the supplied \code{schools}
#' data frame.
#'
#' @examplesIf FALSE
#' summary_df <- summarise_school_data(
#'   schools = schools,
#'   WondeData = secondary_data_raw,
#'   year_codes_target = c("7", "Year 7"),
#'   subject_keywords = c("Maths", "Mathematics")
#' )
#'
#' head(summary_df)
#'
#' @export
summarise_school_data <- function(
    schools,
    WondeData,
    year_codes_target = c("7", "Year 7", "07", "Year 07", "Y7", "Yr7", "Yr 7"),
    subject_keywords = c("Maths", "Mathematics", "Ma", "Mathematic", "Mathematics (Assessment+)")
) {
  URNS <- schools$URN
  subject_keywords_upper <- toupper(subject_keywords)

  # 1. YEAR CODES
  year_codes <- sapply(WondeData, function(x) {
    if ("year.data.code" %in% names(x$student)) {
      val <- unique(sort(x$student$year.data.code))
      val <- paste0(val, collapse = "\n")
      if (val == "") "No year coding available" else val
    } else {
      "year data code not available in extract"
    }
  })
  schools$year_code_values <- year_codes[match(URNS, names(WondeData))]

  # 2. YEAR DETECTION
  year_by_year <- sapply(WondeData, function(x) {
    cols <- names(x$student)
    if ("students.data__year.data.code" %in% cols) {
      val <- unique(x$student$students.data__year.data.code)
      any(val %in% year_codes_target)
    } else if ("year.data.code" %in% cols) {
      any(x$student$year.data.code %in% year_codes_target)
    } else {
      FALSE
    }
  })
  schools$HasTargetYear <- year_by_year[match(URNS, names(WondeData))]

  # 3. SUBJECTS FOR TARGET YEAR
  subject_year_codes <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[x$student$year.data.name %in% year_codes_target]
    ex_class_year <- x$students_classes[x$students_classes$id %in% ex_year_ids, ]
    yr_subjects <- x$subjects$name[x$subjects$id %in% unique(ex_class_year$classes.data__subject)]
    val <- paste0(unique(sort(yr_subjects)), collapse = "\n")
    if (val == "") "No subject coding available" else val
  })
  schools$target_year_subjects <- subject_year_codes[match(URNS, names(WondeData))]

  # 4. SUBJECT DETECTION BY SUBJECT CODE
  subject_detected <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[x$student$year.data.name %in% year_codes_target]
    ex_class_year <- x$students_classes[x$students_classes$id %in% ex_year_ids, ]
    yr_subjects <- x$subjects$name[x$subjects$id %in% unique(ex_class_year$classes.data__subject)]
    any(toupper(yr_subjects) %in% subject_keywords_upper)
  })
  schools$GetSubjectByCode <- subject_detected[match(URNS, names(WondeData))]

  # 5. CLASS DESCRIPTIONS
  class_desc_year <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[x$student$year.data.name %in% year_codes_target]
    yr_class_desc <- x$students_classes$classes.data__description[x$students_classes$id %in% ex_year_ids]
    val <- paste0(unique(sort(yr_class_desc)), collapse = "\n")
    if (val == "") "No class description available" else val
  })
  schools$class_desc_target_year <- class_desc_year[match(URNS, names(WondeData))]
  schools$class_desc_target_year[as.logical(schools$GetSubjectByCode)] <- "-"

  # 6. SUBJECT DETECTION BY CLASS DESCRIPTION
  class_subject_detected <- rep("-", nrow(schools))
  class_subject_detected[schools$class_desc_target_year != "-"] <- FALSE
  class_subject_detected[grepl(paste0(subject_keywords_upper, collapse = "|"),
                               toupper(schools$class_desc_target_year))] <- TRUE
  schools$GetSubjectByClassDescription <- class_subject_detected

  # 7. STUDENT COUNTS
  total_students <- sapply(WondeData, function(x) length(unique(x$student$id)))
  schools$total_students <- total_students[match(URNS, names(WondeData))]
  total_target_year_students <- sapply(WondeData, function(x) {
    length(unique(x$student$id[x$student$year.data.name %in% year_codes_target]))
  })
  schools$total_target_year_students <- total_target_year_students[match(URNS, names(WondeData))]

  # 8. DATA PARTS AVAILABLE
  care <- c("student", "students_group", "students_classes", "subjects", "staff", "education_details")
  have_parts <- lapply(seq_along(WondeData), function(index) {
    x <- WondeData[[index]]
    c(WondeId = names(WondeData)[index],
      sapply(care, function(c) c %in% names(x)))
  })
  have_parts <- do.call(rbind, have_parts)
  have_parts <- as.data.frame(have_parts, stringsAsFactors = FALSE)
  have_parts[, -1] <- lapply(have_parts[, -1], as.logical)

  # 9. JOIN BACK TO SCHOOLS
  schools <- merge(schools, have_parts, by.x = "id", by.y = "WondeId", all.x = TRUE)
  return(schools)
}
