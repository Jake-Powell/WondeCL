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
  WondeIds <- schools$id
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
  schools$year_code_values <- year_codes[match(WondeIds, names(WondeData))]

  # 2. YEAR DETECTION
  year_by_year <- sapply(WondeData, function(x) {
    cols <- names(x$student)
    if ("students.data__year.data.code" %in% cols) {
      val <- unique(x$student$students.data__year.data.code)
      any((val |> as.character()) %in% year_codes_target)
    } else if ("year.data.code" %in% cols) {
      any((x$student$year.data.code |> as.character()) %in% year_codes_target)
    } else {
      FALSE
    }
  })
  schools$HasTargetYear <- year_by_year[match(WondeIds, names(WondeData))]

  # 3. SUBJECTS FOR TARGET YEAR
  subject_year_codes <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[(x$student$year.data.code |> as.character()) %in% year_codes_target]
    ex_class_year <- x$students_classes[x$students_classes$id %in% ex_year_ids, ]
    yr_subjects <- x$subjects$name[x$subjects$id %in% unique(ex_class_year$classes.data__subject)]
    val <- paste0(unique(sort(yr_subjects)), collapse = "\n")
    if (val == "") "No subject coding available" else val
  })
  schools$target_year_subjects <- subject_year_codes[match(WondeIds, names(WondeData))]

  # 3. Group codes FOR TARGET YEAR
  group_year_codes <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[(x$student$year.data.code |> as.character()) %in% year_codes_target]
    ex_class_year <- x$students_group[x$students_group$id %in% ex_year_ids, ]
  yr_groups = ex_class_year$groups.data__name[ex_class_year$groups.data__type %in% 'REGISTRATION']
        val <- paste0(unique(sort(yr_groups)), collapse = "\n")
    if (val == "") "No GROUP coding available" else val
  })
  schools$target_year_registration_groups <- group_year_codes[match(WondeIds, names(WondeData))]
  
  
  # 4. SUBJECT DETECTION BY SUBJECT CODE
  subject_detected <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[(x$student$year.data.code  |> as.character()) %in% year_codes_target]
    ex_class_year <- x$students_classes[x$students_classes$id %in% ex_year_ids, ]
    yr_subjects <- x$subjects$name[x$subjects$id %in% unique(ex_class_year$classes.data__subject)]
    any(toupper(yr_subjects) %in% subject_keywords_upper)
  })
  schools$GetSubjectByCode <- subject_detected[match(WondeIds, names(WondeData))]

  # 5. CLASS DESCRIPTIONS
  class_desc_year <- sapply(WondeData, function(x) {
    ex_year_ids <- x$student$id[(x$student$year.data.code  |> as.character()) %in% year_codes_target]
    yr_class_desc <- x$students_classes$classes.data__description[x$students_classes$id %in% ex_year_ids]
    val <- paste0(unique(sort(yr_class_desc)), collapse = "\n")
    if (val == "") "No class description available" else val
  })
  schools$class_desc_target_year <- class_desc_year[match(WondeIds, names(WondeData))]
  schools$class_desc_target_year[as.logical(schools$GetSubjectByCode)] <- "-"

  # 6. SUBJECT DETECTION BY CLASS DESCRIPTION
  class_subject_detected <- rep("-", nrow(schools))
  class_subject_detected[schools$class_desc_target_year != "-"] <- FALSE
  class_subject_detected[grepl(paste0(subject_keywords_upper, collapse = "|"),
                               toupper(schools$class_desc_target_year))] <- TRUE
  schools$GetSubjectByClassDescription <- class_subject_detected

  # 7. STUDENT COUNTS
  total_students <- sapply(WondeData, function(x) length(unique(x$student$id)))
  schools$total_students <- total_students[match(WondeIds, names(WondeData))]
  total_target_year_students <- sapply(WondeData, function(x) {
    length(unique(x$student$id[(x$student$year.data.code  |> as.character()) %in% year_codes_target]))
  })
  schools$total_target_year_students <- total_target_year_students[match(WondeIds, names(WondeData))]

  schools$percent_school_target =  (schools$total_target_year_students/schools$total_students*100) |> round(1)
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


#' @title Summarise Small Classes by School
#'
#' @description
#' Groups a class list by school and summarises the occurrence of small classes,
#' defined as classes with a number of students less than or equal to `threshold`.
#' For each school, the function reports:
#' \itemize{
#'   \item The total number of classes in the school.
#'   \item The number of classes with ≤ `threshold` students.
#'   \item The total number of students in those small classes.
#'   \item The sizes of all small classes (comma-separated).
#'   \item The percentage of all students in the school who are in small classes.
#' }
#'
#' @param class_list A data frame containing at least the columns:
#'   `"School name"`, `"URN"`, `"Class Name"`, and `"Pupil First Name"`.
#' @param school_name_col Column name for school names. Default: `"School name"`.
#' @param urn_col Column name for school URNs. Default: `"URN"`.
#' @param class_col Column name for class names. Default: `"Class Name"`.
#' @param pupil_col Column name for pupil names (used only if no unique ID column is provided).
#'   Default: `"Pupil First Name"`.
#' @param unique_id_col (Optional) Column name uniquely identifying students (e.g., `"UPN"` or `"ID"`).
#'   If provided and present in `class_list`, counts of students per class will use distinct values
#'   in this column. If `NULL` (default), each row is assumed to represent one student.
#' @param threshold Numeric. The maximum class size considered "small". Default: `3`.
#'
#' @return A tibble summarising, for each school:
#' \describe{
#'   \item{School name}{School name.}
#'   \item{URN}{Unique reference number for the school.}
#'   \item{TotalClasses}{Number of classes in the school.}
#'   \item{SmallClasses}{Number of classes with ≤ `threshold` students.}
#'   \item{StudentsInSmallClasses}{Total students in those small classes.}
#'   \item{SmallClassSizes}{Comma-separated sizes of all small classes.}
#'   \item{TotalStudents}{Total number of students across all classes.}
#'   \item{PercentInSmallClasses}{Percentage of students in small classes.}
#' }
#'
#' @examples
#' \dontrun{
#' summarise_small_classes(class_list_df)
#' summarise_small_classes(class_list_df, unique_id_col = "UPN", threshold = 5)
#' }
#'
#' @export
summarise_small_classes <- function(class_list,
                                    school_name_col = "School name",
                                    urn_col = "URN",
                                    class_col = "Class Name",
                                    pupil_col = "Pupil First Name",
                                    unique_id_col = NULL,
                                    threshold = 3) {
  
  # ---- Check required columns ----
  required <- c(school_name_col, urn_col, class_col, pupil_col)
  missing_cols <- setdiff(required, names(class_list))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in class_list: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # ---- Summarise ----
  dplyr::as_tibble(class_list) |>
    # Count number of students per class (use unique ID if available)
    dplyr::group_by(.data[[school_name_col]],
                    .data[[urn_col]],
                    .data[[class_col]]) |>
    dplyr::summarise(
      StudentsInClass = if (!is.null(unique_id_col) && unique_id_col %in% names(class_list)) {
        dplyr::n_distinct(.data[[unique_id_col]])
      } else {
        dplyr::n()
      },
      .groups = "drop_last"
    ) |>
    # Summarise per school
    dplyr::summarise(
      TotalClasses = dplyr::n(),
      SmallClasses = sum(StudentsInClass <= threshold),
      StudentsInSmallClasses = sum(StudentsInClass[StudentsInClass <= threshold]),
      SmallClassSizes = paste0(StudentsInClass[StudentsInClass <= threshold], collapse = ", "),
      TotalStudents = sum(StudentsInClass),
      PercentInSmallClasses = ifelse(
        TotalStudents > 0,
        round(100 * StudentsInSmallClasses / TotalStudents, 2),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(PercentInSmallClasses))
}

#' @title Summarise Class Sizes and Percent of School Population
#'
#' @description
#' Groups a class list by school and class to calculate:
#' \itemize{
#'   \item The total number of students in each class (class size).
#'   \item The total number of students in the school.
#'   \item The percentage of the school's students that belong to each class.
#' }
#' Optionally counts unique students by ID (e.g. `"UPN"` or `"ID"`).
#'
#' @param class_list A data frame containing at least the columns:
#'   `"School name"`, `"URN"`, `"Class Name"`, and `"Pupil First Name"`.
#' @param school_name_col Column name for school names. Default: `"School name"`.
#' @param urn_col Column name for school URNs. Default: `"URN"`.
#' @param class_col Column name for class names. Default: `"Class Name"`.
#' @param pupil_col Column name for pupil names (used only if no unique ID column is provided).
#'   Default: `"Pupil First Name"`.
#' @param unique_id_col (Optional) Column name uniquely identifying students (e.g., `"UPN"` or `"ID"`).
#'   If provided and present in `class_list`, counts of students per class will use distinct values
#'   in this column. If `NULL` (default), each row is assumed to represent one student.
#'
#' @return A tibble with one row per class, containing:
#' \describe{
#'   \item{School name}{School name.}
#'   \item{URN}{Unique reference number for the school.}
#'   \item{Class Name}{Name of the class.}
#'   \item{StudentsInClass}{Number of students in the class.}
#'   \item{TotalStudentsInSchool}{Total number of students in the school.}
#'   \item{PercentOfSchool}{Percentage of the school's total students in this class.}
#' }
#'
#' @examples
#' \dontrun{
#' summarise_class_sizes(class_list_df)
#' summarise_class_sizes(class_list_df, unique_id_col = "UPN")
#' }
#'
#' @export
summarise_class_sizes <- function(class_list,
                                  school_name_col = "School name",
                                  urn_col = "URN",
                                  class_col = "Class Name",
                                  pupil_col = "Pupil First Name",
                                  unique_id_col = NULL) {
  
  # ---- Check required columns ----
  required <- c(school_name_col, urn_col, class_col, pupil_col)
  missing_cols <- setdiff(required, names(class_list))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in class_list: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # ---- Compute class sizes ----
  class_summary <- dplyr::as_tibble(class_list) |>
    dplyr::group_by(.data[[school_name_col]],
                    .data[[urn_col]],
                    .data[[class_col]]) |>
    dplyr::summarise(
      StudentsInClass = if (!is.null(unique_id_col) && unique_id_col %in% names(class_list)) {
        dplyr::n_distinct(.data[[unique_id_col]])
      } else {
        dplyr::n()
      },
      .groups = "drop"
    )
  
  # ---- Compute school totals and join ----
  school_totals <- class_summary |>
    dplyr::group_by(.data[[school_name_col]], .data[[urn_col]]) |>
    dplyr::summarise(
      TotalStudentsInSchool = sum(StudentsInClass),
      .groups = "drop"
    )
  
  dplyr::left_join(class_summary, school_totals,
                   by = c(school_name_col, urn_col)) |>
    dplyr::mutate(
      PercentOfSchool = ifelse(
        TotalStudentsInSchool > 0,
        round(100 * StudentsInClass / TotalStudentsInSchool, 2),
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(PercentOfSchool))
}

