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
#' @param schools_id_column Character string specifying the column
#'   in the schools data frame containing the school identifiers
#'   used to associate records with individual schools.
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
    subject_keywords = c("Maths", "Mathematics", "Ma", "Mathematic", "Mathematics (Assessment+)"),
    schools_id_column = 'EstablishmentID'
) {
  get_target_year_ids <- function(x) {
    
    # First preference: year code from student extract
    if ("year.data.code" %in% names(x$student)) {
      
      ids <- x$student$id[
        as.character(x$student$year.data.code) %in% year_codes_target
      ]
      
      if (length(ids) > 0) {
        return(unique(ids))
      }
    }
    
    # Fallback: educational details
    if ("education_details.data.current_nc_year" %in% names(x$education_details)) {
      
      ids <- x$education_details$id[
        as.character(x$education_details$education_details.data.current_nc_year) %in%
          year_codes_target
      ]
      
      if (length(ids) > 0) {
        return(unique(ids))
      }
    }
    
    character(0)
  }
  
  WondeIds <- schools[[schools_id_column]]
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
  
  # 1. YEAR CODES (via educational details)
  year_codes <- sapply(WondeData, function(x) {
    if ("education_details.data.current_nc_year" %in% names(x$education_details)) {
      val <- unique(sort(x$education_details$education_details.data.current_nc_year))
      val <- paste0(val, collapse = "\n")
      if (val == "") "No year coding available (in ed.details)" else val
    } else {
      "year data code not available in educational details"
    }
  })
  schools$year_values_ed_details <- year_codes[match(WondeIds, names(WondeData))]
  
  # 2. YEAR DETECTION
  year_by_year <- sapply(WondeData, function(x) {
    cols <- names(x$education_details)
    if ("education_details.data.current_nc_year" %in% cols) {
      val <- unique(x$education_details$education_details.data.current_nc_year)
      any((val |> as.character()) %in% year_codes_target)
    } else if ("education_details.data.current_nc_year" %in% cols) {
      any((x$education_details$education_details.data.current_nc_year |> as.character()) %in% year_codes_target)
    } else {
      FALSE
    }
  })
  schools$HasTargetYear_ed_details <- year_by_year[match(WondeIds, names(WondeData))]
  
  

  # 3. SUBJECTS FOR TARGET YEAR
  subject_year_codes <- sapply(WondeData, function(x) {
    ex_year_ids <- get_target_year_ids(x)
    ex_class_year <- x$students_classes[x$students_classes$id %in% ex_year_ids, ]
    yr_subjects <- x$subjects$name[x$subjects$id %in% unique(ex_class_year$classes.data__subject)]
    val <- paste0(unique(sort(yr_subjects)), collapse = "\n")
    if (val == "") "No subject coding available" else val
  })
  schools$target_year_subjects <- subject_year_codes[match(WondeIds, names(WondeData))]

  # 3. Group codes FOR TARGET YEAR
  group_year_codes <- sapply(WondeData, function(x) {
    ex_year_ids <- get_target_year_ids(x)
    ex_class_year <- x$students_group[x$students_group$id %in% ex_year_ids, ]
  yr_groups = ex_class_year$groups.data__name[ex_class_year$groups.data__type %in% 'REGISTRATION']
        val <- paste0(unique(sort(yr_groups)), collapse = "\n")
    if (val == "") "No GROUP coding available" else val
  })
  schools$target_year_registration_groups <- group_year_codes[match(WondeIds, names(WondeData))]
  
  
  # 4. SUBJECT DETECTION BY SUBJECT CODE
  subject_detected <- sapply(WondeData, function(x) {
    ex_year_ids <- get_target_year_ids(x)
    ex_class_year <- x$students_classes[x$students_classes$id %in% ex_year_ids, ]
    yr_subjects <- x$subjects$name[x$subjects$id %in% unique(ex_class_year$classes.data__subject)]
    any(toupper(yr_subjects) %in% subject_keywords_upper)
  })
  schools$GetSubjectByCode <- subject_detected[match(WondeIds, names(WondeData))]

  # 5. CLASS DESCRIPTIONS
  class_desc_year <- sapply(WondeData, function(x) {
    ex_year_ids <- get_target_year_ids(x)
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
  schools <- merge(schools, have_parts, by.x = schools_id_column, by.y = "WondeId", all.x = TRUE)
  return(schools)
}




#' @title Summarise Small Classes by School
#'
#' @description
#' Summarises small classes within each school, where small classes
#' contain at most `threshold` students.
#'
#' School identification uses `est_id` preferentially, falling back
#' to `est_wonde_name` if `est_id` is unavailable.
#'
#' Class identification uses `class_name` preferentially, falling
#' back to `c_wonde_name` if `class_name` is unavailable.
#'
#' Column names are resolved using the package's configurable
#' column-name system.
#'
#' @param class_list A class-list data frame using configured or
#'   internal column names.
#' @param unique_id_col Optional student identifier column.
#'   Can be an internal key (e.g. `"ome_id"`), configured name
#'   (e.g. `"OME_ID"`), or another column present in the data.
#'   If NULL, each row is counted as one student.
#' @param threshold Numeric. Maximum class size considered small.
#'   Defaults to 3.
#'
#' @return A tibble containing the selected school identifier,
#'   using its configured column name, followed by:
#' \describe{
#'   \item{TotalClasses}{Number of classes in the school.}
#'   \item{SmallClasses}{Number of small classes.}
#'   \item{StudentsInSmallClasses}{Total students in small classes.}
#'   \item{SmallClassSizes}{Comma-separated small class sizes.}
#'   \item{TotalStudents}{Sum of student counts across classes.}
#'   \item{PercentInSmallClasses}{Percentage of student counts
#'     belonging to small classes.}
#' }
#'
#' @details
#' Column selection is based on availability, not missing values.
#' If `est_id` exists, it is used even if some values are NA.
#' The same applies to `class_name`.
#'
#' When a unique identifier is supplied, distinct non-missing
#' identifiers are counted within each class. Otherwise, each
#' row is treated as one student.
#'
#' @examples
#' \dontrun{
#' summarise_small_classes(class_list_df)
#'
#' summarise_small_classes(
#'   class_list_df,
#'   unique_id_col = "ome_id",
#'   threshold = 5
#' )
#' }
#'
#' @export
summarise_small_classes <- function(
    class_list,
    unique_id_col = NULL,
    threshold = 3) {
  
  # ------------------------------------------------------------
  # 1. Convert to internal column names
  # ------------------------------------------------------------
  class_list <- rename_to_internal_names(class_list)
  
  # ------------------------------------------------------------
  # 2. Select preferred school identifier
  # ------------------------------------------------------------
  if ("est_id" %in% names(class_list)) {
    
    school_col <- "est_id"
    
  } else if ("est_wonde_name" %in% names(class_list)) {
    
    school_col <- "est_wonde_name"
    
  } else {
    
    stop(
      "Missing school identifier. Requires either ",
      "est_id or est_wonde_name.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Select preferred class identifier
  # ------------------------------------------------------------
  if ("class_name" %in% names(class_list)) {
    
    class_col <- "class_name"
    
  } else if ("c_wonde_name" %in% names(class_list)) {
    
    class_col <- "c_wonde_name"
    
  } else {
    
    stop(
      "Missing class identifier. Requires either ",
      "class_name or c_wonde_name.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Validate threshold
  # ------------------------------------------------------------
  if (!is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      !is.finite(threshold) ||
      threshold < 0) {
    
    stop(
      "threshold must be a single non-negative number.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 5. Resolve optional unique student identifier
  # ------------------------------------------------------------
  unique_col <- NULL
  
  if (!is.null(unique_id_col)) {
    
    if (!is.character(unique_id_col) ||
        length(unique_id_col) != 1L ||
        is.na(unique_id_col) ||
        !nzchar(unique_id_col)) {
      
      stop(
        "unique_id_col must be NULL or a single column name.",
        call. = FALSE
      )
    }
    
    column_names <- get_column_names()
    
    if (unique_id_col %in% names(column_names)) {
      
      unique_col <- unique_id_col
      
    } else if (unique_id_col %in% unname(column_names)) {
      
      unique_col <- names(column_names)[
        match(unique_id_col, unname(column_names))
      ]
      
    } else {
      
      unique_col <- unique_id_col
    }
    
    # Preserve the previous fallback to row counts when
    # the requested identifier column is unavailable.
    if (!unique_col %in% names(class_list)) {
      unique_col <- NULL
    }
  }
  
  # ------------------------------------------------------------
  # 6. Count students within each class
  # ------------------------------------------------------------
  class_summary <- dplyr::as_tibble(class_list) |>
    dplyr::group_by(
      .data[[school_col]],
      .data[[class_col]]
    ) |>
    dplyr::summarise(
      StudentsInClass = if (!is.null(unique_col)) {
        dplyr::n_distinct(
          .data[[unique_col]],
          na.rm = TRUE
        )
      } else {
        dplyr::n()
      },
      .groups = "drop"
    )
  
  # ------------------------------------------------------------
  # 7. Summarise small classes by school
  # ------------------------------------------------------------
  result <- class_summary |>
    dplyr::group_by(
      .data[[school_col]]
    ) |>
    dplyr::summarise(
      TotalClasses = dplyr::n(),
      
      SmallClasses = sum(
        .data$StudentsInClass <= threshold
      ),
      
      StudentsInSmallClasses = sum(
        .data$StudentsInClass[
          .data$StudentsInClass <= threshold
        ]
      ),
      
      SmallClassSizes = paste0(
        .data$StudentsInClass[
          .data$StudentsInClass <= threshold
        ],
        collapse = ", "
      ),
      
      TotalStudents = sum(
        .data$StudentsInClass
      ),
      
      PercentInSmallClasses = ifelse(
        TotalStudents > 0,
        round(
          100 * StudentsInSmallClasses / TotalStudents,
          2
        ),
        NA_real_
      ),
      
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$PercentInSmallClasses)
    )
  
  # ------------------------------------------------------------
  # 8. Restore configured column names
  # ------------------------------------------------------------
  rename_to_configured_names(result)
}




#' @title Summarise Class Sizes and Percent of School Population
#'
#' @description
#' Groups a class list by one or more establishment and class identifiers
#' to calculate:
#' \itemize{
#'   \item The number of students in each class.
#'   \item The total number of students across classes in each school.
#'   \item The percentage of the school's students belonging to each class.
#' }
#'
#' Column names are resolved using the package's configurable
#' column-name system. Multiple establishment and class grouping
#' variables are supported.
#'
#' @param class_list A class-list data frame using configured or
#'   internal column names.
#' @param establishment_cols Character vector of establishment
#'   identifiers. Defaults to `c("est_id", "est_wonde_name")`.
#'   All available columns are used for grouping.
#' @param class_cols Character vector of class identifiers.
#'   Defaults to `c("class_name", "c_wonde_name")`.
#'   All available columns are used for grouping.
#' @param unique_id_col Optional student identifier column.
#'   Accepts an internal key, configured name or another column
#'   present in the data. If NULL, each row counts as one student.
#'   If specified but unavailable, row counts are used.
#'
#' @return A tibble containing the selected establishment and class
#'   identifiers, followed by:
#' \describe{
#'   \item{STUDENTS_IN_CLASS}{Number of students in the class.}
#'   \item{TOTAL_STUDENTS_IN_SCHOOL}{Sum of class-level student counts
#'     for the establishment.}
#'   \item{PERCENT_OF_SCHOOL}{Percentage of the establishment's
#'     student counts belonging to the class.}
#' }
#'
#' All returned columns use uppercase names with underscore separators.
#'
#' @details
#' Both internal and configured column names are supported in
#' `establishment_cols`, `class_cols` and `unique_id_col`.
#'
#' Missing grouping columns are skipped, provided at least one
#' establishment and one class identifier remain.
#'
#' If a unique identifier is supplied, distinct values are counted
#' within each class, including NA as one distinct value, consistent
#' with the original function.
#'
#' Students appearing in multiple classes contribute separately
#' to each class's count.
#'
#' @examples
#' \dontrun{
#' summarise_class_sizes(class_list_df)
#'
#' summarise_class_sizes(
#'   class_list_df,
#'   unique_id_col = "ome_id"
#' )
#'
#' summarise_class_sizes(
#'   class_list_df,
#'   establishment_cols = c("est_id", "est_wonde_name"),
#'   class_cols = c("class_name", "c_wonde_name")
#' )
#' }
#'
#' @export
summarise_class_sizes <- function(
    class_list,
    establishment_cols = c("est_id", "est_wonde_name"),
    class_cols = c("class_name", "c_wonde_name"),
    unique_id_col = NULL) {
  
  # ------------------------------------------------------------
  # 1. Convert class-list columns to internal names
  # ------------------------------------------------------------
  class_list <- rename_to_internal_names(class_list)
  
  column_names <- get_column_names()
  
  # ------------------------------------------------------------
  # 2. Validate grouping arguments
  # ------------------------------------------------------------
  if (!is.character(establishment_cols) ||
      length(establishment_cols) == 0L ||
      anyNA(establishment_cols) ||
      any(!nzchar(establishment_cols))) {
    
    stop(
      "establishment_cols must be a non-empty character vector.",
      call. = FALSE
    )
  }
  
  if (!is.character(class_cols) ||
      length(class_cols) == 0L ||
      anyNA(class_cols) ||
      any(!nzchar(class_cols))) {
    
    stop(
      "class_cols must be a non-empty character vector.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Resolve internal and configured column names
  # ------------------------------------------------------------
  resolve_cols <- function(cols) {
    
    resolved <- vapply(
      cols,
      function(col) {
        
        if (col %in% names(column_names)) {
          return(col)
        }
        
        if (col %in% unname(column_names)) {
          return(names(column_names)[
            match(col, unname(column_names))
          ])
        }
        
        col
      },
      character(1),
      USE.NAMES = FALSE
    )
    
    unique(resolved)
  }
  
  establishment_cols <- resolve_cols(establishment_cols)
  class_cols <- resolve_cols(class_cols)
  
  # ------------------------------------------------------------
  # 4. Retain available grouping columns
  # ------------------------------------------------------------
  establishment_cols <- establishment_cols[
    establishment_cols %in% names(class_list)
  ]
  
  class_cols <- class_cols[
    class_cols %in% names(class_list)
  ]
  
  if (length(establishment_cols) == 0L) {
    stop(
      "No establishment identifier columns found in class_list.",
      call. = FALSE
    )
  }
  
  if (length(class_cols) == 0L) {
    stop(
      "No class identifier columns found in class_list.",
      call. = FALSE
    )
  }
  
  if (length(intersect(establishment_cols, class_cols)) > 0L) {
    stop(
      "Establishment and class grouping columns must be distinct.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 5. Resolve optional unique student identifier
  # ------------------------------------------------------------
  unique_col <- NULL
  
  if (!is.null(unique_id_col)) {
    
    if (!is.character(unique_id_col) ||
        length(unique_id_col) != 1L ||
        is.na(unique_id_col) ||
        !nzchar(unique_id_col)) {
      
      stop(
        "unique_id_col must be NULL or a single column name.",
        call. = FALSE
      )
    }
    
    unique_col <- resolve_cols(unique_id_col)
    
    # Fall back to row counts if the identifier is unavailable.
    if (!unique_col %in% names(class_list)) {
      unique_col <- NULL
    }
  }
  
  # ------------------------------------------------------------
  # 6. Calculate students per class
  # ------------------------------------------------------------
  grouping_cols <- c(establishment_cols, class_cols)
  
  class_summary <- dplyr::as_tibble(class_list) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_cols))
    ) |>
    dplyr::summarise(
      STUDENTS_IN_CLASS = if (!is.null(unique_col)) {
        dplyr::n_distinct(.data[[unique_col]])
      } else {
        dplyr::n()
      },
      .groups = "drop"
    )
  
  # ------------------------------------------------------------
  # 7. Calculate school totals
  # ------------------------------------------------------------
  school_totals <- class_summary |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(establishment_cols))
    ) |>
    dplyr::summarise(
      TOTAL_STUDENTS_IN_SCHOOL = sum(
        .data$STUDENTS_IN_CLASS
      ),
      .groups = "drop"
    )
  
  # ------------------------------------------------------------
  # 8. Calculate percentage of school population
  # ------------------------------------------------------------
  result <- dplyr::left_join(
    class_summary,
    school_totals,
    by = establishment_cols
  ) |>
    dplyr::mutate(
      PERCENT_OF_SCHOOL = ifelse(
        .data$TOTAL_STUDENTS_IN_SCHOOL > 0,
        round(
          100 * .data$STUDENTS_IN_CLASS /
            .data$TOTAL_STUDENTS_IN_SCHOOL,
          2
        ),
        NA_real_
      )
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$PERCENT_OF_SCHOOL)
    )
  
  # ------------------------------------------------------------
  # 9. Restore configured column names
  # ------------------------------------------------------------
  result <- rename_to_configured_names(result)
  
  # ------------------------------------------------------------
  # 10. Standardise all returned column names
  # ------------------------------------------------------------
  names(result) <- toupper(
    gsub(
      "([a-z0-9])([A-Z])",
      "\\1_\\2",
      names(result)
    )
  )
  
  names(result) <- gsub(
    "[^A-Z0-9]+",
    "_",
    names(result)
  )
  
  names(result) <- gsub(
    "^_+|_+$",
    "",
    names(result)
  )
  
  result
}



