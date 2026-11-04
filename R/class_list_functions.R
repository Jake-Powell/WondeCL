
#' Reduce Duplicated Student-Teacher Records to the Best Teacher
#'
#' @description
#' Filters a class list data frame to prioritise records associated with
#' the main teacher for each student. Where a student has one or more
#' records marked as the main teacher, only those records are retained.
#' Where no main teacher is identified, all existing teacher records
#' for that student are retained.
#'
#' The function supports the package's configurable column-name system,
#' automatically converting input column names to their internal
#' equivalents before processing and restoring the configured names
#' in the returned data frame.
#'
#' @param data A data frame containing student-teacher records.
#'   The following columns are required, using either their internal
#'   names or the currently configured column names:
#'   \describe{
#'     \item{est_wonde_id}{Wonde establishment identifier
#'       (default: `EST_WONDE_ID`).}
#'     \item{s_wonde_id}{Wonde student identifier
#'       (default: `STUDENT_WONDE_ID`).}
#'     \item{t_main}{Indicator of whether the teacher is the main
#'       teacher (default: `TEACHER_MAIN`).}
#'   }
#'   The main teacher indicator should contain logical values or
#'   values that can be converted to logical using `as.logical()`.
#'
#' @return
#' A data frame containing the retained student-teacher records,
#' with column names restored to the package's configured naming
#' convention.
#'
#' For each unique establishment-student combination:
#' \itemize{
#'   \item If one or more main-teacher records exist, all such records
#'         are retained and non-main-teacher records are removed.
#'   \item If no main-teacher record exists, all existing records
#'         for that student are retained.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts configured column names to the package's internal
#'         naming convention using `rename_to_internal_names()`.
#'   \item Checks that the required establishment, student and main
#'         teacher columns are present.
#'   \item Replaces missing (`NA`) main teacher indicators with `FALSE`.
#'   \item Identifies records where the teacher is marked as the main
#'         teacher.
#'   \item Identifies students using the combination of establishment
#'         ID and student ID, preventing records from different schools
#'         being treated as belonging to the same student.
#'   \item Retains main-teacher records where available, otherwise
#'         retaining the student's existing non-main-teacher records.
#'   \item Checks that all original establishment-student combinations
#'         remain represented, issuing a warning if any are lost.
#'   \item Converts the column names back to the configured naming
#'         convention using `rename_to_configured_names()`.
#' }
#'
#' The function operates at the student level rather than the class
#' level. Consequently, if a student has a main teacher in one class,
#' non-main-teacher records from their other classes may also be removed.
#'
#' The function does not guarantee a single record per student where
#' multiple main teachers exist or where no main teacher is identified.
#'
#' This function is typically used after joining student, class and
#' teacher data, and before producing summary class lists using
#' functions such as `produce_class_lists()`.
#'
#' @examples
#' \dontrun{
#' # Reduce student-teacher records using the default column names
#' reduced <- reduce_to_best_teacher(class_list_df)
#'
#' # Change the configured naming convention
#' set_column_names(
#'   s_wonde_id = "PUPIL_ID",
#'   t_main = "IS_MAIN_TEACHER"
#' )
#'
#' # The function automatically uses the updated column names
#' reduced <- reduce_to_best_teacher(class_list_df)
#'
#' # Restore the default configuration
#' reset_column_names()
#' }
#'
#' @family class_list
#'
#' @export
reduce_to_best_teacher <- function(data) {
  
  # Convert to internal column names
  data <- rename_to_internal_names(data)
  
  # -------------------------------------------------------------------
  # 1. Check required columns
  # -------------------------------------------------------------------
  required_cols <- c(
    "est_wonde_id",
    "s_wonde_id",
    "t_main"
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 2. Replace missing main teacher flags with FALSE
  # -------------------------------------------------------------------
  data$t_main[is.na(data$t_main)] <- FALSE
  
  # -------------------------------------------------------------------
  # 3. Identify students with a main teacher
  # -------------------------------------------------------------------
  main_index <- which(as.logical(data$t_main))
  
  to_keep <- data[main_index, , drop = FALSE]
  
  other_index <- setdiff(seq_len(nrow(data)), main_index)
  
  to_decide <- data[other_index, , drop = FALSE]
  
  # -------------------------------------------------------------------
  # 4. Remove non-main teacher records where a main teacher exists
  # -------------------------------------------------------------------
  student_key <- paste(
    data$est_wonde_id,
    data$s_wonde_id,
    sep = "\r"
  )
  
  main_student_keys <- unique(student_key[main_index])
  
  keep_other <- !student_key[other_index] %in% main_student_keys
  
  data_new <- rbind(
    to_keep,
    to_decide[keep_other, , drop = FALSE]
  )
  
  # -------------------------------------------------------------------
  # 5. Check unique student counts
  # -------------------------------------------------------------------
  original_students <- unique(student_key)
  
  remaining_students <- unique(
    paste(
      data_new$est_wonde_id,
      data_new$s_wonde_id,
      sep = "\r"
    )
  )
  
  if (!setequal(original_students, remaining_students)) {
    warning(
      "Reducing teacher has caused student loss!",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 6. Convert back to configured column names
  # -------------------------------------------------------------------
  rename_to_configured_names(data_new)
}


#' Clean Class Names
#'
#' Cleans class names, assigns default class information to students
#' without a class, and identifies classes containing identical students
#' within the same school.
#'
#' @param data A class list dataframe.
#' @param default_class_name Default name for missing or merged classes.
#'   Defaults to "Reception".
#'
#' @return A dataframe with additional configured columns
#'   CLASS_ID and CLASS_NAME.
#'
#' @export
clean_class_names <- function(data, default_class_name = "Reception") {
  
  # Convert to internal column names
  data <- rename_to_internal_names(data)
  
  # -------------------------------------------------------------------
  # 1. Extract class-related information
  # -------------------------------------------------------------------
  required_cols <- c(
    "est_wonde_id",
    "s_wonde_id",
    "c_wonde_id",
    "c_wonde_name"
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  classes <- data[, required_cols, drop = FALSE]
  
  # -------------------------------------------------------------------
  # 2. Fill missing class IDs and names
  # -------------------------------------------------------------------
  no_class_ID_index <- which(is.na(classes$c_wonde_id))
  
  classes$c_wonde_id[no_class_ID_index] <- paste0(
    classes$est_wonde_id[no_class_ID_index],
    "CLASS"
  )
  
  classes$c_wonde_name[no_class_ID_index] <- default_class_name
  
  # -------------------------------------------------------------------
  # 3. Identify classes with identical student memberships
  #    separately within each school
  # -------------------------------------------------------------------
  school_ids <- unique(classes$est_wonde_id)
  
  for (school_id in school_ids) {
    
    # Restrict to the current school
    school_index <- which(
      !is.na(classes$est_wonde_id) &
        classes$est_wonde_id == school_id
    )
    
    school_classes <- classes[school_index, , drop = FALSE]
    
    class_ids <- unique(school_classes$c_wonde_id)
    
    # Nothing to compare if there is only one class
    if (length(class_ids) < 2) next
    
    # Get the unique student membership of each class
    class_students <- lapply(class_ids, function(class_id) {
      
      unique(
        school_classes$s_wonde_id[
          school_classes$c_wonde_id == class_id
        ]
      )
    })
    
    names(class_students) <- class_ids
    
    # Check whether every class contains the same set of students
    identical_memberships <- all(
      vapply(
        class_students[-1],
        function(students) {
          setequal(students, class_students[[1]])
        },
        logical(1)
      )
    )
    
    if (identical_memberships) {
      classes$c_wonde_name[school_index] <- default_class_name
    }
  }
  
  # -------------------------------------------------------------------
  # 4. Add cleaned columns using internal names
  # -------------------------------------------------------------------
  data$class_id <- classes$c_wonde_id
  
  data$class_name <- classes$c_wonde_name |>
    stringr::str_remove("[A-Za-z ]: ") |>
    stringr::str_remove("[A-Za-z ] - ") |>
    stringr::str_to_title()
  
  # -------------------------------------------------------------------
  # 5. Convert back to configured column names
  # -------------------------------------------------------------------
  rename_to_configured_names(data)
}


#' Produce Consolidated Class Lists
#'
#' @description
#' Produces a consolidated class list by aggregating student and class
#' information and selecting one teacher record per student.
#'
#' The function supports the package's configurable column-name system,
#' converting input columns to internal names before processing and
#' restoring configured names in the output.
#'
#' @param data A data frame containing student, class and teacher
#'   information. Required columns include establishment identifiers,
#'   student identifiers, student names, date of birth, UPN, class
#'   name and teacher details.
#'
#' @param class_name_column Character string specifying the class-name
#'   column to use. Accepts an internal column key or its configured
#'   name. Defaults to `"class_name"`.
#'
#' @param default_teacher Character string used when the selected
#'   teacher's full name is missing. Defaults to `"Reception teacher"`.
#'
#' @param default_teacher_FN Character string used when the selected
#'   teacher's first name is missing. Defaults to `"Reception"`.
#'
#' @param default_teacher_SN Character string used when the selected
#'   teacher's surname is missing. Defaults to `"Teacher"`.
#'
#' @return
#' A data frame containing one row per unique establishment-student
#' combination, including:
#' \itemize{
#'   \item Establishment identifiers and school information.
#'   \item Consolidated class names.
#'   \item Student names, date of birth and UPN.
#'   \item Selected teacher's full name, first name and surname.
#' }
#'
#' Column names are returned using the package's configured naming
#' convention.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts input column names to the internal naming convention.
#'   \item Validates the required columns and selected class-name field.
#'   \item Groups records by establishment ID and Wonde student ID.
#'   \item Aggregates student information and all unique class names.
#'   \item Selects one teacher record per student, prioritising
#'     records marked as the main teacher. Where multiple records have
#'     equal priority, the first record is selected.
#'   \item Retains the selected teacher's full name, first name and
#'     surname together.
#'   \item Replaces missing teacher name fields with the specified defaults.
#'   \item Converts student names to title case and sorts the output.
#'   \item Restores the configured column names.
#' }
#'
#' Class information is aggregated independently of teacher selection.
#' Students assigned to multiple classes therefore retain all unique
#' class names, even though only one teacher is reported.
#'
#' Teacher selection is performed within each establishment using
#' the combination of `est_wonde_id` and `s_wonde_id`.
#'
#' This function is typically used after `clean_class_names()` and
#' `reduce_to_best_teacher()`.
#'
#' @examples
#' \dontrun{
#' class_list <- produce_class_lists(data)
#'
#' class_list <- produce_class_lists(
#'   data,
#'   class_name_column = "CLASS_WONDE_NAME"
#' )
#' }
#'
#' @importFrom rlang .data
#'
#' @family class_list
#' @export
produce_class_lists <- function(
    data,
    class_name_column = "class_name",
    default_teacher = "Reception teacher",
    default_teacher_FN = "Reception",
    default_teacher_SN = "Teacher") {
  
  # -------------------------------------------------------------------
  # 1. Convert to internal column names
  # -------------------------------------------------------------------
  data <- rename_to_internal_names(data)
  
  # Resolve class-name column
  cols <- get_column_names()
  
  if (class_name_column %in% names(cols)) {
    class_col <- class_name_column
  } else if (class_name_column %in% unname(cols)) {
    class_col <- names(cols)[match(class_name_column, unname(cols))]
  } else {
    stop(
      "Unknown class_name_column: ",
      class_name_column,
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 2. Validate required columns
  # -------------------------------------------------------------------
  required_cols <- c(
    "est_id",
    "est_wonde_id",
    "est_wonde_name",
    "est_wonde_urn",
    "s_wonde_id",
    "sfn",
    "sln",
    "dob",
    "upn",
    "t_full",
    "tfn",
    "tln",
    class_col
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 3. Helper functions
  # -------------------------------------------------------------------
  
  collapse_unique <- function(x) {
    
    x <- unique(as.character(x))
    x <- x[!is.na(x) & nzchar(trimws(x))]
    
    if (length(x) == 0) {
      return(NA_character_)
    }
    
    paste(x, collapse = ", ")
  }
  
  first_valid <- function(x) {
    
    valid <- which(
      !is.na(x) & nzchar(trimws(as.character(x)))
    )
    
    if (length(valid) == 0) {
      return(NA_character_)
    }
    
    as.character(x[valid[1]])
  }
  
  # -------------------------------------------------------------------
  # 4. Aggregate student and class information
  # -------------------------------------------------------------------
  student_info <- data |>
    dplyr::group_by(est_id, est_wonde_id, s_wonde_id) |>
    dplyr::summarise(
      est_wonde_name = first_valid(est_wonde_name),
      est_wonde_urn = first_valid(est_wonde_urn),
      
      class_name = collapse_unique(.data[[class_col]]),
      
      sfn = first_valid(sfn),
      sln = first_valid(sln),
      dob = first_valid(dob),
      upn = first_valid(upn),
      
      .groups = "drop"
    )
  
  # -------------------------------------------------------------------
  # 5. Select one teacher record per student
  # -------------------------------------------------------------------
  
  # Create main-teacher indicator
  if ("t_main" %in% names(data)) {
    main <- as.logical(data$t_main)
    main[is.na(main)] <- FALSE
  } else {
    main <- rep(FALSE, nrow(data))
  }
  
  # Prioritise main teachers; preserve input order for ties
  data$.teacher_priority <- !main
  data$.original_row <- seq_len(nrow(data))
  
  teacher_info <- data |>
    dplyr::arrange(.teacher_priority, .original_row) |>
    dplyr::group_by(est_wonde_id, s_wonde_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(
      est_wonde_id,
      s_wonde_id,
      t_full,
      tfn,
      tln
    )
  
  # -------------------------------------------------------------------
  # 6. Join the selected teacher to the aggregated student information
  # -------------------------------------------------------------------
  format <- student_info |>
    dplyr::left_join(
      teacher_info,
      by = c("est_wonde_id", "s_wonde_id")
    )
  
  # -------------------------------------------------------------------
  # 7. Replace missing teacher names
  # -------------------------------------------------------------------
  is_missing <- function(x) {
    is.na(x) | !nzchar(trimws(as.character(x)))
  }
  
  format$t_full[is_missing(format$t_full)] <- default_teacher
  format$tfn[is_missing(format$tfn)] <- default_teacher_FN
  format$tln[is_missing(format$tln)] <- default_teacher_SN
  
  # -------------------------------------------------------------------
  # 8. Format student names
  # -------------------------------------------------------------------
  format$sfn <- stringr::str_to_title(format$sfn)
  format$sln <- stringr::str_to_title(format$sln)
  
  # -------------------------------------------------------------------
  # 9. Sort final class list
  # -------------------------------------------------------------------
  format <- format |>
    dplyr::arrange(
      est_wonde_name,
      class_name,
      sln,
      sfn
    )
  
  # -------------------------------------------------------------------
  # 10. Select and order output columns
  # -------------------------------------------------------------------
  format <- format |>
    dplyr::select(
      est_id,
      est_wonde_name,
      est_wonde_urn,
      class_name,
      t_full,
      sfn,
      sln,
      dob,
      upn,
      tfn,
      tln
    )
  
  # -------------------------------------------------------------------
  # 11. Restore configured column names
  # -------------------------------------------------------------------
  rename_to_configured_names(format)
}




#' Add Spare Rows to Each Class List
#'
#' @description
#' Appends a specified number of spare (blank) rows to each unique
#' school, class and teacher combination in a class list.
#'
#' Spare rows retain available school, class and teacher information,
#' while student-specific fields are left blank.
#'
#' Column names are resolved using the package's configurable
#' column-name system, including the spare indicator.
#'
#' @param class_list A class-list data frame using configured or
#'   internal column names.
#' @param no_spares Integer. Number of spare rows to add for each
#'   unique class combination. Defaults to 3.
#' @param include_spare_column Logical. If TRUE, adds or updates
#'   the configured `spare` column, marking new spare rows with "Y".
#'   Defaults to FALSE.
#'
#' @return
#' The updated class list, retaining the original column names
#' and order, except that the spare indicator uses its configured
#' name when `include_spare_column = TRUE`.
#'
#' Existing spare indicators are preserved. Newly generated spare
#' rows are marked "Y" when `include_spare_column = TRUE`.
#'
#' @details
#' Schools are identified preferentially using `est_id`,
#' falling back to `urn` and then `est_wonde_name`.
#'
#' Classes are identified preferentially using `class_name`,
#' falling back to `c_wonde_name`.
#'
#' Spare rows retain all available school, class and teacher
#' identifiers. Other character fields are blank and other
#' data types are assigned NA.
#'
#' The spare column name is taken from
#' `get_column_names()[["spare"]]`, which defaults to "SPARE".
#'
#' @examples
#' \dontrun{
#' updated_list <- add_spares(
#'   class_list,
#'   no_spares = 3,
#'   include_spare_column = TRUE
#' )
#' }
#'
#' @export
add_spares <- function(
    class_list,
    no_spares = 3,
    include_spare_column = FALSE) {
  
  # ------------------------------------------------------------
  # 1. Validate arguments
  # ------------------------------------------------------------
  if (!is.numeric(no_spares) ||
      length(no_spares) != 1L ||
      is.na(no_spares) ||
      !is.finite(no_spares) ||
      no_spares < 0 ||
      no_spares != floor(no_spares)) {
    
    stop(
      "no_spares must be a single non-negative integer.",
      call. = FALSE
    )
  }
  
  if (!is.logical(include_spare_column) ||
      length(include_spare_column) != 1L ||
      is.na(include_spare_column)) {
    
    stop(
      "include_spare_column must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 2. Resolve column configuration
  # ------------------------------------------------------------
  spare_col <- get_column_names()[["spare"]]
  
  original_names <- names(class_list)
  
  class_list <- rename_to_internal_names(class_list)
  
  internal_names_at_start <- names(class_list)
  
  original_size <- nrow(class_list)
  
  # ------------------------------------------------------------
  # 3. Select school identifier
  # ------------------------------------------------------------
  if ("est_id" %in% names(class_list)) {
    
    school_col <- "est_id"
    
  } else if ("urn" %in% names(class_list)) {
    
    school_col <- "urn"
    
  } else if ("est_wonde_name" %in% names(class_list)) {
    
    school_col <- "est_wonde_name"
    
  } else {
    
    stop(
      "Missing school identifier. Requires est_id, ",
      "urn or est_wonde_name.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Select class identifier
  # ------------------------------------------------------------
  if ("class_name" %in% names(class_list)) {
    
    class_col <- "class_name"
    
  } else if ("c_wonde_name" %in% names(class_list)) {
    
    class_col <- "c_wonde_name"
    
  } else {
    
    stop(
      "Missing class identifier. Requires class_name ",
      "or c_wonde_name.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 5. Validate student name columns
  # ------------------------------------------------------------
  required_cols <- c("sfn", "sln")
  
  missing_cols <- setdiff(
    required_cols,
    names(class_list)
  )
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required student columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 6. Identify fields to retain in spare rows
  # ------------------------------------------------------------
  care <- intersect(
    c(
      "est_id",
      "est_wonde_id",
      "est_wonde_name",
      "est_wonde_urn",
      "urn",
      "class_id",
      "class_name",
      "c_wonde_name",
      "c_wonde_id",
      "t_full",
      "tfn",
      "tln",
      "t_wonde_id",
      "t_main"
    ),
    names(class_list)
  )
  
  # Keep the original grouping behaviour:
  # one set of spares per unique school/class/teacher combination.
  school_class <- unique(
    class_list[, care, drop = FALSE]
  )
  
  # ------------------------------------------------------------
  # 7. Generate spare rows
  # ------------------------------------------------------------
  if (nrow(school_class) > 0L && no_spares > 0L) {
    
    n_new <- nrow(school_class) * no_spares
    
    # Preserve existing column types
    spare_rows <- class_list[
      rep(NA_integer_, n_new),
      ,
      drop = FALSE
    ]
    
    # Blank student-specific character fields
    for (col in names(spare_rows)) {
      
      if (!col %in% care &&
          is.character(spare_rows[[col]])) {
        
        spare_rows[[col]] <- rep("", n_new)
      }
    }
    
    # Repeat school/class/teacher fields
    for (col in care) {
      
      spare_rows[[col]] <- rep(
        school_class[[col]],
        each = no_spares
      )
    }
    
    class_list <- rbind(
      class_list,
      spare_rows
    )
  }
  
  # ------------------------------------------------------------
  # 8. Add or update the configured spare indicator
  # ------------------------------------------------------------
  is_spare <- seq_len(nrow(class_list)) > original_size
  
  if (include_spare_column) {
    
    # Use the internal key after conversion.
    # The configured name is restored before returning.
    if (!"spare" %in% names(class_list)) {
      
      class_list$spare <- rep(
        "",
        nrow(class_list)
      )
    }
    
    # Preserve existing values and mark new spare rows
    class_list$spare[is_spare] <- "Y"
  }
  
  # ------------------------------------------------------------
  # 9. Sort by school, class and student name
  # ------------------------------------------------------------
  sort_last_name <- as.character(
    class_list$sln
  )
  
  sort_last_name[is_spare] <- "ZZZZZZZZZZZZZZZZZ"
  
  sort_first_name <- as.character(
    class_list$sfn
  )
  
  sort_first_name[is.na(sort_first_name)] <- ""
  
  order_index <- order(
    as.character(class_list[[school_col]]),
    as.character(class_list[[class_col]]),
    sort_last_name,
    sort_first_name,
    na.last = TRUE
  )
  
  class_list <- class_list[
    order_index,
    ,
    drop = FALSE
  ]
  
  rownames(class_list) <- NULL
  
  # ------------------------------------------------------------
  # 10. Restore original column names and order
  # ------------------------------------------------------------
  # Preserve the input names for all existing non-spare columns.
  # The spare indicator uses its configured name when requested.
  
  output_names <- original_names
  
  if (include_spare_column) {
    
    output_names[
      internal_names_at_start == "spare"
    ] <- spare_col
    
    if (!"spare" %in% internal_names_at_start) {
      output_names <- c(output_names, spare_col)
    }
  }
  
  # Columns remain in their original positions, with any new
  # spare indicator appended at the end.
  names(class_list) <- output_names
  
  class_list
}



#' Clean and Standardise Teacher Name Formatting
#'
#' @description
#' Converts teacher name fields in a class list to title case (e.g.,
#' "john SMITH" becomes "John Smith"). This ensures consistent formatting
#' across teacher names for display, reporting and downstream processing.
#'
#' The function supports the package's configurable column-name system,
#' automatically converting input column names to their internal
#' equivalents before processing and restoring the configured names
#' in the returned data frame.
#'
#' @param data A data frame containing class list information.
#'   The following columns are required, using either their internal
#'   names or the currently configured column names:
#'   \describe{
#'     \item{t_full}{Teacher's full name
#'       (default: `TEACHER_FULL_NAME`).}
#'     \item{tfn}{Teacher's first name
#'       (default: `TEACHER_FIRST_NAME`).}
#'     \item{tln}{Teacher's surname
#'       (default: `TEACHER_LAST_NAME`).}
#'   }
#'
#' @return
#' A data frame identical to the input `data`, except that the teacher
#' name columns are converted to title case. Column names are returned
#' using the package's configured naming convention.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts the input column names to their internal equivalents
#'         using `rename_to_internal_names()`.
#'   \item Checks that all required teacher name columns are present.
#'   \item Converts the teacher full name, first name and surname to title
#'         case using `stringr::str_to_title()`.
#'   \item Restores the configured column names using
#'         `rename_to_configured_names()`.
#' }
#'
#' Missing (`NA`) values are preserved. All other columns and their
#' values remain unchanged.
#'
#' This function is typically used after data extraction or merging,
#' or following `produce_class_lists()`, as a final formatting step
#' before export.
#'
#' @examples
#' \dontrun{
#' # Clean teacher names using the default configured column names
#' cleaned_df <- clean_teacher_names(class_list_df)
#'
#' # Inspect the cleaned teacher names
#' cleaned_df[, unname(get_column_names(
#'   c("t_full", "tfn", "tln")
#' ))]
#'
#' # The function also supports modified column names
#' set_column_names(t_full = "MAIN_TEACHER")
#' cleaned_df <- clean_teacher_names(class_list_df)
#'
#' # Restore defaults
#' reset_column_names()
#' }
#'
#' @family class_list
#'
#' @export
clean_teacher_names <- function(data) {
  
  # -------------------------------------------------------------------
  # 1. Convert to internal column names
  # -------------------------------------------------------------------
  data <- rename_to_internal_names(data)
  
  # -------------------------------------------------------------------
  # 2. Check required columns
  # -------------------------------------------------------------------
  teacher_columns <- c("t_full", "tfn", "tln")
  
  missing_cols <- setdiff(teacher_columns, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required teacher columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 3. Convert teacher names to title case
  # -------------------------------------------------------------------
  for (column in teacher_columns) {
    data[[column]] <- stringr::str_to_title(data[[column]])
  }
  
  # -------------------------------------------------------------------
  # 4. Restore configured column names
  # -------------------------------------------------------------------
  rename_to_configured_names(data)
}


#' Calculate class sizes from a class list
#'
#' @description
#' Summarises the number of students in each class by school, based on a
#' provided class list data frame. This function is typically used to check
#' class distribution and detect unusually large or small classes.
#'
#' @param class_list A data frame containing at least the following columns:
#'   \itemize{
#'     \item `School name` – the name of the school
#'     \item `Class Name` – the class name or identifier
#'     \item `Pupil First Name` – the first name of each student
#'   }
#'
#' @return
#' A tibble (grouped data frame) with three columns:
#' \describe{
#'   \item{`School name`}{The school name.}
#'   \item{`Class Name`}{The class name.}
#'   \item{`Students`}{The number of students in each class.}
#' }
#'
#' @details
#' The function groups rows by `School name` and `Class Name` and counts the
#' number of student entries based on the `Pupil First Name` column.  
#' It is a lightweight summary tool intended for quality checks on class list data.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' class_sizes <- get_class_sizes(class_list_df)
#' print(class_sizes)
#' }
#'
#' @export
get_class_sizes <- function(class_list){
  class_list |>
    dplyr::group_by(`School name`, `Class Name`) |>
    dplyr::summarise(Students = length(`Pupil First Name`))
}


