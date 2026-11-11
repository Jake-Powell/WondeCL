# Class-list editing functions using the package's configured naming convention.
# Requires existing internal package functions:
#   get_column_names(), rename_to_internal_names(), rename_to_configured_names().
# The helpers beginning with a dot are internal (not exported).


# Resolve exactly one named school identifier in ... .
# Its name can be an internal key or a configured external column name.
.resolve_school_selector <- function(..., data) {
  selectors <- list(...)
  keys <- names(selectors)
  if (length(selectors) != 1L || is.null(keys) || is.na(keys[1L]) ||
      !nzchar(keys[1L])) {
    stop("Supply exactly one named school identifier, e.g. URN = 111 or EST_ID = '001'.",
         call. = FALSE)
  }
  cols <- get_column_names()
  key <- keys[1L]
  if (key %in% names(cols)) {
    internal <- key
  } else if (key %in% unname(cols)) {
    internal <- names(cols)[match(key, unname(cols))]
  } else {
    stop("Unknown school identifier '", key, "'. Use an internal lookup key or configured column name.",
         call. = FALSE)
  }
  if (!internal %in% c("urn", "est_id", "est_wonde_id", "est_wonde_urn", "est_wonde_estno")) {
    stop("'", key, "' is configured, but is not a supported school identifier.",
         call. = FALSE)
  }
  if (!internal %in% names(data)) {
    stop("School identifier '", key, "' resolves to '", internal,
         "', which is not in the class list.", call. = FALSE)
  }
  value <- selectors[[1L]]
  if (length(value) == 0L || anyNA(value)) {
    stop("School identifier value must not be empty or contain NA.", call. = FALSE)
  }
  list(school_col = internal, school_id = value)
}

.prepare_class_edit <- function(data, selectors, required = character()) {
  data <- rename_to_internal_names(data)
  # do.call passes selector names exactly as supplied to ...
  selected <- do.call(.resolve_school_selector, c(selectors, list(data = data)))
  missing <- setdiff(unique(c(selected$school_col, required)), names(data))
  if (length(missing)) {
    stop("Missing required class-list columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  list(data = data, school_col = selected$school_col,
       school_id = selected$school_id)
}

.school_rows <- function(data, school_col, school_id) {
  which(!is.na(data[[school_col]]) &
          as.character(data[[school_col]]) %in% as.character(school_id))
}

#' Add a student to an existing school class
#'
#' @param data A class-list data frame using configured or internal column names.
#' @param first_name,last_name Name of new student.
#' @param ... Exactly one named school identifier, using an internal lookup key
#'   or configured external name, e.g. `URN = 123456`, `EST_ID = '001'`,
#'   or `est_wonde_id = 'A123'`.
#' @param class_name Existing class name to copy class and teacher information from.
#' @param dob Date of birth.
#' @param upn UPN, if known.
#' @param verbose Print informative messages.
#' @return Updated data frame using the configured column names.
#' @family class_list
#' @export
add_student_to_school_class <- function(data, first_name, last_name, class_name,
                                        dob = NA, upn = NA, ..., verbose = TRUE) {
  x <- .prepare_class_edit(data, list(...),
                           c("sfn", "sln", "c_wonde_name", "dob"))
  data <- x$data
  index <- .school_rows(data, x$school_col, x$school_id)
  index <- index[!is.na(data$c_wonde_name[index]) &
                   data$c_wonde_name[index] %in% class_name]
  if (!length(index)) {
    stop("No matching school/class found for school identifier ",
         paste(x$school_id, collapse = ", "), ".", call. = FALSE)
  }
  new_row <- data[index[1L], , drop = FALSE]
  new_row$sfn <- first_name
  new_row$sln <- last_name
  new_row$dob <- dob
  if ("upn" %in% names(data)) new_row$upn <- upn
  # Do not duplicate the existing student's Wonde ID when copying a row.
  if ("s_wonde_id" %in% names(data)) new_row$s_wonde_id <- NA
  after <- max(index)
  trailing <- if (after < nrow(data)) {
    data[seq.int(after + 1L, nrow(data)), , drop = FALSE]
  } else {
    data[0, , drop = FALSE]
  }
  data <- rbind(data[seq_len(after), , drop = FALSE], new_row, trailing)
  rownames(data) <- NULL
  if (isTRUE(verbose)) message("Added ", first_name, " ", last_name,
                               " to class ", class_name, ".")
  rename_to_configured_names(data)
}

#' Update a student's class and associated teacher details
#' @inheritParams add_student_to_school_class
#' @param class_name Current class name; NA to match across all classes in the school.
#' @param new_class_name Destination class name in the same school.
#' @family class_list
#' @export

update_student_class <- function(data, first_name, last_name, new_class_name,
                                 class_name = NA, ..., verbose = FALSE) {
  
  x <- .prepare_class_edit(
    data,
    list(...),
    c("sfn", "sln")
  )
  
  data <- x$data
  
  # Prefer the cleaned class name when available.
  class_col <- if ("class_name" %in% names(data)) {
    "class_name"
  } else if ("c_wonde_name" %in% names(data)) {
    "c_wonde_name"
  } else {
    stop(
      "Missing required class name column: class_name or c_wonde_name.",
      call. = FALSE
    )
  }
  
  school_rows <- .school_rows(
    data,
    x$school_col,
    x$school_id
  )
  
  # ------------------------------------------------------------
  # 1. Identify the student within the selected school
  # ------------------------------------------------------------
  
  target <- school_rows[
    !is.na(data$sfn[school_rows]) &
      !is.na(data$sln[school_rows]) &
      data$sfn[school_rows] == first_name &
      data$sln[school_rows] == last_name
  ]
  
  # Optionally restrict to the student's current class.
  if (!all(is.na(class_name))) {
    
    target <- target[
      !is.na(data[[class_col]][target]) &
        data[[class_col]][target] %in% class_name
    ]
  }
  
  # ------------------------------------------------------------
  # 2. Identify destination class within the same school
  # ------------------------------------------------------------
  
  destination <- school_rows[
    !is.na(data[[class_col]][school_rows]) &
      data[[class_col]][school_rows] == new_class_name
  ]
  
  if (!length(destination)) {
    stop(
      "Destination class not found within the selected school.",
      call. = FALSE
    )
  }
  
  if (!length(target)) {
    warning(
      "No matching student/class rows found.",
      call. = FALSE
    )
    
    return(rename_to_configured_names(data))
  }
  
  # ------------------------------------------------------------
  # 3. Select destination teacher information
  # ------------------------------------------------------------
  
  teacher_cols <- intersect(
    c(
      "t_wonde_id",
      "t_wonde_ID",
      "tfn",
      "tln",
      "t_main",
      "t_full"
    ),
    names(data)
  )
  
  if (length(teacher_cols) &&
      nrow(unique(
        data[destination, teacher_cols, drop = FALSE]
      )) > 1L) {
    
    warning(
      "Multiple teacher records in destination class; using first.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Copy destination class information
  # ------------------------------------------------------------
  
  # Copy both cleaned and original Wonde class names,
  # where available, from the destination class.
  class_cols <- intersect(
    c(
      "class_name",
      "c_wonde_name",
      "c_wonde_id",
      "class_id"
    ),
    names(data)
  )
  
  columns_to_copy <- c(
    class_cols,
    teacher_cols
  )
  
  for (col in columns_to_copy) {
    data[[col]][target] <- data[[col]][destination[1L]]
  }
  
  # ------------------------------------------------------------
  # 5. Return configured data
  # ------------------------------------------------------------
  
  if (isTRUE(verbose)) {
    message(
      "Updated class for ",
      length(target),
      " row(s)."
    )
  }
  
  rename_to_configured_names(data)
}


#' Update the name of a class within a school
#' @inheritParams add_student_to_school_class
#' @param new_name Replacement name.
#' @param class_name Old class name; NA changes all class names in the school.
#' @family class_list
#' @export

update_class_name <- function(data, class_name, new_name, ...,
                              verbose = FALSE) {
  
  x <- .prepare_class_edit(
    data,
    list(...),
    character(0)
  )
  
  data <- x$data
  
  # Prefer the cleaned class name.
  class_col <- if ("class_name" %in% names(data)) {
    "class_name"
  } else if ("c_wonde_name" %in% names(data)) {
    "c_wonde_name"
  } else {
    stop(
      "Missing required class name column: class_name or c_wonde_name.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 1. Identify selected school
  # ------------------------------------------------------------
  
  index <- .school_rows(
    data,
    x$school_col,
    x$school_id
  )
  
  # ------------------------------------------------------------
  # 2. Restrict to requested class
  # ------------------------------------------------------------
  
  if (!all(is.na(class_name))) {
    
    index <- index[
      !is.na(data[[class_col]][index]) &
        data[[class_col]][index] %in% class_name
    ]
  }
  
  # ------------------------------------------------------------
  # 3. Update class name
  # ------------------------------------------------------------
  
  data[[class_col]][index] <- new_name
  
  # ------------------------------------------------------------
  # 4. Return configured data
  # ------------------------------------------------------------
  
  if (isTRUE(verbose)) {
    message(
      "Updated class name for ",
      length(index),
      " row(s)."
    )
  }
  
  rename_to_configured_names(data)
}


#' Update the name of one or more schools
#' @inheritParams add_student_to_school_class
#' @param new_name Replacement school name(s); one per selected school identifier.
#' @family class_list
#' @export
update_school_name <- function(data, new_name, ...) {
  x <- .prepare_class_edit(data, list(...), "est_wonde_name")
  data <- x$data
  if (length(x$school_id) != length(new_name)) {
    stop("School identifier values and 'new_name' must have equal lengths.", call. = FALSE)
  }
  for (i in seq_along(x$school_id)) {
    index <- .school_rows(data, x$school_col, x$school_id[i])
    data$est_wonde_name[index] <- new_name[i]
  }
  rename_to_configured_names(data)
}


#' Update or Append a Class Teacher
#'
#' @description
#' Updates or appends a teacher to a specified class within a school.
#'
#' Class identification preferentially uses `class_name`, falling
#' back to `c_wonde_name` when `class_name` is unavailable.
#'
#' @inheritParams add_student_to_school_class
#' @param class_name Class or classes to edit. NA edits all classes
#'   within the selected school.
#' @param first_name Teacher first name.
#' @param last_name Teacher last name.
#' @param new_name Teacher full name. Defaults to the concatenation
#'   of `first_name` and `last_name`.
#' @param overwrite If TRUE, update existing teacher information.
#'   If FALSE, append copies of matching rows with the new teacher.
#' @param verbose Logical. If TRUE, reports the number of rows
#'   updated or appended.
#'
#' @return The updated class list with configured column names.
#'
#' @family class_list
#' @export
update_class_teacher <- function(
    data,
    class_name,
    first_name,
    last_name,
    new_name = paste(first_name, last_name),
    ...,
    overwrite = TRUE,
    verbose = FALSE) {
  
  # ------------------------------------------------------------
  # 1. Convert to internal column names
  # ------------------------------------------------------------
  data <- rename_to_internal_names(data)
  
  # ------------------------------------------------------------
  # 2. Select class identifier
  # ------------------------------------------------------------
  if ("class_name" %in% names(data)) {
    
    class_col <- "class_name"
    
  } else if ("c_wonde_name" %in% names(data)) {
    
    class_col <- "c_wonde_name"
    
  } else {
    
    stop(
      "Missing class identifier: requires ",
      "CLASS_NAME or CLASS_WONDE_NAME.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Resolve school identifier
  # ------------------------------------------------------------
  x <- .prepare_class_edit(
    data,
    list(...),
    class_col
  )
  
  data <- x$data
  
  # ------------------------------------------------------------
  # 4. Identify rows in selected school
  # ------------------------------------------------------------
  index <- .school_rows(
    data,
    x$school_col,
    x$school_id
  )
  
  # ------------------------------------------------------------
  # 5. Restrict to selected class(es), unless NA
  # ------------------------------------------------------------
  if (!all(is.na(class_name))) {
    
    index <- index[
      !is.na(data[[class_col]][index]) &
        data[[class_col]][index] %in% class_name
    ]
  }
  
  # ------------------------------------------------------------
  # 6. Handle no matching rows
  # ------------------------------------------------------------
  if (!length(index)) {
    
    warning(
      "No matching rows found.",
      call. = FALSE
    )
    
    return(
      rename_to_configured_names(data)
    )
  }
  
  # ------------------------------------------------------------
  # 7. Update or append teacher rows
  # ------------------------------------------------------------
  if (overwrite) {
    
    dest <- index
    
  } else {
    
    new_rows <- data[index, , drop = FALSE]
    
    data <- rbind(
      data,
      new_rows
    )
    
    dest <- seq.int(
      nrow(data) - length(index) + 1L,
      nrow(data)
    )
  }
  
  # ------------------------------------------------------------
  # 8. Update teacher information
  # ------------------------------------------------------------
  if ("t_full" %in% names(data)) {
    data$t_full[dest] <- new_name
  }
  
  if ("tfn" %in% names(data)) {
    data$tfn[dest] <- first_name
  }
  
  if ("tln" %in% names(data)) {
    data$tln[dest] <- last_name
  }
  
  # The previous teacher's Wonde ID should not be retained.
  for (col in intersect(
    c("t_wonde_id", "t_wonde_ID"),
    names(data)
  )) {
    data[[col]][dest] <- NA
  }
  
  # Clear the existing main teacher flag.
  if ("t_main" %in% names(data)) {
    data$t_main[dest] <- NA
  }
  
  # ------------------------------------------------------------
  # 9. Progress message
  # ------------------------------------------------------------
  if (isTRUE(verbose)) {
    
    message(
      if (overwrite) "Updated " else "Added ",
      length(dest),
      " teacher row(s)."
    )
  }
  
  # ------------------------------------------------------------
  # 10. Restore configured column names
  # ------------------------------------------------------------
  rename_to_configured_names(data)
}


#' Remove one or more classes from a school
#' @inheritParams add_student_to_school_class
#' @param class_name Class name(s) to remove.
#' @family class_list
#' @export
remove_class <- function(data, class_name, ..., verbose = FALSE) {
  
  # Convert column names to internal names
  data <- rename_to_internal_names(data)
  
  # Prefer CLASS_NAME, falling back to CLASS_WONDE_NAME
  if ("class_name" %in% names(data)) {
    class_col <- "class_name"
  } else if ("c_wonde_name" %in% names(data)) {
    class_col <- "c_wonde_name"
  } else {
    stop(
      "Missing class identifier: requires CLASS_NAME or CLASS_WONDE_NAME.",
      call. = FALSE
    )
  }
  
  # Resolve school identifier
  x <- .prepare_class_edit(data, list(...), class_col)
  data <- x$data
  
  # Identify rows belonging to the selected school
  index <- .school_rows(data, x$school_col, x$school_id)
  
  # Restrict to the specified class
  index <- index[
    !is.na(data[[class_col]][index]) &
      data[[class_col]][index] %in% class_name
  ]
  
  # Remove matching rows
  if (length(index) > 0L) {
    data <- data[-index, , drop = FALSE]
  }
  
  if (isTRUE(verbose)) {
    message("Removed ", length(index), " class-list row(s).")
  }
  
  rename_to_configured_names(data)
}
#' Remove specified student(s) from specified class(es)
#' @inheritParams add_student_to_school_class
#' @param student_id Vector of Wonde student IDs.
#' @param class_name Class name(s) from which to remove students.
#' @family class_list
#' @export
remove_student_from_class <- function(data, student_id, class_name, ...,
                                      verbose = FALSE) {
  x <- .prepare_class_edit(data, list(...),
                           c("c_wonde_name", "s_wonde_id"))
  data <- x$data
  index <- .school_rows(data, x$school_col, x$school_id)
  index <- index[!is.na(data$c_wonde_name[index]) &
                   data$c_wonde_name[index] %in% class_name &
                   !is.na(data$s_wonde_id[index]) &
                   as.character(data$s_wonde_id[index]) %in% as.character(student_id)]
  data <- data[-index, , drop = FALSE]
  if (isTRUE(verbose)) message("Removed ", length(index), " student/class row(s).")
  rename_to_configured_names(data)
}



#' @title Manually Withdraw a Student
#'
#' @description
#' Marks a student as withdrawn by setting their withdrawal status to
#' `"Y"`. The student is identified using a school identifier supplied
#' through `...`, together with their first name and surname.
#'
#' Column names are resolved using the package's configurable
#' column-name system.
#'
#' @param data A class-list data frame using configured or internal
#'   column names.
#' @param first_name Student's first name (exact, case-sensitive).
#' @param last_name Student's surname (exact, case-sensitive).
#' @param ... A named school identifier, such as `URN`, `EST_ID`,
#'   or `EST_WONDE_ID`, using the configured column names.
#' @param verbose Logical. If TRUE, displays a message confirming
#'   the withdrawal.
#'
#' @return
#' The updated data frame with the matching student marked as
#' withdrawn. Existing withdrawal statuses are preserved.
#'
#' If no student or multiple students match, a warning is issued
#' and no changes are made.
#'
#' @details
#' School selection uses `.prepare_class_edit()` and `.school_rows()`,
#' consistent with the other manual class-list editing functions.
#'
#' Student names are matched exactly and are case-sensitive.
#' No Wonde student identifier is required.
#'
#' @family withdraw
#' @export
withdraw_student_from_school_class <- function(
    data,
    first_name,
    last_name,
    ...,
    verbose = FALSE) {
  
  # ------------------------------------------------------------
  # 1. Prepare data and resolve school identifier
  # ------------------------------------------------------------
  x <- .prepare_class_edit(data, list(...), "sfn")
  
  data <- x$data
  
  # ------------------------------------------------------------
  # 2. Identify rows belonging to the specified school
  # ------------------------------------------------------------
  index <- .school_rows(
    data,
    x$school_col,
    x$school_id
  )
  
  # ------------------------------------------------------------
  # 3. Validate student name columns
  # ------------------------------------------------------------
  required_cols <- c("sfn", "sln")
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(
        get_column_names()[missing_cols],
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Match student within selected school
  # ------------------------------------------------------------
  index <- index[
    !is.na(data$sfn[index]) &
      !is.na(data$sln[index]) &
      data$sfn[index] == first_name &
      data$sln[index] == last_name
  ]
  
  if (length(index) == 0L) {
    warning(
      "No student found with the specified details. No changes made.",
      call. = FALSE
    )
    
    return(rename_to_configured_names(data))
  }
  
  if (length(index) > 1L) {
    warning(
      "Multiple matching students found. No changes made.",
      call. = FALSE
    )
    
    return(rename_to_configured_names(data))
  }
  
  # ------------------------------------------------------------
  # 5. Ensure withdrawal column exists
  # ------------------------------------------------------------
  if (!"withdrawn" %in% names(data)) {
    data$withdrawn <- rep("", nrow(data))
  }
  
  # ------------------------------------------------------------
  # 6. Mark student as withdrawn
  # ------------------------------------------------------------
  data$withdrawn[index] <- "Y"
  
  if (isTRUE(verbose)) {
    cli::cli_inform(
      paste0(
        "Student ",
        data$sfn[index], " ",
        data$sln[index],
        " marked as withdrawn."
      )
    )
  }
  
  # ------------------------------------------------------------
  # 7. Return configured column names
  # ------------------------------------------------------------
  rename_to_configured_names(data)
}



#' Remove Schools from a Class List
#'
#' @description
#' Removes all records belonging to one or more specified schools,
#' identified by school name, URN, or both.
#'
#' Supports the package's configurable column-name system. School
#' names are identified preferentially using `est_name`, falling
#' back to `est_wonde_name` when unavailable.
#'
#' When both school names and URNs are supplied, rows matching
#' either criterion are removed.
#'
#' @param data A class-list data frame using configured or internal
#'   column names.
#' @param school_names Character vector of school names to remove.
#'   Optional when `URNs` is supplied.
#' @param URNs Character or numeric vector of URNs to remove.
#'   Optional when `school_names` is supplied.
#' @param SchoolNameColumn Optional character string specifying the
#'   school-name column. Accepts an internal key, configured name,
#'   or literal column name. Defaults to NULL, which preferentially
#'   selects `est_name`, falling back to `est_wonde_name`.
#' @param URNColumn Character string identifying the URN column.
#'   Accepts an internal key, configured name, or literal column
#'   name. Defaults to `"urn"`.
#' @param verbose Logical. If TRUE, reports the number of rows and
#'   schools removed.
#'
#' @return
#' A data frame containing the remaining class-list records, with
#' column names restored to the configured naming convention.
#'
#' @details
#' Both school names and URNs are matched exactly. URNs are
#' converted to character before matching so numeric and character
#' identifiers can be compared.
#'
#' Missing values are not used as removal criteria.
#'
#' @examples
#' \dontrun{
#' # Remove by school name
#' updated <- remove_school_from_class_list(
#'   class_list,
#'   school_names = "Greenfield Primary School"
#' )
#'
#' # Remove by URN
#' updated <- remove_school_from_class_list(
#'   class_list,
#'   URNs = c(123456, 987654)
#' )
#'
#' # Remove by either school name or URN
#' updated <- remove_school_from_class_list(
#'   class_list,
#'   school_names = "Riverside Primary",
#'   URNs = 123456
#' )
#' }
#'
#' @family class_list
#' @export
remove_school_from_class_list <- function(
    data,
    school_names = NULL,
    URNs = NULL,
    SchoolNameColumn = NULL,
    URNColumn = "urn",
    verbose = FALSE) {
  
  # ------------------------------------------------------------
  # 1. Validate inputs
  # ------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  
  if (is.null(school_names) && is.null(URNs)) {
    stop(
      "At least one of 'school_names' or 'URNs' must be provided.",
      call. = FALSE
    )
  }
  
  if (!is.null(school_names) && !is.character(school_names)) {
    stop(
      "school_names must be a character vector.",
      call. = FALSE
    )
  }
  
  if (!is.null(URNs) &&
      !(is.character(URNs) || is.numeric(URNs))) {
    stop(
      "URNs must be a character or numeric vector.",
      call. = FALSE
    )
  }
  
  if (!is.logical(verbose) || length(verbose) != 1L ||
      is.na(verbose)) {
    stop(
      "verbose must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 2. Convert to internal column names
  # ------------------------------------------------------------
  original_names <- names(data)
  
  data <- rename_to_internal_names(data)
  
  cols <- get_column_names()
  
  # Resolve an internal key, configured column name, or literal
  # data-frame column name to the current internal name.
  resolve_column <- function(column) {
    
    if (!is.character(column) ||
        length(column) != 1L ||
        is.na(column) ||
        !nzchar(column)) {
      stop(
        "Column names must be non-empty character strings.",
        call. = FALSE
      )
    }
    
    if (column %in% names(cols)) {
      return(column)
    }
    
    if (column %in% unname(cols)) {
      return(names(cols)[match(column, unname(cols))])
    }
    
    # Allow an unconfigured column present in the data.
    if (column %in% names(data)) {
      return(column)
    }
    
    # Allow a literal column name from the original data.
    if (column %in% original_names) {
      index <- match(column, original_names)
      return(names(data)[index])
    }
    
    stop(
      "Unknown column: ", column,
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Select school-name column
  # ------------------------------------------------------------
  school_col <- NULL
  
  if (!is.null(school_names)) {
    
    if (is.null(SchoolNameColumn)) {
      
      if ("est_name" %in% names(data)) {
        school_col <- "est_name"
        
      } else if ("est_wonde_name" %in% names(data)) {
        school_col <- "est_wonde_name"
        
      } else {
        stop(
          "Missing school-name column. Requires est_name ",
          "or est_wonde_name, or specify SchoolNameColumn.",
          call. = FALSE
        )
      }
      
    } else {
      school_col <- resolve_column(SchoolNameColumn)
      
      if (!school_col %in% names(data)) {
        stop(
          "School-name column not found: ",
          SchoolNameColumn,
          call. = FALSE
        )
      }
    }
  }
  
  # ------------------------------------------------------------
  # 4. Select URN column
  # ------------------------------------------------------------
  urn_col <- NULL
  
  if (!is.null(URNs)) {
    
    urn_col <- resolve_column(URNColumn)
    
    if (!urn_col %in% names(data)) {
      stop(
        "URN column not found: ",
        URNColumn,
        call. = FALSE
      )
    }
  }
  
  # ------------------------------------------------------------
  # 5. Identify rows to remove
  # ------------------------------------------------------------
  remove_index <- rep(FALSE, nrow(data))
  
  if (!is.null(school_names)) {
    
    valid_names <- unique(school_names[!is.na(school_names)])
    
    remove_index <- remove_index |
      (
        !is.na(data[[school_col]]) &
          as.character(data[[school_col]]) %in% valid_names
      )
  }
  
  if (!is.null(URNs)) {
    
    valid_urns <- unique(
      as.character(URNs[!is.na(URNs)])
    )
    
    remove_index <- remove_index |
      (
        !is.na(data[[urn_col]]) &
          as.character(data[[urn_col]]) %in% valid_urns
      )
  }
  
  # ------------------------------------------------------------
  # 6. Handle no matches
  # ------------------------------------------------------------
  if (!any(remove_index)) {
    
    if (isTRUE(verbose)) {
      cli::cli_inform(
        "No matching schools found; no rows removed."
      )
    }
    
    return(rename_to_configured_names(data))
  }
  
  # ------------------------------------------------------------
  # 7. Remove matching school rows
  # ------------------------------------------------------------
  removed_data <- data[remove_index, , drop = FALSE]
  
  data <- data[!remove_index, , drop = FALSE]
  
  # ------------------------------------------------------------
  # 8. Report removal summary
  # ------------------------------------------------------------
  if (isTRUE(verbose)) {
    
    # Prefer school identifier for counting distinct schools.
    count_col <- if ("est_id" %in% names(removed_data)) {
      "est_id"
    } else if ("urn" %in% names(removed_data)) {
      "urn"
    } else if ("est_name" %in% names(removed_data)) {
      "est_name"
    } else if ("est_wonde_name" %in% names(removed_data)) {
      "est_wonde_name"
    } else if (!is.null(school_col)) {
      school_col
    } else {
      urn_col
    }
    
    n_schools <- length(unique(
      removed_data[[count_col]][
        !is.na(removed_data[[count_col]])
      ]
    ))
    
    cli::cli_inform(
      paste0(
        "Removed ",
        nrow(removed_data),
        " row(s) from ",
        n_schools,
        " school(s)."
      )
    )
  }
  
  # ------------------------------------------------------------
  # 9. Restore configured column names
  # ------------------------------------------------------------
  rename_to_configured_names(data)
}

