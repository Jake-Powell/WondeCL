#' Create Blank Class Lists for Schools Without Wonde Data
#'
#' @description
#' Creates placeholder class lists for schools without Wonde access. School
#' information is read from `schools_with_size`; class and teacher details can
#' be specified or generated automatically. Output uses the package's
#' configured column names.
#'
#' @param schools_with_size Data frame with a school name (configured
#'   `est_wonde_name` or `SchoolName`), a school URN (configured `urn`), and
#'   `NoStudents`. Optional columns are `ClassInfo`, `TeacherInfo`, and the
#'   configured `est_id` (establishment number).
#' @param numbering_type Either `"school"` (placeholder pupil numbers continue
#'   across classes within a school) or `"class"` (restart at 1 per class).
#' @param teacher_name Character vector of length 2 containing default teacher
#'   first name and surname. Defaults to `c("Reception", "Teacher")`.
#' @param class_name Base class name used when generating classes automatically.
#' @param type If a single non-negative whole number, the number of blank rows
#'   per automatically generated class. Otherwise, e.g. `"auto"`, use
#'   `ceiling(NoStudents / number_of_classes) + class_buffer`.
#' @param class_buffer Non-negative whole number added to automatically
#'   estimated class sizes when `type` is not numeric.
#' @param class_size Positive whole number used to calculate how many classes
#'   a school needs when `ClassInfo` is not supplied.
#' @param class_list_columns `"master"`, `"print"`, or a character vector of
#'   internal column keys and/or configured column names. Additional literal
#'   names are allowed and filled with `NA` if unavailable. The `master` preset
#'   includes `DATE_OF_WITHDRAWAL`, an extra field not in the column mapping.
#'
#' @details
#' `ClassInfo` accepts entries like `"A -> 28, B -> 30"`. Those class sizes
#' are used as provided, independently of `NoStudents` and `type`.
#' `TeacherInfo` accepts comma-separated full names, e.g.
#' `"Jane Smith, Alex Jones"`; one teacher can be reused across all classes,
#' or one teacher may be supplied per class. The separator `---` is also
#' supported between teachers. Names are split at the first space.
#'
#' Placeholder pupil numbers are stored in the configured `sfn` column;
#' other pupil-specific information is missing (`NA`). The function does not
#' assign real OME IDs, print IDs or student Wonde IDs.
#'
#' @return Data frame in the requested column order and configured naming
#'   convention. With no schools or zero-sized schools, returns a zero-row
#'   data frame with the requested columns.
#'
#' @examples
#' schools <- data.frame(
#'   SchoolName = "Oak Primary", URN = "123456", NoStudents = 45
#' )
#' blanks <- create_school_blanks(schools, class_list_columns = "master")
#'
#' @family class_list
#' @export
create_school_blanks <- function(schools_with_size,
                                 numbering_type = "school",
                                 teacher_name = c("Reception", "Teacher"),
                                 class_name = "Reception Class",
                                 type = 30,
                                 class_buffer = 5,
                                 class_size = 30,
                                 class_list_columns = "master") {
  
  if (!is.data.frame(schools_with_size)) {
    stop("schools_with_size must be a data frame.", call. = FALSE)
  }
  if (!is.character(numbering_type) || length(numbering_type) != 1L ||
      is.na(numbering_type) || !numbering_type %in% c("school", "class")) {
    stop("numbering_type must be 'school' or 'class'.", call. = FALSE)
  }
  if (!is.character(teacher_name) || length(teacher_name) != 2L ||
      anyNA(teacher_name) || any(!nzchar(trimws(teacher_name)))) {
    stop("teacher_name must contain a first name and surname.", call. = FALSE)
  }
  if (!is.character(class_name) || length(class_name) != 1L ||
      is.na(class_name) || !nzchar(trimws(class_name))) {
    stop("class_name must be one non-empty string.", call. = FALSE)
  }
  if (!is.numeric(class_size) || length(class_size) != 1L ||
      is.na(class_size) || !is.finite(class_size) || class_size < 1 ||
      class_size != floor(class_size)) {
    stop("class_size must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(class_buffer) || length(class_buffer) != 1L ||
      is.na(class_buffer) || !is.finite(class_buffer) || class_buffer < 0 ||
      class_buffer != floor(class_buffer)) {
    stop("class_buffer must be a non-negative whole number.", call. = FALSE)
  }
  if (is.numeric(type) &&
      (length(type) != 1L || is.na(type) || !is.finite(type) ||
       type < 0 || type != floor(type))) {
    stop("Numeric type must be a non-negative whole number.", call. = FALSE)
  }
  if (!is.numeric(type) &&
      (!is.character(type) || length(type) != 1L || is.na(type))) {
    stop("type must be a single number or a string such as 'auto'.",
         call. = FALSE)
  }
  if (!is.character(class_list_columns) ||
      !length(class_list_columns) || anyNA(class_list_columns) ||
      any(!nzchar(class_list_columns))) {
    stop("class_list_columns must be a non-empty character vector.",
         call. = FALSE)
  }
  
  cols <- get_column_names()
  if (length(class_list_columns) == 1L &&
      identical(class_list_columns, "master")) {
    requested <- c(
      "est_name", "urn", "class_name", "tfn", "tln", "sfn",
      "sln", "dob", "upn", "spare", "ome_id", "hex_id",
      "withdrawn", "est_id", "withdrawal_date"
    )
  } else if (length(class_list_columns) == 1L &&
             identical(class_list_columns, "print")) {
    requested <- c(
      "est_name", "class_name", "tfn", "tln", "sfn",
      "sln", "dob", "upn", "hex_id", "withdrawn", "spare", "est_id"
    )
  } else {
    requested <- class_list_columns
  }
  
  # Resolve internal column keys to the current configured column names.
  output_names <- vapply(requested, function(x) {
    if (x %in% names(cols)) unname(cols[[x]]) else x
  }, character(1))
  if (anyDuplicated(output_names)) {
    stop("class_list_columns resolves to duplicate column names.",
         call. = FALSE)
  }
  
  schools <- rename_to_internal_names(schools_with_size)
  school_name_col <- if ("est_name" %in% names(schools)) {
    "est_name"
  } else if ("SchoolName" %in% names(schools)) {
    "SchoolName"
  } else {
    NA_character_
  }
  urn_col <- if ("urn" %in% names(schools)) "urn" else NA_character_
  number_col <- if ("NoStudents" %in% names(schools)) {
    "NoStudents"
  } else if ("NO_STUDENTS" %in% names(schools)) {
    "NO_STUDENTS"
  } else {
    NA_character_
  }
  if (is.na(school_name_col) || is.na(urn_col) || is.na(number_col)) {
    stop("schools_with_size requires a school name, URN and NoStudents.",
         call. = FALSE)
  }
  
  # Each individual class contributes the same internal fields.
  empty <- data.frame(
    est_name  = character(), urn = character(),
    class_name = character(), t_full = character(),
    tfn = character(), tln = character(), sfn = integer(),
    sln = character(), dob = character(), upn = character(),
    spare = character(), ome_id = character(), hex_id = character(),
    withdrawn = character(), est_id = character(),
    withdrawal_date  = character(), stringsAsFactors = FALSE
  )
  
  school_parts <- lapply(seq_len(nrow(schools)), function(i) {
    one <- schools[i, , drop = FALSE]
    total <- suppressWarnings(as.numeric(one[[number_col]]))
    if (length(total) != 1L || is.na(total) || !is.finite(total) ||
        total < 0 || total != floor(total)) {
      stop("NoStudents must contain non-negative whole numbers (row ", i,
           ").", call. = FALSE)
    }
    if (is.na(one[[school_name_col]]) ||
        !nzchar(trimws(as.character(one[[school_name_col]]))) ||
        is.na(one[[urn_col]])) {
      stop("School name and URN cannot be missing (row ", i, ").",
           call. = FALSE)
    }
    
    info <- if ("ClassInfo" %in% names(one)) {
      as.character(one$ClassInfo)
    } else {
      NA_character_
    }
    teacher_info <- if ("TeacherInfo" %in% names(one)) {
      as.character(one$TeacherInfo)
    } else {
      NA_character_
    }
    
    if (is.na(info) || !nzchar(trimws(info))) {
      n_classes <- as.integer(ceiling(total / class_size))
      if (!n_classes) return(empty)
      counts <- if (is.numeric(type)) {
        rep(as.integer(type), n_classes)
      } else {
        rep(as.integer(ceiling(total / n_classes) + class_buffer),
            n_classes)
      }
      class_names <- paste(class_name, seq_len(n_classes))
    } else {
      entries <- trimws(strsplit(info, ",", fixed = TRUE)[[1]])
      matched <- regmatches(
        entries, regexec("^(.+?)\\s*->\\s*([0-9]+)\\s*$",
                         entries, perl = TRUE)
      )
      if (!length(entries) || any(lengths(matched) != 3L)) {
        stop("Invalid ClassInfo in row ", i,
             "; expected 'Class A -> 28, Class B -> 30'.",
             call. = FALSE)
      }
      class_names <- trimws(vapply(matched, `[[`, character(1), 2L))
      counts <- as.integer(vapply(matched, `[[`, character(1), 3L))
      if (anyNA(counts) || any(!nzchar(class_names)) || any(counts < 0L)) {
        stop("Invalid class name or size in ClassInfo (row ", i, ").",
             call. = FALSE)
      }
      n_classes <- length(counts)
    }
    
    if (is.na(teacher_info) || !nzchar(trimws(teacher_info))) {
      teacher_first <- rep(teacher_name[1], n_classes)
      teacher_last <- rep(teacher_name[2], n_classes)
    } else {
      teacher_entries <- trimws(
        strsplit(teacher_info, ",|---", perl = TRUE)[[1]]
      )
      if (any(!nzchar(teacher_entries))) {
        stop("TeacherInfo contains an empty teacher (row ", i, ").",
             call. = FALSE)
      }
      if (!length(teacher_entries) %in% c(1L, n_classes)) {
        stop("TeacherInfo must supply one teacher or one per class (row ",
             i, ").", call. = FALSE)
      }
      # A full name consists of first name followed by a surname.
      parts <- regmatches(
        teacher_entries,
        regexec("^([^[:space:]]+)\\s+(.+)$", teacher_entries,
                perl = TRUE)
      )
      if (any(lengths(parts) != 3L)) {
        stop("TeacherInfo names must contain first name and surname (row ",
             i, ").", call. = FALSE)
      }
      teacher_first <- rep(vapply(parts, `[[`, character(1), 2L),
                           length.out = n_classes)
      teacher_last <- rep(vapply(parts, `[[`, character(1), 3L),
                          length.out = n_classes)
    }
    
    class_parts <- lapply(seq_len(n_classes), function(j) {
      n <- counts[j]
      data.frame(
        est_name = rep(as.character(one[[school_name_col]]), n),
        urn = rep(as.character(one[[urn_col]]), n),
        class_name = rep(class_names[j], n),
        t_full = rep(paste(teacher_first[j], teacher_last[j]), n),
        tfn = rep(teacher_first[j], n),
        tln = rep(teacher_last[j], n),
        sfn = seq_len(n),
        sln = rep(NA_character_, n), dob = rep(NA_character_, n),
        upn = rep(NA_character_, n), spare = rep(NA_character_, n),
        ome_id = rep(NA_character_, n), hex_id = rep(NA_character_, n),
        withdrawn = rep(NA_character_, n),
        est_id = rep(if ("est_id" %in% names(one))
          as.character(one$est_id) else NA_character_, n),
        withdrawal_date = rep(NA_character_, n),
        stringsAsFactors = FALSE
      )
    })
    current <- do.call(rbind, class_parts)
    if (identical(numbering_type, "school")) {
      current$sfn <- seq_len(nrow(current))
    }
    current
  })
  
  result <- if (length(school_parts)) do.call(rbind, school_parts) else empty
  rownames(result) <- NULL
  result <- rename_to_configured_names(result)
  
  for (name in output_names) {
    if (!name %in% names(result)) result[[name]] <- rep(NA, nrow(result))
  }
  result[, output_names, drop = FALSE]
}







