
#' Apply Student Withdrawals to a Class List
#'
#' @description
#' Matches withdrawal requests to students in a class list and
#' updates their withdrawal status. Supports both legacy
#' school-name/date-of-birth matching and current
#' establishment-identifier matching.
#'
#' @param class_list A data frame containing student records.
#'   Columns may use the configured or internal naming convention.
#'
#' @param withdrawn_students A data frame containing withdrawal
#'   responses, or a path to an XLSX file.
#'
#' @param school_dob_match Logical. If TRUE, use the legacy
#'   school-name and date-of-birth matching method. If FALSE,
#'   use the establishment-identifier matching method.
#'
#' @param legacy_school_link Path to the external school reference
#'   XLSX file used by the legacy matching method.
#'
#' @param links_category School category, retained for compatibility
#'   with the legacy method. Filtering is not currently applied.
#'
#' @param expected_years Numeric vector of acceptable birth years
#'   used by the legacy date-of-birth processing function.
#'
#' @param do_log Logical. Whether to write a log when using
#'   the legacy matching method.
#'
#' @param reference_school_name_column Column containing school
#'   names in the external school reference file. Used only by
#'   the legacy method. Defaults to "EstablishmentName".
#'
#' @param reference_school_id_column Column containing school
#'   identifiers in the external reference file. Used only by
#'   the legacy method. Defaults to "URN". Values must match
#'   the class list's configured `urn` column.
#'
#' @param keep_grep Regular expression identifying withdrawal
#'   responses to retain. Used by the current method.
#'
#' @param TypeOfWithdrawalColumn Column containing the type of
#'   withdrawal in the withdrawal form.
#'
#' @param FirstNameColumn Column containing the student's first
#'   name in the withdrawal form.
#'
#' @param LastNameColumn Column containing the student's surname
#'   in the withdrawal form.
#'
#' @param EstablishmentIDColumn Column containing the school
#'   identifier in the withdrawal form.
#'
#' @param school_id_column Internal or configured column name
#'   identifying the school in the class list for the current
#'   matching method. Defaults to "est_id".
#'
#' @param ... Additional arguments passed to the selected
#'   internal withdrawal function.
#'
#' @return A list containing:
#' \describe{
#'   \item{class_list}{Updated class list using configured
#'     column names, with matched students marked as withdrawn.}
#'   \item{withdraw}{Withdrawal matching report containing
#'     MatchDetail, SchoolCheck, CLASS_LIST_INDEX,
#'     matchedFirstName, matchedLastName, matchedSchoolName,
#'     and IGNORE_RESULT.}
#' }
#'
#' @details
#' When `school_dob_match = TRUE`, the function calls
#' `.apply_withdrawal_old()`. This matches school names against
#' an external reference file and uses student names and
#' dates of birth to identify withdrawals.
#'
#' When `school_dob_match = FALSE`, the function calls
#' `.apply_withdrawal_new()`. This matches students using
#' an establishment identifier and student names.
#'
#' Both methods preserve existing withdrawal statuses and
#' create the configured withdrawal column if it is missing.
#' Only uniquely matched students are marked as withdrawn.
#'
#' The function does not require a Wonde student identifier.
#' It marks withdrawals but does not automatically remove
#' identifying information. Use
#' `clear_withdrawn_student_data()` separately for that purpose.
#'
#' @family withdrawing
#' @export
apply_withdrawal <- function(
    class_list,
    withdrawn_students,
    school_dob_match = FALSE,
    
    # Legacy matching arguments
    legacy_school_link = "~/Wonde/Establishment_List_ID.xlsx",
    links_category = "Primary",
    expected_years = c(2019, 2020),
    do_log = FALSE,
    reference_school_name_column = "EstablishmentName",
    reference_school_id_column = "URN",
    
    # Current matching arguments
    keep_grep = paste0(
      "I want to withdraw my child from the study|",
      "No, please delete any previous data about my child"
    ),
    TypeOfWithdrawalColumn = "Type of withdrawal",
    FirstNameColumn = "Your child's first name",
    LastNameColumn = "Your child's surname",
    EstablishmentIDColumn = "EstablishmentID",
    school_id_column = "est_id",
    
    ...) {
  
  # ------------------------------------------------------------
  # 1. Validate matching method
  # ------------------------------------------------------------
  if (!is.logical(school_dob_match) ||
      length(school_dob_match) != 1L ||
      is.na(school_dob_match)) {
    
    stop(
      "school_dob_match must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 2. Legacy school-name and DoB matching
  # ------------------------------------------------------------
  if (school_dob_match) {
    
    message(
      "Routing to internal legacy (school + DoB) method"
    )
    
    return(
      .apply_withdrawal_old(
        class_list = class_list,
        withdrawn_students = withdrawn_students,
        school_link = legacy_school_link,
        links_category = links_category,
        expected_years = expected_years,
        do_log = do_log,
        reference_school_name_column =
          reference_school_name_column,
        reference_school_id_column =
          reference_school_id_column,
        ...
      )
    )
  }
  
  # ------------------------------------------------------------
  # 3. Current establishment-identifier matching
  # ------------------------------------------------------------
  message(
    "Routing to internal current (EstablishmentID) method"
  )
  
  .apply_withdrawal_new(
    class_list = class_list,
    withdrawn_students = withdrawn_students,
    keep_grep = keep_grep,
    TypeOfWithdrawalColumn = TypeOfWithdrawalColumn,
    FirstNameColumn = FirstNameColumn,
    LastNameColumn = LastNameColumn,
    EstablishmentIDColumn = EstablishmentIDColumn,
    school_id_column = school_id_column,
    ...
  )
}





#' Apply Withdrawals Using Legacy School and DoB Matching
#'
#' @description
#' Matches legacy withdrawal records to a class list using school names,
#' student names and, where available, dates of birth.
#'
#' @param class_list Class-list data frame using configured or internal
#'   column names. Requires `urn`, `sfn` and `sln`.
#' @param withdrawn_students Withdrawal-form data frame or XLSX file path.
#' @param school_link Path to an XLSX school reference file.
#' @param links_category School category. Retained for compatibility;
#'   filtering by this argument is not currently applied.
#' @param expected_years Valid birth years passed to
#'   `.process_withdrawn_dob()`.
#' @param do_log Logical. Whether to write a legacy withdrawal log.
#' @param reference_school_name_column Column containing school names
#'   in the external school reference file. Defaults to
#'   `"EstablishmentName"`.
#' @param reference_school_id_column Column containing school URNs
#'   in the external school reference file. Defaults to `"URN"`.
#'   Values must correspond to the class list's `urn` field.
#'
#' @return A list containing:
#' \describe{
#'   \item{class_list}{Updated class list with configured column names.}
#'   \item{withdraw}{Withdrawal matching report containing MatchDetail,
#'     SchoolCheck, CLASS_LIST_INDEX, matchedFirstName, matchedLastName,
#'     matchedSchoolName and IGNORE_RESULT.}
#' }
#'
#' @details
#' Matching follows the original priority:
#' \enumerate{
#'   \item Exact first name and surname.
#'   \item Partial first name and exact surname.
#'   \item Exact first name and partial surname.
#'   \item Partial first name and partial surname.
#'   \item Exact first name and date of birth.
#'   \item Exact surname only.
#' }
#'
#' Candidate students are restricted to schools identified by
#' `.match_school_name()`. A withdrawal is applied only when one student
#' is matched. Ambiguous exact matches are not automatically withdrawn.
#'
#' Existing withdrawal statuses are preserved. If the withdrawal
#' column is absent, it is created with blank values. Successfully
#' matched students are assigned "Y".
#'
#' The external reference file has independently configurable column
#' names. The class-list column names are handled using the package's
#' existing column-name lookup.
#'
#' No Wonde student ID is required.
#'
#' @keywords internal
#' @noRd
.apply_withdrawal_old <- function(
    class_list,
    withdrawn_students,
    school_link = "~/Wonde/Establishment_List_ID.xlsx",
    links_category = "Primary",
    expected_years = c(2019, 2020),
    do_log = FALSE,
    reference_school_name_column = "EstablishmentName",
    reference_school_id_column = "URN") {
  
  # ------------------------------------------------------------
  # 1. Start optional logging
  # ------------------------------------------------------------
  if (do_log) {
    logr::log_open("Withdrawn_students.log")
    on.exit(logr::log_close(), add = TRUE)
  }
  
  # ------------------------------------------------------------
  # 2. Read withdrawal records
  # ------------------------------------------------------------
  if (is.character(withdrawn_students) &&
      length(withdrawn_students) == 1L) {
    
    withdrawn_students <- readxl::read_xlsx(
      withdrawn_students
    ) |>
      as.data.frame()
  }
  
  required_form_cols <- c(
    "Your child's first name",
    "Your child's surname",
    "Name of school"
  )
  
  missing_cols <- setdiff(
    required_form_cols,
    names(withdrawn_students)
  )
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing withdrawal form columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Filter withdrawal responses
  # ------------------------------------------------------------
  consent_col <- paste0(
    "Please confirm that you do not want your child ",
    "to take part in the Observatory for Mathematical ",
    "Education's cohort study:"
  )
  
  if (consent_col %in% names(withdrawn_students)) {
    
    keep <- grepl(
      "^No|^I want to withdraw",
      withdrawn_students[[consent_col]]
    )
    
    keep[is.na(keep)] <- FALSE
    
    withdrawn_students <- withdrawn_students[
      keep, , drop = FALSE
    ]
  }
  
  # ------------------------------------------------------------
  # 4. Read and validate school reference data
  # ------------------------------------------------------------
  links <- readxl::read_xlsx(school_link) |>
    as.data.frame()
  
  required_link_cols <- c(
    reference_school_name_column,
    reference_school_id_column
  )
  
  missing_cols <- setdiff(
    required_link_cols,
    names(links)
  )
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing school reference columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 5. Convert class-list columns to internal names
  # ------------------------------------------------------------
  class_list <- rename_to_internal_names(class_list)
  
  required_class_cols <- c("urn", "sfn", "sln")
  
  missing_cols <- setdiff(
    required_class_cols,
    names(class_list)
  )
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing class list columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Preserve existing withdrawal statuses
  if (!"withdrawn" %in% names(class_list)) {
    class_list$withdrawn <- rep("", nrow(class_list))
  }
  
  if (!"hex_id" %in% names(class_list)) {
    class_list$hex_id <- rep("", nrow(class_list))
  }
  
  if (!"ome_id" %in% names(class_list)) {
    class_list$ome_id <- rep("", nrow(class_list))
  }
  
  # ------------------------------------------------------------
  # 6. Prepare withdrawal records
  # ------------------------------------------------------------
  firstName <- withdrawn_students$`Your child's first name` |>
    as.character() |>
    toupper() |>
    stringr::str_replace_all("-", " ") |>
    stringi::stri_trans_general(id = "Latin-ASCII")
  
  lastName <- withdrawn_students$`Your child's surname` |>
    as.character() |>
    toupper() |>
    stringr::str_replace_all("-", " ") |>
    stringi::stri_trans_general(id = "Latin-ASCII")
  
  SchoolName <- withdrawn_students$`Name of school` |>
    as.character() |>
    toupper()
  
  DoB <- .process_withdrawn_dob(
    withdrawn_students,
    expected_years
  )
  
  withdraw <- data.frame(
    firstName = firstName,
    lastName = lastName,
    DoB = DoB,
    SchoolName = SchoolName,
    stringsAsFactors = FALSE
  )
  
  if (do_log) {
    logr::log_print("Cleaned withdrawn students data frame:")
    logr::log_print(withdraw)
  }
  
  # ------------------------------------------------------------
  # 7. Initialise match report fields
  # ------------------------------------------------------------
  match_desc <- rep(NA_character_, nrow(withdraw))
  school_look <- rep(NA_character_, nrow(withdraw))
  classListIndex <- rep(NA_integer_, nrow(withdraw))
  matchedFirstName <- rep(NA_character_, nrow(withdraw))
  matchedLastName <- rep(NA_character_, nrow(withdraw))
  matchedSchoolName <- rep(NA_character_, nrow(withdraw))
  
  # ------------------------------------------------------------
  # 8. Match withdrawn students
  # ------------------------------------------------------------
  for (i in seq_len(nrow(withdraw))) {
    
    cur <- withdraw[i, , drop = FALSE]
    
    if (do_log) {
      logr::log_print("Current withdrawal:")
      logr::log_print(cur)
    }
    
    # ----------------------------------------------------------
    # 8.1 Match school against reference file
    # ----------------------------------------------------------
    school_matches <- .match_school_name(
      cur$SchoolName,
      links,
      school_name_column = reference_school_name_column,
      school_id_column = reference_school_id_column
    )
    
    school_look[i] <- paste(
      school_matches$SchoolName,
      collapse = "\n"
    )
    
    if (do_log) {
      logr::log_print(school_matches)
    }
    
    # The school-matching helper returns its selected identifier
    # in the column URN, regardless of the reference column name.
    if (nrow(school_matches) == 0L ||
        !any(
          as.character(school_matches$URN) %in%
          as.character(class_list$urn)
        )) {
      
      match_desc[i] <-
        "No match, Wonde data for school not available."
      
      warning_msg <- paste0(
        "Row: ", i,
        ". Unable to withdraw student as matched school '",
        cur$SchoolName,
        "' is not found in the class list."
      )
      
      if (do_log) {
        logr::log_warning(warning_msg)
      } else {
        warning(warning_msg, call. = FALSE)
      }
      
      next
    }
    
    # ----------------------------------------------------------
    # 8.2 Restrict to the matched school(s)
    # ----------------------------------------------------------
    potent_index <- which(
      !is.na(class_list$urn) &
        as.character(class_list$urn) %in%
        as.character(school_matches$URN)
    )
    
    potent <- class_list[
      potent_index, , drop = FALSE
    ]
    
    # ----------------------------------------------------------
    # 8.3 Standardise student names
    # ----------------------------------------------------------
    potent_fn <- potent$sfn |>
      as.character() |>
      toupper() |>
      stringr::str_replace_all("-", " ") |>
      stringi::stri_trans_general(id = "Latin-ASCII")
    
    potent_ln <- potent$sln |>
      as.character() |>
      toupper() |>
      stringr::str_replace_all("-", " ") |>
      stringi::stri_trans_general(id = "Latin-ASCII")
    
    matched <- integer(0)
    desc <- NULL
    
    # Initialise both name components before fallback matching
    withdraw_FN_parts <- stringr::str_split(
      cur$firstName,
      "\\s+"
    )[[1]]
    
    withdraw_LN_parts <- stringr::str_split(
      cur$lastName,
      "\\s+"
    )[[1]]
    
    withdraw_FN_parts <- withdraw_FN_parts[
      !is.na(withdraw_FN_parts) &
        nzchar(withdraw_FN_parts)
    ]
    
    withdraw_LN_parts <- withdraw_LN_parts[
      !is.na(withdraw_LN_parts) &
        nzchar(withdraw_LN_parts)
    ]
    
    # ----------------------------------------------------------
    # 8.4 Exact first name and surname
    # ----------------------------------------------------------
    match_index <- which(
      !is.na(cur$firstName) &
        !is.na(cur$lastName) &
        nzchar(cur$firstName) &
        nzchar(cur$lastName) &
        potent_fn == cur$firstName &
        potent_ln == cur$lastName
    )
    
    if (length(match_index) == 1L) {
      
      matched <- match_index
      desc <- "Exact first and last name match (to one student)"
      
    } else if (length(match_index) > 1L) {
      
      match_desc[i] <- paste0(
        "Multiple first and last name match, ",
        "do not remove any"
      )
      
      next
    }
    
    # ----------------------------------------------------------
    # 8.5 Partial first name, exact surname
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        length(withdraw_FN_parts) > 0L &&
        !is.na(cur$lastName) &&
        nzchar(cur$lastName)) {
      
      match_index <- which(
        grepl(
          paste(withdraw_FN_parts, collapse = "|"),
          potent_fn
        ) &
          potent_ln == cur$lastName
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Partial first name, exact surname (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 8.6 Exact first name, partial surname
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        length(withdraw_LN_parts) > 0L &&
        !is.na(cur$firstName) &&
        nzchar(cur$firstName)) {
      
      match_index <- which(
        grepl(
          paste(withdraw_LN_parts, collapse = "|"),
          potent_ln
        ) &
          potent_fn == cur$firstName
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Partial last name, exact first name (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 8.7 Partial first name and surname
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        length(withdraw_FN_parts) > 0L &&
        length(withdraw_LN_parts) > 0L) {
      
      match_index <- which(
        grepl(
          paste(withdraw_FN_parts, collapse = "|"),
          potent_fn
        ) &
          grepl(
            paste(withdraw_LN_parts, collapse = "|"),
            potent_ln
          )
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Partial last name, partial first name (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 8.8 Exact first name and date of birth
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        !is.na(cur$DoB) &&
        nzchar(cur$DoB) &&
        "dob" %in% names(potent) &&
        !is.na(cur$firstName) &&
        nzchar(cur$firstName)) {
      
      # The existing DoB helper can return multiple candidate dates.
      possible_dobs <- trimws(
        unlist(strsplit(cur$DoB, ",", fixed = TRUE))
      )
      
      match_index <- which(
        potent_fn == cur$firstName &
          as.character(potent$dob) %in% possible_dobs
      )
      
      if (length(match_index) == 1L) {
        
        matched <- match_index
        desc <- "Exact first name, DoB match (to one student)"
        
      } else if (length(match_index) > 1L) {
        
        match_desc[i] <- paste0(
          "Multiple first name match, DoB match, ",
          "do not remove any"
        )
        
        next
      }
    }
    
    # ----------------------------------------------------------
    # 8.9 Exact surname only
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        !is.na(cur$lastName) &&
        nzchar(cur$lastName)) {
      
      match_index <- which(
        potent_ln == cur$lastName
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Exact match surname only (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 8.10 Apply unique match
    # ----------------------------------------------------------
    if (length(matched) == 1L) {
      
      selected_index <- potent_index[matched]
      
      match_desc[i] <- desc
      classListIndex[i] <- selected_index
      
      matchedFirstName[i] <- as.character(
        potent$sfn[matched]
      )
      
      matchedLastName[i] <- as.character(
        potent$sln[matched]
      )
      
      if ("est_wonde_name" %in% names(potent)) {
        
        matchedSchoolName[i] <- as.character(
          potent$est_wonde_name[matched]
        )
        
      } else {
        
        school_match_index <- match(
          as.character(potent$urn[matched]),
          as.character(school_matches$URN)
        )
        
        matchedSchoolName[i] <- as.character(
          school_matches$SchoolName[school_match_index]
        )
      }
      
      # Preserve existing statuses for all other students
      class_list$withdrawn[selected_index] <- "Y"
      
    } else {
      
      match_desc[i] <- "Student not found in class list"
    }
  }
  
  # ------------------------------------------------------------
  # 9. Produce withdrawal report
  # ------------------------------------------------------------
  withdraw$MatchDetail <- match_desc
  withdraw$SchoolCheck <- school_look
  
  withdraw$CLASS_LIST_INDEX <- classListIndex
  withdraw$matchedFirstName <- matchedFirstName
  withdraw$matchedLastName <- matchedLastName
  withdraw$matchedSchoolName <- matchedSchoolName
  
  withdraw$IGNORE_RESULT <- rep(
    NA_character_,
    nrow(withdraw)
  )
  
  # ------------------------------------------------------------
  # Standardise withdrawal report column names
  # ------------------------------------------------------------
  names(withdraw) <- c(
    firstName = "FIRST_NAME",
    lastName = "LAST_NAME",
    DoB = "DOB",
    SchoolName = "SCHOOL_NAME",
    EstablishmentID = "EST_ID",
    MatchDetail = "MATCH_DETAIL",
    SchoolCheck = "SCHOOL_CHECK",
    CLASS_LIST_INDEX = "CLASS_LIST_INDEX",
    matchedFirstName = "MATCHED_FIRST_NAME",
    matchedLastName = "MATCHED_LAST_NAME",
    matchedSchoolName = "MATCHED_SCHOOL_NAME",
    IGNORE_RESULT = "IGNORE_RESULT"
  )[names(withdraw)]
  
  # ------------------------------------------------------------
  # 10. Return results
  # ------------------------------------------------------------
  list(
    class_list = rename_to_configured_names(class_list),
    withdraw = withdraw
  )
}






#' Apply Withdrawals Using an Establishment Identifier
#'
#' @description
#' Matches student withdrawal records to a class list using an
#' establishment identifier and student names. Updates withdrawal
#' statuses and produces a report of matching outcomes.
#'
#' @param class_list A data frame containing student records.
#'   Column names may use the configured or internal naming convention.
#'
#' @param withdrawn_students A data frame containing withdrawal
#'   responses, or a path to an XLSX file.
#'
#' @param keep_grep Regular expression identifying withdrawal responses
#'   to retain.
#'
#' @param TypeOfWithdrawalColumn Column containing the withdrawal
#'   response type in the withdrawal form.
#'
#' @param FirstNameColumn Column containing the student's first name
#'   in the withdrawal form.
#'
#' @param LastNameColumn Column containing the student's surname
#'   in the withdrawal form.
#'
#' @param EstablishmentIDColumn Column containing the establishment
#'   identifier in the withdrawal form.
#'
#' @param school_id_column Internal or configured column name for
#'   the school identifier in the class list. Defaults to "est_id".
#'   For example, "est_id", "est_wonde_id", or "urn".
#'
#' @return A list containing:
#' \describe{
#'   \item{class_list}{Updated class list using configured column
#'     names, with matched students marked as withdrawn.}
#'   \item{withdraw}{Withdrawal matching report containing
#'     MatchDetail, SchoolCheck, CLASS_LIST_INDEX,
#'     matchedFirstName, matchedLastName, matchedSchoolName,
#'     and IGNORE_RESULT.}
#' }
#'
#' @details
#' Matching follows six stages within the specified establishment:
#' \enumerate{
#'   \item Exact first name and surname.
#'   \item Partial first name and exact surname.
#'   \item Exact first name and partial surname.
#'   \item Partial first name and partial surname.
#'   \item Exact first name and date of birth, if available.
#'   \item Exact surname only.
#' }
#'
#' A withdrawal is applied only when exactly one student is matched.
#' Multiple exact first-name and surname matches are treated as
#' ambiguous and are not automatically withdrawn.
#'
#' Existing withdrawal statuses are preserved. If the withdrawal
#' status column does not exist, it is created with blank values.
#' Successfully matched students are assigned a status of "Y".
#'
#' Class-list column names are resolved through the package's column
#' configuration. Withdrawal form column names are not modified.
#'
#' No Wonde student identifier is required.
#'
#' @keywords internal
#' @noRd
.apply_withdrawal_new <- function(
    class_list,
    withdrawn_students,
    keep_grep = paste0(
      "I want to withdraw my child from the study|",
      "No, please delete any previous data about my child"
    ),
    TypeOfWithdrawalColumn = "Type of withdrawal",
    FirstNameColumn = "Your child's first name",
    LastNameColumn = "Your child's surname",
    EstablishmentIDColumn = "EstablishmentID",
    school_id_column = "est_id") {
  
  # ------------------------------------------------------------
  # 1. Read withdrawal data
  # ------------------------------------------------------------
  if (is.character(withdrawn_students) &&
      length(withdrawn_students) == 1L) {
    
    withdrawn_students <- readxl::read_xlsx(
      withdrawn_students
    ) |>
      as.data.frame()
  }
  
  required_withdraw_cols <- c(
    TypeOfWithdrawalColumn,
    FirstNameColumn,
    LastNameColumn,
    EstablishmentIDColumn
  )
  
  missing_cols <- setdiff(
    required_withdraw_cols,
    names(withdrawn_students)
  )
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing withdrawal form columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 2. Filter withdrawal responses
  # ------------------------------------------------------------
  keep <- grepl(
    keep_grep,
    withdrawn_students[[TypeOfWithdrawalColumn]]
  )
  
  keep[is.na(keep)] <- FALSE
  
  # Preserve previous filtering behaviour
  if (any(keep)) {
    withdrawn_students <- withdrawn_students[
      keep, , drop = FALSE
    ]
  }
  
  # ------------------------------------------------------------
  # 3. Resolve class-list column names
  # ------------------------------------------------------------
  class_list <- rename_to_internal_names(class_list)
  
  cols <- get_column_names()
  
  if (!is.character(school_id_column) ||
      length(school_id_column) != 1L ||
      is.na(school_id_column)) {
    
    stop(
      "school_id_column must be a single column name.",
      call. = FALSE
    )
  }
  
  if (school_id_column %in% names(cols)) {
    
    school_col <- school_id_column
    
  } else if (school_id_column %in% unname(cols)) {
    
    school_col <- names(cols)[match(
      school_id_column,
      unname(cols)
    )]
    
  } else {
    
    stop(
      "Unknown school identifier column: ",
      school_id_column,
      call. = FALSE
    )
  }
  
  required_class_cols <- c(
    school_col,
    "sfn",
    "sln"
  )
  
  missing_cols <- setdiff(
    required_class_cols,
    names(class_list)
  )
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing class list columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Prepare withdrawal records
  # ------------------------------------------------------------
  firstName <- withdrawn_students[[FirstNameColumn]] |>
    as.character() |>
    toupper() |>
    stringr::str_replace_all("-", " ") |>
    stringi::stri_trans_general(id = "Latin-ASCII")
  
  lastName <- withdrawn_students[[LastNameColumn]] |>
    as.character() |>
    toupper() |>
    stringr::str_replace_all("-", " ") |>
    stringi::stri_trans_general(id = "Latin-ASCII")
  
  withdraw <- data.frame(
    firstName = firstName,
    lastName = lastName,
    DoB = rep(NA_character_, length(firstName)),
    EstablishmentID = withdrawn_students[[EstablishmentIDColumn]],
    stringsAsFactors = FALSE
  )
  
  # ------------------------------------------------------------
  # 5. Prepare class-list withdrawal columns
  # ------------------------------------------------------------
  # Preserve existing withdrawn statuses where available
  if (!"withdrawn" %in% names(class_list)) {
    class_list$withdrawn <- rep("", nrow(class_list))
  }
  
  if (!"hex_id" %in% names(class_list)) {
    class_list$hex_id <- rep("", nrow(class_list))
  }
  
  if (!"ome_id" %in% names(class_list)) {
    class_list$ome_id <- rep("", nrow(class_list))
  }
  
  match_desc <- rep(NA_character_, nrow(withdraw))
  school_look <- rep(NA_character_, nrow(withdraw))
  classListIndex <- rep(NA_integer_, nrow(withdraw))
  matchedFirstName <- rep(NA_character_, nrow(withdraw))
  matchedLastName <- rep(NA_character_, nrow(withdraw))
  matchedSchoolName <- rep(NA_character_, nrow(withdraw))
  
  # ------------------------------------------------------------
  # 6. Match withdrawal records
  # ------------------------------------------------------------
  for (i in seq_len(nrow(withdraw))) {
    
    cur <- withdraw[i, , drop = FALSE]
    
    potent_index <- which(
      !is.na(class_list[[school_col]]) &
        !is.na(cur$EstablishmentID) &
        as.character(class_list[[school_col]]) ==
        as.character(cur$EstablishmentID)
    )
    
    if (length(potent_index) == 0L) {
      match_desc[i] <- "School not found in class list"
      next
    }
    
    potent <- class_list[
      potent_index, , drop = FALSE
    ]
    
    potent_fn <- potent$sfn |>
      as.character() |>
      toupper() |>
      stringr::str_replace_all("-", " ") |>
      stringi::stri_trans_general(id = "Latin-ASCII")
    
    potent_ln <- potent$sln |>
      as.character() |>
      toupper() |>
      stringr::str_replace_all("-", " ") |>
      stringi::stri_trans_general(id = "Latin-ASCII")
    
    matched <- integer(0)
    desc <- NULL
    
    # Prepare name components for partial matching
    withdraw_FN_parts <- stringr::str_split(
      cur$firstName,
      "\\s+"
    )[[1]]
    
    withdraw_LN_parts <- stringr::str_split(
      cur$lastName,
      "\\s+"
    )[[1]]
    
    withdraw_FN_parts <- withdraw_FN_parts[
      !is.na(withdraw_FN_parts) &
        nzchar(withdraw_FN_parts)
    ]
    
    withdraw_LN_parts <- withdraw_LN_parts[
      !is.na(withdraw_LN_parts) &
        nzchar(withdraw_LN_parts)
    ]
    
    # ----------------------------------------------------------
    # 6.1 Exact first and last name
    # ----------------------------------------------------------
    match_index <- which(
      !is.na(potent_fn) &
        !is.na(potent_ln) &
        !is.na(cur$firstName) &
        !is.na(cur$lastName) &
        nzchar(cur$firstName) &
        nzchar(cur$lastName) &
        potent_fn == cur$firstName &
        potent_ln == cur$lastName
    )
    
    if (length(match_index) == 1L) {
      
      matched <- match_index
      desc <- "Exact first and last name match (to one student)"
      
    } else if (length(match_index) > 1L) {
      
      match_desc[i] <- paste0(
        "Multiple first and last name match, ",
        "do not remove any"
      )
      
      next
    }
    
    # ----------------------------------------------------------
    # 6.2 Partial first name and exact surname
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        length(withdraw_FN_parts) > 0L &&
        !is.na(cur$lastName) &&
        nzchar(cur$lastName)) {
      
      match_index <- which(
        grepl(
          paste(withdraw_FN_parts, collapse = "|"),
          potent_fn
        ) &
          !is.na(potent_ln) &
          potent_ln == cur$lastName
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Partial first name, exact surname (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 6.3 Exact first name and partial surname
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        length(withdraw_LN_parts) > 0L &&
        !is.na(cur$firstName) &&
        nzchar(cur$firstName)) {
      
      match_index <- which(
        grepl(
          paste(withdraw_LN_parts, collapse = "|"),
          potent_ln
        ) &
          !is.na(potent_fn) &
          potent_fn == cur$firstName
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Partial last name, exact first name (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 6.4 Partial first and last name
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        length(withdraw_FN_parts) > 0L &&
        length(withdraw_LN_parts) > 0L) {
      
      match_index <- which(
        grepl(
          paste(withdraw_FN_parts, collapse = "|"),
          potent_fn
        ) &
          grepl(
            paste(withdraw_LN_parts, collapse = "|"),
            potent_ln
          )
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Partial last name, partial first name (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 6.5 Exact first name and date of birth
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        !is.na(cur$DoB) &&
        "dob" %in% names(potent) &&
        !is.na(cur$firstName) &&
        nzchar(cur$firstName)) {
      
      match_index <- which(
        potent_fn == cur$firstName &
          as.character(potent$dob) ==
          as.character(cur$DoB)
      )
      
      if (length(match_index) == 1L) {
        
        matched <- match_index
        desc <- "Exact first name, DoB match (to one student)"
        
      } else if (length(match_index) > 1L) {
        
        match_desc[i] <- paste0(
          "Multiple first name match, DoB match, ",
          "do not remove any"
        )
        
        next
      }
    }
    
    # ----------------------------------------------------------
    # 6.6 Exact surname only
    # ----------------------------------------------------------
    if (length(matched) == 0L &&
        !is.na(cur$lastName) &&
        nzchar(cur$lastName)) {
      
      match_index <- which(
        !is.na(potent_ln) &
          potent_ln == cur$lastName
      )
      
      if (length(match_index) == 1L) {
        matched <- match_index
        desc <- "Exact match surname only (to one student)"
      }
    }
    
    # ----------------------------------------------------------
    # 6.7 Apply unique match
    # ----------------------------------------------------------
    if (length(matched) == 1L) {
      
      selected_index <- potent_index[matched]
      
      match_desc[i] <- desc
      classListIndex[i] <- selected_index
      
      matchedFirstName[i] <- potent$sfn[matched]
      matchedLastName[i] <- potent$sln[matched]
      
      if ("est_wonde_name" %in% names(potent)) {
        matchedSchoolName[i] <- as.character(
          potent$est_wonde_name[matched]
        )
      }
      
      school_look[i] <- as.character(cur$EstablishmentID)
      
      # Set the matched student to withdrawn
      # without modifying other withdrawal statuses
      class_list$withdrawn[selected_index] <- "Y"
      
    } else {
      
      match_desc[i] <- "Student not found in class list"
    }
  }
  
  # ------------------------------------------------------------
  # 7. Produce withdrawal report
  # ------------------------------------------------------------
  withdraw$MatchDetail <- match_desc
  withdraw$SchoolCheck <- school_look
  
  withdraw$CLASS_LIST_INDEX <- classListIndex
  withdraw$matchedFirstName <- matchedFirstName
  withdraw$matchedLastName <- matchedLastName
  withdraw$matchedSchoolName <- matchedSchoolName
  
  withdraw$IGNORE_RESULT <- rep(
    NA_character_,
    nrow(withdraw)
  )
  
  # ------------------------------------------------------------
  # Standardise withdrawal report column names
  # ------------------------------------------------------------
  names(withdraw) <- c(
    firstName = "FIRST_NAME",
    lastName = "LAST_NAME",
    DoB = "DOB",
    SchoolName = "SCHOOL_NAME",
    EstablishmentID = "EST_ID",
    MatchDetail = "MATCH_DETAIL",
    SchoolCheck = "SCHOOL_CHECK",
    CLASS_LIST_INDEX = "CLASS_LIST_INDEX",
    matchedFirstName = "MATCHED_FIRST_NAME",
    matchedLastName = "MATCHED_LAST_NAME",
    matchedSchoolName = "MATCHED_SCHOOL_NAME",
    IGNORE_RESULT = "IGNORE_RESULT"
  )[names(withdraw)]
  # ------------------------------------------------------------
  # 8. Return results
  # ------------------------------------------------------------
  list(
    class_list = rename_to_configured_names(class_list),
    withdraw = withdraw
  )
}



#' Process and Standardise Withdrawn Student Dates of Birth
#'
#' @description
#' Internal helper function for `apply_withdrawal()`. Extracts, cleans
#' and standardises date-of-birth information from withdrawal records.
#' Supports multiple date formats and handles missing or invalid dates.
#'
#' @param withdrawn_students A data frame containing withdrawal records,
#'   typically read from an Excel file using `readxl::read_xlsx()`.
#'   The date-of-birth field is expected to be named
#'   `"Your child's date of birth"`.
#'
#' @param expected_years Numeric vector of acceptable birth years,
#'   e.g. `c(2019, 2020)`. Dates outside these years are discarded.
#'
#' @return
#' A character vector of dates in YYYY-MM-DD format, or `NA` where
#' no valid date can be identified.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Checks whether the date-of-birth column exists.
#'   \item Cleans whitespace, punctuation and ordinal suffixes.
#'   \item Attempts to parse dates using several common formats,
#'         including numeric dates and dates containing month names.
#'   \item Removes dates outside `expected_years`.
#'   \item Returns the unique valid date for each withdrawal record.
#' }
#'
#' If multiple distinct valid dates are identified for one record,
#' they are returned as a comma-separated string, preserving the
#' original function's behaviour.
#'
#' This function processes withdrawal form data, not class-list
#' data, so the package's column-name lookup is not applied.
#'
#' @keywords internal
#' @noRd

.process_withdrawn_dob <- function(withdrawn_students, expected_years) {
  if (!is.numeric(expected_years) ||
      length(expected_years) == 0 ||
      anyNA(expected_years) ||
      any(!is.finite(expected_years)) ||
      any(expected_years %% 1 != 0) ||
      any(expected_years < 1000 | expected_years > 9999)) {
    
    stop(
      "expected_years must be a non-empty numeric vector of valid four-digit years.",
      call. = FALSE
    )
  }
  
  # If DoB column missing, return NA vector
  if (!("Your child's date of birth" %in% names(withdrawn_students))) {
    return(rep(NA_character_, nrow(withdrawn_students)))
  }
  
  withdrawn_students$`Your child's date of birth` <-
    withdrawn_students$`Your child's date of birth` |>
    toupper() |>
    stringr::str_squish()
  
  # Try multiple date formats
  DoB1A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+") |>
    as.Date(format = "%d/%m/%Y")
  
  DoB1B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+") |>
    as.Date(format = "%d/%m/%y")
  
  DoB2A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+") |>
    as.Date(format = "%d.%m.%Y")
  
  DoB2B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+") |>
    as.Date(format = "%d.%m.%y")
  
  DoB3A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+") |>
    as.Date(format = "%d-%m-%Y")
  
  DoB3B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+") |>
    as.Date(format = "%d-%m-%y")
  
  DoB4A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("TH |ST |ND |RD |,") |>
    as.Date(format = "%d %B %Y")
  
  DoB4B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("TH |ST |ND |RD |,") |>
    as.Date(format = "%d %B %y")
  
  DoB5A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("TH |ST |ND |RD |,") |>
    as.Date(format = "%B %d %Y")
  
  DoB5B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("TH |ST |ND |RD |,") |>
    as.Date(format = "%B %d %y")
  
  DoB6A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("TH |ST |ND |RD |,") |>
    as.Date(format = "%b %d %Y")
  
  DoB6B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("TH |ST |ND |RD |,") |>
    as.Date(format = "%b %d %y")
  
  DoB7A <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+|\\.|-|/") |>
    as.Date(format = "%d%m%Y")
  
  DoB7B <- withdrawn_students$`Your child's date of birth` |>
    stringr::str_remove_all("\\s+|\\.|-|/") |>
    as.Date(format = "%d%m%y")
  
  DoBs <- data.frame(
    withdrawn_students$`Your child's date of birth`,
    DoB1A, DoB1B,
    DoB2A, DoB2B,
    DoB3A, DoB3B,
    DoB4A, DoB4B,
    DoB5A, DoB5B,
    DoB6A, DoB6B,
    DoB7A, DoB7B
  )
  
  # Remove incorrect date interpretations
  for (i in seq(2, ncol(DoBs), 2)) {
    DoBs[, i][
      grepl("^[0-9]{2}-", as.character(DoBs[, i]))
    ] <- NA
  }
  
  for (i in seq(2, ncol(DoBs), 2)) {
    DoBs[, i + 1][which(!is.na(DoBs[, i]))] <- NA
  }
  
  # Retain only expected birth years
  for (i in 2:ncol(DoBs)) {
    DoBs[, i][
      !grepl(
        paste0(expected_years, collapse = "|"),
        as.character(DoBs[, i])
      )
    ] <- NA
  }
  
  # Combine unique valid dates
  DoB <- apply(DoBs[, -1, drop = FALSE], 1, function(x) {
    x <- unique(stats::na.omit(x))
    
    if (length(x) == 0) {
      return(NA_character_)
    }
    
    paste(x, collapse = ", ")
  })
  
  return(DoB)
}




#' Match a School Name to a List of Known Schools
#'
#' @description
#' Internal helper function that identifies the most likely matching
#' school or schools from a reference data frame using exact matching
#' or shared words between school names.
#'
#' @param school_name Character string containing the school name
#'   to match.
#'
#' @param schools_df Data frame containing reference school names
#'   and school identifiers.
#'
#' @param school_name_column Character string specifying the school
#'   name column in `schools_df`. Defaults to `"EstablishmentName"`.
#'   Accepts either a literal column name, an internal lookup name
#'   or a configured column name.
#'
#' @param school_id_column Character string specifying the school
#'   identifier column in `schools_df`. Defaults to `"URN"`.
#'   Accepts either a literal column name, an internal lookup name
#'   or a configured column name.
#'
#' @return
#' A data frame with columns:
#' \describe{
#'   \item{SchoolName}{Matched school name from the reference data.}
#'   \item{URN}{Matched school identifier. Retained for compatibility
#'     with the existing legacy withdrawal function, even when
#'     another identifier is selected.}
#' }
#'
#' Returns an empty data frame if no suitable match is found.
#'
#' @details
#' The matching process:
#' \enumerate{
#'   \item Standardises school names to uppercase, removes
#'         punctuation and normalises whitespace.
#'   \item Returns exact school-name matches where available.
#'   \item Otherwise compares the individual words in each name.
#'   \item Returns the school or schools with the highest number
#'         of shared words.
#' }
#'
#' Unlike the original implementation, schools with zero shared
#' words are not returned as potential matches.
#'
#' The function does not require Wonde identifiers.
#'
#' @keywords internal
#' @noRd
.match_school_name <- function(
    school_name,
    schools_df,
    school_name_column = "EstablishmentName",
    school_id_column = "URN") {
  
  # -------------------------------------------------------------------
  # 1. Validate input
  # -------------------------------------------------------------------
  if (length(school_name) != 1L ||
      is.na(school_name) ||
      !nzchar(trimws(school_name))) {
    
    return(data.frame(
      SchoolName = character(),
      URN = character()
    ))
  }
  
  # -------------------------------------------------------------------
  # 2. Resolve reference column names
  # -------------------------------------------------------------------
  cols <- get_column_names()
  
  # Resolve internal lookup keys into configured names
  if (school_name_column %in% names(cols)) {
    school_name_column <- unname(cols[[school_name_column]])
  }
  
  if (school_id_column %in% names(cols)) {
    school_id_column <- unname(cols[[school_id_column]])
  }
  
  # Check required columns exist
  required_cols <- c(school_name_column, school_id_column)
  
  missing_cols <- setdiff(required_cols, names(schools_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required school reference columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 3. Standardise school names
  # -------------------------------------------------------------------
  clean_name <- school_name |>
    toupper() |>
    stringr::str_remove_all("[:punct:]") |>
    stringr::str_squish()
  
  clean_schools <- schools_df[[school_name_column]] |>
    as.character() |>
    toupper() |>
    stringr::str_remove_all("[:punct:]") |>
    stringr::str_squish()
  
  # -------------------------------------------------------------------
  # 4. Exact school-name matching
  # -------------------------------------------------------------------
  exact_match_index <- which(
    !is.na(clean_schools) &
      clean_name == clean_schools
  )
  
  if (length(exact_match_index) > 0) {
    
    return(data.frame(
      SchoolName = schools_df[[school_name_column]][exact_match_index],
      URN = as.character(
        schools_df[[school_id_column]][exact_match_index]
      )
    ))
  }
  
  # -------------------------------------------------------------------
  # 5. Match using shared words
  # -------------------------------------------------------------------
  input_words <- clean_name |>
    stringr::str_split("\\s+") |>
    unlist() |>
    unique()
  
  school_words <- clean_schools |>
    stringr::str_split("\\s+")
  
  exact_match_counts <- vapply(
    school_words,
    function(words) {
      sum(input_words %in% words)
    },
    numeric(1)
  )
  
  # Avoid matching schools with no words in common
  if (length(exact_match_counts) == 0 ||
      max(exact_match_counts) == 0) {
    
    return(data.frame(
      SchoolName = character(),
      URN = character()
    ))
  }
  
  # -------------------------------------------------------------------
  # 6. Return best matching school(s)
  # -------------------------------------------------------------------
  best_match <- which(
    exact_match_counts == max(exact_match_counts)
  )
  
  data.frame(
    SchoolName = schools_df[[school_name_column]][best_match],
    URN = as.character(
      schools_df[[school_id_column]][best_match]
    )
  )
}


#' Revert ignored withdrawals
#'
#' @description
#' This function removes the withdrawal flag and restores identifying fields
#' for students who were previously marked as withdrawn but have been flagged
#' to be ignored (i.e., `IgnoreResult == "Y"` in the `withdraw` table).
#'
#' @param class_list A data frame of class list data (as returned by
#'   [apply_withdrawn()]).
#' @param withdraw A data frame of withdrawal results (as returned by
#'   [apply_withdrawn()]), containing the columns `classListIndex` and
#'   `IgnoreResult`.
#'
#' @return
#' An updated version of `class_list` with withdrawal flags and identifying
#' fields (`Withdrawn?`, `DoB`, `UPN`, `Print ID`, and `OME_ID`) restored
#' for ignored records.
#'
#' @details
#' For every row in `withdraw` where `IgnoreResult == "Y"` and a valid
#' `classListIndex` exists, this function:
#' \itemize{
#'   \item Sets `Withdrawn?` back to `""`
#'   \item Restores (clears) blank fields (`DoB`, `UPN`, `Print ID`, `OME_ID`)
#'     to `NA` where appropriate.
#' }
#'
#' @examples
#' \dontrun{
#' updated_class_list <- revert_ignored_withdrawals(class_list, withdraw)
#' }
#'
#' @family withdrawing
#'
#' @export

revert_ignored_withdrawals <- function(class_list, withdraw) {
  
  # -------------------------------------------------------------------
  # 1. Convert to internal column names
  # -------------------------------------------------------------------
  class_list <- rename_to_internal_names(class_list)
  
  # -------------------------------------------------------------------
  # 2. Validate required columns
  # -------------------------------------------------------------------
  required_cols <- c("IGNORE_RESULT", "CLASS_LIST_INDEX")
  
  missing_cols <- setdiff(required_cols, names(withdraw))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required withdrawal columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!"withdrawn" %in% names(class_list)) {
    stop(
      "Withdrawal status column not found: ",
      get_column_names("withdrawn"),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------
  # 3. Identify ignored withdrawals
  # -------------------------------------------------------------------
  ignore_rows <- which(
    withdraw$IGNORE_RESULT == "Y" &
      !is.na(withdraw$CLASS_LIST_INDEX)
  )
  
  if (length(ignore_rows) == 0) {
    message(
      "No ignored withdrawals found. Returning class_list unchanged."
    )
    return(rename_to_configured_names(class_list))
  }
  
  # -------------------------------------------------------------------
  # 4. Validate class list indices
  # -------------------------------------------------------------------
  revert_indices <- withdraw$CLASS_LIST_INDEX[ignore_rows]
  
  valid_indices <- is.finite(revert_indices) &
    revert_indices == floor(revert_indices) &
    revert_indices >= 1 &
    revert_indices <= nrow(class_list)
  
  if (any(!valid_indices)) {
    warning(
      "Some withdrawal indices are invalid and will be ignored.",
      call. = FALSE
    )
  }
  
  revert_indices <- unique(
    as.integer(revert_indices[valid_indices])
  )
  
  if (length(revert_indices) == 0) {
    return(rename_to_configured_names(class_list))
  }
  
  # -------------------------------------------------------------------
  # 5. Remove withdrawal flags
  # -------------------------------------------------------------------
  class_list$withdrawn[revert_indices] <- ""
  
  # -------------------------------------------------------------------
  # 6. Convert blank identifying fields to NA
  # -------------------------------------------------------------------
  cols_to_restore <- c(
    "dob",
    "upn",
    "hex_id",
    "ome_id"
  )
  
  existing_cols <- intersect(
    cols_to_restore,
    names(class_list)
  )
  
  for (col in existing_cols) {
    
    blank_index <- revert_indices[
      !is.na(class_list[[col]][revert_indices]) &
        class_list[[col]][revert_indices] == ""
    ]
    
    if (length(blank_index) > 0) {
      class_list[[col]][blank_index] <- NA
    }
  }
  
  # -------------------------------------------------------------------
  # 7. Return configured column names
  # -------------------------------------------------------------------
  message(
    length(revert_indices),
    " withdrawals reverted (IGNORE_RESULT == 'Y')."
  )
  
  rename_to_configured_names(class_list)
}



#' Clear Identifying Data for Withdrawn Students
#'
#' @description
#' Removes identifying information for students marked as withdrawn.
#' Only rows where the withdrawal indicator equals `"Y"` are updated.
#'
#' Supports the package's configurable column-name system, accepting
#' either internal identifiers or configured external column names.
#'
#' @param data A data frame containing class list information.
#'
#' @param WithdrawColumn Character string identifying the withdrawal
#'   status column. Defaults to `"withdrawn"`, corresponding to
#'   `"Withdrawn?"` in the default column-name lookup.
#'
#' @param columns_to_clear Character vector of columns to clear.
#'   Defaults to `c("upn", "dob", "print_id", "ome_id")`.
#'   Columns not present in the data frame are ignored.
#'
#' @param verbose Logical; if TRUE, prints a summary message.
#'
#' @return
#' A data frame with identifying fields set to NA for withdrawn
#' students. Column names are returned using the configured
#' naming convention.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts configured column names to internal names.
#'   \item Resolves the requested withdrawal and identifying columns.
#'   \item Identifies records where the withdrawal status is `"Y"`.
#'   \item Sets the requested identifying fields to NA for these records.
#'   \item Restores the configured column names.
#' }
#'
#' No student identifier is required. Records are processed
#' individually using the withdrawal indicator.
#'
#' If the withdrawal column is absent, a warning is issued and
#' the original data frame is returned unchanged.
#'
#' @examples
#' \dontrun{
#' updated <- clear_withdrawn_student_data(class_list)
#'
#' updated <- clear_withdrawn_student_data(
#'   class_list,
#'   columns_to_clear = c("DOB", "UPN")
#' )
#' }
#'
#' @family withdrawing
#'
#' @export
clear_withdrawn_student_data <- function(
    data,
    WithdrawColumn = "withdrawn",
    columns_to_clear = c("upn", "dob", "hex_id", "ome_id"),
    verbose = FALSE) {
  
  # -------------------------------------------------------------------
  # 1. Convert to internal column names
  # -------------------------------------------------------------------
  data <- rename_to_internal_names(data)
  
  cols <- get_column_names()
  
  # Resolve internal and configured column names
  lookup <- stats::setNames(
    names(cols),
    unname(cols)
  )
  
  resolve <- function(x) {
    matched <- x %in% names(lookup)
    x[matched] <- unname(lookup[x[matched]])
    x
  }
  
  WithdrawColumn <- resolve(WithdrawColumn)
  columns_to_clear <- resolve(columns_to_clear)
  
  # -------------------------------------------------------------------
  # 2. Check withdrawal column exists
  # -------------------------------------------------------------------
  if (!WithdrawColumn %in% names(data)) {
    warning(
      "Column '", WithdrawColumn,
      "' not found; no changes made.",
      call. = FALSE
    )
    return(rename_to_configured_names(data))
  }
  
  # -------------------------------------------------------------------
  # 3. Identify withdrawn students
  # -------------------------------------------------------------------
  withdrawn_index <- which(
    !is.na(data[[WithdrawColumn]]) &
      data[[WithdrawColumn]] == "Y"
  )
  
  if (length(withdrawn_index) == 0) {
    if (verbose) {
      cli::cli_inform(
        "No withdrawn students found; no changes made."
      )
    }
    return(rename_to_configured_names(data))
  }
  
  # -------------------------------------------------------------------
  # 4. Identify existing columns to clear
  # -------------------------------------------------------------------
  existing_cols <- intersect(
    columns_to_clear,
    names(data)
  )
  
  if (length(existing_cols) == 0) {
    if (verbose) {
      cli::cli_inform("No matching columns found to clear.")
    }
    return(rename_to_configured_names(data))
  }
  
  # -------------------------------------------------------------------
  # 5. Clear identifying information
  # -------------------------------------------------------------------
  for (col in existing_cols) {
    data[[col]][withdrawn_index] <- NA
  }
  
  if (verbose) {
    cli::cli_inform(
      paste0(
        "Cleared data in columns (",
        paste(existing_cols, collapse = ", "),
        ") for ",
        length(withdrawn_index),
        " withdrawn students."
      )
    )
  }
  
  # -------------------------------------------------------------------
  # 6. Restore configured column names
  # -------------------------------------------------------------------
  rename_to_configured_names(data)
}
