#' Apply withdrawn status to class list
#'
#' Matches withdrawn students (from a parent or guardian withdrawal spreadsheet)
#' to the corresponding students in a class list and marks them as withdrawn.
#'
#' The function attempts to find each withdrawn student within the provided
#' class_list by matching on school name, first name, last name, and where
#' available, date of birth. It handles various date formats and normalizes name
#' casing and punctuation to ensure robust matching.
#'
#' Matching is performed in this priority order:
#' 1. Exact first and last name match
#' 2. Partial first or last name match
#' 3. Partial match on both names
#' 4. First name and DoB match
#' 5. Exact surname match only
#'
#' The school name matching is handled by the internal function
#' `.match_school_name()`.
#'
#' @param class_list Data frame of class membership data.
#' @param withdrawn_students Data frame of withdrawal form responses, or path to
#'   an Excel file containing them.
#' @param school_link Path to the school reference list Excel file.
#' @param links_category Character vector of categories (for example "Primary")
#'   to filter the reference school list.
#' @param expected_years Numeric vector of expected birth years to validate DoBs.
#' @param do_log Logical; if TRUE, a log file ("Withdrawn_students.log") will be written.
#'
#' @return
#' A list with two elements:
#' * class_list: Updated version of the input class list with a Withdrawn? column.
#' * withdraw: Data frame of withdrawal records, including match details and indices.
#'
#' @seealso .match_school_name(), .process_withdrawn_dob()
#'
#' @export
apply_withdrawn <- function(class_list,
                            withdrawn_students,
                            school_link = "~/Wonde/Establishment_List_ID.xlsx",
                            links_category = "Primary",
                            expected_years = c(2019, 2020),
                            do_log = FALSE) {
  
  if (do_log) logr::log_open("Withdrawn_students.log")
  
  # 1. Read and filter withdrawn students (if file path given)
  if (is.character(withdrawn_students)) {
    withdrawn_students <- readxl::read_xlsx(withdrawn_students) |> as.data.frame()
  }
  
  if ("Please confirm that you do not want your child to take part in the Observatory for Mathematical Education's cohort study:" %in%
      names(withdrawn_students)) {
    withdrawn_students <- withdrawn_students[grepl(
      "^No|^I want to withdraw",
      withdrawn_students$`Please confirm that you do not want your child to take part in the Observatory for Mathematical Education's cohort study:`
    ), ]
  }
  
  # 2. Read school links and subset by category
  links <- readxl::read_xlsx(school_link) |> as.data.frame()
  # links <- links[grep(paste0(links_category, collapse = "|"), links$Category), ]
  
  # 3. Prepare withdrawn student name and school data
  firstName <- withdrawn_students$`Your child's first name` |>
    toupper() |>
    stringr::str_replace_all("-", " ") |>
    stringi::stri_trans_general(id = "Latin-ASCII")
  
  lastName <- withdrawn_students$`Your child's surname` |>
    toupper() |>
    stringr::str_replace_all("-", " ") |>
    stringi::stri_trans_general(id = "Latin-ASCII")
  
  SchoolName <- withdrawn_students$`Name of school` |> toupper()
  
  # 4. Process date of birth (via internal helper)
  DoB <- .process_withdrawn_dob(withdrawn_students, expected_years)
  
  withdraw <- data.frame(firstName, lastName, DoB, SchoolName)
  
  if (do_log) {
    logr::log_print("Cleaned withdraw students data frame:")
    logr::log_print(withdraw)
  }
  
  # 5. Prepare output class list and tracking variables
  class_list$`Withdrawn?` <- rep("", nrow(class_list))
  if (!"Print ID" %in% names(class_list)) class_list$`Print ID` <- ""
  if (!"OME_ID" %in% names(class_list)) class_list$OME_ID <- ""
  
  match_desc <- rep(NA, nrow(withdraw))
  school_look <- rep(NA, nrow(withdraw))
  classListIndex <- rep(NA, nrow(withdraw))
  matchedFirstName <- rep(NA, nrow(withdraw))
  matchedLastName <- rep(NA, nrow(withdraw))
  matchedSchoolName <- rep(NA, nrow(withdraw))
  
  # 6. Loop through each withdrawn student
  for (i in seq_len(nrow(withdraw))) {
    print(i)
    if (do_log) {
      logr::log_print("Current withdrawal:")
      logr::log_print(withdraw[i, ])
    }
    
    cur <- withdraw[i, ]
    
    # Find best matching school
    if (do_log) logr::log_print("Find the best school match...")
    school_matches <- .match_school_name(cur$SchoolName, links)
    
    school_look[i] <- paste0(school_matches$SchoolName, collapse = "\n")
    if (do_log) logr::log_print(school_matches)
    
    if (nrow(school_matches) == 0 ||
        !any(school_matches$URN %in% class_list$URN)) {
      warning_msg <- paste0(
        "Row: ", i,
        ". Unable to withdraw student as matched school '",
        cur$SchoolName,
        "' is not found in the class list. This is usually caused by a school not extracted from Wonde."
      )
      if (do_log) logr::log_warning(warning_msg) else warning(warning_msg)
      match_desc[i] <- "No match, Wonde data for school not available."
      next
    }
    
    potent_index <- which(class_list$URN %in% school_matches$URN)
    potent <- class_list[potent_index, ]
    
    # Helper to fill match details locally
    apply_match <- function(match_index, desc) {
      list(
        match_desc = desc,
        classListIndex = potent_index[match_index],
        matchedFirstName = potent$`Pupil First Name`[match_index],
        matchedLastName = potent$`Pupil Last Name`[match_index],
        matchedSchoolName = potent$`School name`[match_index],
        withdrawn_index = potent_index[match_index]
      )
    }
    
    matched <- NULL
    
    # 1. Exact first + last
    match_index <- which(
      cur$firstName == toupper(potent$`Pupil First Name`) &
        cur$lastName == toupper(potent$`Pupil Last Name`)
    )
    if (length(match_index) == 1) {
      matched <- apply_match(match_index, "Exact first and last name match (to one student)")
    } else if (length(match_index) > 1) {
      match_desc[i] <- "Multiple first and last name match, do not remove any"
      next
    }
    
    # 2. Partial first name + exact last
    if (is.null(matched)) {
      withdraw_FN_parts <- stringr::str_split(cur$firstName, " ") |> unlist()
      match_index <- which(
        grepl(paste(withdraw_FN_parts, collapse = "|"),
              toupper(potent$`Pupil First Name`) |> stringr::str_replace_all("-", " ")) &
          cur$lastName == toupper(potent$`Pupil Last Name`)
      )
      if (length(match_index) == 1) {
        matched <- apply_match(match_index, "Partial first name, exact surname (to one student)")
      }
    }
    
    # 3. Partial last name + exact first
    if (is.null(matched)) {
      withdraw_LN_parts <- stringr::str_split(cur$lastName, " ") |> unlist()
      match_index <- which(
        grepl(paste(withdraw_LN_parts, collapse = "|"),
              toupper(potent$`Pupil Last Name`) |> stringr::str_replace_all("-", " ")) &
          cur$firstName == toupper(potent$`Pupil First Name`)
      )
      if (length(match_index) == 1) {
        matched <- apply_match(match_index, "Partial last name, exact first name (to one student)")
      }
    }
    
    # 4. Partial both
    if (is.null(matched)) {
      match_index <- which(
        grepl(paste(withdraw_LN_parts, collapse = "|"),
              toupper(potent$`Pupil Last Name`) |> stringr::str_replace_all("-", " ")) &
          grepl(paste(withdraw_FN_parts, collapse = "|"),
                toupper(potent$`Pupil First Name`) |> stringr::str_replace_all("-", " "))
      )
      if (length(match_index) == 1) {
        matched <- apply_match(match_index, "Partial last name, partial first name (to one student)")
      }
    }
    
    # 5. First name + DoB
    if (is.null(matched) && !is.na(cur$DoB) && "DoB" %in% names(potent)) {
      match_index <- which(
        cur$firstName == toupper(potent$`Pupil First Name`) &
          cur$DoB == potent$DoB
      )
      if (length(match_index) == 1) {
        matched <- apply_match(match_index, "Exact first name, DoB match (to one student)")
      } else if (length(match_index) > 1) {
        match_desc[i] <- "Multiple first name match, DoB match, do not remove any"
        next
      }
    }
    
    # 6. Surname only
    if (is.null(matched)) {
      match_index <- which(cur$lastName == toupper(potent$`Pupil Last Name`))
      if (length(match_index) == 1) {
        matched <- apply_match(match_index, "Exact match surname only (to one student)")
      }
    }
    
    # Apply match if found
    if (!is.null(matched)) {
      match_desc[i] <- matched$match_desc
      classListIndex[i] <- matched$classListIndex
      matchedFirstName[i] <- matched$matchedFirstName
      matchedLastName[i] <- matched$matchedLastName
      matchedSchoolName[i] <- matched$matchedSchoolName
      class_list$`Withdrawn?`[matched$withdrawn_index] <- "Y"
    } else {
      match_desc[i] <- "Student not found in class list"
    }
  }
  
  if (do_log) logr::log_close()
  
  withdraw$MatchDetail <- match_desc
  withdraw$SchoolCheck <- school_look
  withdraw$classListIndex <- classListIndex
  withdraw$matchedFirstName <- matchedFirstName
  withdraw$matchedLastName <- matchedLastName
  withdraw$matchedSchoolName <- matchedSchoolName
  withdraw$IgnoreResult <- rep(NA, nrow(withdraw))
  
  list(class_list = class_list, withdraw = withdraw)
}



#' @title Process and standardise withdrawn student date of birth information
#' @description
#' Internal helper function for [apply_withdrawn()]. This function reads and
#' cleans the `"Your child's date of birth"` column from a withdrawn students
#' data frame. It supports multiple date formats and handles missing or malformed
#' data gracefully.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Checks whether the `"Your child's date of birth"` column exists.
#'   \item If missing, returns a vector of `NA` values.
#'   \item If present, cleans and parses dates from a wide range of formats:
#'         `"%d/%m/%Y"`, `"%d-%m-%y"`, `"TH"`, `"ST"`, `"ND"`, `"RD"`, month names, etc.
#'   \item Filters out impossible years based on `expected_years`.
#'   \item Returns a vector of best-matched DoB strings per student.
#' }
#'
#' @param withdrawn_students A data frame of withdrawn student records,
#'   typically read from an `.xlsx` file via [readxl::read_xlsx()].
#' @param expected_years Numeric vector of acceptable birth years (e.g. `c(2011, 2012)`).
#'
#' @return
#' A character vector of parsed and validated dates of birth, or `NA` where
#' unavailable or invalid.
#'
#' @keywords internal
#' @noRd
.process_withdrawn_dob <- function(withdrawn_students, expected_years) {
  # If DoB column missing → return NA vector
  if (!("Your child's date of birth" %in% names(withdrawn_students))) {
    return(rep(NA, nrow(withdrawn_students)))
  }
  
  withdrawn_students$`Your child's date of birth` <- withdrawn_students$`Your child's date of birth` |>
    toupper() |> stringr::str_squish()
  
  # Try multiple date formats
  DoB1A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = '%d/%m/%Y')
  DoB1B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = '%d/%m/%y')
  DoB2A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = '%d.%m.%Y')
  DoB2B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = '%d.%m.%y')
  DoB3A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = '%d-%m-%Y')
  DoB3B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = '%d-%m-%y')
  DoB4A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |> as.Date(format = '%d %B %Y')
  DoB4B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |> as.Date(format = '%d %B %y')
  DoB5A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |> as.Date(format = '%B %d %Y')
  DoB5B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |> as.Date(format = '%B %d %y')
  DoB6A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |> as.Date(format = '%b %d %Y')
  DoB6B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |> as.Date(format = '%b %d %y')
  DoB7A <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+|\\.|-|/') |> as.Date(format = '%d%m%Y')
  DoB7B <- withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+|\\.|-|/') |> as.Date(format = '%d%m%y')
  
  DoBs <- data.frame(withdrawn_students$`Your child's date of birth`,
                     DoB1A, DoB1B,
                     DoB2A, DoB2B,
                     DoB3A, DoB3B,
                     DoB4A, DoB4B,
                     DoB5A, DoB5B,
                     DoB6A, DoB6B,
                     DoB7A, DoB7B)
  
  for (i in seq(2, ncol(DoBs), 2)) {
    DoBs[, i][grepl('^[0-9]{2}-', DoBs[, i] |> as.character())] <- NA
  }
  for (i in seq(2, ncol(DoBs), 2)) {
    DoBs[, i + 1][which(!is.na(DoBs[, i]))] <- NA
  }
  for (i in 2:ncol(DoBs)) {
    DoBs[, i][!grepl(paste0(expected_years, collapse = '|'), DoBs[, i])] <- NA
  }
  
  DoB <- apply(DoBs[, -1], 1, function(x) {
    x |> na.omit() |> unique() |> paste0(collapse = ', ')
  })
  
  return(DoB)
}


#' @title Match a school name to a list of known schools
#'
#' @description
#' Internal helper function that finds the most likely match (or matches) for a
#' given school name from a data frame of known schools, based on exact or partial
#' word overlap between the provided name and each school in the list.
#'
#' @details
#' The function first checks for an exact match between the input `school_name`
#' and the `EstablishmentName` field in `schools_df` (case-insensitive and ignoring
#' punctuation). If an exact match is found, it is returned immediately.
#'
#' If no exact match is found, the function:
#' \enumerate{
#'   \item Converts both the input name and the reference names to uppercase.
#'   \item Removes punctuation and splits names into tokens (words).
#'   \item Counts the number of overlapping exact words between the input and each school.
#'   \item Returns the school(s) with the highest overlap.
#' }
#'
#' @param school_name Character string giving the name of a school to match.
#' @param schools_df Data frame containing at least two columns:
#'   \itemize{
#'     \item `EstablishmentName` – the official school name.
#'     \item `URN` – unique reference number for the school.
#'   }
#'
#' @return
#' A data frame with columns:
#' \describe{
#'   \item{`SchoolName`}{Matched establishment name(s) from `schools_df`.}
#'   \item{`URN`}{The corresponding URN(s).}
#' }
#' If no match is found, an empty data frame is returned.
#'
#' @examples
#' \dontrun{
#' links <- data.frame(
#'   EstablishmentName = c("St Mary's Primary School", "Oakfield Academy"),
#'   URN = c("12345", "67890")
#' )
#'
#' # Exact match
#' .match_school_name("St Mary's Primary School", links)
#'
#' # Fuzzy match
#' .match_school_name("St Marys Primary", links)
#' }
#'
#' @keywords internal
#' @noRd
.match_school_name <- function(school_name, schools_df) {
  # Validate inputs
  if (missing(school_name) || is.na(school_name) || trimws(school_name) == "") {
    return(data.frame(SchoolName = character(), URN = character()))
  }
  if (!all(c("EstablishmentName", "URN") %in% names(schools_df))) {
    stop("`schools_df` must contain columns 'EstablishmentName' and 'URN'.")
  }
  
  # Normalize input and comparison names (remove punctuation, uppercase)
  clean_name <- school_name |>
    toupper() |>
    stringr::str_remove_all('[:punct:]') |>
    stringr::str_squish()
  
  clean_schools <- schools_df$EstablishmentName |>
    toupper() |>
    stringr::str_remove_all('[:punct:]') |>
    stringr::str_squish()
  
  # --- ✅ Step 1: Direct exact match check
  exact_match_index <- which(clean_name == clean_schools)
  if (length(exact_match_index) > 0) {
    return(data.frame(
      SchoolName = schools_df$EstablishmentName[exact_match_index],
      URN = schools_df$URN[exact_match_index]
    ))
  }
  
  # --- Step 2: Token-based fuzzy word overlap
  input_words <- clean_name |>
    stringr::str_split("\\s+") |>
    unlist()
  
  school_words <- clean_schools |>
    stringr::str_split("\\s+")
  
  # Count overlapping words
  overlap_counts <- lapply(input_words, function(word) {
    grepl(word, clean_schools)
  }) |> data.frame() |> rowSums(na.rm = TRUE)
  
  exact_match_counts <- lapply(input_words, function(word) {
    lapply(school_words, function(words) word %in% words) |> unlist()
  }) |> data.frame() |> rowSums(na.rm = TRUE)
  
  # Identify best match(es)
  best_match <- which(exact_match_counts == max(exact_match_counts))
  if (length(best_match) == 0) {
    return(data.frame(SchoolName = character(), URN = character()))
  }
  
  data.frame(
    SchoolName = schools_df$EstablishmentName[best_match],
    URN = schools_df$URN[best_match]
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
#' @export
revert_ignored_withdrawals <- function(class_list, withdraw) {
  if (!"IgnoreResult" %in% names(withdraw) || !"classListIndex" %in% names(withdraw)) {
    stop("withdraw must contain 'IgnoreResult' and 'classListIndex' columns.")
  }
  
  # Find which withdrawal records are marked as ignored
  ignore_rows <- which(withdraw$IgnoreResult == "Y" & !is.na(withdraw$classListIndex))
  
  if (length(ignore_rows) == 0) {
    message("No ignored withdrawals found. Returning class_list unchanged.")
    return(class_list)
  }
  
  # Indices in class_list to revert
  revert_indices <- withdraw$classListIndex[ignore_rows]
  
  # Remove withdrawn flag
  class_list$`Withdrawn?`[revert_indices] <- ""
  
  # Optionally restore identifying information (if blanked earlier)
  cols_to_restore <- c("DoB", "UPN", "Print ID", "OME_ID")
  for (col in cols_to_restore) {
    if (col %in% names(class_list)) {
      # Convert empty strings back to NA for these records
      class_list[[col]][revert_indices][class_list[[col]][revert_indices] == ""] <- NA
    }
  }
  
  message(length(revert_indices), " withdrawals reverted (IgnoreResult == 'Y').")
  return(class_list)
}


#' @title Clear identifying data for withdrawn students
#' @description
#' For any students marked as withdrawn (i.e., rows where the
#' \code{'Withdrawn?'} column equals \code{'Y'}), this function removes
#' identifying information by setting selected columns to \code{NA}.
#' 
#' Specifically, the function sets the values in \code{'UPN'}, \code{'DoB'},
#' \code{'Print ID'}, and \code{'OME_ID'} to \code{NA}, but only for columns
#' that already exist in the data frame. Missing columns are ignored.
#'
#' @param data A data frame representing the class list.
#' @param WithdrawColumn The name of the column used to indicate withdrawn
#'   students (default: \code{'Withdrawn?'}).
#' @param columns_to_clear A character vector of column names to clear for
#'   withdrawn students. Default: \code{c("UPN", "DoB", "Print ID", "OME_ID")}.
#' @param verbose Logical; if \code{TRUE}, prints a summary message.
#'
#' @return
#' The updated data frame with specified fields cleared for withdrawn students.
#'
#' @examples
#' \dontrun{
#' updated_class_list <- clear_withdrawn_student_data(class_list)
#' }
#'
#' @export
clear_withdrawn_student_data <- function(data,
                                         WithdrawColumn = "Withdrawn?",
                                         columns_to_clear = c("UPN", "DoB", "Print ID", "OME_ID"),
                                         verbose = FALSE) {
  # Identify withdrawn students
  if (!WithdrawColumn %in% names(data)) {
    warning(paste0("Column '", WithdrawColumn, "' not found; no changes made."))
    return(data)
  }
  
  withdrawn_index <- which(data[[WithdrawColumn]] == "Y")
  
  if (length(withdrawn_index) == 0) {
    if (verbose) cli::cli_inform("No withdrawn students found; no changes made.")
    return(data)
  }
  
  # Columns that exist in the dataset
  existing_cols <- columns_to_clear[columns_to_clear %in% names(data)]
  
  if (length(existing_cols) == 0) {
    if (verbose) cli::cli_inform("No matching columns found to clear.")
    return(data)
  }
  
  # Set selected columns to NA for withdrawn students
  for (col in existing_cols) {
    data[[col]][withdrawn_index] <- NA
  }
  
  if (verbose) {
    cli::cli_inform(paste0(
      "Cleared data in columns (",
      paste(existing_cols, collapse = ", "),
      ") for ", length(withdrawn_index), " withdrawn students."
    ))
  }
  
  data
}
