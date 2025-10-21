#' Extract a subject-specific class list from Wonde API data
#'
#' @param WondeData List of school-level data as returned by
#'   \code{get_secondary_school_student_data()}.
#' @param schools Data frame of school metadata containing at least \code{id}, \code{name}, and \code{urn}.
#' @param maths_names Character vector of subject names to match (used when
#'   \code{GetMathBySubject = TRUE}).
#' @param maths_desc_names Character vector of text fragments to match in class
#'   descriptions (used when \code{GetMathByClassDescription = TRUE}).
#' @param year_pattern Character or numeric pattern used to match student year codes
#'   (e.g., `"7"` for Year 7 or `"8"` for Year 8).
#' @param verbose Logical; if \code{TRUE}, prints progress.
#'
#' @return A data frame combining all matched student, class, and teacher information
#'   across schools, or \code{NULL} if no valid data are found.
#'
#' @examplesIf FALSE
#' class_list_df <- extract_class_list(
#'   WondeData = WondeData,
#'   schools = schools,
#'   secondary = secondary,
#'   maths_names = c("Mathematics", "Maths"),
#'   maths_desc_names = c("MATH", "NUMERACY"),
#'   year_pattern = "7"
#' )
#'
#' @export
extract_class_list <- function(WondeData,
                               schools,
                               maths_names,
                               maths_desc_names,
                               year_pattern,
                               verbose = TRUE) {
  
  issue_list <- data.frame(matrix(nrow = 0, ncol = 4),
                           stringsAsFactors = FALSE)
  names(issue_list) <- c("Index", "URN", "SchoolName", "Issue")
  
  out_list <- pbapply::pblapply(seq_along(WondeData), function(index) {
    if (verbose) message("Processing index ", index)
    
    ID <- names(WondeData)[index]
    URN_school <- schools$URN[match(ID, schools$id)]
    school_name <- schools$name[match(ID, schools$id)]
    school_data <- WondeData[[index]]
    
    # ----------------------------------------------------------
    # 1. Basic checks
    # ----------------------------------------------------------
    if (is.null(school_data$student)) {
      warning("Index ", index, " (", URN_school, "): Missing student data.")
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name, "Missing student data")
      return(NULL)
    }
    
    student <- school_data$student
    yr_students <- student[grep(year_pattern, student$year.data.code), ]
    if (nrow(yr_students) == 0) {
      warning("Index ", index, " (", URN_school, "): No students match year pattern '", year_pattern, "'.")
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name,
                                               paste0("No students matching year pattern ", year_pattern))
      return(NULL)
    }
    
    # ----------------------------------------------------------
    # 2. Extract student details
    # ----------------------------------------------------------
    StudentId <- unique(yr_students$id)
    StudentFirstName <- as.character(yr_students$forename)
    StudentLastName <- as.character(yr_students$surname)
    SchoolId <- as.character(yr_students$school_id)
    SchoolName <- as.character(yr_students$school_name)
    EstablishmentNumber <- as.character(yr_students$school_establishment_number)
    URN <- as.character(yr_students$school_urn)
    DoB <- stringr::str_extract(yr_students$date_of_birth.date, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
    
    # Education details (UPN)
    ed_details <- school_data$education_details
    UPN <- if (!is.null(ed_details)) {
      ed_details$education_details.data.upn[match(StudentId, ed_details$id)]
    } else {
      NA
    }
    
    student_df <- data.frame(
      SchoolId, SchoolName, EstablishmentNumber, URN,
      StudentId, StudentFirstName, StudentLastName, DoB, UPN,
      stringsAsFactors = FALSE
    )
    
    # ----------------------------------------------------------
    # 3. Class and subject extraction
    # ----------------------------------------------------------
    school_info <- as.data.frame(schools[which(schools$URN == URN_school), ])
    class <- school_data$students_classes
    
    if (is.null(class)) {
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name, "Missing class data")
      return(student_df)
    }
    
    yr_class <- class[class$id %in% StudentId, ]
    
    # Determine method of finding subject
    if (isTRUE(school_info$GetSubjectByCode)) {
      subject_ids <- school_data$subjects$id[
        which(toupper(school_data$subjects$name) %in% toupper(maths_names))
      ]
      yr_class <- yr_class[yr_class$classes.data__subject %in% subject_ids, ]
      
    } else if (isTRUE(school_info$GetSubjectByClassDescription)) {
      yr_class <- yr_class[
        grep(paste0(maths_desc_names, collapse = "|"),
             toupper(yr_class$classes.data__description)), ]
      
    } else {
      warning("Index ", index, " (URN: ", URN_school, "): Neither GetSubjectByCode nor GetSubjectByClassDescription is TRUE.")
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name,
                                               "Neither subject nor class description match settings TRUE")
      return(NULL)
    }
    
    # ----------------------------------------------------------
    # 4. Check for missing students in classes
    # ----------------------------------------------------------
    if (!all(StudentId %in% yr_class$id)) {
      warning("Index ", index, " (URN: ", URN_school, "): Not all students have a class match.")
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name,
                                               "Not all students have a class match")
    }
    
    # ----------------------------------------------------------
    # 5. Ensure required fields exist
    # ----------------------------------------------------------
    wanted_fields <- c(
      "id", "classes.data__id", "classes.data__name",
      "classes.data__employees.data__id",
      "classes.data__employees.data__surname",
      "classes.data__employees.data__forename",
      "classes.data__employees.data__meta.is_main_teacher"
    )
    missing <- setdiff(wanted_fields, names(yr_class))
    if (length(missing) > 0) {
      warning("Index ", index, " (URN: ", URN_school, "): Missing fields -> ",
              paste(missing, collapse = ", "))
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name,
                                               paste("Missing fields:", paste(missing, collapse = ", ")))
      wanted_fields <- setdiff(wanted_fields, missing)
    }
    yr_class <- yr_class[wanted_fields]
    
    # ----------------------------------------------------------
    # 6. Add teacher names and clean names
    # ----------------------------------------------------------
    if (all(c("classes.data__employees.data__forename", "classes.data__employees.data__surname") %in% names(yr_class))) {
      yr_class$TeacherName <- paste(
        yr_class$classes.data__employees.data__forename,
        yr_class$classes.data__employees.data__surname
      )
    }
    
    rename_map <- c(
      "id" = "ID",
      "classes.data__id" = "ClassId",
      "classes.data__name" = "ClassName",
      "classes.data__employees.data__id" = "TeacherId",
      "classes.data__employees.data__surname" = "TeacherSurName",
      "classes.data__employees.data__forename" = "TeacherForeName",
      "classes.data__employees.data__meta.is_main_teacher" = "MainTeacher"
    )
    names(yr_class) <- dplyr::recode(names(yr_class), !!!rename_map)
    yr_class <- unique(yr_class)
    
    # ----------------------------------------------------------
    # 7. Merge student and class info
    # ----------------------------------------------------------
    final_df <- dplyr::full_join(
      student_df, yr_class,
      by = dplyr::join_by(StudentId == ID),
      keep = TRUE
    )
    
    return(final_df)
  })
  
  # ----------------------------------------------------------
  # 8. Combine all results
  # ----------------------------------------------------------
  names(out_list) <- names(WondeData)
  combined_df <- rbind_aggro(out_list)
  
  attr(combined_df, "issues") <- issue_list
  return(combined_df)
}



#' Extract class lists for schools using class or registration group structures
#'
#' This function creates a detailed class list for schools where teaching groups
#' are organised by *classes* or *registration groups* rather than by subjects.
#' It compiles student, class, and teacher information for the specified year
#' code(s) across all schools included in the provided Wonde dataset list.
#'
#' The function supports two data structures:
#' \itemize{
#'   \item \strong{Class structure:} where students are linked via \code{students_classes}.
#'   \item \strong{Registration group structure:} where students are linked via
#'         \code{students_group} with \code{groups.data__type == "REGISTRATION"}.
#' }
#'
#' @param WondeData A named list of school datasets, typically generated by Wonde
#'   data retrieval functions (e.g., \code{\link{get_primary_school_student_data}}). Each
#'   element represents one school's full dataset.
#' @param schools A data frame containing metadata for the schools, including at least
#'   the columns:
#'   \describe{
#'     \item{id}{Wonde school ID (matches names of \code{WondeData})}
#'     \item{urn}{Unique Reference Number}
#'     \item{classes}{Indicator or description of whether class data are available}
#'     \item{registration_groups}{Indicator or description of whether registration group data are available}
#'   }
#' @param yearCodes Character vector of year codes identifying the students to include
#'   (e.g. \code{c("YR", "R")} for Reception, \code{"Y1"} for Year 1, etc.).
#' @param verbose Logical (default = TRUE). Whether to print progress messages and
#'   warnings for missing or inconsistent data.
#'
#' @return A data frame containing class list information for the selected year
#'   code(s) across all schools, with columns:
#' \describe{
#'   \item{SchoolId}{Wonde school ID}
#'   \item{SchoolName}{School name}
#'   \item{EstablishmentNumber}{Establishment number from Wonde}
#'   \item{URN}{Unique Reference Number}
#'   \item{UPN}{Unique pupil number (if available)}
#'   \item{StudentId}{Wonde student ID}
#'   \item{StudentFirstName}{Student forename}
#'   \item{StudentLastName}{Student surname}
#'   \item{DoB}{Student date of birth (YYYY-MM-DD)}
#'   \item{ClassId}{Class or registration group ID}
#'   \item{ClassName}{Class or group name/code}
#'   \item{TeacherId}{Teacher’s Wonde ID}
#'   \item{TeacherForeName}{Teacher’s forename}
#'   \item{TeacherSurName}{Teacher’s surname}
#'   \item{TeacherName}{Combined teacher name}
#' }
#'
#' @details
#' The function first attempts to build the class list using the \code{students_classes}
#' dataset. If no valid class data are available for a school, it falls back to
#' using registration group data from \code{students_group}. Warnings are issued
#' for schools or students that cannot be matched across datasets.
#'
#' @examples
#' \dontrun{
#' # Extract Reception class lists across all schools using class/group structures
#' reception_classes <- extract_class_list_classes_groups(
#'   WondeData = school_data_raw,
#'   schools = schools_metadata,
#'   yearCodes = c("R", "YR")
#' )
#'
#' # Extract Year 1 class lists
#' y1_classes <- extract_class_list_classes_groups(
#'   WondeData = school_data_raw,
#'   schools = schools_metadata,
#'   yearCodes = "Y1"
#' )
#' }
#'
#' @seealso
#' \code{\link{extract_class_list}} for the version that uses subject-based structures
#' (typically for secondary schools).
#'
#' @export
extract_class_list_classes_groups <- function(WondeData,
                                              schools,
                                              yearCodes,
                                              verbose = TRUE) {
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  
  # Main loop: each iteration returns both data and issues
  results <- pbapply::pblapply(seq_along(WondeData), function(index) {
    ID <- names(WondeData)[index]
    URN_school <- schools$URN[match(ID, schools$id)]
    SchoolName <- schools$name[match(ID, schools$id)]
    msg("Processing school ", index, ": ", URN_school)
    
    issues <- data.frame(Index = integer(), URN = character(),
                         SchoolName = character(), Issue = character(),
                         stringsAsFactors = FALSE)
    
    x <- WondeData[[index]]
    
    # ---- Missing student dataset ----
    if (is.null(x$student)) {
      warning("Index ", index, " (URN: ", URN_school, "): Missing student data.")
      issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                         SchoolName = SchoolName,
                                         Issue = "Missing student data",
                                         stringsAsFactors = FALSE))
      return(list(data = NULL, issues = issues))
    }
    
    student <- x$student
    year_students <- subset(student, year.data.code %in% yearCodes)
    
    if (nrow(year_students) == 0) {
      warning("Index ", index, " (URN: ", URN_school, "): No students found for yearCodes.")
      issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                         SchoolName = SchoolName,
                                         Issue = "No students for selected year(s)",
                                         stringsAsFactors = FALSE))
      return(list(data = NULL, issues = issues))
    }
    
    # ---- Student details ----
    StudentId <- unique(year_students$id)
    StudentFirstName <- student$forename[match(StudentId, student$id)]
    StudentLastName <- student$surname[match(StudentId, student$id)]
    SchoolId <- as.character(student$school_id[match(StudentId, student$id)])
    SchoolName <- as.character(student$school_name[match(StudentId, student$id)])
    EstablishmentNumber <- as.character(student$school_establishment_number[match(StudentId, student$id)])
    URN <- as.character(student$school_urn[match(StudentId, student$id)])
    DoB <- stringr::str_extract(student$date_of_birth.date[match(StudentId, student$id)],
                                "[0-9]{4}-[0-9]{2}-[0-9]{2}")
    
    ed_details <- x$education_details
    UPN <- if (!is.null(ed_details)) {
      ed_details$education_details.data.upn[match(StudentId, ed_details$id)]
    } else {
      NA
    }
    
    data <- data.frame(
      SchoolId, SchoolName, EstablishmentNumber, URN, UPN,
      StudentId, StudentFirstName, StudentLastName, DoB,
      stringsAsFactors = FALSE
    )
    
    school_info <- schools[schools$URN == URN_school, , drop = FALSE]
    
    # ---- Classes route ----
    if (!is.null(school_info$class_desc_target_year) &&
        school_info$class_desc_target_year != "No class description available") {
      class <- x$students_classes
      if (is.null(class)) {
        warning("Index ", index, " (URN: ", URN_school, "): Missing class data.")
        issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                           SchoolName = SchoolName,
                                           Issue = "Missing class data",
                                           stringsAsFactors = FALSE))
        return(list(data = data, issues = issues))
      }
      
      rec_class <- subset(class, id %in% StudentId)
      if (all(is.na(rec_class$classes.data__code))) {
        warning("URN ", URN_school, ": classes exist but none assigned.")
        issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                           SchoolName = SchoolName,
                                           Issue = "Classes exist but none assigned",
                                           stringsAsFactors = FALSE))
      } else if (!all(StudentId %in% rec_class$id)) {
        warning("URN ", URN_school, ": Not all students found in class dataset.")
        issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                           SchoolName = SchoolName,
                                           Issue = "Not all students found in class dataset",
                                           stringsAsFactors = FALSE))
      }
      
      wanted_fields <- c(
        "id", "classes.data__id", "classes.data__code",
        "classes.data__employees.data__id",
        "classes.data__employees.data__surname",
        "classes.data__employees.data__forename",
        "classes.data__employees.data__meta.is_main_teacher"
      )
      rec_class <- ensure_columns(rec_class, wanted_fields)
      
      rec_class$TeacherName <- paste(
        rec_class$classes.data__employees.data__forename,
        rec_class$classes.data__employees.data__surname
      )
      names(rec_class) <- c(
        "ID", "ClassId", "ClassName",
        "TeacherId", "TeacherSurName", "TeacherForeName",
        "MainTeacher", "TeacherName"
      )[seq_along(names(rec_class))]
      
      data <- dplyr::full_join(data, rec_class, by = dplyr::join_by(StudentId == ID), keep = TRUE)
      
    } else if (isTRUE(school_info$class_desc_target_year == "No class description available") &&
               !school_info$target_year_registration_groups %in%
               c("groups data not available", "No registration coding available")) {
      
      # ---- Registration groups route ----
      group <- x$students_group
      if (is.null(group)) {
        warning("Index ", index, " (URN: ", URN_school, "): Missing registration group data.")
        issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                           SchoolName = SchoolName,
                                           Issue = "Missing registration group data",
                                           stringsAsFactors = FALSE))
        return(list(data = data, issues = issues))
      }
      
      rec_group <- subset(group, id %in% StudentId & groups.data__type == "REGISTRATION")
      if (!all(StudentId %in% rec_group$id)) {
        warning("URN ", URN_school, ": Not all students found in registration group dataset.")
        issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                           SchoolName = SchoolName,
                                           Issue = "Not all students found in registration group dataset",
                                           stringsAsFactors = FALSE))
      }
      
      wanted_fields <- c(
        "id", "groups.data__id", "groups.data__name",
        "groups.data__employees.data__id",
        "groups.data__employees.data__forename",
        "groups.data__employees.data__surname",
        "groups.data__employees.data__meta.is_main_teacher"
      )
      rec_group <- ensure_columns(rec_group, wanted_fields)
      
      rec_group$TeacherName <- paste(
        rec_group$groups.data__employees.data__forename,
        rec_group$groups.data__employees.data__surname
      )
      names(rec_group) <- c(
        "ID", "ClassId", "ClassName",
        "TeacherId", "TeacherForeName", "TeacherSurName", "MainTeacher", "TeacherName"
      )[seq_along(names(rec_group))]
      
      data <- dplyr::full_join(data, rec_group, by = dplyr::join_by(StudentId == ID), keep = TRUE)
    } else {
      warning("Index ", index, " (URN: ", URN_school, "): No valid class or group structure found.")
      issues <- rbind(issues, data.frame(Index = index, URN = URN_school,
                                         SchoolName = SchoolName,
                                         Issue = "No valid class/group structure found",
                                         stringsAsFactors = FALSE))
    }
    
    list(data = data, issues = issues)
  })
  
  # Combine outputs
  class_list_raw <- lapply(results, `[[`, "data")
  issue_list <- do.call(rbind, lapply(results, `[[`, "issues"))
  
  names(class_list_raw) <- names(WondeData)
  class_list_df <- do.call(rbind, class_list_raw)
  class_list_df$TeacherName[class_list_df$TeacherName %in% c("NA NA", "NA")] <- NA
  
  # Attach issues as attribute
  attr(class_list_df, "issues") <- issue_list
  
  # Summary
  n_issues <- nrow(issue_list)
  n_schools <- length(WondeData)
  if (isTRUE(verbose)) {
    msg("------------------------------------------------------------")
    msg("Processed ", n_schools, " schools.")
    msg("Found ", n_issues, " issue(s) across ", length(unique(issue_list$URN)), " school(s) with problems.")
    msg("------------------------------------------------------------")
  }
  
  class_list_df
}

