#' Extract a subject-specific class list from Wonde API data
#'
#' @param secondary_data_raw List of school-level data as returned by
#'   \code{get_secondary_school_student_data()}.
#' @param schools Data frame of school metadata containing at least \code{id}, \code{name}, and \code{urn}.
#' @param secondary Data frame defining per-school extraction settings, including
#'   \code{GetMathBySubject} and \code{GetMathByClassDescription}.
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
#'   secondary_data_raw = secondary_data_raw,
#'   schools = schools,
#'   secondary = secondary,
#'   maths_names = c("Mathematics", "Maths"),
#'   maths_desc_names = c("MATH", "NUMERACY"),
#'   year_pattern = "7"
#' )
#'
#' @export
extract_class_list <- function(secondary_data_raw,
                               schools,
                               secondary,
                               maths_names,
                               maths_desc_names,
                               year_pattern,
                               verbose = TRUE) {

  issue_list <- data.frame(matrix(nrow = 0, ncol = 4),
                           stringsAsFactors = FALSE)
  names(issue_list) <- c("Index", "URN", "SchoolName", "Issue")

  out_list <- pbapply::pblapply(seq_along(secondary_data_raw), function(index) {
    if (verbose) message("Processing index ", index)
    ID <- names(secondary_data_raw)[index]
    URN_school <- schools$urn[match(ID, schools$id)]
    school_name <- schools$name[match(ID, schools$id)]

    school_data <- secondary_data_raw[[index]]
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

    StudentId <- unique(yr_students$id)
    StudentFirstName <- student$forename[match(StudentId, student$id)]
    StudentLastName <- student$surname[match(StudentId, student$id)]
    SchoolId <- student$school_id[match(StudentId, student$id)]
    SchoolName <- student$school_name[match(StudentId, student$id)]
    EstablishmentNumber <- student$school_establishment_number[match(StudentId, student$id)]
    URN <- student$school_urn[match(StudentId, student$id)]
    DoB <- student$date_of_birth.date[match(StudentId, student$id)] |>
      stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")

    # --- Add education details (UPN)
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

    # --- Class & subject extraction
    secondary_info <- as.data.frame(secondary[which(secondary$URN == URN_school), ])
    class <- school_data$students_classes
    if (is.null(class)) {
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name, "Missing class data")
      return(student_df)
    }

    yr_class <- class[class$id %in% StudentId, ]

    if (isTRUE(secondary_info$GetMathBySubject)) {
      subject_ids <- school_data$subjects$id[
        which(toupper(school_data$subjects$name) %in% toupper(maths_names))
      ]
      yr_class <- yr_class[yr_class$classes.data__subject %in% subject_ids, ]
    } else if (isTRUE(secondary_info$GetMathByClassDescription)) {
      yr_class <- yr_class[
        grep(paste0(maths_desc_names, collapse = "|"),
             toupper(yr_class$classes.data__description)),
      ]
    } else {
      warning("Index ", index, " (URN: ", URN_school, "): Neither GetMathBySubject nor GetMathByClassDescription is TRUE.")
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name,
                                               "Neither subject nor class description match settings TRUE")
      return(NULL)
    }

    # --- Check for missing students in classes
    if (!all(StudentId %in% yr_class$id)) {
      warning("Index ", index, " (URN: ", URN_school, "): Not all students have a class match.")
      issue_list[nrow(issue_list) + 1, ] <<- c(index, URN_school, school_name,
                                               "Not all students have a class match")
    }

    # --- Required fields
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

    # --- Add teacher name
    if ("classes.data__employees.data__forename" %in% wanted_fields &&
        "classes.data__employees.data__surname" %in% wanted_fields) {
      yr_class$TeacherName <- paste(
        yr_class$classes.data__employees.data__forename,
        yr_class$classes.data__employees.data__surname
      )
    }

    # --- Rename for clarity
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

    # --- Merge student & class info
    final_df <- dplyr::full_join(
      student_df, yr_class,
      by = dplyr::join_by(StudentId == ID),
      keep = TRUE
    )

    return(final_df)
  })

  names(out_list) <- names(secondary_data_raw)
  combined_df <- rbind_aggro(out_list)

  attr(combined_df, "issues") <- issue_list
  return(combined_df)
}
