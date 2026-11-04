#' Detect students missing a class assignment
#'
#' @description
#' Identifies students in the provided class list who do not have a recorded
#' class assignment (i.e., those with missing `ClassId` values). This function
#' is useful for quickly checking data completeness after class extraction or
#' cleaning.
#'
#' @param class_list_df A data frame containing at least the column `ClassId`
#'   and student-level information. Typically the output of
#'   \code{\link{extract_class_list}()} or a subsequent cleaning step.
#'
#' @return
#' A data frame containing only those rows where the `ClassId` field is missing
#' (`NA`), representing students who are not assigned to any class.
#'
#' @details
#' This function is a lightweight diagnostic tool used to flag students
#' who have no class membership information available. It does not modify
#' the input data frame, only filters it.
#'
#' It is commonly used prior to or in conjunction with
#' \code{\link{check_students_missing_class}()} or other validation
#' utilities that check subject-specific or year-specific class assignments.
#'
#' @examples
#' \dontrun{
#' missing_class_students <- detect_students_missing_class(class_list_df)
#' head(missing_class_students)
#' }
#'
#' @export
detect_students_missing_class <- function(class_list_df) {
  
  class_list_df |>
    rename_to_internal_names() |>
    dplyr::filter(is.na(c_wonde_id)) |>
    rename_to_configured_names()
  
}


#' Check for students missing subject class assignments
#'
#' @description
#' Identifies students in the provided class list who have no recorded class
#' assignment (i.e., missing `ClassId`), retrieves their related subject and
#' class data from Wonde exports, and summarises the issue per school. This
#' function provides a clear overview of students who are not currently
#' enrolled in a subject class, such as Maths.
#'
#' @param class_list_df A data frame containing class list information,
#'   typically the output from \code{\link{extract_class_list}()}. Must include
#'   at least the columns `SchoolId`, `SchoolName`, `StudentId`, `ClassId`,
#'   `StudentFirstName`, and `StudentLastName`.
#'
#' @param WondeData A named list of Wonde data frames, usually returned by
#'   \code{\link{get_secondary_school_student_data}()}, where each element
#'   corresponds to a school and includes components such as
#'   `students_classes` and `subjects`.
#'
#' @param subject_name Character string specifying the subject of interest
#'   (e.g., `"Maths"`). This value is used only for output labeling.
#'   Default is `"Maths"`.
#'
#' @param output_prefix Character string specifying a prefix for output
#'   filenames or logs. Default is `"Students_without_subject_class"`.
#'
#' @return
#' A list containing three data frames:
#' \describe{
#'   \item{`missing_students`}{Data frame of students with missing class IDs.}
#'   \item{`class_subject_of_missing`}{Data frame showing available class and
#'   subject details for each missing student, derived from `WondeData`.}
#'   \item{`missing_per_school`}{Summary data frame showing the total number
#'   and percentage of affected students per school.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Uses \code{\link{detect_students_missing_class}()} to identify
#'         students without a class assignment.
#'   \item For each missing student, extracts any available class and subject
#'         information from their corresponding school's `WondeData` entry.
#'   \item Aggregates a school-level summary reporting the total number and
#'         percentage of affected students.
#' }
#'
#' It is typically used as a diagnostic or quality-control step after data
#' extraction, before final analysis or export.
#'
#' @examples
#' \dontrun{
#' missing_summary <- check_students_missing_class(
#'   class_list_df = class_list_df,
#'   WondeData = WondeData,
#'   subject_name = "Maths"
#' )
#'
#' head(missing_summary$missing_per_school)
#' }
#'
#' @export
check_students_missing_class <- function(
    class_list_df,
    WondeData,
    subject_name = "Maths",
    output_prefix = "Students_without_subject_class") {
  
  # -------------------------------------------------------------------------
  # 0. Convert to internal column names
  # -------------------------------------------------------------------------
  class_list_df <- rename_to_internal_names(class_list_df)
  
  # -------------------------------------------------------------------------
  # 1. Identify students without a subject class
  # -------------------------------------------------------------------------
  SS <- detect_students_missing_class(class_list_df) |>
    rename_to_internal_names()
  
  # -------------------------------------------------------------------------
  # 2. Extract classes and subjects for each missing student
  # -------------------------------------------------------------------------
  students_class_subject <- lapply(seq_len(nrow(SS)), function(index) {
    
    cur <- SS[index, , drop = FALSE]
    
    est_id <- as.character(cur$est_id[[1]])
    student_id <- as.character(cur$s_wonde_id[[1]])
    
    school_data <- WondeData[[est_id]]
    
    if (is.null(school_data) ||
        is.null(school_data$students_classes)) {
      return(NULL)
    }
    
    g <- school_data$students_classes
    
    g <- g[
      !is.na(g$id) & as.character(g$id) == student_id,
      ,
      drop = FALSE
    ]
    
    wanted_cols <- c(
      "school_name",
      "school_urn",
      "id",
      "initials",
      "title",
      "surname",
      "forename",
      "classes.data__name",
      "classes.data__code",
      "classes.data__description",
      "classes.data__subject",
      "classes.data__alternative",
      "classes.data__priority",
      "classes.data__academic_year",
      "classes.data__year_group"
    )
    
    # Select available Wonde fields
    cols_present <- intersect(wanted_cols, names(g))
    
    out <- g[, cols_present, drop = FALSE]
    
    # Map subject IDs to subject names
    if ("classes.data__subject" %in% names(out) &&
        !is.null(school_data$subjects) &&
        all(c("id", "name") %in% names(school_data$subjects))) {
      
      out$subject_name <- school_data$subjects$name[
        match(
          out$classes.data__subject,
          school_data$subjects$id
        )
      ]
      
    } else {
      out$subject_name <- rep(NA_character_, nrow(out))
    }
    
    out
  })
  
  # Combine individual student results
  students_class_subject <- Filter(Negate(is.null), students_class_subject)
  
  miss_student_class_subject <- if (length(students_class_subject) > 0) {
    rbind_aggro(students_class_subject)
  } else {
    data.frame()
  }
  
  # -------------------------------------------------------------------------
  # 3. Compute per-school issue summary
  # -------------------------------------------------------------------------
  issue_breakdown <- class_list_df |>
    dplyr::group_by(
      est_wonde_id,
      est_wonde_name,
      est_wonde_urn
    ) |>
    dplyr::summarise(
      total_student = dplyr::n_distinct(s_wonde_id),
      
      total_with_issue = dplyr::n_distinct(
        s_wonde_id[is.na(c_wonde_id)]
      ),
      
      issue_percent = dplyr::if_else(
        total_student > 0,
        round(total_with_issue / total_student * 100, 1),
        NA_real_
      ),
      
      students = paste(
        paste(
          sfn[is.na(c_wonde_id)],
          sln[is.na(c_wonde_id)]
        ),
        collapse = "\n"
      ),
      
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_with_issue))
  
  # Convert configured columns back to external naming convention
  SS <- rename_to_configured_names(SS)
  
  issue_breakdown <- rename_to_configured_names(issue_breakdown)
  
  # -------------------------------------------------------------------------
  # 4. Return structured output
  # -------------------------------------------------------------------------
  return(list(
    missing_students = SS,
    class_subject_of_missing = miss_student_class_subject,
    missing_per_school = issue_breakdown
  ))
}

#' Check and summarize multiple class memberships
#'
#' @description
#' Identifies and summarizes cases where students are assigned to multiple
#' classes for the same subject within schools. This is commonly used to detect
#' overlapping or duplicated class memberships (for example, students appearing
#' in multiple mathematics classes). The function creates per-school summaries
#' and writes them to an Excel workbook for manual review.
#'
#' @param class_list_df A data frame containing the combined class list across
#'   all schools. Must include at least the columns `"Class Name"` and `"URN"`.
#'
#' @param schools A data frame containing school metadata, with at least the
#'   columns `id`, `URN`, and `name`.
#'
#' @param WondeData A named list of Wonde school extracts, such as those returned
#'   by `get_secondary_school_student_data()`. Each list element should include
#'   at least the components `student`, `students_classes`, and `subjects`.
#'
#' @param year_codes_target Character vector of year codes used to identify
#'   target students (for example, `c("Year 7", "7", "Y7")`).
#'
#' @param subject_keywords Character vector of subject name keywords
#'   (case-insensitive) used to select relevant subject classes, typically for
#'   a specific subject such as mathematics.
#'
#' @param output_prefix Character string giving the file name prefix for the
#'   generated Excel report. Default is `"Checking_subject_classes_overlap"`.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Cleans and restructures the input `class_list_df` using helper
#'         functions: `clean_class_names()`, `reduce_to_best_teacher()`, and
#'         `produce_class_lists()`.
#'   \item Identifies schools with students listed in multiple classes (based on
#'         comma-separated `"Class Name"` entries).
#'   \item For each school, filters students to those with year codes in
#'         `year_codes_target` and subjects matching `subject_keywords`.
#'   \item Summarizes each class's description, size, and student overlap counts.
#'   \item Exports the results to an Excel workbook (`*_before_fix.xlsx`), with
#'         one sheet per school showing class size and overlap information.
#' }
#'
#' @section Excel output structure:
#' The exported workbook (named using `output_prefix`) contains one worksheet per
#' school. Each worksheet includes a summary table with the following columns:
#' \describe{
#'   \item{`classes.data__name`}{The name of the class.}
#'   \item{`classes_desc`}{The class description field from Wonde data, if available.}
#'   \item{`size`}{The number of unique students assigned to the class.}
#'   \item{`classes_overlap`}{A newline-separated list showing which other classes
#'         share students with the given class, in the format
#'         `"OtherClassName -> N"`, where `N` is the number of overlapping students.
#'         If no overlaps are found, the value is `"No other"`.}
#' }
#'
#' The workbook file is saved as:
#' \preformatted{
#'   <output_prefix>_before_fix.xlsx
#' }
#'
#' and can be used to manually inspect or validate data cleaning results.
#'
#' @return
#' A named list in which each element corresponds to a school and contains:
#' \describe{
#'   \item{`class_stud`}{A data frame summarizing each class's description,
#'         student count, and overlap information.}
#' }
#'
#' @seealso
#' [openxlsx::createWorkbook()], [openxlsx::writeDataTable()],
#' [clean_class_names()], [reduce_to_best_teacher()], [produce_class_lists()]
#'
#' @examples
#' \dontrun{
#' overlaps <- check_multiple_class_memberships(
#'   class_list_df = class_list_df,
#'   schools = schools,
#'   WondeData = WondeData,
#'   year_codes_target = c("Year 7", "7", "Y7"),
#'   subject_keywords = c("Maths", "Mathematics"),
#'   output_prefix = "maths_class_overlap"
#' )
#'
#' # View the first school's summary
#' overlaps[[1]]$class_stud
#' }
#'
#' @export
check_multiple_class_memberships <- function(class_list_df,
                                           schools,
                                           WondeData,
                                           year_codes_target = c("Year 7", "7", "Y7"),
                                           subject_keywords = c("Maths", "Mathematics", "Ma", "Mathematic"),
                                           output_prefix = "Checking/subject_classes_overlap") {
  # -------------------------------------------------------------------
  # 0. Prepare subject and year code filters
  # -------------------------------------------------------------------
  subject_upper <- toupper(subject_keywords)
  
  # -------------------------------------------------------------------
  # 1. Build initial class list using user-defined pipeline functions
  # -------------------------------------------------------------------
  AA = detect_multiple_class_memberships(class_list_df) |> rename_to_internal_names()
  est_ids <- schools$EstablishmentID[schools$URN %in% AA$est_wonde_urn]
  names_schools <- schools$name[schools$URN %in% AA$est_wonde_urn]
  names_urn <- schools$URN[schools$URN %in% AA$est_wonde_urn]
  
  ids_OG = est_ids ; names_schools_OG = names_schools ; names_urn_OG = names_urn
  # -------------------------------------------------------------------
  # 2. Build breakdown of overlapping classes
  # -------------------------------------------------------------------
  class_breakdown <- lapply(seq_along(est_ids), function(index) {
    example <- WondeData[[est_ids[index]]]
    
    ex_year_ids <- get_student_ids_target_year(example,year_codes_target = year_codes_target,type = "grep")
    # ex_year_ids <- example$student$id[example$student$year.data.name %in% year_codes_target]
    
    ex_class_year <- example$students_classes[example$students_classes$id %in% ex_year_ids, ]
    subject_ids <- example$subjects$id[toupper(example$subjects$name) %in% subject_upper]
    year_subject_classes <- ex_class_year[ex_class_year$classes.data__subject %in% subject_ids, ]
    
    class_stud <- year_subject_classes |>
      dplyr::group_by(classes.data__id, classes.data__name) |>
      dplyr::summarise(classes_desc = unique(classes.data__description),
                       size = length(unique(id)),
                       students = paste0(unique(id), collapse = ", "),
                       teachers = classes.data__employees.data__id |> unique() |> paste0(collapse = ', '))
    
    # Calculate overlaps
    classes_sum <- rep(NA, nrow(class_stud))
    for (i in seq_len(nrow(class_stud))) {
      ids_here <- year_subject_classes$id[year_subject_classes$classes.data__name == class_stud$classes.data__name[i]]
      classes_tbl <- year_subject_classes[year_subject_classes$id %in% ids_here, c("id", "classes.data__name")] |> unique()
      tab <- table(classes_tbl$classes.data__name)
      tab <- tab[!names(tab) %in% class_stud$classes.data__name[i]]
      classes_sum[i] <- paste0(names(tab), " -> ", as.numeric(tab), collapse = "\n")
    }
    classes_sum[classes_sum == " -> "] <- "No other"
    class_stud$classes_overlap <- classes_sum
    
    list(class_stud = class_stud)
  })
  names(class_breakdown) <- est_ids
  
  class_breakdown <- class_breakdown[order(names(class_breakdown))]
  # -------------------------------------------------------------------
  # 3. Write initial overlap summary workbook
  # -------------------------------------------------------------------
  wb1 <- openxlsx::createWorkbook()
  for (i in seq_along(class_breakdown)) {
    sh_name <- substr(names(class_breakdown)[i], 1, 20)
    openxlsx::addWorksheet(wb1, sh_name)
    dd <- class_breakdown[[i]]$class_stud
    dd <- dd[order(as.numeric(dd$size), decreasing = TRUE), ]
    dd$students <- NULL
    openxlsx::writeDataTable(wb1, sheet = sh_name, x = dd)
  }
  openxlsx::saveWorkbook(wb1,
                         file = paste0(output_prefix, "_before_fix.xlsx"),
                         overwrite = TRUE)
  message("Saved overlap summary to ", output_prefix, "_before_fix.xlsx")
  
  class_breakdown
}

#' Detect Students with Multiple Class Memberships
#'
#' @description
#' Identifies students who belong to more than one class within the
#' provided class list. Students are grouped using their school
#' identifiers and personal information, and the number of distinct
#' classes associated with each student is calculated.
#'
#' Column names are resolved using the package's configurable
#' column-name system.
#'
#' @param class_list_df A data frame containing class-list information,
#'   typically produced by `extract_class_list()`. The following
#'   internal columns (or their configured equivalents) are required:
#'   `est_wonde_id`, `est_wonde_name`, `est_wonde_urn`,
#'   `s_wonde_id`, `sfn`, `sln`, `dob`, `upn` and `c_wonde_id`.
#'
#' @return
#' A data frame containing students associated with more than one
#' distinct class, with the following columns:
#' \describe{
#'   \item{Establishment and student fields}{The school and student
#'     identifiers used for grouping, returned using their
#'     configured column names.}
#'   \item{classes}{Comma-separated list of unique class IDs.}
#'   \item{no_classes}{Number of distinct classes associated
#'     with the student.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts configured column names to internal names.
#'   \item Groups records by school and student identifiers.
#'   \item Combines unique class IDs into a comma-separated string.
#'   \item Counts distinct class IDs.
#'   \item Retains only students associated with more than one class.
#'   \item Restores configured column names.
#' }
#'
#' Class memberships are identified using `c_wonde_id`, rather than
#' class names. This avoids treating different names for the same
#' class as separate memberships.
#'
#' @examples
#' \dontrun{
#' multi_members <- detect_multiple_class_memberships(class_list_df)
#' head(multi_members)
#' }
#'
#' @export
detect_multiple_class_memberships <- function(class_list_df) {
  
  # Convert to internal column names
  class_list_df <- rename_to_internal_names(class_list_df)
  
  out <- class_list_df |>
    dplyr::group_by(
      est_wonde_id, est_wonde_name, est_wonde_urn,
      s_wonde_id, sfn, sln, dob, upn
    ) |>
    dplyr::summarise(
      classes = paste(unique(c_wonde_id), collapse = ", "),
      no_classes = dplyr::n_distinct(c_wonde_id),
      .groups = "drop"
    ) |>
    dplyr::filter(no_classes > 1)
  
  # Convert back to configured column names
  out <- rename_to_configured_names(out)
  
  return(out)
}

#' Get students from a school
#'
#' @description
#' Extracts student data for a given school name from the supplied Wonde dataset.
#'
#' @param SchoolName Character. The name of the school to retrieve students for.
#' @param WondeData List. A list object containing school-level Wonde data (not directly used but retained for consistency).
#' @param schools Data frame. A table mapping school names to their unique IDs (must include columns `id` and `name`).
#'
#' @details
#' The function matches the provided `SchoolName` to its corresponding `id` in the `schools` table, then selects
#' the relevant student information from `secondary_data_raw` for that school.
#'
#' @return
#' A tibble containing student information with the following columns:
#' `school_name`, `forename`, `surname`, `legal_forename`, `legal_surname`, `date_of_birth.date`, and `year.data.name`.
#'
#' @examples
#' \dontrun{
#' get_students_from_school(
#'   SchoolName = "Springfield High School",
#'   WondeData = secondary_data_raw,
#'   schools = schools_list
#' )
#' }
#'
#' @export
get_students_from_school <- function(SchoolName,
                                     WondeData,
                                     schools){
  school_id = schools$id[match(SchoolName, schools$name)]
  
  WondeData[[school_id]][["student"]] |>
    dplyr::select(
      school_name,
      forename,
      surname,
      legal_forename,
      legal_surname,
      date_of_birth.date,
      year.data.name
    )
}
