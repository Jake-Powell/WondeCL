#' Reduce duplicated student-teacher records to best (main) teacher
#'
#' @description
#' Filters a class list data frame to keep only one record per student,
#' prioritizing those associated with the main teacher. If multiple records
#' exist for the same student, the record marked with `MainTeacher == TRUE`
#' is retained. Students without a main teacher entry are kept once using
#' their first available record.
#'
#' @param data A data frame containing at least the columns `StudentId` and
#'   `MainTeacher`. The `MainTeacher` column should be logical (`TRUE` for
#'   the main teacher, `FALSE` otherwise).
#'
#' @return
#' A data frame reduced to one record per student, preferring main teacher
#' entries where available.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Replaces any missing (`NA`) values in `MainTeacher` with `FALSE`.
#'   \item Splits the dataset into two subsets: one for main teachers and one
#'         for other teachers.
#'   \item For each student, keeps only the main teacher record if available;
#'         otherwise keeps the first non-main-teacher record.
#'   \item Warns if the total number of unique students decreases after
#'         filtering (which may indicate inconsistent or malformed data).
#' }
#'
#' The function is typically used after joining student-class-teacher data
#' and before producing summary class lists with functions such as
#' `produce_class_lists()`.
#'
#' @examples
#' \dontrun{
#' reduced <- reduce_to_best_teacher(class_list_df)
#' }
#'
#' @export
reduce_to_best_teacher <- function(data) {
  # Replace missing MainTeacher with FALSE
  data$MainTeacher[is.na(data$MainTeacher)] <- FALSE
  
  # Split by main teacher flag
  to_keep <- data[which(as.logical(data$MainTeacher)), ]
  to_decide <- data[-which(as.logical(data$MainTeacher)), ]
  
  # Identify students already represented by a main teacher
  have_main_index <- which(to_decide$StudentId %in% to_keep$StudentId)
  
  # Keep all main teachers + students not yet represented
  data_new <- rbind(to_keep, to_decide[-have_main_index, ])
  
  # Safety check: unique student count consistency
  if (length(unique(data$StudentId)) != length(unique(data_new$StudentId))) {
    warning("Reducing teacher has caused student loss!")
  }
  
  return(data_new)
}


#' Clean and standardize class names in school data
#'
#' @description
#' Attempts to clean, fill, and standardize class identifiers and names within
#' a school dataset. Missing or inconsistent class names are replaced with a
#' default name (e.g. `"Reception"`). Classes that contain exactly the same
#' group of students are merged into a single logical class.
#'
#' @param data A data frame containing at least the columns `SchoolId`,
#'   `StudentId`, `ClassId`, and `ClassName`.
#'
#' @param default_class_name Character string specifying the class name to use
#'   when no class name or ID is available. Default is `"Reception"`.
#'
#' @return
#' A data frame identical to the input `data` but with two new columns added:
#' \describe{
#'   \item{`ClassId_use`}{A cleaned class identifier, with missing values
#'   replaced by `<SchoolId>CLASS`.}
#'   \item{`class_name_use`}{A standardized, title-cased class name suitable for
#'   downstream processing.}
#' }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Extracts class-related fields (`SchoolId`, `StudentId`, `ClassId`,
#'         `ClassName`) from the input data.
#'   \item Replaces missing `ClassId` and `ClassName` values with the
#'         `default_class_name` and a synthetic class ID.
#'   \item Checks whether students belong to multiple classes and identifies
#'         sets of classes that share exactly the same student membership.
#'   \item If a set of identical classes belongs entirely to one school and that
#'         school has no other classes, merges them into a single class named
#'         with the `default_class_name`.
#'   \item Produces two new cleaned columns, `ClassId_use` and
#'         `class_name_use`, leaving the original data unchanged.
#' }
#'
#' This function is designed to be used early in the class list cleaning
#' workflow, typically followed by `reduce_to_best_teacher()` and
#' `produce_class_lists()`.
#'
#' @examples
#' \dontrun{
#' cleaned <- clean_class_names(class_list_df, default_class_name = "Year 7")
#' head(cleaned$class_name_use)
#' }
#'
#' @export
clean_class_names <- function(data, default_class_name = "Reception") {
  # Extract class-related info
  required_cols <- c("SchoolId", "StudentId", "ClassId", "ClassName")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ",
         paste(missing_cols, collapse = ", "))
  }
  
  classes <- data[, match(required_cols, names(data))] |> as.data.frame()
  
  # Fill missing class IDs and names
  no_class_ID_index <- which(is.na(classes$ClassId))
  classes$ClassId[no_class_ID_index] <- paste0(classes$SchoolId[no_class_ID_index], "CLASS")
  classes$ClassName[no_class_ID_index] <- default_class_name
  
  # Count classes per student
  no_class_stud <- classes |>
    dplyr::group_by(StudentId) |>
    dplyr::summarise(
      no_class = length(unique(ClassId)),
      classes = paste0(unique(ClassId), collapse = ", ")
    ) |> as.data.frame()
  
  # Identify students in multiple identical classes
  mult <- unique(no_class_stud$classes[grep(", ", no_class_stud$classes)])
  same_students <- lapply(mult, function(classes_string) {
    class_vect <- stringr::str_split(classes_string, ", ") |> unlist()
    students <- lapply(class_vect, function(cla)
      unique(classes$StudentId[classes$ClassId == cla])) |>
      unlist() |> table()
    all(students == length(class_vect))
  }) |> unlist()
  
  to_merge <- mult[same_students]
  if (length(to_merge) > 0) {
    for (i in seq_along(to_merge)) {
      class_vect <- stringr::str_split(to_merge[i], ", ") |> unlist()
      classes_index <- which(classes$ClassId %in% class_vect)
      
      schoolID <- unique(classes$SchoolId[classes_index])
      school_classes <- unique(classes$ClassId[classes$SchoolId == schoolID])
      
      if (setequal(class_vect, school_classes)) {
        classes$ClassName[classes_index] <- default_class_name
      }
    }
  }
  
  # Add cleaned columns
  data$ClassId_use <- classes$ClassId
  data$class_name_use <- classes$ClassName |>
    stringr::str_remove("[A-Za-z ]: ") |>
    stringr::str_remove("[A-Za-z ] - ") |>
    stringr::str_to_title()
  
  return(data)
}


#' Produce final formatted class lists from cleaned student data
#'
#' @description
#' Aggregates cleaned student, class, and teacher data into a single
#' standardized class list suitable for reporting or export. Each student
#' appears once per school, with their associated class, teacher, and unique
#' identifiers. Default values are used for missing teacher names, and student
#' names are converted to title case.
#'
#' @param data A data frame containing student and class information. Must
#'   include at least the columns `SchoolId`, `StudentId`, `SchoolName`, `URN`,
#'   the class name column specified by `class_name_column`, and teacher and
#'   student name fields such as `TeacherName`, `TeacherForeName`,
#'   `TeacherSurName`, `StudentFirstName`, `StudentLastName`, `DoB`, and `UPN`.
#'
#' @param class_name_column Character string specifying the column to use for
#'   class names. Default is `"ClassName"`, but `"class_name_use"` is
#'   recommended if using `clean_class_names()`.
#'
#' @param default_teacher Character string giving the default main teacher name
#'   for records with missing teacher information. Default is `"Reception teacher"`.
#'
#' @param default_teacher_FN Character string giving the default teacher first
#'   name. Default is `"Reception"`.
#'
#' @param default_teacher_SN Character string giving the default teacher surname.
#'   Default is `"Teacher"`.
#'
#' @return
#' A data frame containing one row per student with the following columns:
#' \describe{
#'   \item{`School name`}{The school name.}
#'   \item{`URN`}{Unique reference number for the school.}
#'   \item{`Class Name`}{The class name used in downstream processing.}
#'   \item{`Main Teacher Name`}{Teacher responsible for the class, with defaults
#'   used if missing.}
#'   \item{`Pupil First Name`}{The student's first name (title-cased).}
#'   \item{`Pupil Last Name`}{The student's surname (title-cased).}
#'   \item{`DoB`}{The student's date of birth.}
#'   \item{`UPN`}{The unique pupil number or ID if available.}
#'   \item{`Main Teacher First Name`}{The main teacher's first name.}
#'   \item{`Main Teacher Last Name`}{The main teacher's surname.}
#' }
#'
#' @details
#' This function is typically the final step in the class list processing
#' pipeline, after functions such as `clean_class_names()` and
#' `reduce_to_best_teacher()`. It merges multiple records per student into
#' single entries, applies formatting and default values, and returns a clean,
#' human-readable data frame.
#'
#' @examples
#' \dontrun{
#' final_list <- produce_class_lists(
#'   data = cleaned_data,
#'   class_name_column = "class_name_use"
#' )
#' head(final_list)
#' }
#'
#' @export
produce_class_lists <- function(data, class_name_column = 'ClassName',
                                default_teacher = 'Reception teacher',
                                default_teacher_FN = 'Reception',
                                default_teacher_SN = 'Teacher'){
  
  # Need columns: School name, URN, Class Name, Main Teacher Name, Pupil First Name, Pupil Last Name, DOB, UPN
  
  data$school_pupil = paste0(data$SchoolId, '---', data$StudentId)
  format = data |> dplyr::group_by(school_pupil) |>
    dplyr::summarise(SchoolName = paste0(SchoolName |> unique(), collapse = ', '),
                     URN = paste0(URN |> unique(), collapse = ', '),
                     ClassName = paste0(.data[[class_name_column]] |> unique(), collapse = ', '),
                     TeacherName = (TeacherName |> sort())[1],
                     StudentFirstName = paste0(StudentFirstName |> unique(), collapse = ', '),
                     StudentLastName = paste0(StudentLastName |> unique(), collapse = ', '),
                     DoB =  paste0(DoB |> unique(), collapse = ', '),
                     UPN =  paste0(UPN |> unique(), collapse = ', '),
                     TeacherFirstName = (TeacherForeName)[1],
                     TeacherSurName = (TeacherSurName)[1]
    ) |> as.data.frame()
  format$TeacherName[is.na(format$TeacherName)] = default_teacher
  format$TeacherFirstName[is.na(format$TeacherFirstName)] = default_teacher_FN
  format$TeacherSurName[is.na(format$TeacherSurName)] = default_teacher_SN
  
  format$StudentFirstName = format$StudentFirstName |> stringr::str_to_title()
  format$StudentLastName = format$StudentLastName |> stringr::str_to_title()
  
  
  combo = paste0(format$SchoolName,'---', format$ClassName, '---', format$StudentLastName, format$StudentFirstName)
  format = format[order(combo),]
  names(format) =  c('ID','School name', 'URN', 'Class Name', 'Main Teacher Name',
                     'Pupil First Name', 'Pupil Last Name', 'DoB', 'UPN',
                     'Main Teacher First Name', 'Main Teacher Last Name')
  
  format[,-1]
}


#' Add spare rows to each class list
#'
#' @description
#' Appends a specified number of **spare (blank)** rows to each unique class in a class list.
#' The spare rows inherit key identifying fields (such as school name, URN,
#' class name, and teacher information) to allow for manual data entry,
#' printing, or later updates. Optionally, a `"Spare?"` column can be added to
#' flag the spare entries.
#'
#' @param class_list A data frame containing a class list, typically produced by
#'   \code{\link{produce_class_lists}()} or \code{\link{create_school_blanks}()}.
#'   Must include the following columns:
#'   \itemize{
#'     \item `School name`
#'     \item `URN`
#'     \item `Class Name`
#'     \item `Main Teacher Name`
#'     \item `Main Teacher First Name`
#'     \item `Main Teacher Last Name`
#'     \item `Pupil Last Name`
#'     \item `Pupil First Name`
#'   }
#'
#' @param no_spares Integer. The number of spare (blank) rows to add for each unique
#'   class. Defaults to `3`.
#'
#' @param include_spare_column Logical. If `TRUE`, a column named `"Spare?"`
#'   is added to the output, with `"Y"` marking the spare rows. Defaults to
#'   `FALSE`.
#'
#' @return
#' A modified version of the input data frame with spare rows appended for each
#' unique class. If `include_spare_column = TRUE`, includes an additional
#' `"Spare?"` column.
#'
#' @details
#' This function is typically used to prepare finalized or printable class lists
#' with placeholder rows for late student additions. The spare
#' entries are visually grouped at the end of each class when sorted.
#'
#'
#' @examples
#' \dontrun{
#' # Add three spare entries per class
#'  class_list = WondeCL::class_list_example
#   updated_list = add_spares(class_list,no_spares = 3,include_spare_column = T)
#' }
#'
#' @export
add_spares <- function(class_list, no_spares = 3, include_spare_column = FALSE) {
  no_col <- ncol(class_list)
  original_size <- nrow(class_list)
  care <- c("School name", "URN", "Class Name", "Main Teacher Name",
            "Main Teacher First Name", "Main Teacher Last Name")
  
  # Build unique identifiers for each class
  school_class <- ""
  for (ii in seq_along(care)) {
    school_class <- paste0(school_class, "---", class_list[[care[ii]]])
  }
  school_class <- stringr::str_remove(school_class, "^---") |> unique()
  
  split <- stringr::str_split(school_class, "---")
  index <- match(care, names(class_list))
  
  # Append spare (blank) rows for each unique class
  for (i in seq_along(school_class)) {
    to_add <- rep("", no_col)
    for (ii in seq_along(care)) to_add[index[ii]] <- split[[i]][ii]
    for (y in seq_len(no_spares)) {
      class_list[nrow(class_list) + 1, ] <- to_add
    }
  }
  
  # Mark spare rows
  last_name <- class_list$`Pupil Last Name`
  last_name[(original_size + 1):nrow(class_list)] <- "ZZZZZZZZZZZZZZZZZ"
  
  # Optionally add Spare? column
  if (include_spare_column) {
    spare <- rep("", nrow(class_list))
    spare[last_name == "ZZZZZZZZZZZZZZZZZ"] <- "Y"
    class_list$spare <- spare
    names(class_list)[ncol(class_list)] <- "Spare?"
  }
  
  # Sort by school, class, and student name
  combo <- paste0(
    class_list$`School name`, "---",
    class_list$`Class Name`, "---",
    last_name, class_list$`Pupil First Name`
  )
  class_list <- class_list[order(combo), ]
  
  class_list
}


#' Clean and standardize teacher name formatting
#'
#' @description
#' Converts all teacher name fields in a class list to title case (e.g.,
#' "john SMITH" becomes "John Smith"). This ensures consistent formatting across
#' teacher names for display, reporting, and downstream processing.
#'
#' @param data A data frame containing class list information. Must include
#'   teacher name columns such as `"Main Teacher Name"`, `"Main Teacher First Name"`,
#'   and `"Main Teacher Last Name"`.
#'
#' @param teacher_name_col Character string giving the name of the column
#'   containing the teacher's full name. Defaults to `"Main Teacher Name"`.
#'
#' @param TeacherFNColumn Character string giving the name of the column
#'   containing the teacher's first name. Defaults to `"Main Teacher First Name"`.
#'
#' @param TeacherLNColumn Character string giving the name of the column
#'   containing the teacher's last name. Defaults to `"Main Teacher Last Name"`.
#'
#' @return
#' A data frame identical to the input `data`, with all teacher name columns
#' converted to title case.
#'
#' @details
#' This function is typically used after data extraction or merging steps to
#' clean up inconsistent capitalization in teacher names. It is complementary
#' to \code{\link{clean_class_names}()} and \code{\link{reduce_to_best_teacher}()},
#' providing the final formatting pass before export.
#'
#' If any of the specified teacher name columns are missing from the input data,
#' the function will stop with an informative error message.
#'
#' @examples
#' \dontrun{
#' cleaned_df <- clean_teacher_names(class_list_df)
#' head(cleaned_df[, c("Main Teacher Name", "Main Teacher First Name", "Main Teacher Last Name")])
#' }
#'
#' @export
clean_teacher_names <- function(data,
                                teacher_name_col = "Main Teacher Name",
                                TeacherFNColumn = "Main Teacher First Name",
                                TeacherLNColumn = "Main Teacher Last Name") {
  teacher_columns <- c(teacher_name_col, TeacherFNColumn, TeacherLNColumn)
  indices <- match(teacher_columns, names(data))
  
  # Check for missing columns
  missing_cols <- teacher_columns[is.na(indices)]
  if (length(missing_cols) > 0) {
    stop("Missing required teacher columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Convert each teacher name column to title case
  for (index in indices) {
    data[[index]] <- stringr::str_to_title(data[[index]])
  }
  
  data
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


