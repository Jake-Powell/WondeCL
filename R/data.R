#' Example class list dataset
#'
#' An example of a cleaned class list containing two schools and three classes. The variables are as follows:
#' \itemize{
#'   \item School name.
#'   \item URN.
#'   \item Class Name.
#'   \item Main Teacher First Name.
#'   \item Main Teacher Last Name.
#'   \item Pupil First Name.
#'   \item Pupil Last Name.
#'   \item DoB. Formatted YYYY-MM-DD.
#'   \item UPN.
#' }
#'
#' @docType data
#' @keywords class_list
#' @format A data frame with 60 rows and 9 variables
"class_list_example"



#' Example Wonde-formatted raw data extract
#'
#' @description
#' A simulated example of a Wonde API data extract containing multiple schools,
#' structured to match the output typically returned by
#' \code{get_primary_school_student_data()} or
#' \code{get_secondary_school_student_data()}.
#'
#' This dataset is designed for testing, demonstrations, and development of
#' functions within the package that operate on Wonde-formatted data.
#' It mirrors the typical nested structure returned by the Wonde API, including
#' school-, student-, class-, and group-level elements.
#'
#' @format 
#' A named list of two school datasets:
#' \describe{
#'   \item{\code{S123456}}{Simulated data for *Lavandula Primary School*.}
#'   \item{\code{S654321}}{Simulated data for *Digitalis Purpurea Primary School*.}
#' }
#'
#' Each school entry is a list containing:
#' \describe{
#'   \item{\code{student}}{A data frame of pupil-level information including
#'   \code{id}, \code{forename}, \code{surname}, \code{date_of_birth.date},
#'   and year group fields.}
#'
#'   \item{\code{education_details}}{A data frame linking pupil IDs to their
#'   unique pupil numbers (\code{UPN}).}
#'
#'   \item{\code{students_classes}}{A data frame describing the relationship
#'   between pupils, their classes, and class-level metadata (e.g. subject,
#'   teacher, and year group).}
#'
#'   \item{\code{students_group}}{A data frame of group assignments such as
#'   registration or tutor groups, including the assigned teachers.}
#'
#'   \item{\code{subjects}}{A data frame listing available subjects for the
#'   school, each identified by a subject ID.}
#'
#'   \item{\code{staff}}{A data frame of staff members (teachers) associated
#'   with the classes and groups. Some classes include both a main and an
#'   assistant teacher.}
#' }
#'
#' @details
#' The dataset is suitable for testing package functions such as:
#' \itemize{
#'   \item \code{\link{extract_class_list}()}
#'   \item \code{\link{summarise_school_data}()}
#'   \item \code{\link{extract_field_across_schools}()}
#'   \item \code{\link{extract_group_names_across_schools}()}
#' }
#'
#' All IDs (e.g. school, student, staff, subject, class, group) are generated in
#' the format \code{"A#########"} (one letter followed by 7–10 digits), and the
#' values are simulated for demonstration only.
#'
#' @examples
#' \dontrun{
#' # Load the example data
#' data("WondeData_template")
#'
#' # Inspect structure
#' str(WondeData_template, max.level = 2)
#'
#' # Extract a class list from the first school
#' extract_class_list(WondeData_template, schools, maths_names, maths_desc_names, year_pattern)
#' }
"WondeData_template"
