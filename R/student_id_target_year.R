#' Get Student IDs for a Target Year
#'
#' Identifies students belonging to a target year within a single Wonde school
#' extract. Year information from the student data is used preferentially. If
#' no matching students are found, educational details are used as a fallback.
#'
#' @param school_data A single school extract from `WondeData`.
#' @param year_codes_target Character vector containing year values or patterns
#'   to match.
#' @param type Character. Matching method to use. `"exact"` uses exact matching
#'   with `%in%`, while `"grepl"` treats each value in `year_codes_target` as a
#'   regular expression and returns students matching any of the patterns.
#'
#' @return A character vector containing the unique student IDs for the target
#'   year. Returns `character(0)` if no matching students are found.
#'
#' @examples
#' \dontrun{
#' get_student_ids_target_year(
#'   school_data,
#'   year_codes_target = c("7", "Year 7", "Y7"),
#'   type = "exact"
#' )
#'
#' get_student_ids_target_year(
#'   school_data,
#'   year_codes_target = c("Year 7", "^7$", "Y7"),
#'   type = "grepl"
#' )
#' }
#'
#' @export
get_student_ids_target_year <- function(school_data,
                                year_codes_target = c("7", "Year 7", "07", "Year 07", "Y7", "Yr7", "Yr 7"),
                                type = c( "grepl","exact")) {
  
  type <- match.arg(type)
  
  match_year <- function(x) {
    x <- as.character(x)
    
    if (type == "exact") {
      x %in% year_codes_target
    } else {
      Reduce(`|`, lapply(year_codes_target, function(pattern) grepl(pattern, x)))
    }
  }
  
  student <- school_data$student
  
  if (!is.null(student)) {
    if ("year.data.code" %in% names(student)) {
      ids <- student$id[match_year(student$year.data.code)]
      if (length(ids) > 0) return(unique(as.character(ids)))
    }
    
    if ("year.data.name" %in% names(student)) {
      ids <- student$id[match_year(student$year.data.name)]
      if (length(ids) > 0) return(unique(as.character(ids)))
    }
  }
  
  ed_details <- school_data$education_details
  
  if (!is.null(ed_details) && "education_details.data.current_nc_year" %in% names(ed_details)) {
    ids <- ed_details$id[match_year(ed_details$education_details.data.current_nc_year)]
    if (length(ids) > 0) return(unique(as.character(ids)))
  }
  
  character(0)
}