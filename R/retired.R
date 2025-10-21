#' #' Get school information
#' #'
#' #' @param school_id wonde school ID
#' #' @param KEY API token
#' #' @param verbose flag for console printing
#' #'
#' #'@details
#' #' Given the school ID and KEY the function queries the Wonde API to extract:
#' #'
#' #' - student information
#' #' - student group information
#' #' - student class information
#' #' - staff information
#' #' - subject information (secondary only)
#' #'
#' #' This is returned as a list of data frames containing each subset of information.
#' #'
#' #' Primary school information is obtained using `get_primary_school_student_data()` and secondary school information from `get_secondary_school_student_data()`.
#' #'
#' #'
#' #' @examplesIf FALSE
#' #' school_id = "Enter_ID_here"
#' #' KEY = "Enter_KEY_here"
#' #' primary_school_ids = c('A', 'B', 'C', 'D')
#' #'
#' #' # Get info for individual school.
#' #' primary_info = get_primary_school_student_data(school_id, KEY)
#' #' secondary_info = get_secondary_school_student_data(school_id, KEY)
#' #'
#' #' # Loop over and extract info for all primary schools.
#' #' primary_data_raw = lapply(primary_school_ids, function(id){get_primary_school_student_data(id, KEY) })
#' #' names(primary_data_raw) = primary_school_ids
#' #' primary_data_raw
#' get_primary_school_student_data <- function(school_id, KEY = ''){
#'   print(school_id)
#'   
#'   #### Get school info
#'   url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/")
#'   req = request(url_path) |> httr2::req_auth_basic(username = KEY, password = '') |> httr2::req_perform()
#'   body = req$body |> rawToChar() |> jsonlite::prettify() |> jsonlite::fromJSON(simplifyVector = T,flatten = T)
#'   data = body$data
#'   want = c('id', 'name', 'establishment_number', 'urn', 'phase_of_education')
#'   index = match(want, names(data))
#'   school = rep(NA, length(index))
#'   for(i in index){
#'     school[i] = data[[i]]
#'   }
#'   school = school|> data.frame() |> t() |> data.frame()
#'   names(school) = paste0('school_', want)
#'   
#'   # # #### Get all class info.
#'   # url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/classes/")
#'   # classes = get_query(url_path)
#'   # #
#'   # # ### Get all employee info.
#'   url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/")
#'   employees = get_query(url_path, filter = '&include=groups', KEY = KEY)
#'   if(nrow(employees) > 0){
#'     employees_groups = employees |> convert_list_element_to_df(column_to_unnest = 'groups.data') |>
#'       tidyr::unnest(groups.data, names_sep ='__', keep_empty  = T)
#'   }
#'   
#'   
#'   # employees = get_query(url_path, filter = '&include=roles')
#'   # employees_groups = employees |> convert_list_element_to_df(column_to_unnest = 'groups.data') |>
#'   #   tidyr::unnest(groups.data, names_sep ='__', keep_empty  = T)
#'   # #
#'   # # # #### Get all group info.
#'   # url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/groups/")
#'   # groups = get_query(url_path, filter = '&extended_details')
#'   
#'   #### Get all students info.
#'   # url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
#'   # students =  get_query(url_path, filter = '&include=classes&include=groups')
#'   # QQ  = classes_students |> tidyr::unnest(students.data, names_sep ='__')
#'   # names(students) = paste0('stud_', names(students))
#'   
#'   #### Get all students info.
#'   url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
#'   student =  get_query(url_path, filter = '&include=year', KEY = KEY)
#'   
#'   
#'   #### Get all students with nested classes  info.
#'   url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
#'   students_classes =  get_query(url_path, filter = '&include=classes&include=classes.employees', KEY = KEY)
#'   
#'   if('classes.data' %in% names(students_classes)){
#'     students_classes  = students_classes |>
#'       convert_list_element_to_df(column_to_unnest = 'classes.data') |>
#'       tidyr::unnest(classes.data, names_sep ='__', keep_empty  = T)
#'   }
#'   if('classes.data__employees.data' %in% names(students_classes)){
#'     students_classes  = students_classes |>
#'       convert_list_element_to_df(column_to_unnest = 'classes.data__employees.data') |>
#'       tidyr::unnest(classes.data__employees.data, names_sep ='__', keep_empty  = T)
#'   }
#'   
#'   
#'   
#'   #### Get all students with nested groups info.
#'   url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
#'   students_group = get_query(url_path, filter = '&include=groups&include=groups.employees', KEY = KEY)
#'   students_group = students_group |>
#'     convert_list_element_to_df(column_to_unnest = 'groups.data') |>
#'     tidyr::unnest(groups.data, names_sep ='__', keep_empty  = T) |>
#'     convert_list_element_to_df(column_to_unnest = 'groups.data__employees.data') |>
#'     tidyr::unnest(groups.data__employees.data, names_sep ='__', keep_empty  = T)
#'   
#'   #### Combine school and student information
#'   student  = cbind(school,student)
#'   students_group  = cbind(school,students_group)
#'   students_classes  = cbind(school,students_classes)
#'   
#'   return(list(student = student,
#'               students_group = students_group,
#'               students_classes = students_classes,
#'               staff = employees))
#' }



#' 
#' 
#' #' Extract unique year codes across schools
#' #'
#' #' This function scans through a list of Wonde school datasets (`WondeData`) and
#' #' compiles a summary of all distinct year codes found across schools. For each
#' #' unique year code, it reports the total number of students across all schools
#' #' and a comma-separated list of school IDs where that code appears.
#' #'
#' #' It automatically detects whether the year code column is stored as
#' #' `students.data__year.data.code` or `year.data.code`, depending on the structure
#' #' of each Wonde dataset.
#' #'
#' #' @param WondeData A named list of school datasets, typically created by data retrieval
#' #'   functions such as \code{\link{get_primary_school_student_data}} or
#' #'   \code{\link{get_secondary_school_student_data}}. Each element of the list should
#' #'   contain at least a `student` data frame with a column for year code information.
#' #'
#' #' @return A data frame with the following columns:
#' #' \describe{
#' #'   \item{year_code}{The distinct year code values extracted from all schools.}
#' #'   \item{count}{The total number of students across all schools with that year code.}
#' #'   \item{school_ids}{Comma-separated string of Wonde IDs of schools that include this year code.}
#' #' }
#' #'
#' #' @examples
#' #' \dontrun{
#' #' year_code_summary <- extract_year_codes_across_schools(WondeData)
#' #' head(year_code_summary)
#' #' }
#' #'
#' #' @seealso
#' #' \code{\link{summarise_school_data}} for per-school summaries.
#' #'
#' #' @export
#' extract_year_codes_across_schools <- function(WondeData) {
#'   
#'   # Step 1: collect per-school year codes with counts
#'   year_data_list <- lapply(names(WondeData), function(school_id) {
#'     x <- WondeData[[school_id]]
#'     
#'     # Detect where year codes live
#'     if ("students.data__year.data.code" %in% names(x$student)) {
#'       year_codes <- x$student$students.data__year.data.code
#'     } else if ("year.data.code" %in% names(x$student)) {
#'       year_codes <- x$student$year.data.code
#'     } else {
#'       return(NULL)
#'     }
#'     
#'     year_codes <- year_codes[!is.na(year_codes) & year_codes != ""]
#'     if (length(year_codes) == 0) return(NULL)
#'     
#'     counts <- table(year_codes)
#'     data.frame(
#'       year_code = names(counts),
#'       count = as.integer(counts),
#'       WondeId = school_id,
#'       stringsAsFactors = FALSE
#'     )
#'   })
#'   
#'   # Step 2: combine all schools into one data frame
#'   year_data <- do.call(rbind, year_data_list)
#'   if (is.null(year_data) || nrow(year_data) == 0) {
#'     return(data.frame(
#'       year_code = character(0),
#'       count = integer(0),
#'       school_ids = character(0),
#'       stringsAsFactors = FALSE
#'     ))
#'   }
#'   
#'   # Step 3: aggregate across schools
#'   year_summary <- aggregate(count ~ year_code, data = year_data, sum)
#'   
#'   # Step 4: list schools that have each code
#'   school_list <- tapply(year_data$WondeId, year_data$year_code, function(ids) {
#'     paste(unique(sort(ids)), collapse = ", ")
#'   })
#'   
#'   # Step 5: merge and tidy
#'   year_summary$school_ids <- school_list[match(year_summary$year_code, names(school_list))]
#'   year_summary <- year_summary[order(year_summary$year_code), ]
#'   rownames(year_summary) <- NULL
#'   
#'   return(year_summary)
#' }
#' 
#' 
#' #' Extract unique group names across schools
#' #'
#' #' This function scans through a list of Wonde school datasets (`WondeData`) and
#' #' compiles a summary of all distinct group names found across schools. For each
#' #' unique group name, it reports the total number of students across all schools
#' #' belonging to that group and provides a comma-separated list of school IDs where
#' #' that group appears.
#' #'
#' #' It automatically detects whether the group name column is stored as
#' #' `students_group$groups.data__name` or `groups.data__name`, depending on the
#' #' structure of each Wonde dataset.
#' #'
#' #' @param WondeData A named list of school datasets, typically created by data retrieval
#' #'   functions such as \code{\link{get_primary_school_student_data}} or
#' #'   \code{\link{get_secondary_school_student_data}}. Each element of the list should
#' #'   contain at least a `students_group` data frame with a column for group name
#' #'   information.
#' #'
#' #' @return A data frame with the following columns:
#' #' \describe{
#' #'   \item{group_name}{Distinct group name values extracted from all schools.}
#' #'   \item{count}{Total number of student-group records across all schools for that group.}
#' #'   \item{school_ids}{Comma-separated string of Wonde IDs of schools that include this group.}
#' #' }
#' #'
#' #' @examples
#' #' \dontrun{
#' #' group_summary <- extract_group_names_across_schools(WondeData)
#' #' head(group_summary)
#' #' }
#' #'
#' #' @seealso
#' #' \code{\link{extract_year_codes_across_schools}} for year code summaries.
#' #'
#' #' @export
#' extract_group_names_across_schools <- function(WondeData) {
#'   
#'   # Step 1: collect per-school group names with counts
#'   group_data_list <- lapply(names(WondeData), function(school_id) {
#'     x <- WondeData[[school_id]]
#'     
#'     # Detect where group names live
#'     if (!is.null(x$students_group)) {
#'       if ("groups.data__name" %in% names(x$students_group)) {
#'         group_names <- x$students_group$groups.data__name
#'       } else if ("students_group.groups.data__name" %in% names(x$students_group)) {
#'         group_names <- x$students_group$students_group.groups.data__name
#'       } else {
#'         return(NULL)
#'       }
#'     } else {
#'       return(NULL)
#'     }
#'     
#'     # Clean up and count
#'     group_names <- group_names[!is.na(group_names) & group_names != ""]
#'     if (length(group_names) == 0) return(NULL)
#'     
#'     counts <- table(group_names)
#'     data.frame(
#'       group_name = names(counts),
#'       count = as.integer(counts),
#'       WondeId = school_id,
#'       stringsAsFactors = FALSE
#'     )
#'   })
#'   
#'   # Step 2: combine all schools into one data frame
#'   group_data <- do.call(rbind, group_data_list)
#'   if (is.null(group_data) || nrow(group_data) == 0) {
#'     return(data.frame(
#'       group_name = character(0),
#'       count = integer(0),
#'       school_ids = character(0),
#'       stringsAsFactors = FALSE
#'     ))
#'   }
#'   
#'   # Step 3: aggregate across schools
#'   group_summary <- aggregate(count ~ group_name, data = group_data, sum)
#'   
#'   # Step 4: list schools that have each group
#'   school_list <- tapply(group_data$WondeId, group_data$group_name, function(ids) {
#'     paste(unique(sort(ids)), collapse = ", ")
#'   })
#'   
#'   # Step 5: merge and tidy
#'   group_summary$school_ids <- school_list[match(group_summary$group_name, names(school_list))]
#'   group_summary <- group_summary[order(group_summary$group_name), ]
#'   rownames(group_summary) <- NULL
#'   
#'   return(group_summary)
#' }
