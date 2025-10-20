#' Pull from Wonde API (with error handling)
#'
#' Safely queries a Wonde API endpoint, handling pagination, authentication, and
#' potential access or parsing errors. If an endpoint is inaccessible or returns
#' no data (e.g., 0 rows × 0 columns), the function returns `NULL` instead of
#' stopping execution.
#'
#' @param url_path The API endpoint to query.
#' @param filter Additional parameters to include in the request (e.g., `&include=groups`).
#' @param KEY The Wonde API access token.
#'
#' @return
#' A combined `data.frame` of the pulled information if data are returned,
#' otherwise `NULL` if the endpoint cannot be accessed or contains no records.
#'
#' @details
#' The function automatically paginates through all available pages of results
#' and concatenates them into a single data frame using `rbind_aggro()`.
#'
#' If the endpoint is inaccessible (e.g., insufficient permissions, invalid ID)
#' or returns an empty result, `NULL` is returned so that downstream code can
#' gracefully skip processing.
#'
#' @examplesIf FALSE
#' school_id <- "Enter_ID_here"
#' KEY <- "Enter_KEY_here"
#'
#' # Employee information
#' url_path <- paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/")
#' employees <- get_query(url_path, filter = '&include=groups', KEY = KEY)
#'
#' if (!is.null(employees)) {
#'   employees_groups <- employees |>
#'     convert_list_element_to_df(column_to_unnest = 'groups.data') |>
#'     tidyr::unnest(groups.data, names_sep = '__', keep_empty = TRUE)
#' }
#'
#' @seealso [httr2::req_perform()], [jsonlite::fromJSON()]
#'
#' @export
get_query <- function(url_path, filter = "", KEY = "") {
  carry_on <- TRUE
  page <- 1
  data_all <- list()
  
  while (carry_on) {
    url_use <- paste0(url_path, "?page=", page, filter)
    
    # Safely perform the request
    res <- tryCatch({
      req <- httr2::request(url_use) |>
        httr2::req_auth_basic(username = KEY, password = "") |>
        httr2::req_perform()
      
      # Check for HTTP error codes
      status <- httr2::resp_status(req)
      if (status >= 400) stop(paste("HTTP error", status))
      
      # Parse JSON response
      body <- req$body |>
        rawToChar() |>
        jsonlite::prettify() |>
        jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
      
      body
    }, error = function(e) {
      message("Error accessing endpoint: ", url_use)
      message("   - ", conditionMessage(e))
      return(NULL)
    })
    
    # If request failed or missing expected structure
    if (is.null(res) || !"data" %in% names(res)) return(NULL)
    
    data <- tryCatch(as.data.frame(res$data), error = function(e) NULL)
    if (is.null(data)) return(NULL)
    
    data_all[[page]] <- data
    
    # Stop if no more pages
    if (!isTRUE(res$meta$pagination$more)) {
      carry_on <- FALSE
    } else {
      page <- page + 1
    }
  }
  
  # Combine pages safely
  out <- tryCatch(
    rbind_aggro(data_all),
    error = function(e) NULL
  )
  
  # If empty (0 rows or 0 cols), treat as no data
  if (is.null(out) || nrow(out) == 0 || ncol(out) == 0) {
    return(NULL)
  }
  
  return(out)
}


#' @title Retrieve School Student and Staff Data from Wonde API
#'
#' @description
#' Retrieves detailed student, staff, and class information from the Wonde API
#' for either primary or secondary schools.
#'
#' These functions serve as wrappers around the Wonde v1.0 API and return
#' structured data frames of students, staff, and their relationships to classes,
#' subjects, and groups. Both functions now include education details.
#'
#' @param school_id Character string giving the Wonde school ID.
#' @param KEY Character string; the Wonde API key used for authentication.
#' @param verbose Logical; if TRUE (default), prints progress messages
#' to the console as data are fetched.
#'
#' @details
#' Each function performs several authenticated API requests to Wonde for
#' a given `school_id`, assembling structured data on:
#'
#' \itemize{
#'   \item **school info** – name, URN, establishment number, and phase of education.
#'   \item **staff (employees)** – teacher and employee data, including group memberships.
#'   \item **students (basic info)** – student-level data, including year group.
#'   \item **students + classes** – student-class relationships and associated staff.
#'   \item **students + groups** – student group memberships and related employees.
#'   \item **subjects** – subject-level metadata (typically secondary only).
#'   \item **education details** – student education information (available for both primary and secondary schools).
#' }
#'
#' The functions rely on internal helpers:
#' \itemize{
#'   \item `get_query()` – handles paginated API requests.
#'   \item `convert_list_element_to_df()` – flattens nested list structures.
#' }
#'
#' Any missing or NULL components are removed automatically before returning.
#'
#' @return
#' A named list of data frames, typically including:
#' \itemize{
#'   \item `student` – basic student and school info.
#'   \item `students_group` – student-group relationships.
#'   \item `students_classes` – student-class relationships and staff assignments.
#'   \item `subjects` – available subjects (mainly secondary).
#'   \item `staff` – employee information and groups.
#'   \item `education_details` – student education information.
#' }
#'
#' @examples
#' \dontrun{
#' # Primary school data
#' primary_data <- get_primary_school_student_data(
#'   school_id = "A123456789",
#'   KEY = Sys.getenv("WONDE_API_KEY")
#' )
#'
#' # Secondary school data
#' secondary_data <- get_secondary_school_student_data(
#'   school_id = "B987654321",
#'   KEY = Sys.getenv("WONDE_API_KEY")
#' )
#'
#' # Check available data sets
#' names(primary_data)
#' names(secondary_data)
#' }
#'
#' @seealso
#' \code{\link{get_query}}, \code{\link{convert_list_element_to_df}}
#'
#' @export
get_primary_school_student_data <- function(school_id, KEY = "", verbose = TRUE) {
  # Helper for conditional messages
  vcat <- function(...) if (verbose) message(...)
  
  vcat("Fetching data for school: ", school_id)
  
  # ---- SCHOOL INFO ----
  url_path <- paste0("https://api.wonde.com/v1.0/schools/", school_id, "/")
  req <- tryCatch({
    httr2::request(url_path) |>
      httr2::req_auth_basic(username = KEY, password = "") |>
      httr2::req_perform()
  }, error = function(e) {
    message("Unable to retrieve school info for ", school_id)
    return(NULL)
  })
  if (is.null(req)) return(NULL)
  
  body <- req$body |>
    rawToChar() |>
    jsonlite::prettify() |>
    jsonlite::fromJSON(simplifyVector = TRUE, flatten = TRUE)
  data <- body$data
  
  want <- c("id", "name", "establishment_number", "urn", "phase_of_education")
  school <- as.data.frame(t(data[want]))
  names(school) <- paste0("school_", want)
  
  # ---- STAFF (EMPLOYEES) ----
  vcat("[Employees]")
  employees <- get_query(
    paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/"),
    filter = "&include=groups",
    KEY = KEY
  )
  
  if (!is.null(employees) && nrow(employees) > 0) {
    employees <- employees |>
      convert_list_element_to_df(column_to_unnest = "groups.data") |>
      tidyr::unnest(groups.data, names_sep = "__", keep_empty = TRUE)
  }
  
  # ---- STUDENTS (BASIC INFO) ----
  vcat("[Students - basic info]")
  student <- get_query(
    paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
    filter = "&include=year",
    KEY = KEY
  )
  if (!is.null(student)) student <- cbind(school, student)
  
  # ---- STUDENTS + CLASSES ----
  vcat("[Students + Classes]")
  students_classes <- get_query(
    paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
    filter = "&include=classes&include=classes.employees",
    KEY = KEY
  )
  
  if (!is.null(students_classes)) {
    if ("classes.data" %in% names(students_classes)) {
      students_classes <- students_classes |>
        convert_list_element_to_df(column_to_unnest = "classes.data") |>
        tidyr::unnest(classes.data, names_sep = "__", keep_empty = TRUE)
    }
    if ("classes.data__employees.data" %in% names(students_classes)) {
      students_classes <- students_classes |>
        convert_list_element_to_df(column_to_unnest = "classes.data__employees.data") |>
        tidyr::unnest(classes.data__employees.data, names_sep = "__", keep_empty = TRUE)
    }
    students_classes <- cbind(school, students_classes)
  }
  
  # ---- STUDENTS + GROUPS ----
  vcat("[Students + Groups]")
  students_group <- get_query(
    paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
    filter = "&include=groups&include=groups.employees",
    KEY = KEY
  )
  
  if (!is.null(students_group)) {
    students_group <- students_group |>
      convert_list_element_to_df(column_to_unnest = "groups.data") |>
      tidyr::unnest(groups.data, names_sep = "__", keep_empty = TRUE) |>
      convert_list_element_to_df(column_to_unnest = "groups.data__employees.data") |>
      tidyr::unnest(groups.data__employees.data, names_sep = "__", keep_empty = TRUE)
    students_group <- cbind(school, students_group)
  }
  
  # ---- EDUCATION DETAILS ----
  vcat("[Students + Education details]")
  students_education <- get_query(
    paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
    filter = "&include=education_details",
    KEY = KEY
  )
  if (!is.null(students_education)) {
    students_education <- cbind(school, students_education)
  }
  
  # ---- ASSEMBLE RESULTS ----
  result <- list(
    student = student,
    students_group = students_group,
    students_classes = students_classes,
    staff = employees,
    education_details = students_education
  )
  
  # Remove NULL elements
  result <- result[!vapply(result, is.null, logical(1))]
  
  vcat("Completed school: ", school_id)
  return(result)
}



#' @rdname get_primary_school_student_data
get_secondary_school_student_data <- function(school_id, KEY = "", verbose = TRUE) {
  # Helper for messages
  vcat <- function(...) if (verbose) message(...)
  
  vcat("Fetching data for school: ", school_id)
  
  # --- SCHOOL INFO ---
  url_path <- paste0("https://api.wonde.com/v1.0/schools/", school_id, "/")
  req <- tryCatch({
    httr2::request(url_path) |>
      httr2::req_auth_basic(username = KEY, password = "") |>
      httr2::req_perform()
  }, error = function(e) {
    message("Unable to retrieve school info for ", school_id)
    return(NULL)
  })
  if (is.null(req)) return(NULL)
  
  body <- req$body |>
    rawToChar() |>
    jsonlite::prettify() |>
    jsonlite::fromJSON(simplifyVector = TRUE, flatten = TRUE)
  data <- body$data
  
  want <- c("id", "name", "establishment_number", "urn", "phase_of_education")
  school <- as.data.frame(t(data[want]))
  names(school) <- paste0("school_", want)
  
  # --- EMPLOYEES ---
  vcat("[Employees]")
  employees <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/"),
                         filter = "&include=groups", KEY = KEY)
  if (!is.null(employees)) {
    employees <- employees |>
      convert_list_element_to_df(column_to_unnest = "groups.data") |>
      tidyr::unnest(groups.data, names_sep = "__", keep_empty = TRUE)
  }
  
  # --- STUDENTS ---
  vcat("[Students - basic info]")
  student <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                       filter = "&include=year", KEY = KEY)
  if (!is.null(student)) student <- cbind(school, student)
  
  # --- STUDENTS + CLASSES ---
  vcat("[Students + Classes]")
  students_classes <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                                filter = "&include=classes&include=classes.subject&include=classes.employees",
                                KEY = KEY)
  if (!is.null(students_classes)) {
    if ("classes.data" %in% names(students_classes)) {
      students_classes <- students_classes |>
        convert_list_element_to_df(column_to_unnest = "classes.data") |>
        tidyr::unnest(classes.data, names_sep = "__", keep_empty = TRUE)
    }
    if ("classes.data__employees.data" %in% names(students_classes)) {
      students_classes <- students_classes |>
        convert_list_element_to_df(column_to_unnest = "classes.data__employees.data") |>
        tidyr::unnest(classes.data__employees.data, names_sep = "__", keep_empty = TRUE)
    }
    students_classes <- cbind(school, students_classes)
  }
  
  # --- SUBJECTS ---
  vcat("[Subjects]")
  subjects <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/subjects/"),
                        KEY = KEY)
  
  # --- STUDENTS + GROUPS ---
  vcat("[Students + Groups]")
  students_group <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                              filter = "&include=groups&include=groups.employees", KEY = KEY)
  if (!is.null(students_group)) {
    students_group <- students_group |>
      convert_list_element_to_df(column_to_unnest = "groups.data") |>
      tidyr::unnest(groups.data, names_sep = "__", keep_empty = TRUE) |>
      convert_list_element_to_df(column_to_unnest = "groups.data__employees.data") |>
      tidyr::unnest(groups.data__employees.data, names_sep = "__", keep_empty = TRUE)
    students_group <- cbind(school, students_group)
  }
  
  # --- EDUCATION DETAILS ---
  vcat("[Students + Education details]")
  students_education <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                                  filter = "&include=education_details", KEY = KEY)
  if (!is.null(students_education)) {
    students_education <- cbind(school, students_education)
  }
  
  # --- Assemble available data ---
  result <- list(
    student = student,
    students_group = students_group,
    students_classes = students_classes,
    subjects = subjects,
    staff = employees,
    education_details = students_education
  )
  
  # Remove NULL components
  result <- result[!vapply(result, is.null, logical(1))]
  
  vcat("Completed school: ", school_id)
  return(result)
}




