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
get_query <- function(url_path, filter = '', KEY = '') {
  carry_on <- TRUE
  page <- 1
  data_all <- list()

  while (carry_on) {
    url_use <- paste0(url_path, '?page=', page, filter)

    # Safely perform the request
    res <- tryCatch({
      req <- httr2::request(url_use) |>
        httr2::req_auth_basic(username = KEY, password = '') |>
        httr2::req_perform()

      # Check for HTTP error codes
      status <- httr2::resp_status(req)
      if (status >= 400) stop(paste("HTTP error", status))

      # Parse JSON
      body <- req$body |>
        rawToChar() |>
        jsonlite::prettify() |>
        jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

      body
    }, error = function(e) {
      message("Error accessing endpoint: ", url_use)
      message("   → ", conditionMessage(e))
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

  # 🔍 If empty (0 rows or 0 cols), treat as no data
  if (is.null(out) || nrow(out) == 0 || ncol(out) == 0) {
    return(NULL)
  }

  return(out)
}



#' Get school information
#'
#' @param school_id wonde school ID
#' @param KEY API token
#'
#' @return
#'
#'@details
#' Given the school ID and KEY the function queries the Wonde API to extract:
#'
#' - student information
#' - student group information
#' - student class information
#' - staff information
#' - subject information (secondary only)
#'
#' This is returned as a list of data frames containing each subset of information.
#'
#' Primary school information is obtained using `get_primary_school_student_data()` and secondary school information from `get_secondary_school_student_data()`.
#'
#'
#' @examplesIf FALSE
#' school_id = "Enter_ID_here"
#' KEY = "Enter_KEY_here"
#' primary_school_ids = c('A', 'B', 'C', 'D')
#'
#' # Get info for individual school.
#' primary_info = get_primary_school_student_data(school_id, KEY)
#' secondary_info = get_secondary_school_student_data(school_id, KEY)
#'
#' # Loop over and extract info for all primary schools.
#' primary_data_raw = lapply(primary_school_ids, function(id){get_primary_school_student_data(id, KEY) })
#' names(primary_data_raw) = primary_school_ids
#' primary_data_raw
get_primary_school_student_data <- function(school_id, KEY = ''){
  print(school_id)

  #### Get school info
  url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/")
  req = request(url_path) |> httr2::req_auth_basic(username = KEY, password = '') |> httr2::req_perform()
  body = req$body |> rawToChar() |> jsonlite::prettify() |> jsonlite::fromJSON(simplifyVector = T,flatten = T)
  data = body$data
  want = c('id', 'name', 'establishment_number', 'urn', 'phase_of_education')
  index = match(want, names(data))
  school = rep(NA, length(index))
  for(i in index){
    school[i] = data[[i]]
  }
  school = school|> data.frame() |> t() |> data.frame()
  names(school) = paste0('school_', want)

  # # #### Get all class info.
  # url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/classes/")
  # classes = get_query(url_path)
  # #
  # # ### Get all employee info.
  url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/")
  employees = get_query(url_path, filter = '&include=groups', KEY = KEY)
  if(nrow(employees) > 0){
    employees_groups = employees |> convert_list_element_to_df(column_to_unnest = 'groups.data') |>
      tidyr::unnest(groups.data, names_sep ='__', keep_empty  = T)
  }


  # employees = get_query(url_path, filter = '&include=roles')
  # employees_groups = employees |> convert_list_element_to_df(column_to_unnest = 'groups.data') |>
  #   tidyr::unnest(groups.data, names_sep ='__', keep_empty  = T)
  # #
  # # # #### Get all group info.
  # url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/groups/")
  # groups = get_query(url_path, filter = '&extended_details')

  #### Get all students info.
  # url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
  # students =  get_query(url_path, filter = '&include=classes&include=groups')
  # QQ  = classes_students |> tidyr::unnest(students.data, names_sep ='__')
  # names(students) = paste0('stud_', names(students))

  #### Get all students info.
  url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
  student =  get_query(url_path, filter = '&include=year', KEY = KEY)


  #### Get all students with nested classes  info.
  url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
  students_classes =  get_query(url_path, filter = '&include=classes&include=classes.employees', KEY = KEY)

  if('classes.data' %in% names(students_classes)){
    students_classes  = students_classes |>
      convert_list_element_to_df(column_to_unnest = 'classes.data') |>
      tidyr::unnest(classes.data, names_sep ='__', keep_empty  = T)
  }
  if('classes.data__employees.data' %in% names(students_classes)){
    students_classes  = students_classes |>
      convert_list_element_to_df(column_to_unnest = 'classes.data__employees.data') |>
      tidyr::unnest(classes.data__employees.data, names_sep ='__', keep_empty  = T)
  }



  #### Get all students with nested groups info.
  url_path = paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/")
  students_group = get_query(url_path, filter = '&include=groups&include=groups.employees', KEY = KEY)
  students_group = students_group |>
    convert_list_element_to_df(column_to_unnest = 'groups.data') |>
    tidyr::unnest(groups.data, names_sep ='__', keep_empty  = T) |>
    convert_list_element_to_df(column_to_unnest = 'groups.data__employees.data') |>
    tidyr::unnest(groups.data__employees.data, names_sep ='__', keep_empty  = T)

  #### Combine school and student information
  student  = cbind(school,student)
  students_group  = cbind(school,students_group)
  students_classes  = cbind(school,students_classes)

  return(list(student = student,
              students_group = students_group,
              students_classes = students_classes,
              staff = employees))
}


#' @rdname get_school_student_data
get_secondary_school_student_data <- function(school_id, KEY = '', verbose = TRUE) {
  # Helper for messages
  vcat <- function(...) if (verbose) message(...)

  vcat("Fetching data for school: ", school_id)

  # --- SCHOOL INFO ---
  url_path <- paste0("https://api.wonde.com/v1.0/schools/", school_id, "/")
  req <- tryCatch({
    httr2::request(url_path) |>
      httr2::req_auth_basic(username = KEY, password = '') |>
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

  want <- c('id', 'name', 'establishment_number', 'urn', 'phase_of_education')
  school <- as.data.frame(t(data[want]))
  names(school) <- paste0('school_', want)

  # --- EMPLOYEES ---
  vcat("→ Employees")
  employees <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/"),
                         filter = '&include=groups', KEY = KEY)
  if (!is.null(employees)) {
    employees <- employees |>
      convert_list_element_to_df(column_to_unnest = 'groups.data') |>
      tidyr::unnest(groups.data, names_sep = '__', keep_empty = TRUE)
  }

  # --- STUDENTS ---
  vcat("→ Students (basic info)")
  student <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                       filter = '&include=year', KEY = KEY)
  if (!is.null(student)) student <- cbind(school, student)

  # --- STUDENTS + CLASSES ---
  vcat("→ Students + Classes")
  students_classes <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                                filter = '&include=classes&include=classes.subject&include=classes.employees',
                                KEY = KEY)
  if (!is.null(students_classes)) {
    if ('classes.data' %in% names(students_classes)) {
      students_classes <- students_classes |>
        convert_list_element_to_df(column_to_unnest = 'classes.data') |>
        tidyr::unnest(classes.data, names_sep ='__', keep_empty = TRUE)
    }
    if ('classes.data__employees.data' %in% names(students_classes)) {
      students_classes <- students_classes |>
        convert_list_element_to_df(column_to_unnest = 'classes.data__employees.data') |>
        tidyr::unnest(classes.data__employees.data, names_sep ='__', keep_empty = TRUE)
    }
    students_classes <- cbind(school, students_classes)
  }

  # --- SUBJECTS ---
  vcat("→ Subjects")
  subjects <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/subjects/"),
                        KEY = KEY)

  # --- STUDENTS + GROUPS ---
  vcat("→ Students + Groups")
  students_group <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                              filter = '&include=groups&include=groups.employees', KEY = KEY)
  if (!is.null(students_group)) {
    students_group <- students_group |>
      convert_list_element_to_df(column_to_unnest = 'groups.data') |>
      tidyr::unnest(groups.data, names_sep ='__', keep_empty = TRUE) |>
      convert_list_element_to_df(column_to_unnest = 'groups.data__employees.data') |>
      tidyr::unnest(groups.data__employees.data, names_sep ='__', keep_empty = TRUE)
    students_group <- cbind(school, students_group)
  }

  # --- EDUCATION DETAILS (added endpoint) ---
  vcat("→ Students + Education details")
  students_education <- get_query(paste0("https://api.wonde.com/v1.0/schools/", school_id, "/students/"),
                                  filter = '&include=education_details', KEY = KEY)
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

  # remove NULL components
  result <- result[!vapply(result, is.null, logical(1))]

  vcat("✅ Completed school: ", school_id)
  return(result)
}


#' Add education details (UPI, nc_year_actual) to extracted data.
#'
#' @param extracted_data extracted data
#' @param Key API token
#' @param verbose Flag (TRUE/FALSE) for console messages.
#'
#' @return extracted data with education details appended.
#'
#' @details
#' This function should be consumed into get_primary_school_student_data() and get_secondary_school_student_data(). However, to not have to re-extract all the data again this was currently added as a separate function.
add_upi_info <- function(extracted_data, KEY = '', verbose = T){
  ids = names(extracted_data)

  if(verbose){
    for(i in cli::cli_progress_along(1:length(ids), 'extracting education details across schools...')){
      id = ids[i]
      url_path = paste0("https://api.wonde.com/v1.0/schools/", id, "/students/")
      student_ed_info =  get_query(url_path, filter = '&include=education_details', KEY = KEY)
      extracted_data[[id]]$education_details = student_ed_info
    }
  }else{
    for(i in 1:length(ids)){
      id = ids[i]
      url_path = paste0("https://api.wonde.com/v1.0/schools/", id, "/students/")
      student_ed_info =  get_query(url_path, filter = '&include=education_details', KEY = KEY)
      extracted_data[[id]]$education_details = student_ed_info
    }
  }

  return(extracted_data)
}

