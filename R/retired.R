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

