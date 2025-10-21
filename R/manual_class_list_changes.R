#' functions to make manual changes to a cleaned class list
#'
#' @param data class list
#' @param verbose flag (TRUE/FALSE) for whether console messages are printed.
#' @param student_id the student id/s of students to be removed from the class list.
#' @param first_name the first name of the student/teacher to be added/updated/removed from the class list.
#' @param last_name the last name (surname) of the student/teacher to be added/updated/removed from the class list.
#' @param DoB of the student to be added/updated/removed from the class list.
#' @param URN of the school whose school/class/teacher/student is to be updated. Used to identify class list record/s.
#' @param UPN Unique person number from the DfE extracted from Wonde education details endpoint.
#' @param class_name of the class to be added/updated/removed from the class list.
#' @param new_class_name only used in `update_student_class()` to denote the class that the student is moving too (this is also used to obtain the teacher's name in the new class).
#' @param new_name the new school/class/teacher (first + last) name to be added to the class list.
#' @param StudentIDColumn first name column in `data`. Default is "StudentId".
#' @param FirstNameColumn student first name column in `data`. Default is "Pupil First Name".
#' @param LastNameColumn student last name column in `data`. Default is "Pupil Last Name".
#' @param DoBColumn student DoB column in `data`. Default is 'DoB'.
#' @param UPNColumn UPN column in `data`. Default is 'UPN'.
#' @param TeacherColumn teacher name (first and last name combined) column in `data`. Default is 'Main Teacher Name'.
#' @param TeacherFNColumn teacher first name column in `data`. Default is "Main Teacher First Name".
#' @param TeacherLNColumn teacher last name column in `data`. Default is "Main Teacher Last Name".
#' @param ClassNameColumn class name column in `data`. Default is 'Class Name'.
#' @param URNColumn URN column in `data`. Default is 'URN'.
#' @param SchoolNameColumn school name column in `data`. Default is 'School Name'.
#'
#' @details
#' The functions do as their names suggests:
#' \itemize{
#' \item add_student_to_school_class(): Adds a student to a class within the class list, where all class/school information remains identical (only student name, DOB changes).
#' \item update_student_class(): Updates class and class teacher of a student already in the class list.
#' \item update_class_name(): Updates the class name for all students in a class.
#' \item update_school_name(): Updates the school name for all students in a school.
#' \item update_class_teacher(): Updates the teacher of a class for all students. If the class name is NA then the teacher is updated to all classes within a school.
#' \item remove_class(): Removes all students from a given class in the class list. Note that you can pass multiple classes from within a school at one go by passing a vector of class names.
#' \item remove_student_from_class(): Removes student/s from a given class in the class list.
#'
#' }
#'
#' Note that these manual edits should be performed prior to adding OME ids, withdrawing students or adding blanks (with the exception of remove_student_from_class() which requires some level of student ID to remove students).
#'
#'
#' @export
#'
#' @examplesIf FALSE
#' # Get fake data (note the column names are such that we can use default values)
#' data = WondeCL::class_list_example
#'
#' # Add student to a class within a school.
#' df_new = data |> add_student_to_school_class(first_name = 'Jimmy',
#'                                              last_name = 'Powell',
#'                                              DoB = '1910-05-07',
#'                                              URN = '123456',
#'                                              class_name = 'French')
#'
#' # update the class of a particular student
#' df_new |> update_student_class(URN = '123456',
#'                                first_name = 'Jimmy',
#'                                last_name = 'Powell',
#'                                class_name = 'French',
#'                                new_class_name = 'Fernleaf')
#'
#' # update the class of a particular student
#' df_new |> update_student_class(URN = '123456',
#'                                first_name = 'Jimmy',
#'                                last_name = 'Powell',
#'                                class_name = 'French',
#'                                new_class_name = 'Fernleaf')
#'
#' # Change the class name 'French' in school, URN = 123456, to 'English'.
#' df_new |> update_class_name(URN = '123456',
#'                             class_name = 'French',
#'                             new_name = 'English')
#'
#' # Change the school name 'Lavandula Primary School', URN = 123456, to
#' #  'Lamiaceae Lavandula Primary School'.
#' df_new |> update_school_name(URN = '123456',
#'                              new_name = 'Lamiaceae Lavandula Primary School')
#'
#' # Change the teacher in 'French' class to 'Little Lottie'.
#' df_new |> update_class_teacher(URN = '123456',
#'                                class_name = 'French',
#'                                first_name = 'Little',
#'                                last_name = 'Lottie')
#'
#' # Remove 'French' class from class list.
#' df_new |> remove_class(URN = '123456',
#'                        class_name = 'French')
#'
#' # Remove Jimmy Powell from 'French' class, for this to word each student requires a unique id.
#' df_new$StudentId = 1:nrow(df_new)
#' student_rm = df_new$StudentId[df_new$`Pupil Last Name` == 'Powell']
#' df_new |> remove_student_from_class(URN = '123456',
#'                                     class_name = 'French',
#'                                     student_id = student_rm)
#'
add_student_to_school_class <- function(
    data,
    first_name,
    last_name,
    URN,
    class_name,
    DoB = NA,
    UPN = NA,
    URNColumn = "URN",
    FirstNameColumn = "Pupil First Name",
    LastNameColumn = "Pupil Last Name",
    ClassNameColumn = "Class Name",
    DoBColumn = "DoB",
    UPNColumn = "UPN",
    verbose = TRUE
) {
  vcat <- function(...) if (verbose) cli::cli_inform(list(message = paste(...)))
  
  # --- Validation checks ---
  required_cols <- c(URNColumn, FirstNameColumn, LastNameColumn, ClassNameColumn, DoBColumn)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from 'data': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # --- Locate school/class rows ---
  school_index <- which(data[[URNColumn]] == URN & data[[ClassNameColumn]] == class_name)
  
  if (length(school_index) == 0) {
    stop("No matching school/class found for URN = ", URN,
         " and class = '", class_name, "'.", call. = FALSE)
  }
  
  vcat("Adding new student to", class_name, "(school URN:", URN, ")")
  
  # --- Create new student row ---
  new_row <- data[school_index[1], , drop = FALSE]
  new_row[[FirstNameColumn]] <- first_name
  new_row[[LastNameColumn]] <- last_name
  new_row[[DoBColumn]] <- DoB
  if (UPNColumn %in% names(data)) new_row[[UPNColumn]] <- UPN
  
  # --- Insert new row in same section ---
  insert_after <- max(school_index)
  if (insert_after == nrow(data)) {
    data <- rbind(data, new_row)
  } else {
    data <- rbind(
      data[1:insert_after, ],
      new_row,
      data[(insert_after + 1):nrow(data), ]
    )
  }
  
  vcat("Student", first_name, last_name, "successfully added to", class_name)
  return(data)
}


#' @rdname add_student_to_school_class
update_student_class <- function(data, URN, first_name, last_name, class_name = NA, new_class_name,
                                 SchoolNameColumn = "School name", URNColumn = 'URN', ClassNameColumn = "Class Name",
                                 FirstNameColumn ="Pupil First Name", LastNameColumn = "Pupil Last Name",
                                 TeacherColumn = 'Main Teacher Name',
                                 TeacherFNColumn = "Main Teacher First Name", TeacherLNColumn = "Main Teacher Last Name",
                                 verbose = F){
  class_teacher = data[[TeacherColumn]][which(data[[ClassNameColumn]] == new_class_name)] |> unique()

  if(length(class_teacher) > 1){
    warning('More than one class teacher for updating class! Selecting only first teacher.')
    class_teacher = class_teacher[1]
  }
  if(is.na(class_name)){

    index = which(data[[URNColumn]] == URN & data[[FirstNameColumn]] == first_name & data[[LastNameColumn]] == last_name)
  }else{
    index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] == class_name &
                    data[[FirstNameColumn]] == first_name & data[[LastNameColumn]] == last_name)
  }
  # data[index,] |> View()
  if(verbose) cli::cli_inform(paste0('Updated class name and teacher for ', length(index), ' students'))

  data[[ClassNameColumn]][index] = new_class_name
  data[[TeacherColumn]][index] = class_teacher

  data
}

#' @rdname add_student_to_school_class
update_class_name <- function(data, URN, class_name, new_name,
                              SchoolNameColumn = "School name", URNColumn = 'URN', ClassNameColumn = "Class Name",
                              verbose = F){
  if(is.na(class_name)){
    index = which(data[[URNColumn]] == URN)
  }else{
    index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] == class_name)
  }
  # data[index,] |> View()
  if(verbose) cli::cli_inform(paste0('Updated class name for ', length(index), ' students'))

  data[[ClassNameColumn]][index] = new_name

  data
}


#' @rdname add_student_to_school_class
update_school_name <- function(data, URN, new_name, SchoolNameColumn = "School name", URNColumn = 'URN'){
  if(length(URN) != length(new_name)){
    stop('URN and new_name have different lengths!')
  }
  for(i in 1:length(new_name)){
    data[[SchoolNameColumn]][which(data[[URNColumn]] == URN[i])] = new_name[i]
  }

  data
}

#' @rdname add_student_to_school_class
update_class_teacher <- function(data, URN, class_name, first_name, last_name,  new_name = paste0(first_name, ' ', last_name),
                                 SchoolNameColumn = "School name", URNColumn = 'URN', ClassNameColumn = "Class Name",
                                 TeacherColumn = 'Main Teacher Name',
                                 TeacherFNColumn = "Main Teacher First Name", TeacherLNColumn = "Main Teacher Last Name",
                                 verbose = F){
  if(is.na(class_name)){
    index = which(data[[URNColumn]] == URN)
  }else{
    index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] == class_name)
  }
  # data[index,] |> View()
  if(verbose) cli::cli_inform(paste0('Updated class teacher for ', length(index), ' students'))

  if(TeacherColumn %in% names(data))   data[[TeacherColumn]][index] = new_name
  if(TeacherFNColumn %in% names(data))   data[[TeacherFNColumn]][index] = first_name
  if(TeacherLNColumn %in% names(data))   data[[TeacherLNColumn]][index] = last_name

  data
}

#' @rdname add_student_to_school_class
remove_class <- function(data, URN, class_name,
                         URNColumn = 'URN',
                         ClassNameColumn = "Class Name",
                         verbose = F) {
  if (!ClassNameColumn %in% names(data)) {
    stop(paste0("ClassNameColumn: ", ClassNameColumn, " not found within data"))
  }
  
  to_remove_index <- which(data[[URNColumn]] == URN &
                             data[[ClassNameColumn]] %in% class_name)
  
  if (length(to_remove_index) == 0) {
    if (verbose) cli::cli_inform("No matching classes found to remove.")
    return(data)
  }
  
  classes_removed <- unique(data[[ClassNameColumn]][to_remove_index])
  data <- data[-to_remove_index, ]
  
  if (verbose) {
    cli::cli_inform(paste0(
      "Removed ", paste(classes_removed, collapse = ", "),
      " class(es) containing ", length(to_remove_index), " students"
    ))
  }
  
  return(data)
}


#' @rdname add_student_to_school_class
remove_student_from_class <- function(data, URN, student_id, class_name, URNColumn = 'URN',
                                      ClassNameColumn = "Class Name",  StudentIDColumn = "StudentId",  verbose = F){
  required_cols <- c(URNColumn, ClassNameColumn, StudentIDColumn)
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ",
         paste(missing_cols, collapse = ", "))
  }
  
  to_remove_index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] %in% class_name &
                            data[[StudentIDColumn]] %in% student_id)
  classes_removed = data[[ClassNameColumn]][to_remove_index] |> unique()
  students_removed = data[[StudentIDColumn]][to_remove_index] |> unique()

  data = data[-to_remove_index,]

  if(verbose) cli::cli_inform(paste0('Removed ',classes_removed,' class containing ', length(to_remove_index), ' students'))

  data
}

#' Create blank class lists for schools without Wonde data
#'
#' @description
#' Generates synthetic or "blank" class lists for schools that do not have
#' Wonde data access. The function uses provided school information (including
#' estimated student counts and optional teacher or class information) to
#' create mock class lists in a standardised format for downstream processing
#' and printing.
#'
#' @param schools_with_size A data frame containing at least:
#'   \itemize{
#'     \item `SchoolName` — The name of each school.
#'     \item `URN` — The Unique Reference Number for each school.
#'     \item `NoStudents` — Estimated number of students at the school.
#'     \item Optional: `ClassInfo` (formatted as `"Class1 -> 28, Class2 -> 30"`)
#'           to specify class names and sizes.
#'     \item Optional: `TeacherInfo` (formatted as `"First Last, First Last"`)
#'           to specify class teacher names.
#'   }
#'
#' @param numbering_type Character, default `"school"`. Determines how pupils
#'   are numbered. If `"school"`, assigns sequential pupil numbers across the
#'   entire school.
#'
#' @param teacher_name Character vector of length 2 giving a default teacher
#'   first and last name to use when none is provided in `schools_with_size`.
#'   Defaults to `c("Reception", "Teacher")`.
#'
#' @param class_name Character, default `"Reception Class"`. The base name
#'   to use when automatically generating class names (e.g. `"Reception Class 1"`,
#'   `"Reception Class 2"`).
#'
#' @param type Numeric or character, default `30`. If numeric, sets the default
#'   number of students per class. If not numeric (e.g. `"auto"`), class sizes
#'   are estimated from total student counts divided across inferred classes,
#'   plus an optional buffer (see `class_buffer`).
#'
#' @param class_buffer Integer, default `5`. When class sizes are automatically
#'   estimated, this value is added to the computed class size to avoid undercounting.
#'
#' @param class_size Integer, default `30`. Target class size used to determine
#'   how many classes to create per school when `ClassInfo` is missing.
#'
#' @param class_list_columns Character vector specifying the desired column
#'   structure of the returned data frame. Two shortcuts are available:
#'   \itemize{
#'     \item `"master"` — returns all key fields (school info, teacher, pupil, IDs, withdrawal status).
#'     \item `"print"` — returns a reduced set of fields for printing lists.
#'   }
#'
#' @details
#' For each school, the function:
#' \enumerate{
#'   \item Parses `ClassInfo` and `TeacherInfo` columns (if present) to
#'         determine the number and names of classes, and their associated teachers.
#'   \item If `ClassInfo` is missing, automatically estimates the number
#'         of classes required based on total student count and `class_size`.
#'   \item Creates one or more blank class lists for each school, assigning
#'         placeholder pupil numbers and teacher names.
#'   \item Fills missing required columns with `NA` and reorders columns
#'         according to `class_list_columns`.
#' }
#'
#' This function is designed to ensure that downstream processing scripts
#' (e.g., withdrawal handling, printing, ID assignment) can operate on schools
#' even when no data has been received from Wonde.
#'
#' @return
#' A data frame representing the generated class list for all supplied schools.
#' The columns and order depend on the value of `class_list_columns`.
#'
#' @examples
#' \dontrun{
#' # Example input data
#' schools_df <- data.frame(
#'   SchoolName = c("Maple Primary", "Oakwood Academy"),
#'   URN = c(123456, 789012),
#'   NoStudents = c(60, 90)
#' )
#'
#' # Create blank class lists
#' blanks <- create_school_blanks(
#'   schools_with_size = schools_df,
#'   class_size = 30,
#'   class_list_columns = "master"
#' )
#'
#' head(blanks)
#' }
#'
#' @seealso
#' [rbind_aggro()] for combining multiple data frames safely.
#'
#' @export
create_school_blanks <- function(schools_with_size,
                                 numbering_type = 'school',
                                 teacher_name = c('Reception', 'Teacher'),
                                 class_name = 'Reception Class',
                                 type = 30,
                                 class_buffer = 5,
                                 class_size = 30,
                                 class_list_columns = 'master'
){
  ## Setup
  if(class_list_columns[1] == 'master'){
    class_list_columns = c('School Name',
                           'URN',
                           'Class Name',
                           'Main Teacher First Name',
                           'Main Teacher Last Name',
                           'Pupil First Name',
                           'Pupil Last Name',
                           'DoB',
                           'UPN',
                           'Spare?',
                           'OMEID',
                           'PrintID',
                           'Withdrawn?',
                           'Establishment Number',
                           'Date Of Withdrawal'
    )

  }
  if(class_list_columns[1] == 'print'){
    class_list_columns = c('School Name',
                           'Class Name',
                           'Main Teacher First Name',
                           'Main Teacher Last Name',
                           'Pupil First Name',
                           'Pupil Last Name',
                           'Pupil DoB',
                           'UPN',
                           'PrintID',
                           'Withdrawn?',
                           'Spare?',
                           'School ID Number')
  }


  # Loop over all schools given in "schools_with_size".
  school_blanks = lapply(1:nrow(schools_with_size), function(index){

    ###
    # 1) Setup and checking what information was provided in schools_with_size.
    ###
    cur = schools_with_size[index,]
    # Check if we have "ClassInfo" or "TeacherInfo" in the headers of schools_with_size.
    if('ClassInfo' %in% names(cur)){class_info = cur$ClassInfo}else{class_info = ''}
    if('TeacherInfo' %in% names(cur)){teacher_info = cur$TeacherInfo}else{teacher_info = ''}
    if(class_info == '') class_info = NA ; if(teacher_info == '') teacher_info = NA

    ###
    # 2) Create 'classes' with the information needed to make a class list for the class.
    ###
    # Create "classes" a dataframe with two columns:
    #   - name: the class name
    #   - count: The number of students in the class.
    #   If we were not provided with class info attempt to guess the size.
    if(is.na(class_info)){
      no_classes = ((cur$NoStudents-1) %/% class_size) +1
      if(is.numeric(type)){
        count = rep(type, no_classes)
      }else{
        count = rep( cur$NoStudents/no_classes |> ceiling() + class_buffer, no_classes)
      }
      classes = data.frame(name = paste0(class_name, ' ', 1:no_classes), count = count)
    }
    else{
      classes = class_info |> stringr::str_split(', ') |> unlist() |> stringr::str_split('->') |> unlist() |> stringr::str_squish() |> matrix(ncol = 2, byrow = T) |> data.frame()
      names(classes) = c('name', 'count')
    }

    # Add teacher information to "classes".
    if(is.na(teacher_info)){
      classes$teacher_firstname = teacher_name[1]
      classes$teacher_surname = teacher_name[2]
    }else{
      teachers = teacher_info |> stringr::str_split(', |---') |> unlist() |> matrix(ncol = 2, byrow = T) |> data.frame()
      classes$teacher_firstname = teachers[[1]]
      classes$teacher_surname = teachers[[2]]

    }

    ###
    # 2) Create class list for class.
    ###
    # Create class lists for each class and join together.
    class_list_parts = lapply(1:nrow(classes), function(row_index){
      out = data.frame(A = cur$SchoolName |> rep(classes$count[row_index]),
                       B = cur$URN,
                       C = classes$name[row_index],
                       D = paste0(classes$teacher_firstname[row_index], ' ', classes$teacher_surname[row_index]) ,
                       E = 1:classes$count[row_index],
                       I = classes$teacher_firstname[row_index],
                       J = classes$teacher_surname[row_index]
      )
      names(out) = c("School Name",
                     "URN",
                     "Class Name",
                     "Main Teacher Name",
                     "Pupil First Name",
                     "Main Teacher First Name",
                     "Main Teacher Last Name")
      out
    })
    class_list_school = rbind_aggro(class_list_parts)


    if(numbering_type == 'school') class_list_school[["Pupil First Name"]] = 1:nrow(class_list_school)

    class_list_school
  })
  class_list = rbind_aggro(school_blanks)

  ###
  # Edit class list columns and ordering.
  ###
  # Fill with NA the desired columns that are missing.
  to_add = class_list_columns[which(!class_list_columns %in% names(class_list))]
  if(length(to_add) > 0){
    for(i in 1:length(to_add)){
      class_list[[to_add[i]]] = NA
    }
  }
  out = class_list[,match(class_list_columns, names(class_list))]

  out
}


#' @rdname add_student_to_school_class
#' @title Manually withdraw a student
#'
#' @description
#' Marks a specific student as withdrawn by setting the value of the
#' \code{'Withdrawn?'} column to \code{'Y'} for the matching record.
#' The function checks that exactly one student matches the given
#' URN, first name, and last name.
#'
#' @param data A data frame containing the class list.
#' @param URN The URN (school ID) of the school.
#' @param first_name The student's first name (exact match, case-sensitive).
#' @param last_name The student's last name (exact match, case-sensitive).
#' @param URNColumn The name of the column containing the URN (default \code{'URN'}).
#' @param FirstNameColumn The name of the column containing the first name (default \code{'Pupil First Name'}).
#' @param LastNameColumn The name of the column containing the last name (default \code{'Pupil Last Name'}).
#' @param WithdrawColumn The name of the column to mark as withdrawn (default \code{'Withdrawn?'}).
#' @param verbose Logical; if \code{TRUE}, prints a short message about the change.
#'
#' @return
#' The updated data frame with the student marked as withdrawn.
#' If no student or multiple students match, a warning is issued and
#' no changes are made.
#'
#' @export
withdraw_student_from_school_class <- function(data,
                                               URN,
                                               first_name,
                                               last_name,
                                               URNColumn = "URN",
                                               FirstNameColumn = "Pupil First Name",
                                               LastNameColumn = "Pupil Last Name",
                                               WithdrawColumn = "Withdrawn?",
                                               verbose = FALSE) {
  
  # Find matching student(s)
  index <- which(data[[URNColumn]] == URN &
                   data[[FirstNameColumn]] == first_name &
                   data[[LastNameColumn]] == last_name)
  
  if (length(index) == 0) {
    warning("No student found with the specified details. No changes made.")
    return(data)
  }
  
  if (length(index) > 1) {
    warning("Multiple matching students found. No changes made.")
    return(data)
  }
  
  # Ensure Withdrawn? column exists
  if (!WithdrawColumn %in% names(data)) {
    data[[WithdrawColumn]] <- ""
  }
  
  # Mark the student as withdrawn
  data[[WithdrawColumn]][index] <- "Y"
  
  if (verbose) {
    cli::cli_inform(paste0(
      "Student ",
      data[[FirstNameColumn]][index], " ",
      data[[LastNameColumn]][index],
      " from URN ", URN, " marked as withdrawn."
    ))
  }
  
  data
}


#' @title Remove schools from class list
#' @description
#' Removes all records (rows) from the class list that belong to one or more
#' specified schools, identified either by name, URN, or both.
#'
#' This function is useful for manually excluding schools that should not be
#' included in further analysis or reporting. At least one of
#' \code{school_names} or \code{URNs} must be provided.
#'
#' @param data A data frame containing the class list.
#' @param school_names A character vector of school names to remove (optional).
#' @param URNs A character or numeric vector of URNs to remove (optional).
#' @param SchoolNameColumn The name of the column containing school names
#'   (default: \code{"School name"}).
#' @param URNColumn The name of the column containing URNs
#'   (default: \code{"URN"}).
#' @param verbose Logical; if \code{TRUE}, prints a summary message of the
#'   removals performed.
#'
#' @return
#' The updated data frame with all rows for the specified schools removed.
#'
#' @examples
#' \dontrun{
#' # Remove by school name
#' updated_class_list <- remove_school_from_class_list(
#'   data = class_list,
#'   school_names = c("Greenfield Primary School", "Hillside Academy")
#' )
#'
#' # Remove by URN
#' updated_class_list <- remove_school_from_class_list(
#'   data = class_list,
#'   URNs = c(123456, 987654)
#' )
#'
#' # Remove by both
#' updated_class_list <- remove_school_from_class_list(
#'   data = class_list,
#'   school_names = "Riverside Primary",
#'   URNs = 123456
#' )
#' }
#'
#' @export
remove_school_from_class_list <- function(data,
                                          school_names = NULL,
                                          URNs = NULL,
                                          SchoolNameColumn = "School name",
                                          URNColumn = "URN",
                                          verbose = FALSE) {
  # Check at least one input provided
  if (is.null(school_names) && is.null(URNs)) {
    stop("At least one of 'school_names' or 'URNs' must be provided.")
  }
  
  # Confirm columns exist
  if (!SchoolNameColumn %in% names(data) && !URNColumn %in% names(data)) {
    stop("Neither school name nor URN columns found in data.")
  }
  
  # Identify indices to remove
  remove_index <- integer(0)
  
  if (!is.null(school_names) && SchoolNameColumn %in% names(data)) {
    remove_index <- c(remove_index, which(data[[SchoolNameColumn]] %in% school_names))
  }
  
  if (!is.null(URNs) && URNColumn %in% names(data)) {
    remove_index <- c(remove_index, which(data[[URNColumn]] %in% URNs))
  }
  
  remove_index <- unique(remove_index)
  
  if (length(remove_index) == 0) {
    if (verbose) cli::cli_inform("No matching schools found; no rows removed.")
    return(data)
  }
  
  # Remove the identified rows
  removed_data <- data[remove_index, , drop = FALSE]
  data <- data[-remove_index, , drop = FALSE]
  
  if (verbose) {
    removed_schools <- if (SchoolNameColumn %in% names(removed_data))
      unique(removed_data[[SchoolNameColumn]]) else NULL
    removed_urns <- if (URNColumn %in% names(removed_data))
      unique(removed_data[[URNColumn]]) else NULL
    
    cli::cli_inform(paste0(
      "Removed ", length(remove_index), " rows from ",
      length(unique(c(removed_schools))), " school(s)."
    ))
  }
  
  data
}
