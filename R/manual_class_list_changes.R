add_student_to_school_class <- function(data, first_name, last_name, DoB, URN, class_name,
                                        URNColumn = 'URN',FirstNameColumn ="Pupil First Name",
                                        LastNameColumn = "Pupil Last Name", ClassNameColumn = "Class Name",
                                        DoBColumn = 'DoB'){

  school_index = which(data[[URNColumn]] == URN)
  copy = data[school_index[i],]
  copy[[FirstNameColumn]] = first_name
  copy[[LastNameColumn]] = last_name
  copy[[DoBColumn]] = DoB

  if(1 %in% school_index){
    data = rbind(copy, data)
  }else{
    data = rbind(data[1:(school_index[1]-1),], copy, data[(school_index[1]):nrow(data),])
  }

  data
}

update_student_class <- function(data, URN, first_name, last_name, class_name = NA, new_class_name,
                                 SchoolNameColumn = "School name", URNColumn = 'URN', ClassNameColumn = "Class Name",
                                 FirstNameColumn ="Pupil First Name", LastNameColumn = "Pupil Last Name",
                                 TeacherColumn = 'Main Teacher Name',
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


update_school_name <- function(data, URN, new_name, SchoolNameColumn = "School name", URNColumn = 'URN'){
  if(length(URN) != length(new_name)){
    stop('URN and new_name have different lengths!')
  }
  for(i in 1:length(new_name)){
    data[[SchoolNameColumn]][which(data[[URNColumn]] == URN[i])] = new_name[i]
  }

  data
}

update_class_teacher <- function(data, URN, class_name, new_name, first_name, sur_name,
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

  data[[TeacherColumn]][index] = new_name
  data[[TeacherFNColumn]][index] = first_name
  data[[TeacherLNColumn]][index] = sur_name

  data
}

remove_class <- function(data, URN, class_name,URNColumn = 'URN', ClassNameColumn = "ClassName", verbose = F){
  to_remove_index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] %in% class_name)
  classes_removed = data[[ClassNameColumn]][to_remove_index] |> unique()

  data = data[-to_remove_index,]

  if(verbose) cli::cli_inform(paste0('Removed ',classes_removed,' class containing ', length(to_remove_index), ' students'))

  data
}

remove_student_from_class <- function(data, URN, studentID, class_name, URNColumn = 'URN',
                                      ClassNameColumn = "ClassName",  StudentIDColumn = "StudentId",  verbose = F){
  to_remove_index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] %in% class_name &
                            data[[StudentIDColumn]] %in% studentID)
  classes_removed = data[[ClassNameColumn]][to_remove_index] |> unique()
  students_removed = data[[StudentIDColumn]][to_remove_index] |> unique()

  data = data[-to_remove_index,]

  if(verbose) cli::cli_inform(paste0('Removed ',classes_removed,' class containing ', length(to_remove_index), ' students'))

  data
}

