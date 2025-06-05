



get_school_summary <- function(data, school_info = '~/Wonde/edubasealldata20241202.csv'){
  summary_school =  data |> dplyr::group_by(SchoolId) |>
    dplyr::summarise(school_name = paste0(SchoolName |> unique(), collapse = ' --- '),
                     URN = paste0(URN |> unique(), collapse = ', '),
                     student = StudentId |> unique() |> length(),
                     classes = ClassId |> unique() |> length(),
                     teachers = TeacherId |> unique() |> length())

  data$school_class = paste0(data$SchoolId, '---', data$ClassName)
  class_size = data |> dplyr::group_by(school_class) |>
    dplyr::summarise(total = StudentId |> unique() |> length())
  split = class_size$school_class |> stringr::str_split('---')
  class_size$SchoolId  = lapply(split, function(x)x[1])
  class_size$ClassName  = lapply(split, function(x)x[2])
  class_size$class_size = paste0(class_size$ClassName, '--> ', class_size$total )

  schoolIDs = summary_school$SchoolId
  school_class_size = lapply(schoolIDs, function(ID){
    class_size$class_size[which(class_size$SchoolId == ID)] |> paste0(collapse = ' \n')
  }) |> unlist()

  summary_school$school_class_size = school_class_size
  names(summary_school) = c('SchoolId', 'SchoolName', 'URN', 'Number of students',
                            'Number of classes', 'Number of teachers', 'Students per class')

  if(!is.na(school_info)){
    school_info = readr::read_csv(school_info)

    school_index = match(summary_school$URN, school_info$URN)
    wanted_columns = c('NumberOfPupils', 'SchoolCapacity', 'StatutoryLowAge', 'StatutoryHighAge', "OpenDate")

    to_adjoin = school_info[school_index, match(wanted_columns, names(school_info))]
    og_names = names(summary_school); og_names2 = names(to_adjoin)
    summary_school = data.frame(summary_school, to_adjoin)
    names(summary_school) = c(og_names, paste0('National_data_',og_names2))
  }
  summary_school

}

clean_class_names <- function(data, default_class_name = 'Reception'){
  #Attempt to clean up class names.

  # Get class info with school and student ID.
  classes = data[,match(c('SchoolId', 'StudentId', "ClassId", "ClassName"), names(data))] |> as.data.frame()

  # For schools with no class information (NA) set the classID to SchoolID concaternated with CLASS,
  # and set the class name to 'Reception'.
  no_class_ID_index = which(is.na(classes$ClassId))
  classes$ClassId[no_class_ID_index] = paste0(classes$SchoolId[no_class_ID_index], 'CLASS')
  classes$ClassName[no_class_ID_index] = default_class_name

  # check if students only belong to one class.
  no_class_stud = classes |> dplyr::group_by(StudentId) |>
    dplyr::summarise(no_class = ClassId |> unique() |> length(),
                     classes = ClassId |> unique() |> paste0(collapse = ', ')) |> as.data.frame()

  # For the students that belong to more than one class...
  # Check the classes to see if it exactly the same students in each class.
  # In this case we can merge class names.
  mult = no_class_stud$classes[grep(', ', no_class_stud$classes)] |> unique()
  same_students = lapply(mult, function(classes_string){
    class_vect = classes_string |> stringr::str_split(', ') |> unlist()
    students = lapply(class_vect, function(cla) classes$StudentId[which(classes$ClassId == cla)] |> unique()) |>
      unlist() |> table()
    if(all(students == length(class_vect))){
      return(T)
    }
    return(F)
  }) |> unlist()

  to_merge = mult[same_students]
  if(length(to_merge) > 0){
    for(i in 1:length(to_merge)){
      class_vect = to_merge[i] |> stringr::str_split(', ') |> unlist()
      classes_index = which(classes$ClassId %in% class_vect)

      # If all the classes belong to one school and the school has no other classes then set to default.
      schoolID = classes$SchoolId[classes_index] |> unique()
      school_classes = classes$ClassId[which(classes$SchoolId == schoolID)] |> unique()
      if(setequal(class_vect, school_classes)){
        classes$ClassName[classes_index] = default_class_name
      }




    }
  }

  data$ClassId_use = classes$ClassId

  data$class_name_use = classes$ClassName |>
    stringr::str_remove('[A-Za-z ]: ') |>
    stringr::str_remove('[A-Za-z ] - ') |>
    stringr::str_to_title()
  return(data)
}

reduce_to_best_teacher <- function(data){
  data$MainTeacher[is.na(data$MainTeacher)] = FALSE
  to_keep = data[which(data$MainTeacher |> as.logical()),]
  to_decide = data[-which(data$MainTeacher |> as.logical()),]

  have_main_index = which(to_decide$StudentId %in% to_keep$StudentId)
  data_new = rbind(to_keep, to_decide[-have_main_index,])


  if(data$StudentId |> unique() |> length() != data_new$StudentId |> unique() |> length()){
    warning('Reducing teacher has caused student loss!')
  }
  data_new
}

clean_teacher_names <- function(data, teacher_name_col = 'Main Teacher Name',  TeacherFNColumn = "Main Teacher First Name", TeacherLNColumn = "Main Teacher Last Name" ){
  teacher_columns = c(teacher_name_col, TeacherFNColumn, TeacherLNColumn)
  indices = match(teacher_columns, names(data))

  for(i in 1:length(indices)){
    index = indices[i]
    if(length(index) != 1){
      stop('Teacher name column does not exist!')
    }
    data[[index]] = data[[index]] |> stringr::str_to_title()
  }

  data
}

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

produce_class_lists2 <- function(data, class_name_column = 'ClassName',
                                default_teacher = 'Reception teacher',
                                default_teacher_FN = 'Reception',
                                default_teacher_SN = 'Teacher'){

  # Need columns: School name, URN, Class Name, Main Teacher Name, Pupil First Name, Pupil Last Name, DOB, UPN

  data$school_pupil = paste0(data$SchoolId, '---', data$StudentId)
  format = data |> dplyr::group_by(school_pupil) |>
    dplyr::summarise(SchoolName = paste0(SchoolName |> unique(), collapse = ', '),
                     URN = paste0(URN |> unique(), collapse = ', '),
                     ClassName = paste0(.data[[class_name_column]] |> unique(), collapse = ', '),
                     TeacherName2 = (TeacherName |> sort())[1],
                     StudentFirstName = paste0(StudentFirstName |> unique(), collapse = ', '),
                     StudentLastName = paste0(StudentLastName |> unique(), collapse = ', '),
                     DoB =  paste0(DoB |> unique(), collapse = ', '),
                     UPN =  paste0(UPN |> unique(), collapse = ', '),
                     TeacherFirstName = (TeacherForeName)[1],
                     TeacherSurName = (TeacherSurName)[1],
                     AllTeacher = paste0(TeacherName |> unique(), collapse = ', ')
    ) |> as.data.frame()
  format$TeacherName2[is.na(format$TeacherName2)] = default_teacher
  format$TeacherFirstName[is.na(format$TeacherFirstName)] = default_teacher_FN
  format$TeacherSurName[is.na(format$TeacherSurName)] = default_teacher_SN

  format$StudentFirstName = format$StudentFirstName |> stringr::str_to_title()
  format$StudentLastName = format$StudentLastName |> stringr::str_to_title()


  combo = paste0(format$SchoolName,'---', format$ClassName, '---', format$StudentLastName, format$StudentFirstName)
  format = format[order(combo),]
  names(format) =  c('ID','School name', 'URN', 'Class Name', 'Main Teacher Name',
                     'Pupil First Name', 'Pupil Last Name', 'DoB', 'UPN',
                     'Main Teacher First Name', 'Main Teacher Last Name', 'AllTeacher')

  format[,-1]
}

add_blanks <- function(class_list, no_blanks = 3, include_spare_column = FALSE){
  no_col = ncol(class_list)
  original_size = nrow(class_list)
  care = c('School name', 'URN', 'Class Name', "Main Teacher Name", "Main Teacher First Name", "Main Teacher Last Name")
  school_class = ''
  for(ii in 1:length(care)) school_class = paste0(school_class, '---', class_list[[care[ii]]])
  school_class = school_class |> stringr::str_remove('^---') |> unique()
  # school_class = paste0(class_list[[care[1]]], '---', class_list[[care[2]]], '---', class_list[[care[3]]], '---',
  #                       class_list[[care[4]]], '---', class_list[[care[5]]]) |> unique()
  split = school_class |> stringr::str_split('---')
  index = match(care, names(class_list))
  for(i in 1:length(school_class)){
    to_add = rep('', no_col)
    for(ii in 1:length(care)) to_add[index[ii]] = split[[i]][ii]
    # to_add = c(split[[i]][1], split[[i]][2], split[[i]][3], split[[i]][4], rep('', no_col-4))
    for(y in 1:no_blanks){
      class_list[nrow(class_list)+1, ] = to_add
    }
  }
  last_name = class_list$`Pupil Last Name`
  last_name[(original_size+1):nrow(class_list)] = 'ZZZZZZZZZZZZZZZZZ'

  if(include_spare_column){
    spare = rep('', nrow(class_list))
    spare[last_name == 'ZZZZZZZZZZZZZZZZZ'] = 'Y'
    class_list$spare = spare
    names(class_list)[ncol(class_list)] = 'Spare?'
  }


  combo = paste0(class_list$`School name`,'---', class_list$`Class Name`, '---', last_name, class_list$`Pupil First Name`)
  class_list = class_list[order(combo),]


  class_list

}

add_hex_id <- function(data, ID_column = 'OME_ID', hex_column = 'HexId'){
  hex = data[[ID_column]] |> as.character.hexmode()
  data[[hex_column]] = hex
  data
}



add_OME_id <- function(class_list, school_link = "~/Wonde/Establishment_List_ID.xlsx", already_issued = ''){
  links = readxl::read_xlsx(school_link) |> as.data.frame()

  # Find the iDs we can still use.
  all_possible = 1:99999 |> formatC(width = 5, format = 'd', flag = '0')
  ID_info = already_issued |> stringr::str_sub(start = -5)
  all_possible = all_possible[which(!all_possible %in% ID_info)]

  OME_ID = rep(NA, nrow(class_list))

  estab_pre = links$EstablishmentID[which(links$URN == class_list$URN[1])]
  SchoolType_pre = links$Category[which(links$URN == class_list$URN[1])] |> dplyr::case_match('Primary' ~ 1,
                                                                                              'Secondary 11-16' ~2,
                                                                                              'Secondary 11-18' ~ 3)
  ID_counter = 1 ;
  OME_ID[1] = paste0('1', SchoolType_pre, estab_pre, all_possible[ID_counter])
  for(i in 2:nrow(class_list)){
    ID_counter = ID_counter + 1
    estab_cur = links$EstablishmentID[which(links$URN == class_list$URN[i])]
    SchoolType_cur = links$Category[which(links$URN == class_list$URN[i])] |> dplyr::case_match('Primary' ~ 1,
                                                                                                'Secondary 11-16' ~2,
                                                                                                'Secondary 11-18' ~ 3)
    if(length(estab_cur) == 0){
      stop(paste0('URN: ',class_list$URN[i], ' is not found in the establishment list.'))
    }
    # Estab remains the same.
    if(estab_pre == estab_cur){
      OME_ID[i] = paste0('1', SchoolType_pre, estab_pre, all_possible[ID_counter])
    }
    # Estab changes
    if(estab_pre != estab_cur){
      estab_pre = estab_cur
      SchoolType_pre = SchoolType_cur
      OME_ID[i] = paste0('1', SchoolType_pre, estab_pre, all_possible[ID_counter])
    }
  }
  class_list$OME_ID = OME_ID
  class_list
}



get_student_from_class <- function(data, URN, studentID, class_name, URNColumn = 'URN',
                                   ClassNameColumn = "ClassName",  StudentIDColumn = "StudentId"){
  index = which(data[[URNColumn]] == URN & data[[ClassNameColumn]] %in% class_name)

  data = data[index,]

  data

}


