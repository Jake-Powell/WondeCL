


#' Summarise school information
#'
#' @param data pre-cleaned class list
#' @param school_info path to school level information
#'
#' @return
#' @export
#'
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

apply_withdrawn <- function(class_list,
                            withdrawn_students = "Primary Cohort Study - Parent_guardian withdrawal form(1-89).xlsx",
                            school_link= "~/Wonde/Establishment_List_ID.xlsx",
                            links_category = 'Primary',
                            expected_years = c(2019, 2020),
                            do_log = F){
  if(do_log) logr::log_open('Withdrawn_students.log')

  # Not started and not sure if needed.
  withdrawn_students = readxl::read_xlsx(withdrawn_students) |> as.data.frame()
  withdrawn_students = withdrawn_students[grepl('^No|^I want to withdraw',
                                                withdrawn_students$`Please confirm that you do not want your child to take part in the Observatory for Mathematical Education's cohort study:`),]

  links = readxl::read_xlsx(school_link) |> as.data.frame()
  links = links[grep(paste0(links_category, collapse = '|'), links$Category),]

  firstName = withdrawn_students$`Your child's first name` |>  toupper() |> stringr::str_replace_all('-',' ') |> stringi::stri_trans_general(id = 'Latin-ASCII')
  lastName = withdrawn_students$`Your child's surname` |> toupper() |> stringr::str_replace_all('-',' ') |> stringi::stri_trans_general(id = 'Latin-ASCII')
  SchoolName = withdrawn_students$`Name of school` |> toupper()
  withdrawn_students$`Your child's date of birth` = withdrawn_students$`Your child's date of birth` |>
    toupper() |> stringr::str_squish()
  DoB1A = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |>  as.Date(format = c('%d/%m/%Y'))
  DoB1B = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+') |> as.Date(format = c('%d/%m/%y'))
  DoB2A = withdrawn_students$`Your child's date of birth` |>  stringr::str_remove_all('\\s+') |> as.Date(format = c('%d.%m.%Y'))
  DoB2B = withdrawn_students$`Your child's date of birth` |>  stringr::str_remove_all('\\s+') |> as.Date(format = c('%d.%m.%y'))
  DoB3A = withdrawn_students$`Your child's date of birth` |>  stringr::str_remove_all('\\s+') |> as.Date(format = c('%d-%m-%Y'))
  DoB3B = withdrawn_students$`Your child's date of birth` |>  stringr::str_remove_all('\\s+') |> as.Date(format = c('%d-%m-%y'))
  DoB4A = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |>  as.Date(format = c('%d %B %Y'))
  DoB4B = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |>  as.Date(format = c('%d %B %y'))
  DoB5A = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |>  as.Date(format = c('%B %d %Y'))
  DoB5B = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |>  as.Date(format = c('%B %d %y'))
  DoB6A = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |>  as.Date(format = c('%b %d %Y'))
  DoB6B = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('TH |ST |ND |RD |,') |>  as.Date(format = c('%b %d %y'))
  DoB7A = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+|\\.|-|/') |>  as.Date(format = c('%d%m%Y'))
  DoB7B = withdrawn_students$`Your child's date of birth` |> stringr::str_remove_all('\\s+|\\.|-|/') |>  as.Date(format = c('%d%m%y'))

  DoBs = data.frame(withdrawn_students$`Your child's date of birth`,
                    DoB1A, DoB1B,
                    DoB2A, DoB2B,
                    DoB3A, DoB3B,
                    DoB4A, DoB4B,
                    DoB5A, DoB5B,
                    DoB6A, DoB6B,
                    DoB7A, DoB7B
                    )
  for(i in seq(2,ncol(DoBs),2)){
    DoBs[,i][grepl('^[0-9]{2}-', DoBs[,i] |> as.character())] = NA
  }
  for(i in seq(2,ncol(DoBs),2)){
    DoBs[,i+1][which(!is.na(DoBs[,i]))] = NA
  }
  for(i in 2:ncol(DoBs)){
    DoBs[,i][!grepl(paste0(expected_years, collapse = '|'), DoBs[,i])] = NA
  }

  DoB = apply(DoBs[,-1],1, function(x){x |> na.omit() |> unique() |> paste0(collapse = ', ')})
  # data.frame(withdrawn_students$`Your child's date of birth`, DoB) |> View()

  withdraw = data.frame(firstName, lastName, DoB, SchoolName)

  if(do_log) logr::log_print('Cleaned withdraw students data frame:')
  if(do_log) logr::log_print(withdraw)

  class_list$`Withdrawn?` = rep('',nrow(class_list))
  if(!'Print ID' %in% names(class_list)) class_list$`Print ID` = ''
  if(!'OME_ID' %in% names(class_list)) class_list$OME_ID = ''


  # loop over each student we want to withdraw.
  match_desc = rep(NA, nrow(withdraw))
  school_look = rep(NA, nrow(withdraw))
  for(i in 1:nrow(withdraw)){
    print(i)
    if(do_log) logr::log_print('Current withdrawl:')
    if(do_log) logr::log_print(withdraw[i,])
    cur = withdraw[i,]
    # Check the school is included in the class list.
    if(do_log) logr::log_print('Find the best school match...')
    w_school_words = withdraw$SchoolName[i] |>
      stringr::str_remove_all('[:punct:]') |>
      # stringr::str_replace_all('\\.', '\\\\.') |>
      stringr::str_split("\\s+") |>
      unlist()

    school_words = links$EstablishmentName |> toupper() |> stringr::str_remove_all('[:punct:]') |> stringr::str_split('\\s+')

    contain_words = lapply(w_school_words, function(word){
      grepl(word, links$EstablishmentName |> toupper())
    }) |> data.frame() |> rowSums(na.rm = T)
    exact_words = lapply(w_school_words, function(word){
      lapply(school_words, function(words){ word %in% words}) |> unlist()
    }) |> data.frame() |> rowSums(na.rm = T)


    best_match = which(exact_words == max(exact_words))

    match_schools = links$EstablishmentName[best_match]
    match_URN = links$URN[best_match]

    school_matches = data.frame(SchoolName = match_schools, URN = match_URN)
    school_look[i] = school_matches$SchoolName |> paste0(collapse = '\n')
    if(do_log) logr::log_print(school_matches)

    if(!all(match_URN %in% class_list$URN)){
      warning_msg = paste0('Row: ', i, '. Unable to withdraw student as matched school "', match_schools, '" is not found in the class list. This is usually caused by a school which has not been extracted from Wonde.')
      if(do_log){logr::log_warning(warning_msg)}else{warning(warning_msg)}
      match_desc[i] = 'No match, Wonde data for school not available.'
      next
    }

    potent_index = which(class_list$URN %in% school_matches$URN)

    potent = class_list[potent_index,]
    # potent |> View() ; cur

    # 1)Check for match using identical first name and surname.
    match_index = which(cur$firstName ==  potent$`Pupil First Name` |> toupper() &
            cur$lastName ==  potent$`Pupil Last Name` |> toupper() )
    if(length(match_index) == 1){
      match_desc[i] = 'Exact first and last name match (to one student)'
      class_list$DoB[potent_index[match_index]] =''
      class_list$UPN[potent_index[match_index]] =''
      class_list$`Print ID`[potent_index[match_index]] =''
      class_list$OME_ID[potent_index[match_index]] =''
      class_list$`Withdrawn?`[potent_index[match_index]] = 'Y'
      next
    }
    if(length(match_index) > 1){
      match_desc[i] = 'Multiple first and last name match, do not remove any'
      next
    }

    # 2) Check for match using partial first name and identical surname.
    withdraw_FN_parts = cur$firstName |> stringr::str_split(' ') |> unlist()

    match_index = which(grepl(paste(withdraw_FN_parts, collapse =  '|'), potent$`Pupil First Name` |> toupper() |> stringr::str_replace_all('-',' ')) &
                          cur$lastName ==  potent$`Pupil Last Name` |> toupper() )
    if(length(match_index) == 1){
      match_desc[i] = 'Partial first name, exact surname (to one student)'
      class_list$DoB[potent_index[match_index]] =''
      class_list$UPN[potent_index[match_index]] =''
      class_list$`Print ID`[potent_index[match_index]] =''
      class_list$OME_ID[potent_index[match_index]] =''
      class_list$`Withdrawn?`[potent_index[match_index]] = 'Y'
      next
    }

    # 2) Check for match using partial surname and identical first name.
    withdraw_FN_parts = cur$lastName |> stringr::str_split(' ') |> unlist()

    match_index = which(grepl(paste(withdraw_FN_parts, collapse =  '|'), potent$`Pupil Last Name` |> toupper() |> stringr::str_replace_all('-',' ')) &
                          cur$firstName ==  potent$`Pupil First Name` |> toupper() )
    if(length(match_index) == 1){
      match_desc[i] = 'Partial last name, exact first name (to one student)'
      class_list$DoB[potent_index[match_index]] =''
      class_list$UPN[potent_index[match_index]] =''
      class_list$`Print ID`[potent_index[match_index]] =''
      class_list$OME_ID[potent_index[match_index]] =''
      class_list$`Withdrawn?`[potent_index[match_index]] = 'Y'
      next
    }

    # 2) Check for match using partial surname and partial first name.
    withdraw_FN_parts = cur$firstName |> stringr::str_split(' ') |> unlist()
    withdraw_SN_parts = cur$lastName |> stringr::str_split(' ') |> unlist()

    match_index = which(grepl(paste(withdraw_SN_parts, collapse =  '|'), potent$`Pupil Last Name` |> toupper() |> stringr::str_replace_all('-',' ')) &
                          grepl(paste(withdraw_FN_parts, collapse =  '|'), potent$`Pupil First Name` |> toupper() |> stringr::str_replace_all('-',' ')))
    if(length(match_index) == 1){
      match_desc[i] = 'Partial last name, partial first name (to one student)'
      class_list$DoB[potent_index[match_index]] =''
      class_list$UPN[potent_index[match_index]] =''
      class_list$`Print ID`[potent_index[match_index]] =''
      class_list$OME_ID[potent_index[match_index]] =''
      class_list$`Withdrawn?`[potent_index[match_index]] = 'Y'
      next
    }

    # 3) Check for match using identical first name not identical surname and identical DoB.
    match_index = which(cur$firstName ==  potent$`Pupil First Name` |> toupper() &
                          cur$DoB ==  potent$DoB )
    if(length(match_index) == 1){
      match_desc[i] = 'Exact first name, no match surname, DoB match (to one student)'
      class_list$DoB[potent_index[match_index]] =''
      class_list$UPN[potent_index[match_index]] =''
      class_list$`Print ID`[potent_index[match_index]] =''
      class_list$OME_ID[potent_index[match_index]] =''
      class_list$`Withdrawn?`[potent_index[match_index]] = 'Y'
      next
    }
    if(length(match_index) > 1){
      match_desc[i] = 'Multiple first name match, no match surname, DoB match, do not remove any'
      next
    }

    # 3) Check for match using identical last name only
    match_index = which(cur$lastName ==  potent$`Pupil Last Name` |> toupper())
    if(length(match_index) == 1){
      match_desc[i] = 'Exact match surname only (to one student)'
      class_list$DoB[potent_index[match_index]] =''
      class_list$UPN[potent_index[match_index]] =''
      class_list$`Print ID`[potent_index[match_index]] =''
      class_list$OME_ID[potent_index[match_index]] =''
      class_list$`Withdrawn?`[potent_index[match_index]] = 'Y'
      next
    }

    match_desc[i] = 'Student not found in class list'

  }
  # data.frame(match_desc) |> View()
  if(do_log) logr::log_close()

  withdraw$MatchDetail = match_desc
  withdraw$SchoolCheck = school_look


  return(list(class_list = class_list,
              withdraw = withdraw))
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


create_school_blanks <- function(schools_with_size,
                                 teacher_name = c('Reception', 'Teacher'),
                                 class_name = 'Reception Class',
                                 type = 30,
                                 class_buffer = 5,
                                 class_size = 30,
                                 include_spare_column = F){
  school_blanks = lapply(1:nrow(schools_with_size), function(index){
    cur = schools_with_size[index,]
    print(index)
    if('classInfo' %in% names(cur)){class_info = cur$classInfo}else{class_info = NA}
    if('TeacherInfo' %in% names(cur)){teacher_info = cur$TeacherInfo}else{teacher_info = NA}

    if(class_info == '') class_info = NA ; if(teacher_info == '') teacher_info = NA
    if(is.na(class_info)){
      no_classes = ((cur$receptionStudents-1) %/% class_size) +1
      if(is.numeric(type)){
        count = rep(type, no_classes)
      }else{
        count = rep( cur$receptionStudents/no_classes |> ceiling() + class_buffer, no_classes)
      }
      classes = data.frame(name = paste0(class_name, ' ', 1:no_classes), count = count)
    }
    else{
      classes = class_info |> stringr::str_split(', ') |> unlist() |> stringr::str_split('->') |> unlist() |> stringr::str_squish() |> matrix(ncol = 2, byrow = T) |> data.frame()
      names(classes) = c('name', 'count')
    }

    if(is.na(teacher_info)){
      classes$teacher_firstname = teacher_name[1]
      classes$teacher_surname = teacher_name[2]
    }else{
      teachers = teacher_info |> stringr::str_split(', |---') |> unlist() |> matrix(ncol = 2, byrow = T) |> data.frame()
      classes$teacher_firstname = teachers[[1]]
      classes$teacher_surname = teachers[[2]]

    }

    class_list_list = lapply(1:nrow(classes), function(row_index){
      out = data.frame(A = cur$SchoolName |> rep(classes$count[row_index]),
                              B = cur$URN,
                              C = classes$name[row_index],
                              D = paste0(classes$teacher_firstname[row_index], ' ', classes$teacher_surname[row_index]) ,
                              E = 1:classes$count[row_index],
                              G = '',
                              H = NA,
                              H2 = NA,
                              I = classes$teacher_firstname[row_index],
                              J = classes$teacher_surname[row_index]
                              )
      names(out) = c("School name", "URN", "Class Name", "Main Teacher Name", "Pupil First Name", "Pupil Last Name",
                      "DoB", "UPN", "Main Teacher First Name", "Main Teacher Last Name")
      out
    })
    dd = rbind_aggro(class_list_list)
    dd[["Pupil First Name"]] = 1:nrow(dd)

    dd
  })
  out = rbind_aggro(school_blanks)

  if(include_spare_column){
    out$`Spare?` = ''
  }

  out
}


