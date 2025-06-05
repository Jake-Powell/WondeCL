#' Withdraw students from a class list
#'
#' @param class_list class list
#' @param withdrawn_students path to an .xlsx file containing the withdrawn students.
#' @param school_link
#' @param links_category
#' @param expected_years
#' @param do_log
#'
#' @return
#' @export
#'
#' @examples
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
