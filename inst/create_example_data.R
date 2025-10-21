#' Example realistic WondeData template
#'
#' This dataset simulates a realistic Wonde export for two primary schools,
#' aligned with the structure used throughout the package functions.
#'
#' @format Named list with two schools ("S123456" and "S654321"), each containing:
#' \describe{
#'   \item{student}{Student-level details}
#'   \item{education_details}{Student UPN mappings}
#'   \item{students_classes}{Class assignments and teacher links}
#'   \item{students_group}{Registration-style grouping data}
#'   \item{subjects}{Subjects offered (Maths, English, French)}
#'   \item{staff}{Simplified teacher data}
#' }
#'
#' @examples
#' data("WondeData_template")
#' str(WondeData_template$S123456)
#'
#' @keywords internal
create_WondeData_template <- function() {
  
  # Helper to make random IDs like A123456789
  make_id <- function(n = 1) {
    paste0("A", sample(1e7:1e10, n, replace = TRUE))
  }
  
  # helper function to build one school
  make_school <- function(school_id, school_name, urn, teacher_first, teacher_last, class_names, pupil_data) {
    
    # Standardize column names
    names(pupil_data) <- trimws(names(pupil_data))
    
    required_cols <- c("Class Name", "Pupil First Name", "Pupil Last Name", "DoB", "UPN")
    missing_cols <- setdiff(required_cols, names(pupil_data))
    if (length(missing_cols) > 0) {
      stop("Missing columns in pupil_data: ", paste(missing_cols, collapse = ", "))
    }
    
    n <- nrow(pupil_data)
    unique_classes <- unique(pupil_data$`Class Name`)
    
    # ---- Generate IDs ----
    student_ids <- make_id(n)
    class_ids   <- setNames(make_id(length(unique_classes)), unique_classes)
    group_ids   <- setNames(make_id(length(unique_classes)), unique_classes)
    subject_ids <- setNames(make_id(2), c("Mathematics", "French"))
    
    # ---- Choose one "multi-teacher" class ----
    multi_teacher_class <- sample(unique_classes, 1)
    
    # ---- Teachers ----
    # Base main teacher
    main_teacher <- data.frame(
      id = make_id(length(unique_classes)),
      class_name = unique_classes,
      forename = rep(teacher_first, length(unique_classes)),
      surname  = rep(teacher_last, length(unique_classes)),
      role = "Main Teacher",
      stringsAsFactors = FALSE
    )
    
    # Add assistant only for one class
    assistant_teacher <- data.frame(
      id = make_id(1),
      class_name = multi_teacher_class,
      forename = paste0(teacher_first, "_Assist"),
      surname  = paste0(teacher_last, "_Assist"),
      role = "Assistant Teacher",
      stringsAsFactors = FALSE
    )
    
    teachers <- rbind(main_teacher, assistant_teacher)
    
    # ---- Students ----
    student <- data.frame(
      id = student_ids,
      forename = pupil_data$`Pupil First Name`,
      surname = pupil_data$`Pupil Last Name`,
      school_id = school_id,
      school_name = school_name,
      school_establishment_number = as.character(urn),
      school_urn = as.character(urn),
      date_of_birth.date = pupil_data$DoB,
      year.data.code = "7",
      year.data.name = "Year 7",
      students.data__year.data.code = "7",
      stringsAsFactors = FALSE
    )
    
    # ---- Education Details (UPN) ----
    education_details <- data.frame(
      id = student$id,
      education_details.data.upn = pupil_data$UPN,
      stringsAsFactors = FALSE
    )
    
    # ---- Subjects ----
    subjects <- data.frame(
      id = unname(subject_ids),
      name = names(subject_ids),
      stringsAsFactors = FALSE
    )
    rownames(subjects) <- subjects$name
    
    # ---- Students ↔ Classes ----
    # If the student's class is the "multi-teacher" one, give them both teachers
    students_classes <- do.call(rbind, lapply(seq_len(n), function(i) {
      class_name <- pupil_data$`Class Name`[i]
      student_id <- student$id[i]
      if (class_name == multi_teacher_class) {
        assigned_teachers <- teachers[teachers$class_name == class_name, ]
      } else {
        assigned_teachers <- teachers[teachers$class_name == class_name & teachers$role == "Main Teacher", ]
      }
      
      do.call(rbind, lapply(seq_len(nrow(assigned_teachers)), function(j) {
        data.frame(
          id = student_id,
          classes.data__id = class_ids[class_name],
          classes.data__name = class_name,
          classes.data__code = paste0("CL_", substr(class_ids[class_name], 2, 6)),
          classes.data__description = paste("Class", class_name),
          classes.data__subject = ifelse(class_name == "French", subjects["French", "id"], subjects["Mathematics", "id"]),
          classes.data__employees.data__id = assigned_teachers$id[j],
          classes.data__employees.data__forename = assigned_teachers$forename[j],
          classes.data__employees.data__surname = assigned_teachers$surname[j],
          classes.data__employees.data__meta.is_main_teacher = assigned_teachers$role[j] == "Main Teacher",
          classes.data__year_group = "Year 7",
          stringsAsFactors = FALSE
        )
      }))
    }))
    
    # ---- Registration Groups ----
    students_group <- do.call(rbind, lapply(seq_len(n), function(i) {
      class_name <- pupil_data$`Class Name`[i]
      student_id <- student$id[i]
      if (class_name == multi_teacher_class) {
        assigned_teachers <- teachers[teachers$class_name == class_name, ]
      } else {
        assigned_teachers <- teachers[teachers$class_name == class_name & teachers$role == "Main Teacher", ]
      }
      
      do.call(rbind, lapply(seq_len(nrow(assigned_teachers)), function(j) {
        data.frame(
          id = student_id,
          groups.data__id = group_ids[class_name],
          groups.data__name = paste0("Reg_", class_name),
          groups.data__type = "REGISTRATION",
          groups.data__employees.data__forename = assigned_teachers$forename[j],
          groups.data__employees.data__surname = assigned_teachers$surname[j],
          groups.data__employees.data__meta.is_main_teacher = assigned_teachers$role[j] == "Main Teacher",
          stringsAsFactors = FALSE
        )
      }))
    }))
    
    list(
      student = student,
      education_details = education_details,
      students_classes = students_classes,
      students_group = students_group,
      subjects = subjects,
      staff = teachers
    )
  }
  
  # ===============================================================
  # Example pupil subsets
  # ===============================================================
  
  lav_pupils <- data.frame(
    `Class Name` = c(rep("Fernleaf", 15), rep("French", 15)),
    `Pupil First Name` = c(
      "Sherilyn","Angelyn","Lynelle","Mora","Phoebe","Ashley","Aubrey","Opal",
      "Natalia","Joel","Davis","Jeannetta","Antonette","Susanna","Julissa",
      "Danielle","Stefani","Tawana","Ilene","Deetta","Vicki","Saundra",
      "Brain","Minna","Marcelina","Andy","Kristofer","Meaghan","Wade","Antonette"
    )[1:30],
    `Pupil Last Name` = c(
      "Tamarind","Cherimoya","Strawberry","Kiwifruit","Lemon","Cloudberry","Blackcurrant",
      "Cucumber","Yuzu","Durian","Melon","Huckleberry","Salak","Lemon","Pear",
      "Gooseberry","Passionfruit","Goji berry","Bilberry","Rambutan","Mangosteen","Apricot",
      "Kiwifruit","Boysenberry","Cherimoya","Passionfruit","Blackcurrant","Mango","Damson","Salak"
    )[1:30],
    DoB = paste0("1910-", sprintf("%02d", sample(1:12, 30, TRUE)), "-", sprintf("%02d", sample(1:28, 30, TRUE))),
    UPN = replicate(30, paste0(sample(c(LETTERS, 0:9), 12, TRUE), collapse = "")),
    stringsAsFactors = FALSE
  )
  
  dig_pupils <- data.frame(
    `Class Name` = rep("Foxglove", 30),
    `Pupil First Name` = c(
      "Andera","Michele","Bettyann","Susy","Jamie","Shanita","Moshe","Allison",
      "Wynona","Dick","Rosario","Caleb","Claud","Margherita","Vivian",
      "Leatrice","Krystle","Floria","Rosalyn","Rosalind","Jerrell","Bella",
      "Sophia","Pamila","Kaleigh","Delinda","Lorriane","Silva","Beula","Nolan"
    ),
    `Pupil Last Name` = c(
      "Durian","Marionberry","Rambutan","Clementine","Watermelon","Lime","Damson","Salak",
      "Salmonberry","Gooseberry","Bilberry","Miracle fruit","Strawberry","Gooseberry",
      "Purple mangosteen","Pineapple","Cherry","Soursop","Mango","Salak","Jackfruit",
      "Loquat","Jackfruit","Boysenberry","Mandarine","Banana","Yuzu","Banana","Grape","Cantaloupe"
    ),
    DoB = paste0("1910-", sprintf("%02d", sample(1:12, 30, TRUE)), "-", sprintf("%02d", sample(1:28, 30, TRUE))),
    UPN = replicate(30, paste0(sample(c(LETTERS, 0:9), 12, TRUE), collapse = "")),
    stringsAsFactors = FALSE
  )
  
  names(lav_pupils) <- names(lav_pupils) |> stringr::str_replace_all("\\.", " ")
  names(dig_pupils) <- names(dig_pupils) |> stringr::str_replace_all("\\.", " ")
  
  list(
    S123456 = make_school(
      make_id(1), "Lavandula Primary School", 123456,
      teacher_first = "Katherine", teacher_last = "Brian",
      class_names = unique(lav_pupils$`Class Name`),
      pupil_data = lav_pupils
    ),
    S654321 = make_school(
      make_id(1), "Digitalis Purpurea Primary School", 654321,
      teacher_first = "Elsie", teacher_last = "Kelsey",
      class_names = unique(dig_pupils$`Class Name`),
      pupil_data = dig_pupils
    )
  )
}

# To add this dataset to your package:
# usethis::use_data(create_WondeData_template(), name = "WondeData_template", overwrite = TRUE)
