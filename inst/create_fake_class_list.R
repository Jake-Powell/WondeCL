Headers = c('EST_ID',	'EST_NAME',	'URN',	'EST_CATEGORY',	'EST_TYPE',	'CLASS_ID',	'CLASS_NAME',	'OME_ID_TEACHER',	'TEACHER_FIRST_NAME',	'TEACHER_LAST_NAME',	'OME_ID',	'PRINT_ID',	'UPN',	'STUDENT_FIRST_NAME',	'STUDENT_LAST_NAME',	'DOB')

# for pupil first name we use -> https://github.com/smashew/NameDatabases/blob/master/NamesDatabases/first%20names/us.txt
# last name -> https://gist.github.com/lasagnaphil/7667eaeddb6ed0c565f0cb653d756942
FN = c('Melina', 'Gary', 'Demarcus', 'Marcelo', 'Aliza', 'Germaine', 'Jonell', 'Marcos', 'Chantelle', 'Chi', 'Gertrude', 'Cherry', 'Kenton', 'Bula', 'Elisabeth', 'Kandra', 'Aleen', 'Shenita', 'Laurinda', 'Lissa', 'Kera', 'Janett', 'Jana', 'Sandy', 'Brain', 'Olevia', 'Dannette', 'Tisha', 'Caitlin', 'Novella', 'Clementina', 'Pandora', 'Janey', 'Cherryl', 'Erline', 'Lawana', 'Eliana', 'Rich', 'Shantelle', 'Cletus', 'Pauletta', 'Ollie', 'Joline', 'Rudy', 'Chantell', 'Kaye', 'Gilda', 'Russell', 'Ardath', 'Thea', 'Riley', 'Candelaria', 'Emeline', 'Teisha', 'Emeline', 'Shawnee', 'Elidia', 'Denita', 'Maira', 'Grayce')
LN = c('Apple', 'Apricot', 'Avocado', 'Banana', 'Bilberry', 'Blackberry', 'Blackcurrant', 'Blueberry', 'Boysenberry', 'Currant', 'Cherry', 'Cherimoya', 'Chico fruit', 'Cloudberry', 'Coconut', 'Cranberry', 'Cucumber', 'Custard apple', 'Damson', 'Date', 'Dragonfruit', 'Durian', 'Elderberry', 'Feijoa', 'Fig', 'Goji berry', 'Gooseberry', 'Grape', 'Raisin', 'Grapefruit', 'Guava', 'Honeyberry', 'Huckleberry', 'Jabuticaba', 'Jackfruit', 'Jambul', 'Jujube', 'Juniper berry', 'Kiwano', 'Kiwifruit', 'Kumquat', 'Lemon', 'Lime', 'Loquat', 'Longan', 'Lychee', 'Mango', 'Mangosteen', 'Marionberry', 'Melon', 'Cantaloupe', 'Honeydew', 'Watermelon', 'Miracle fruit', 'Mulberry', 'Nectarine', 'Nance', 'Olive', 'Orange', 'Blood orange', 'Clementine', 'Mandarine', 'Tangerine', 'Papaya', 'Passionfruit', 'Peach', 'Pear', 'Persimmon', 'Physalis', 'Plantain', 'Plum', 'Prune', 'Pineapple', 'Plumcot', 'Pomegranate', 'Pomelo', 'Purple mangosteen', 'Quince', 'Raspberry', 'Salmonberry', 'Rambutan', 'Redcurrant', 'Salal berry', 'Salak', 'Satsuma', 'Soursop', 'Star fruit', 'Solanum quitoense', 'Strawberry', 'Tamarillo', 'Tamarind', 'Ugli fruit', 'Yuzu')

# ============================================================
# WondeCL example data generation
# ============================================================

# Requires the FN and LN vectors defined earlier in this script.

set.seed(12)

# ------------------------------------------------------------
# 1. School information
# ------------------------------------------------------------

schools <- data.frame(
  EST_ID = c("A", "B"),
  EST_NAME = c(
    "Lavandula Primary School",
    "Digitalis Purpurea Primary School"
  ),
  URN = c("123456", "654321"),
  EST_CATEGORY = rep("Primary", 2),
  EST_TYPE = rep("P", 2),
  EST_WONDE_ID = c("WONDE-E001", "WONDE-E002"),
  EST_WONDE_NAME = c(
    "Lavandula Primary School",
    "Digitalis Purpurea Primary School"
  ),
  EST_WONDE_EST_NO = c("1001", "1002"),
  EST_WONDE_URN = c("123456", "654321"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 2. Class information
# ------------------------------------------------------------

classes <- data.frame(
  EST_ID = c("A", "A", "B"),
  CLASS_ID = c(
    "449073-1",
    "449089-1",
    "802077-1"
  ),
  CLASS_NAME = c(
    "Fernleaf",
    "French",
    "Foxglove"
  ),
  CLASS_WONDE_ID = c(
    "WONDE-C001",
    "WONDE-C002",
    "WONDE-C003"
  ),
  CLASS_WONDE_NAME = c(
    "Fernleaf",
    "French",
    "Foxglove"
  ),
  TEACHER_WONDE_ID = c(
    "WONDE-T001",
    "WONDE-T002",
    "WONDE-T003"
  ),
  TEACHER_FIRST_NAME = c(
    "Miss",
    "Linda",
    "Elsie"
  ),
  TEACHER_LAST_NAME = c(
    "Katherine",
    "Ligon",
    "Kelsey"
  ),
  OME_ID_TEACHER = c(
    "Cult-1992",
    "Cult-1996",
    "Cult-1990s"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 3. Student information
# ------------------------------------------------------------

n_students <- 60L

class_index <- rep(
  seq_len(nrow(classes)),
  times = c(15L, 15L, 30L)
)

school_index <- match(
  classes$EST_ID[class_index],
  schools$EST_ID
)

student_data <- data.frame(
  STUDENT_WONDE_ID = sprintf(
    "WONDE-S%03d",
    seq_len(n_students)
  ),
  OME_ID = as.character(seq_len(n_students)),
  PRINT_ID = as.character(seq_len(n_students)),
  UPN = replicate(
    n_students,
    paste0(
      sample(c(LETTERS, 0:9), 13),
      collapse = ""
    )
  ),
  PUPIL_FIRST_NAME = FN,
  PUPIL_LAST_NAME = sample(LN, n_students),
  DOB = paste0(
    "1910-",
    sprintf("%02d", sample(1:12, n_students, replace = TRUE)),
    "-",
    sprintf("%02d", sample(1:28, n_students, replace = TRUE))
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 4. Combine into class_list_example
# ------------------------------------------------------------

class_list_example <- data.frame(
  # Establishment information
  EST_ID = schools$EST_ID[school_index],
  EST_NAME = schools$EST_NAME[school_index],
  URN = schools$URN[school_index],
  EST_CATEGORY = schools$EST_CATEGORY[school_index],
  EST_TYPE = schools$EST_TYPE[school_index],
  
  # Wonde establishment information
  EST_WONDE_ID = schools$EST_WONDE_ID[school_index],
  EST_WONDE_NAME = schools$EST_WONDE_NAME[school_index],
  EST_WONDE_EST_NO = schools$EST_WONDE_EST_NO[school_index],
  EST_WONDE_URN = schools$EST_WONDE_URN[school_index],
  
  # Class information
  CLASS_ID = classes$CLASS_ID[class_index],
  CLASS_NAME = classes$CLASS_NAME[class_index],
  CLASS_WONDE_ID = classes$CLASS_WONDE_ID[class_index],
  CLASS_WONDE_NAME = classes$CLASS_WONDE_NAME[class_index],
  
  # Teacher information
  OME_ID_TEACHER = classes$OME_ID_TEACHER[class_index],
  TEACHER_WONDE_ID = classes$TEACHER_WONDE_ID[class_index],
  TEACHER_FIRST_NAME = classes$TEACHER_FIRST_NAME[class_index],
  TEACHER_LAST_NAME = classes$TEACHER_LAST_NAME[class_index],
  TEACHER_FULL_NAME = paste(
    classes$TEACHER_FIRST_NAME[class_index],
    classes$TEACHER_LAST_NAME[class_index]
  ),
  TEACHER_MAIN = rep(TRUE, n_students),
  
  # Student information
  STUDENT_WONDE_ID = student_data$STUDENT_WONDE_ID,
  OME_ID = student_data$OME_ID,
  PRINT_ID = student_data$PRINT_ID,
  UPN = student_data$UPN,
  PUPIL_FIRST_NAME = student_data$PUPIL_FIRST_NAME,
  PUPIL_LAST_NAME = student_data$PUPIL_LAST_NAME,
  DOB = student_data$DOB,
  
  # Operational fields
  WITHDRAWN = rep(NA_character_, n_students),
  SPARE = rep(NA_character_, n_students),
  DATE_OF_WITHDRAWAL = rep(NA_character_, n_students),
  
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 5. Save package dataset
# ------------------------------------------------------------

usethis::use_data(
  class_list_example,
  overwrite = TRUE
)

# ============================================================
# Teacher master list example
# ============================================================

teacher_master_list_example <- data.frame(
  
  OME_ID = c(
    "Cult-1992",
    "Cult-1996",
    "Cult-1990s",
    "Cult-1983",
    "Cult-1989",
    "Cult-1931"
  ),
  
  EXTERNAL_REFERENCE = sprintf(
    "EXT-T%03d",
    1:6
  ),
  
  TEACHER_WONDE_ID = sprintf(
    "WONDE-T%03d",
    1:6
  ),
  
  TEACHER_WONDE_MIS_ID = sprintf(
    "MIS-T%03d",
    1:6
  ),
  
  TEACHER_FIRST_NAME = c(
    "Miss",
    "Linda",
    "Elsie",
    "Irene",
    "Sharon",
    "Summer"
  ),
  
  TEACHER_LAST_NAME = c(
    "Katherine",
    "Ligon",
    "Kelsey",
    "Doyle",
    "Roberts",
    "King"
  ),
  
  EST_ID = c(
    "A", "A", "B",
    "A", "A", "B"
  ),
  
  EST_TYPE = rep("P", 6),
  
  CLASS_ID = c(
    "449073-1",
    "449089-1",
    "802077-1",
    "449073-1",
    "449073-1, 449089-1",
    "802077-1"
  ),
  
  LAST_UPDATED = as.Date(c(
    "2026-08-01",
    "2026-08-01",
    "2026-08-01",
    "2026-08-05",
    "2026-08-05",
    "2026-08-07"
  )),
  
  NOTES = c(
    NA,
    NA,
    NA,
    "Example additional teacher",
    "Example teacher associated with two classes",
    "Example additional teacher"
  ),
  
  stringsAsFactors = FALSE
)

# Add a full teacher name
teacher_master_list_example$TEACHER_FULL_NAME <- paste(
  teacher_master_list_example$TEACHER_FIRST_NAME,
  teacher_master_list_example$TEACHER_LAST_NAME
)

# Add standardised school name
teacher_master_list_example$EST_NAME <- schools$EST_NAME[
  match(
    teacher_master_list_example$EST_ID,
    schools$EST_ID
  )
]

# Reorder columns
teacher_master_list_example <- teacher_master_list_example[
  ,
  c(
    "OME_ID",
    "EST_ID",
    "EST_NAME",
    "EST_TYPE",
    "CLASS_ID",
    "TEACHER_WONDE_ID",
    "TEACHER_WONDE_MIS_ID",
    "TEACHER_FIRST_NAME",
    "TEACHER_LAST_NAME",
    "TEACHER_FULL_NAME",
    "EXTERNAL_REFERENCE",
    "LAST_UPDATED",
    "NOTES"
  )
]

# Save package dataset
usethis::use_data(
  teacher_master_list_example,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 6. Validate example datasets
# ------------------------------------------------------------

stopifnot(
  nrow(class_list_example) == 60L,
  nrow(teacher_master_list_example) == 6L,
  length(unique(class_list_example$EST_ID)) == 2L,
  length(unique(class_list_example$CLASS_WONDE_ID)) == 3L,
  !anyDuplicated(class_list_example$STUDENT_WONDE_ID),
  all(class_list_example$TEACHER_WONDE_ID %in%
        teacher_master_list_example$TEACHER_WONDE_ID)
)

# Check that the generated class list contains the columns
# required by WondeCL::produce_class_lists().
required <- c(
  "EST_ID",
  "EST_WONDE_ID",
  "EST_WONDE_NAME",
  "EST_WONDE_URN",
  "STUDENT_WONDE_ID",
  "PUPIL_FIRST_NAME",
  "PUPIL_LAST_NAME",
  "DOB",
  "UPN",
  "TEACHER_FULL_NAME",
  "TEACHER_FIRST_NAME",
  "TEACHER_LAST_NAME",
  "CLASS_NAME"
)

stopifnot(
  all(required %in% names(class_list_example))
)
