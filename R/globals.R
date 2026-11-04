
# Column names used in dplyr non-standard evaluation.
# These are registered to avoid R CMD check notes about
# undefined global variables.

utils::globalVariables(c(
  # Establishment identifiers
  "est_id",
  "est_wonde_id",
  "est_wonde_name",
  "est_wonde_urn",
  "school_name",
  
  # Student identifiers and details
  "s_wonde_id",
  "sfn",
  "sln",
  "dob",
  "upn",
  "forename",
  "surname",
  "legal_forename",
  "legal_surname",
  "date_of_birth.date",
  "id",
  
  # Class information
  "c_wonde_id",
  "class_name",
  "no_classes",
  "classes.data",
  "classes.data__id",
  "classes.data__name",
  "classes.data__description",
  "classes.data__employees.data",
  "classes.data__employees.data__id",
  
  # Groups and years
  "groups.data",
  "groups.data__type",
  "groups.data__employees.data",
  "year.data.code",
  "year.data.name",
  
  # Teacher information
  "t_full",
  "tfn",
  "tln",
  ".teacher_priority",
  ".original_row",
  
  # Summary statistics
  "total_student",
  "total_with_issue",
  "TotalStudents",
  "StudentsInSmallClasses",
  
  # Legacy class-list column names
  "School name",
  "Class Name",
  "Pupil First Name"
))
