# Check for students missing subject class assignments

Identifies students in the provided class list who have no recorded
class assignment (i.e., missing \`ClassId\`), retrieves their related
subject and class data from Wonde exports, and summarises the issue per
school. This function provides a clear overview of students who are not
currently enrolled in a subject class, such as Maths.

## Usage

``` r
check_students_missing_class(
  class_list_df,
  WondeData,
  subject_name = "Maths",
  output_prefix = "Students_without_subject_class"
)
```

## Arguments

- class_list_df:

  A data frame containing class list information, typically the output
  from
  [`extract_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/extract_class_list.md).
  Must include at least the columns \`SchoolId\`, \`SchoolName\`,
  \`StudentId\`, \`ClassId\`, \`StudentFirstName\`, and
  \`StudentLastName\`.

- WondeData:

  A named list of Wonde data frames, usually returned by
  [`get_secondary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md),
  where each element corresponds to a school and includes components
  such as \`students_classes\` and \`subjects\`.

- subject_name:

  Character string specifying the subject of interest (e.g.,
  \`"Maths"\`). This value is used only for output labeling. Default is
  \`"Maths"\`.

- output_prefix:

  Character string specifying a prefix for output filenames or logs.
  Default is \`"Students_without_subject_class"\`.

## Value

A list containing three data frames:

- \`missing_students\`:

  Data frame of students with missing class IDs.

- \`class_subject_of_missing\`:

  Data frame showing available class and subject details for each
  missing student, derived from \`WondeData\`.

- \`missing_per_school\`:

  Summary data frame showing the total number and percentage of affected
  students per school.

## Details

The function performs the following steps:

1.  Uses
    [`detect_students_missing_class()`](https://Jake-Powell.github.io/WondeCL/reference/detect_students_missing_class.md)
    to identify students without a class assignment.

2.  For each missing student, extracts any available class and subject
    information from their corresponding school's \`WondeData\` entry.

3.  Aggregates a school-level summary reporting the total number and
    percentage of affected students.

It is typically used as a diagnostic or quality-control step after data
extraction, before final analysis or export.

## Examples

``` r
if (FALSE) { # \dontrun{
missing_summary <- check_students_missing_class(
  class_list_df = class_list_df,
  WondeData = WondeData,
  subject_name = "Maths"
)

head(missing_summary$missing_per_school)
} # }
```
