# Retrieve School Student and Staff Data from Wonde API

Retrieves detailed student, staff, and class information from the Wonde
API for either primary or secondary schools.

These functions serve as wrappers around the Wonde v1.0 API and return
structured data frames of students, staff, and their relationships to
classes, subjects, and groups. Both functions now include education
details.

## Usage

``` r
get_primary_school_student_data(school_id, token = NULL, verbose = TRUE)

get_secondary_school_student_data(school_id, token = NULL, verbose = TRUE)
```

## Arguments

- school_id:

  Character string giving the Wonde school ID.

- token:

  Character string; the Wonde API token used for authentication.

- verbose:

  Logical; if TRUE (default), prints progress messages to the console as
  data are fetched.

## Value

A named list of data frames, typically including:

- \`student\` – basic student and school info.

- \`students_group\` – student-group relationships.

- \`students_classes\` – student-class relationships and staff
  assignments.

- \`subjects\` – available subjects (mainly secondary).

- \`staff\` – employee information and groups.

- \`education_details\` – student education information.

## Details

Each function performs several authenticated API requests to Wonde for a
given \`school_id\`, assembling structured data on:

- \*\*school info\*\* – name, URN, establishment number, and phase of
  education.

- \*\*staff (employees)\*\* – teacher and employee data, including group
  memberships.

- \*\*students (basic info)\*\* – student-level data, including year
  group.

- \*\*students + classes\*\* – student-class relationships and
  associated staff.

- \*\*students + groups\*\* – student group memberships and related
  employees.

- \*\*subjects\*\* – subject-level metadata (typically secondary only).

- \*\*education details\*\* – student education information (available
  for both primary and secondary schools).

The functions rely on internal helpers:

- \`get_query()\` – handles paginated API requests.

- \`convert_list_element_to_df()\` – flattens nested list structures.

Any missing or NULL components are removed automatically before
returning.

## See also

[`get_query`](https://Jake-Powell.github.io/WondeCL/reference/get_query.md),
[`convert_list_element_to_df`](https://Jake-Powell.github.io/WondeCL/reference/convert_list_element_to_df.md)

Other wonde:
[`get_query()`](https://Jake-Powell.github.io/WondeCL/reference/get_query.md),
[`get_schools()`](https://Jake-Powell.github.io/WondeCL/reference/get_schools.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Primary school data
primary_data <- get_primary_school_student_data(
  school_id = "A123456789",
  token = Sys.getenv("WONDE_API_KEY")
)

# Secondary school data
secondary_data <- get_secondary_school_student_data(
  school_id = "B987654321",
  token = Sys.getenv("WONDE_API_KEY")
)

# Check available data sets
names(primary_data)
names(secondary_data)
} # }
```
