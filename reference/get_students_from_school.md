# Get students from a school

Extracts student data for a given school name from the supplied Wonde
dataset.

## Usage

``` r
get_students_from_school(SchoolName, WondeData, schools)
```

## Arguments

- SchoolName:

  Character. The name of the school to retrieve students for.

- WondeData:

  List. A list object containing school-level Wonde data (not directly
  used but retained for consistency).

- schools:

  Data frame. A table mapping school names to their unique IDs (must
  include columns \`id\` and \`name\`).

## Value

A tibble containing student information with the following columns:
\`school_name\`, \`forename\`, \`surname\`, \`legal_forename\`,
\`legal_surname\`, \`date_of_birth.date\`, and \`year.data.name\`.

## Details

The function matches the provided \`SchoolName\` to its corresponding
\`id\` in the \`schools\` table, then selects the relevant student
information from \`secondary_data_raw\` for that school.

## Examples

``` r
if (FALSE) { # \dontrun{
get_students_from_school(
  SchoolName = "Springfield High School",
  WondeData = secondary_data_raw,
  schools = schools_list
)
} # }
```
