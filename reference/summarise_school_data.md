# Summarise school-level data extracted from Wonde API

Processes a list of Wonde school extracts and produces a summary table
showing year group availability, subject detection, student counts, and
data part coverage for each school. Designed to work with data returned
by
[`get_secondary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md).

## Usage

``` r
summarise_school_data(
  schools,
  WondeData,
  year_codes_target = c("7", "Year 7", "07", "Year 07", "Y7", "Yr7", "Yr 7"),
  subject_keywords = c("Maths", "Mathematics", "Ma", "Mathematic",
    "Mathematics (Assessment+)"),
  schools_id_column = "EstablishmentID"
)
```

## Arguments

- schools:

  Data frame containing school metadata (must include at least `URN`,
  `id`, and `name` columns).

- WondeData:

  Named list of school data extracts (as returned from
  [`get_secondary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md)).

- year_codes_target:

  Character vector of year codes used to identify the target year group
  (default includes multiple Year 7 code variations).

- subject_keywords:

  Character vector of subject name patterns to identify relevant
  subjects (default identifies Maths-related names).

- schools_id_column:

  Character string specifying the column in the schools data frame
  containing the school identifiers used to associate records with
  individual schools.

## Value

A data frame containing school-level summaries including:

- `year_code_values`: available year codes in the extract

- `HasTargetYear`: whether the target year group is present

- `target_year_subjects`: subjects available for the target year

- `GetSubjectByCode`: detection of target subject by subject code

- `class_desc_target_year`: available class descriptions for the target
  year

- `GetSubjectByClassDescription`: detection of subject by class
  description

- `total_students`: total number of students in the school

- `total_target_year_students`: total number of students in the target
  year group

- Logical indicators for which data parts were successfully extracted
  (e.g. `student`, `subjects`, `staff`, etc.)

## Details

The function checks the structure of each school's data extract, detects
the presence of the user-specified year group and subjects, and
identifies whether relevant Wonde endpoints were available. It is robust
to missing or incomplete data and merges all derived summaries back into
the supplied `schools` data frame.

## Examples

``` r
if (FALSE) {
summary_df <- summarise_school_data(
  schools = schools,
  WondeData = secondary_data_raw,
  year_codes_target = c("7", "Year 7"),
  subject_keywords = c("Maths", "Mathematics")
)

head(summary_df)
}
```
