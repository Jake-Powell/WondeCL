# Example Wonde-formatted raw data extract

A simulated example of a Wonde API data extract containing multiple
schools, structured to match the output typically returned by
[`get_primary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md)
or
[`get_secondary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md).

This dataset is designed for testing, demonstrations, and development of
functions within the package that operate on Wonde-formatted data. It
mirrors the typical nested structure returned by the Wonde API,
including school-, student-, class-, and group-level elements.

## Usage

``` r
WondeData_template
```

## Format

A named list of two school datasets:

- `S123456`:

  Simulated data for \*Lavandula Primary School\*.

- `S654321`:

  Simulated data for \*Digitalis Purpurea Primary School\*.

Each school entry is a list containing:

- `student`:

  A data frame of pupil-level information including `id`, `forename`,
  `surname`, `date_of_birth.date`, and year group fields.

- `education_details`:

  A data frame linking pupil IDs to their unique pupil numbers (`UPN`).

- `students_classes`:

  A data frame describing the relationship between pupils, their
  classes, and class-level metadata (e.g. subject, teacher, and year
  group).

- `students_group`:

  A data frame of group assignments such as registration or tutor
  groups, including the assigned teachers.

- `subjects`:

  A data frame listing available subjects for the school, each
  identified by a subject ID.

- `staff`:

  A data frame of staff members (teachers) associated with the classes
  and groups. Some classes include both a main and an assistant teacher.

## Details

The dataset is suitable for testing package functions such as:

- [`extract_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/extract_class_list.md)

- [`summarise_school_data()`](https://Jake-Powell.github.io/WondeCL/reference/summarise_school_data.md)

- [`extract_field_across_schools()`](https://Jake-Powell.github.io/WondeCL/reference/extract_field_across_schools.md)

- [`extract_group_names_across_schools()`](https://Jake-Powell.github.io/WondeCL/reference/extract_group_names_across_schools.md)

All IDs (e.g. school, student, staff, subject, class, group) are
generated in the format `"A#########"` (one letter followed by 7–10
digits), and the values are simulated for demonstration only.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the example data
data("WondeData_template")

# Inspect structure
str(WondeData_template, max.level = 2)

# Extract a class list from the first school
extract_class_list(WondeData_template, schools, maths_names, maths_desc_names, year_pattern)
} # }
```
