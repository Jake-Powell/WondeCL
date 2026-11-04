# Reduce Duplicated Student-Teacher Records to the Best Teacher

Filters a class list data frame to prioritise records associated with
the main teacher for each student. Where a student has one or more
records marked as the main teacher, only those records are retained.
Where no main teacher is identified, all existing teacher records for
that student are retained.

The function supports the package's configurable column-name system,
automatically converting input column names to their internal
equivalents before processing and restoring the configured names in the
returned data frame.

## Usage

``` r
reduce_to_best_teacher(data)
```

## Arguments

- data:

  A data frame containing student-teacher records. The following columns
  are required, using either their internal names or the currently
  configured column names:

  est_wonde_id

  :   Wonde establishment identifier (default: \`EST_WONDE_ID\`).

  s_wonde_id

  :   Wonde student identifier (default: \`STUDENT_WONDE_ID\`).

  t_main

  :   Indicator of whether the teacher is the main teacher (default:
      \`TEACHER_MAIN\`).

  The main teacher indicator should contain logical values or values
  that can be converted to logical using \`as.logical()\`.

## Value

A data frame containing the retained student-teacher records, with
column names restored to the package's configured naming convention.

For each unique establishment-student combination:

- If one or more main-teacher records exist, all such records are
  retained and non-main-teacher records are removed.

- If no main-teacher record exists, all existing records for that
  student are retained.

## Details

The function performs the following steps:

1.  Converts configured column names to the package's internal naming
    convention using \`rename_to_internal_names()\`.

2.  Checks that the required establishment, student and main teacher
    columns are present.

3.  Replaces missing (\`NA\`) main teacher indicators with \`FALSE\`.

4.  Identifies records where the teacher is marked as the main teacher.

5.  Identifies students using the combination of establishment ID and
    student ID, preventing records from different schools being treated
    as belonging to the same student.

6.  Retains main-teacher records where available, otherwise retaining
    the student's existing non-main-teacher records.

7.  Checks that all original establishment-student combinations remain
    represented, issuing a warning if any are lost.

8.  Converts the column names back to the configured naming convention
    using \`rename_to_configured_names()\`.

The function operates at the student level rather than the class level.
Consequently, if a student has a main teacher in one class,
non-main-teacher records from their other classes may also be removed.

The function does not guarantee a single record per student where
multiple main teachers exist or where no main teacher is identified.

This function is typically used after joining student, class and teacher
data, and before producing summary class lists using functions such as
\`produce_class_lists()\`.

## See also

Other class_list:
[`add_student_to_school_class()`](https://Jake-Powell.github.io/WondeCL/reference/add_student_to_school_class.md),
[`clean_teacher_names()`](https://Jake-Powell.github.io/WondeCL/reference/clean_teacher_names.md),
[`create_school_blanks()`](https://Jake-Powell.github.io/WondeCL/reference/create_school_blanks.md),
[`produce_class_lists()`](https://Jake-Powell.github.io/WondeCL/reference/produce_class_lists.md),
[`remove_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_class.md),
[`remove_school_from_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/remove_school_from_class_list.md),
[`remove_student_from_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_student_from_class.md),
[`update_class_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_name.md),
[`update_class_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_teacher.md),
[`update_school_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_school_name.md),
[`update_student_class()`](https://Jake-Powell.github.io/WondeCL/reference/update_student_class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Reduce student-teacher records using the default column names
reduced <- reduce_to_best_teacher(class_list_df)

# Change the configured naming convention
set_column_names(
  s_wonde_id = "PUPIL_ID",
  t_main = "IS_MAIN_TEACHER"
)

# The function automatically uses the updated column names
reduced <- reduce_to_best_teacher(class_list_df)

# Restore the default configuration
reset_column_names()
} # }
```
