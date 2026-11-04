# Produce Consolidated Class Lists

Produces a consolidated class list by aggregating student and class
information and selecting one teacher record per student.

The function supports the package's configurable column-name system,
converting input columns to internal names before processing and
restoring configured names in the output.

## Usage

``` r
produce_class_lists(
  data,
  class_name_column = "class_name",
  default_teacher = "Reception teacher",
  default_teacher_FN = "Reception",
  default_teacher_SN = "Teacher"
)
```

## Arguments

- data:

  A data frame containing student, class and teacher information.
  Required columns include establishment identifiers, student
  identifiers, student names, date of birth, UPN, class name and teacher
  details.

- class_name_column:

  Character string specifying the class-name column to use. Accepts an
  internal column key or its configured name. Defaults to
  \`"class_name"\`.

- default_teacher:

  Character string used when the selected teacher's full name is
  missing. Defaults to \`"Reception teacher"\`.

- default_teacher_FN:

  Character string used when the selected teacher's first name is
  missing. Defaults to \`"Reception"\`.

- default_teacher_SN:

  Character string used when the selected teacher's surname is missing.
  Defaults to \`"Teacher"\`.

## Value

A data frame containing one row per unique establishment-student
combination, including:

- Establishment identifiers and school information.

- Consolidated class names.

- Student names, date of birth and UPN.

- Selected teacher's full name, first name and surname.

Column names are returned using the package's configured naming
convention.

## Details

The function performs the following steps:

1.  Converts input column names to the internal naming convention.

2.  Validates the required columns and selected class-name field.

3.  Groups records by establishment ID and Wonde student ID.

4.  Aggregates student information and all unique class names.

5.  Selects one teacher record per student, prioritising records marked
    as the main teacher. Where multiple records have equal priority, the
    first record is selected.

6.  Retains the selected teacher's full name, first name and surname
    together.

7.  Replaces missing teacher name fields with the specified defaults.

8.  Converts student names to title case and sorts the output.

9.  Restores the configured column names.

Class information is aggregated independently of teacher selection.
Students assigned to multiple classes therefore retain all unique class
names, even though only one teacher is reported.

Teacher selection is performed within each establishment using the
combination of \`est_wonde_id\` and \`s_wonde_id\`.

This function is typically used after \`clean_class_names()\` and
\`reduce_to_best_teacher()\`.

## See also

Other class_list:
[`add_student_to_school_class()`](https://Jake-Powell.github.io/WondeCL/reference/add_student_to_school_class.md),
[`clean_teacher_names()`](https://Jake-Powell.github.io/WondeCL/reference/clean_teacher_names.md),
[`create_school_blanks()`](https://Jake-Powell.github.io/WondeCL/reference/create_school_blanks.md),
[`reduce_to_best_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/reduce_to_best_teacher.md),
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
class_list <- produce_class_lists(data)

class_list <- produce_class_lists(
  data,
  class_name_column = "CLASS_WONDE_NAME"
)
} # }
```
