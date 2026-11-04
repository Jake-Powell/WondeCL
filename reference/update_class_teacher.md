# Update or Append a Class Teacher

Updates or appends a teacher to a specified class within a school.

Class identification preferentially uses \`class_name\`, falling back to
\`c_wonde_name\` when \`class_name\` is unavailable.

## Usage

``` r
update_class_teacher(
  data,
  class_name,
  first_name,
  last_name,
  new_name = paste(first_name, last_name),
  ...,
  overwrite = TRUE,
  verbose = FALSE
)
```

## Arguments

- data:

  A class-list data frame using configured or internal column names.

- class_name:

  Class or classes to edit. NA edits all classes within the selected
  school.

- first_name:

  Teacher first name.

- last_name:

  Teacher last name.

- new_name:

  Teacher full name. Defaults to the concatenation of \`first_name\` and
  \`last_name\`.

- ...:

  Exactly one named school identifier, using an internal lookup key or
  configured external name, e.g. \`URN = 123456\`, \`EST_ID = '001'\`,
  or \`est_wonde_id = 'A123'\`.

- overwrite:

  If TRUE, update existing teacher information. If FALSE, append copies
  of matching rows with the new teacher.

- verbose:

  Logical. If TRUE, reports the number of rows updated or appended.

## Value

The updated class list with configured column names.

## See also

Other class_list:
[`add_student_to_school_class()`](https://Jake-Powell.github.io/WondeCL/reference/add_student_to_school_class.md),
[`clean_teacher_names()`](https://Jake-Powell.github.io/WondeCL/reference/clean_teacher_names.md),
[`create_school_blanks()`](https://Jake-Powell.github.io/WondeCL/reference/create_school_blanks.md),
[`produce_class_lists()`](https://Jake-Powell.github.io/WondeCL/reference/produce_class_lists.md),
[`reduce_to_best_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/reduce_to_best_teacher.md),
[`remove_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_class.md),
[`remove_school_from_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/remove_school_from_class_list.md),
[`remove_student_from_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_student_from_class.md),
[`update_class_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_name.md),
[`update_school_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_school_name.md),
[`update_student_class()`](https://Jake-Powell.github.io/WondeCL/reference/update_student_class.md)
