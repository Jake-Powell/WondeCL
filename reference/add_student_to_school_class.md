# Add a student to an existing school class

Add a student to an existing school class

## Usage

``` r
add_student_to_school_class(
  data,
  first_name,
  last_name,
  class_name,
  dob = NA,
  upn = NA,
  ...,
  verbose = TRUE
)
```

## Arguments

- data:

  A class-list data frame using configured or internal column names.

- first_name, last_name:

  Name of new student.

- class_name:

  Existing class name to copy class and teacher information from.

- dob:

  Date of birth.

- upn:

  UPN, if known.

- ...:

  Exactly one named school identifier, using an internal lookup key or
  configured external name, e.g. \`URN = 123456\`, \`EST_ID = '001'\`,
  or \`est_wonde_id = 'A123'\`.

- verbose:

  Print informative messages.

## Value

Updated data frame using the configured column names.

## See also

Other class_list:
[`clean_teacher_names()`](https://Jake-Powell.github.io/WondeCL/reference/clean_teacher_names.md),
[`create_school_blanks()`](https://Jake-Powell.github.io/WondeCL/reference/create_school_blanks.md),
[`produce_class_lists()`](https://Jake-Powell.github.io/WondeCL/reference/produce_class_lists.md),
[`reduce_to_best_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/reduce_to_best_teacher.md),
[`remove_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_class.md),
[`remove_school_from_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/remove_school_from_class_list.md),
[`remove_student_from_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_student_from_class.md),
[`update_class_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_name.md),
[`update_class_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_teacher.md),
[`update_school_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_school_name.md),
[`update_student_class()`](https://Jake-Powell.github.io/WondeCL/reference/update_student_class.md)
