# Remove Schools from a Class List

Removes all records belonging to one or more specified schools,
identified by school name, URN, or both.

Supports the package's configurable column-name system. School names are
identified preferentially using \`est_name\`, falling back to
\`est_wonde_name\` when unavailable.

When both school names and URNs are supplied, rows matching either
criterion are removed.

## Usage

``` r
remove_school_from_class_list(
  data,
  school_names = NULL,
  URNs = NULL,
  SchoolNameColumn = NULL,
  URNColumn = "urn",
  verbose = FALSE
)
```

## Arguments

- data:

  A class-list data frame using configured or internal column names.

- school_names:

  Character vector of school names to remove. Optional when \`URNs\` is
  supplied.

- URNs:

  Character or numeric vector of URNs to remove. Optional when
  \`school_names\` is supplied.

- SchoolNameColumn:

  Optional character string specifying the school-name column. Accepts
  an internal key, configured name, or literal column name. Defaults to
  NULL, which preferentially selects \`est_name\`, falling back to
  \`est_wonde_name\`.

- URNColumn:

  Character string identifying the URN column. Accepts an internal key,
  configured name, or literal column name. Defaults to \`"urn"\`.

- verbose:

  Logical. If TRUE, reports the number of rows and schools removed.

## Value

A data frame containing the remaining class-list records, with column
names restored to the configured naming convention.

## Details

Both school names and URNs are matched exactly. URNs are converted to
character before matching so numeric and character identifiers can be
compared.

Missing values are not used as removal criteria.

## See also

Other class_list:
[`add_student_to_school_class()`](https://Jake-Powell.github.io/WondeCL/reference/add_student_to_school_class.md),
[`clean_teacher_names()`](https://Jake-Powell.github.io/WondeCL/reference/clean_teacher_names.md),
[`create_school_blanks()`](https://Jake-Powell.github.io/WondeCL/reference/create_school_blanks.md),
[`produce_class_lists()`](https://Jake-Powell.github.io/WondeCL/reference/produce_class_lists.md),
[`reduce_to_best_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/reduce_to_best_teacher.md),
[`remove_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_class.md),
[`remove_student_from_class()`](https://Jake-Powell.github.io/WondeCL/reference/remove_student_from_class.md),
[`update_class_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_name.md),
[`update_class_teacher()`](https://Jake-Powell.github.io/WondeCL/reference/update_class_teacher.md),
[`update_school_name()`](https://Jake-Powell.github.io/WondeCL/reference/update_school_name.md),
[`update_student_class()`](https://Jake-Powell.github.io/WondeCL/reference/update_student_class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove by school name
updated <- remove_school_from_class_list(
  class_list,
  school_names = "Greenfield Primary School"
)

# Remove by URN
updated <- remove_school_from_class_list(
  class_list,
  URNs = c(123456, 987654)
)

# Remove by either school name or URN
updated <- remove_school_from_class_list(
  class_list,
  school_names = "Riverside Primary",
  URNs = 123456
)
} # }
```
