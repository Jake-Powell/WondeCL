# Clean and Standardise Teacher Name Formatting

Converts teacher name fields in a class list to title case (e.g., "john
SMITH" becomes "John Smith"). This ensures consistent formatting across
teacher names for display, reporting and downstream processing.

The function supports the package's configurable column-name system,
automatically converting input column names to their internal
equivalents before processing and restoring the configured names in the
returned data frame.

## Usage

``` r
clean_teacher_names(data)
```

## Arguments

- data:

  A data frame containing class list information. The following columns
  are required, using either their internal names or the currently
  configured column names:

  t_full

  :   Teacher's full name (default: \`TEACHER_FULL_NAME\`).

  tfn

  :   Teacher's first name (default: \`TEACHER_FIRST_NAME\`).

  tln

  :   Teacher's surname (default: \`TEACHER_LAST_NAME\`).

## Value

A data frame identical to the input \`data\`, except that the teacher
name columns are converted to title case. Column names are returned
using the package's configured naming convention.

## Details

The function performs the following steps:

1.  Converts the input column names to their internal equivalents using
    \`rename_to_internal_names()\`.

2.  Checks that all required teacher name columns are present.

3.  Converts the teacher full name, first name and surname to title case
    using \`stringr::str_to_title()\`.

4.  Restores the configured column names using
    \`rename_to_configured_names()\`.

Missing (\`NA\`) values are preserved. All other columns and their
values remain unchanged.

This function is typically used after data extraction or merging, or
following \`produce_class_lists()\`, as a final formatting step before
export.

## See also

Other class_list:
[`add_student_to_school_class()`](https://Jake-Powell.github.io/WondeCL/reference/add_student_to_school_class.md),
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

## Examples

``` r
if (FALSE) { # \dontrun{
# Clean teacher names using the default configured column names
cleaned_df <- clean_teacher_names(class_list_df)

# Inspect the cleaned teacher names
cleaned_df[, unname(get_column_names(
  c("t_full", "tfn", "tln")
))]

# The function also supports modified column names
set_column_names(t_full = "MAIN_TEACHER")
cleaned_df <- clean_teacher_names(class_list_df)

# Restore defaults
reset_column_names()
} # }
```
