# Calculate class sizes from a class list

Summarises the number of students in each class by school, based on a
provided class list data frame. This function is typically used to check
class distribution and detect unusually large or small classes.

## Usage

``` r
get_class_sizes(class_list)
```

## Arguments

- class_list:

  A data frame containing at least the following columns:

  - \`School name\` – the name of the school

  - \`Class Name\` – the class name or identifier

  - \`Pupil First Name\` – the first name of each student

## Value

A tibble (grouped data frame) with three columns:

- \`School name\`:

  The school name.

- \`Class Name\`:

  The class name.

- \`Students\`:

  The number of students in each class.

## Details

The function groups rows by \`School name\` and \`Class Name\` and
counts the number of student entries based on the \`Pupil First Name\`
column. It is a lightweight summary tool intended for quality checks on
class list data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
class_sizes <- get_class_sizes(class_list_df)
print(class_sizes)
} # }
```
