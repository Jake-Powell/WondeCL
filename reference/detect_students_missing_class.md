# Detect students missing a class assignment

Identifies students in the provided class list who do not have a
recorded class assignment (i.e., those with missing \`ClassId\` values).
This function is useful for quickly checking data completeness after
class extraction or cleaning.

## Usage

``` r
detect_students_missing_class(class_list_df)
```

## Arguments

- class_list_df:

  A data frame containing at least the column \`ClassId\` and
  student-level information. Typically the output of
  [`extract_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/extract_class_list.md)
  or a subsequent cleaning step.

## Value

A data frame containing only those rows where the \`ClassId\` field is
missing (\`NA\`), representing students who are not assigned to any
class.

## Details

This function is a lightweight diagnostic tool used to flag students who
have no class membership information available. It does not modify the
input data frame, only filters it.

It is commonly used prior to or in conjunction with
[`check_students_missing_class()`](https://Jake-Powell.github.io/WondeCL/reference/check_students_missing_class.md)
or other validation utilities that check subject-specific or
year-specific class assignments.

## Examples

``` r
if (FALSE) { # \dontrun{
missing_class_students <- detect_students_missing_class(class_list_df)
head(missing_class_students)
} # }
```
