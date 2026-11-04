# Extract distinct values across schools for a specified field

This general-purpose function scans through a list of Wonde school
datasets (\`WondeData\`) and compiles a summary of all distinct values
found for a user-specified field (column) within each school's dataset.

## Usage

``` r
extract_field_across_schools(
  WondeData,
  field_path,
  value_name = "value",
  verbose = TRUE
)
```

## Arguments

- WondeData:

  A named list of school datasets, typically created by data retrieval
  functions such as
  [`get_primary_school_student_data`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md)
  or
  [`get_secondary_school_student_data`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md).

- field_path:

  Character. The path to the column of interest within each Wonde
  dataset, using \`\$\` notation (e.g.,
  "student\$students.data\_\_year.data.code").

- value_name:

  Character. Optional. The name to give the column containing the
  distinct field values in the output (default = "value").

- verbose:

  Logical. If TRUE (default), prints messages about schools that were
  skipped due to missing data or fields.

## Value

A data frame with:

- value_name:

  The distinct values extracted across all schools.

- count:

  The total number of records across all schools for that value.

- school_ids:

  Comma-separated list of school IDs where that value appears.

## Details

For example, it can be used to summarise: \* Year codes - from
\`student\$students.data\_\_year.data.code\` \* Group names - from
\`students_group\$groups.data\_\_name\`

## Examples

``` r
if (FALSE) { # \dontrun{
extract_field_across_schools(WondeData,
 "student$students.data__year.data.code")
extract_field_across_schools(WondeData,
 "students_group$groups.data__name",
  "group_name",
   verbose = FALSE)
} # }
```
