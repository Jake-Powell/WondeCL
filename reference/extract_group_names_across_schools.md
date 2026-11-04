# Extract unique group names across schools

Wrapper for
[`extract_field_across_schools`](https://Jake-Powell.github.io/WondeCL/reference/extract_field_across_schools.md)
that summarises group names across all schools.

## Usage

``` r
extract_group_names_across_schools(WondeData, verbose = TRUE)
```

## Arguments

- WondeData:

  A named list of school datasets, typically created by data retrieval
  functions such as
  [`get_primary_school_student_data`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md)
  or
  [`get_secondary_school_student_data`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md).

- verbose:

  Logical. If TRUE (default), prints messages about schools that were
  skipped due to missing data or fields.
