# Filter Wonde raw data

Restricts raw Wonde data to only those students, teachers, and classes
that meet user-defined filtering criteria such as year group or subject.
Designed to generalise across both primary and secondary school
datasets.

## Usage

``` r
filter_WondeData(
  WondeData,
  student_filter = NULL,
  subject_filter = NULL,
  year_fallback_column = "year.data.code",
  fallback_codes = NULL,
  verbose = TRUE
)
```

## Arguments

- WondeData:

  A list of raw Wonde data, where each element represents a single
  school's extracted dataset from Wonde.

- student_filter:

  Optional function applied to the \`education_details\` element of each
  school list, returning the student IDs to keep. The function should
  return a character vector of IDs.

- subject_filter:

  Optional function applied to the \`subjects\` element of each school
  list, returning either a logical vector or IDs to keep. Use this to
  retain data for specific subjects, such as Mathematics.

- year_fallback_column:

  A character string naming the column in the \`student\` data that
  contains year or code information. Used to identify students when
  \`education_details\` IDs do not match. Defaults to
  \`"year.data.code"\`.

- fallback_codes:

  Optional character vector of values used for matching in
  \`year_fallback_column\` when no students are returned by
  \`student_filter\`. For example, \`c("R", "Reception")\`.

- verbose:

  Logical; if \`TRUE\`, displays progress messages while filtering.

## Value

A list of filtered Wonde data objects. Each element will contain only
the relevant students, teachers, and classes. The returned list also
includes an attribute named \`"issue_list"\`, which records any schools
where fallback rules were applied.

## See also

Other raw_clean:
[`filter_primary_WondeData()`](https://Jake-Powell.github.io/WondeCL/reference/filter_primary_WondeData.md),
[`filter_secondary_WondeData()`](https://Jake-Powell.github.io/WondeCL/reference/filter_secondary_WondeData.md)
