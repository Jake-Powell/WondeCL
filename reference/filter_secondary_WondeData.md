# Filter Secondary Wonde Data by Year and Subject

Filters secondary Wonde raw data to only include students and teachers
for selected years and (optionally) a given subject area.

## Usage

``` r
filter_secondary_WondeData(
  WondeData,
  years = c("7", "12"),
  subject_names = NULL,
  subject_desc = NULL,
  fallback_codes = NULL,
  verbose = TRUE
)
```

## Arguments

- WondeData:

  A list of raw Wonde data (one element per school).

- years:

  Character vector of year identifiers to filter for (for example
  \`c("7","12")\`).

- subject_names:

  Optional character vector of subject names to match
  (case-insensitive).

- subject_desc:

  Optional character vector of subject descriptions to match if names
  fail.

- fallback_codes:

  Optional fallback year codes for mismatched ID cases. If \`NULL\`,
  defaults to the same as \`years\`.

- verbose:

  Logical; whether to print progress messages.

## Value

A filtered list of Wonde data, with an \`"issue_list"\` attribute noting
schools where fallbacks were used.

## See also

Other raw_clean:
[`filter_WondeData()`](https://Jake-Powell.github.io/WondeCL/reference/filter_WondeData.md),
[`filter_primary_WondeData()`](https://Jake-Powell.github.io/WondeCL/reference/filter_primary_WondeData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
maths_data <- filter_secondary_WondeData(
  secondary_data_raw,
  years = c("7", "12"),
  subject_names = c("Maths", "Mathematics", "Ma")
)
} # }
```
