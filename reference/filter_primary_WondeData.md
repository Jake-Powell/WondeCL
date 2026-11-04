# Filter Primary Wonde Data by Year

Filters primary Wonde raw data to only include students, classes, and
teachers belonging to a selected National Curriculum year (for example
Reception or Year 1).

## Usage

``` r
filter_primary_WondeData(
  WondeData,
  years = c("0", "R", "Reception"),
  fallback_codes = NULL,
  verbose = TRUE
)
```

## Arguments

- WondeData:

  A list of raw Wonde data (one element per school).

- years:

  Character vector of year identifiers to filter for, for example
  \`c("0", "R", "Reception")\`.

- fallback_codes:

  Optional vector of fallback year codes to use when IDs do not align.
  If \`NULL\`, defaults to the same as \`years\`.

- verbose:

  Logical; whether to print progress messages.

## Value

A filtered list of Wonde data, with an \`"issue_list"\` attribute noting
schools where fallbacks were used.

## See also

Other raw_clean:
[`filter_WondeData()`](https://Jake-Powell.github.io/WondeCL/reference/filter_WondeData.md),
[`filter_secondary_WondeData()`](https://Jake-Powell.github.io/WondeCL/reference/filter_secondary_WondeData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
reception_data <- filter_primary_WondeData(
  primary_data_raw,
  years = c("0", "R", "Reception")
)
} # }
```
