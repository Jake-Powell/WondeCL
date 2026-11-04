# Get Student IDs for a Target Year

Identifies students belonging to a target year within a single Wonde
school extract. Year information from the student data is used
preferentially. If no matching students are found, educational details
are used as a fallback.

## Usage

``` r
get_student_ids_target_year(
  school_data,
  year_codes_target = c("7", "Year 7", "07", "Year 07", "Y7", "Yr7", "Yr 7"),
  type = c("grepl", "exact")
)
```

## Arguments

- school_data:

  A single school extract from \`WondeData\`.

- year_codes_target:

  Character vector containing year values or patterns to match.

- type:

  Character. Matching method to use. \`"exact"\` uses exact matching
  with \` regular expression and returns students matching any of the
  patterns.

## Value

A character vector containing the unique student IDs for the target
year. Returns \`character(0)\` if no matching students are found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_student_ids_target_year(
  school_data,
  year_codes_target = c("7", "Year 7", "Y7"),
  type = "exact"
)

get_student_ids_target_year(
  school_data,
  year_codes_target = c("Year 7", "^7$", "Y7"),
  type = "grepl"
)
} # }
```
