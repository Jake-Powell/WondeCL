# Extract a subject-specific class list from Wonde API data

Extract a subject-specific class list from Wonde API data

## Usage

``` r
extract_class_list(
  WondeData,
  schools,
  maths_names,
  maths_desc_names,
  year_pattern,
  verbose = TRUE,
  schools_id_column = "EstablishmentID"
)
```

## Arguments

- WondeData:

  List of school-level data as returned by
  [`get_secondary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md).

- schools:

  Data frame of school metadata containing at least `id`, `name`, and
  `urn`.

- maths_names:

  Character vector of subject names to match (used when
  `GetMathBySubject = TRUE`).

- maths_desc_names:

  Character vector of text fragments to match in class descriptions
  (used when `GetMathByClassDescription = TRUE`).

- year_pattern:

  Character or numeric pattern used to match student year codes (e.g.,
  \`"7"\` for Year 7 or \`"8"\` for Year 8).

- verbose:

  Logical; if `TRUE`, prints progress.

- schools_id_column:

  Character string specifying the column in the supplied schools data
  frame containing the school identifiers used to link schools to their
  Wonde extracts.

## Value

A data frame combining all matched student, class, and teacher
information across schools, or `NULL` if no valid data are found.

## Examples

``` r
if (FALSE) {
class_list_df <- extract_class_list(
  WondeData = WondeData,
  schools = schools,
  secondary = secondary,
  maths_names = c("Mathematics", "Maths"),
  maths_desc_names = c("MATH", "NUMERACY"),
  year_pattern = "7"
)
}
```
