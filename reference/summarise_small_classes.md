# Summarise Small Classes by School

Summarises small classes within each school, where small classes contain
at most \`threshold\` students.

School identification uses \`est_id\` preferentially, falling back to
\`est_wonde_name\` if \`est_id\` is unavailable.

Class identification uses \`class_name\` preferentially, falling back to
\`c_wonde_name\` if \`class_name\` is unavailable.

Column names are resolved using the package's configurable column-name
system.

## Usage

``` r
summarise_small_classes(class_list, unique_id_col = NULL, threshold = 3)
```

## Arguments

- class_list:

  A class-list data frame using configured or internal column names.

- unique_id_col:

  Optional student identifier column. Can be an internal key (e.g.
  \`"ome_id"\`), configured name (e.g. \`"OME_ID"\`), or another column
  present in the data. If NULL, each row is counted as one student.

- threshold:

  Numeric. Maximum class size considered small. Defaults to 3.

## Value

A tibble containing the selected school identifier, using its configured
column name, followed by:

- TotalClasses:

  Number of classes in the school.

- SmallClasses:

  Number of small classes.

- StudentsInSmallClasses:

  Total students in small classes.

- SmallClassSizes:

  Comma-separated small class sizes.

- TotalStudents:

  Sum of student counts across classes.

- PercentInSmallClasses:

  Percentage of student counts belonging to small classes.

## Details

Column selection is based on availability, not missing values. If
\`est_id\` exists, it is used even if some values are NA. The same
applies to \`class_name\`.

When a unique identifier is supplied, distinct non-missing identifiers
are counted within each class. Otherwise, each row is treated as one
student.

## Examples

``` r
if (FALSE) { # \dontrun{
summarise_small_classes(class_list_df)

summarise_small_classes(
  class_list_df,
  unique_id_col = "ome_id",
  threshold = 5
)
} # }
```
