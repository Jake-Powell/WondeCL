# Add Spare Rows to Each Class List

Appends a specified number of spare (blank) rows to each unique school,
class and teacher combination in a class list.

Spare rows retain available school, class and teacher information, while
student-specific fields are left blank.

Column names are resolved using the package's configurable column-name
system, including the spare indicator.

## Usage

``` r
add_spares(class_list, no_spares = 3, include_spare_column = FALSE)
```

## Arguments

- class_list:

  A class-list data frame using configured or internal column names.

- no_spares:

  Integer. Number of spare rows to add for each unique class
  combination. Defaults to 3.

- include_spare_column:

  Logical. If TRUE, adds or updates the configured \`spare\` column,
  marking new spare rows with "Y". Defaults to FALSE.

## Value

The updated class list, retaining the original column names and order,
except that the spare indicator uses its configured name when
\`include_spare_column = TRUE\`.

Existing spare indicators are preserved. Newly generated spare rows are
marked "Y" when \`include_spare_column = TRUE\`.

## Details

Schools are identified preferentially using \`est_id\`, falling back to
\`urn\` and then \`est_wonde_name\`.

Classes are identified preferentially using \`class_name\`, falling back
to \`c_wonde_name\`.

Spare rows retain all available school, class and teacher identifiers.
Other character fields are blank and other data types are assigned NA.

The spare column name is taken from
\`get_column_names()\[\["spare"\]\]\`, which defaults to "SPARE".

## Examples

``` r
if (FALSE) { # \dontrun{
updated_list <- add_spares(
  class_list,
  no_spares = 3,
  include_spare_column = TRUE
)
} # }
```
