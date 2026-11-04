# Clear Identifying Data for Withdrawn Students

Removes identifying information for students marked as withdrawn. Only
rows where the withdrawal indicator equals \`"Y"\` are updated.

Supports the package's configurable column-name system, accepting either
internal identifiers or configured external column names.

## Usage

``` r
clear_withdrawn_student_data(
  data,
  WithdrawColumn = "withdrawn",
  columns_to_clear = c("upn", "dob", "hex_id", "ome_id"),
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame containing class list information.

- WithdrawColumn:

  Character string identifying the withdrawal status column. Defaults to
  \`"withdrawn"\`, corresponding to \`"Withdrawn?"\` in the default
  column-name lookup.

- columns_to_clear:

  Character vector of columns to clear. Defaults to \`c("upn", "dob",
  "print_id", "ome_id")\`. Columns not present in the data frame are
  ignored.

- verbose:

  Logical; if TRUE, prints a summary message.

## Value

A data frame with identifying fields set to NA for withdrawn students.
Column names are returned using the configured naming convention.

## Details

The function:

1.  Converts configured column names to internal names.

2.  Resolves the requested withdrawal and identifying columns.

3.  Identifies records where the withdrawal status is \`"Y"\`.

4.  Sets the requested identifying fields to NA for these records.

5.  Restores the configured column names.

No student identifier is required. Records are processed individually
using the withdrawal indicator.

If the withdrawal column is absent, a warning is issued and the original
data frame is returned unchanged.

## See also

Other withdrawing:
[`apply_withdrawal()`](https://Jake-Powell.github.io/WondeCL/reference/apply_withdrawal.md),
[`revert_ignored_withdrawals()`](https://Jake-Powell.github.io/WondeCL/reference/revert_ignored_withdrawals.md)

## Examples

``` r
if (FALSE) { # \dontrun{
updated <- clear_withdrawn_student_data(class_list)

updated <- clear_withdrawn_student_data(
  class_list,
  columns_to_clear = c("DOB", "UPN")
)
} # }
```
