# Revert ignored withdrawals

This function removes the withdrawal flag and restores identifying
fields for students who were previously marked as withdrawn but have
been flagged to be ignored (i.e., \`IgnoreResult == "Y"\` in the
\`withdraw\` table).

## Usage

``` r
revert_ignored_withdrawals(class_list, withdraw)
```

## Arguments

- class_list:

  A data frame of class list data (as returned by
  \[apply_withdrawn()\]).

- withdraw:

  A data frame of withdrawal results (as returned by
  \[apply_withdrawn()\]), containing the columns \`classListIndex\` and
  \`IgnoreResult\`.

## Value

An updated version of \`class_list\` with withdrawal flags and
identifying fields (\`Withdrawn?\`, \`DoB\`, \`UPN\`, \`Print ID\`, and
\`OME_ID\`) restored for ignored records.

## Details

For every row in \`withdraw\` where \`IgnoreResult == "Y"\` and a valid
\`classListIndex\` exists, this function:

- Sets \`Withdrawn?\` back to \`""\`

- Restores (clears) blank fields (\`DoB\`, \`UPN\`, \`Print ID\`,
  \`OME_ID\`) to \`NA\` where appropriate.

## See also

Other withdrawing:
[`apply_withdrawal()`](https://Jake-Powell.github.io/WondeCL/reference/apply_withdrawal.md),
[`clear_withdrawn_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/clear_withdrawn_student_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
updated_class_list <- revert_ignored_withdrawals(class_list, withdraw)
} # }
```
