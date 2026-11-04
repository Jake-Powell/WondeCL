# Manually Withdraw a Student

Marks a student as withdrawn by setting their withdrawal status to
\`"Y"\`. The student is identified using a school identifier supplied
through \`...\`, together with their first name and surname.

Column names are resolved using the package's configurable column-name
system.

## Usage

``` r
withdraw_student_from_school_class(
  data,
  first_name,
  last_name,
  ...,
  verbose = FALSE
)
```

## Arguments

- data:

  A class-list data frame using configured or internal column names.

- first_name:

  Student's first name (exact, case-sensitive).

- last_name:

  Student's surname (exact, case-sensitive).

- ...:

  A named school identifier, such as \`URN\`, \`EST_ID\`, or
  \`EST_WONDE_ID\`, using the configured column names.

- verbose:

  Logical. If TRUE, displays a message confirming the withdrawal.

## Value

The updated data frame with the matching student marked as withdrawn.
Existing withdrawal statuses are preserved.

If no student or multiple students match, a warning is issued and no
changes are made.

## Details

School selection uses \`.prepare_class_edit()\` and \`.school_rows()\`,
consistent with the other manual class-list editing functions.

Student names are matched exactly and are case-sensitive. No Wonde
student identifier is required.
