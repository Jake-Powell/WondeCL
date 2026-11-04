# Detect Students with Multiple Class Memberships

Identifies students who belong to more than one class within the
provided class list. Students are grouped using their school identifiers
and personal information, and the number of distinct classes associated
with each student is calculated.

Column names are resolved using the package's configurable column-name
system.

## Usage

``` r
detect_multiple_class_memberships(class_list_df)
```

## Arguments

- class_list_df:

  A data frame containing class-list information, typically produced by
  \`extract_class_list()\`. The following internal columns (or their
  configured equivalents) are required: \`est_wonde_id\`,
  \`est_wonde_name\`, \`est_wonde_urn\`, \`s_wonde_id\`, \`sfn\`,
  \`sln\`, \`dob\`, \`upn\` and \`c_wonde_id\`.

## Value

A data frame containing students associated with more than one distinct
class, with the following columns:

- Establishment and student fields:

  The school and student identifiers used for grouping, returned using
  their configured column names.

- classes:

  Comma-separated list of unique class IDs.

- no_classes:

  Number of distinct classes associated with the student.

## Details

The function:

1.  Converts configured column names to internal names.

2.  Groups records by school and student identifiers.

3.  Combines unique class IDs into a comma-separated string.

4.  Counts distinct class IDs.

5.  Retains only students associated with more than one class.

6.  Restores configured column names.

Class memberships are identified using \`c_wonde_id\`, rather than class
names. This avoids treating different names for the same class as
separate memberships.

## Examples

``` r
if (FALSE) { # \dontrun{
multi_members <- detect_multiple_class_memberships(class_list_df)
head(multi_members)
} # }
```
