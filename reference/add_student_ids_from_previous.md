# Add Student IDs from a Master List

Reuses existing OME_ID values from a Student Master List by matching
students in a new class list.

Matching is attempted first using UPN. Remaining students are matched by
name within the same establishment using
OMELink::match_person_to_data(), with an exact-name fallback if OMELink
is unavailable.

Column names in the class list are resolved using the package's column
configuration. Master-list column names are specified independently
using master_columns.

## Usage

``` r
add_student_ids_from_previous(
  new_class_list,
  master_list,
  master_columns = c(ome_id = "OME_ID", sfn = "FNS", sln = "LNS", dob = "DOBS", upn =
    "UPN", est_id = "EST_ID"),
  school_id_column = "est_id",
  verbose = FALSE,
  fuzzy_match = TRUE,
  max_dist = 2,
  method = "osa"
)
```

## Arguments

- new_class_list:

  Current class-list data frame.

- master_list:

  Student Master List data frame.

- master_columns:

  Named character vector mapping internal field names to columns in the
  master list. By default, FNS and LNS contain historical names
  separated by "—".

- school_id_column:

  Internal or configured school identifier in the class list. Defaults
  to "est_id".

- verbose:

  Logical. If TRUE, displays progress information.

- fuzzy_match:

  Logical. If TRUE, performs name matching for remaining unmatched
  students. Defaults to TRUE.

- max_dist:

  Maximum edit distance passed to OMELink::match_person_to_data().

- method:

  Matching method passed to OMELink::match_person_to_data().

## Value

Updated class list containing OME_ID, MATCH_METHOD,
OLD_STUDENT_FIRST_NAME, OLD_STUDENT_LAST_NAME and OLD_DOB.

## Details

The original matching methodology is retained. UPN matching uses
match(), taking the first matching master-list record. Name-based
matching is restricted to the selected school.

OLD_STUDENT_FIRST_NAME and OLD_STUDENT_LAST_NAME are always returned in
lowercase. For UPN matches, these contain the complete historical name
strings from FNS and LNS. For OMELink matches, they contain the matched
name candidates.

Historical DoBs separated by "—" are considered individually when
checking for mismatches for fuzzy/partial name matches.

Additional master-list fields are not used in matching.
