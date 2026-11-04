# Combine two Wonde raw data lists element-wise

Merges two lists of raw Wonde school datasets (`a` and `b`) into a
single combined list. This is typically used to append newly retrieved
school data (e.g., additional Wonde API extracts) to an existing dataset
that was pulled earlier.

Each element of list `b` is added to list `a` using its name as the key.
If a name already exists in `a`, it will be overwritten by the
corresponding element from `b`.

## Usage

``` r
combine_raw_data(a, b)
```

## Arguments

- a:

  A named list of Wonde raw datasets, usually the original data pulled
  via
  [`get_primary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md)
  or
  [`get_secondary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md).

- b:

  A second named list of Wonde raw datasets to be appended or merged
  into `a`.

## Value

A single combined list containing all elements from both `a` and `b`.
The resulting structure is suitable for downstream processing functions
such as
[`extract_class_list()`](https://Jake-Powell.github.io/WondeCL/reference/extract_class_list.md)
or
[`summarise_school_data()`](https://Jake-Powell.github.io/WondeCL/reference/summarise_school_data.md).

## Details

This function is commonly used when some schools are pulled from the
Wonde API at a later time (for example, schools missing from the
original retrieval). Instead of re-pulling all data, you can append the
new raw extracts directly using this helper.

The combination is done by name: each element in `b` is added to `a`
under its existing name (usually the Wonde school ID). If duplicate
names are present, entries from `b` will overwrite those in `a`.

## See also

Other utils:
[`clean_name()`](https://Jake-Powell.github.io/WondeCL/reference/clean_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Original Wonde data
raw_data_1 <- get_secondary_school_student_data(school_ids = c("A100001", "A100002"))

# Later, new schools become available
raw_data_2 <- get_secondary_school_student_data(school_ids = c("A100003", "A100004"))

# Combine both into one object
combined_raw <- combine_raw_data(raw_data_1, raw_data_2)

# The result can then be used in extract_class_list()
class_list <- extract_class_list(WondeData = combined_raw, ...)
} # }
```
