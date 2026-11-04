# Convert list-column elements into data frames

Ensures that all elements within a specified list-column of a data frame
are valid data frames. This function is particularly useful when working
with nested data structures returned by APIs or list-based imports (such
as Wonde data), where some entries may contain empty lists instead of
empty data frames.

Each element of the specified column is checked; if the element is an
empty list, it is replaced with an empty data frame. This helps prevent
downstream errors in functions that expect every list element to be a
data frame.

## Usage

``` r
convert_list_element_to_df(data, column_to_unnest)
```

## Arguments

- data:

  A data frame containing at least one list-column to process.

- column_to_unnest:

  Character string giving the name of the column whose elements should
  be checked and converted.

## Value

The input `data` with the specified column modified such that any empty
list elements are replaced by empty data frames.

## Details

This function is typically used as a preprocessing step to standardize
nested Wonde API outputs or other hierarchical data sources prior to
unnesting, binding, or summarising operations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with an empty list element
df <- data.frame(info = I(list(data.frame(a = 1), list(), data.frame(a = 2))))
cleaned <- convert_list_element_to_df(df, "info")
str(cleaned$info)
} # }
```
