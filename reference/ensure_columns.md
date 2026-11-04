# Ensure all specified columns exist in a data frame

This internal helper function checks whether all specified column names
are present in a data frame. If any requested columns are missing, they
are automatically created and filled with `NA`. The function then
returns the data frame restricted to the specified column order.

## Usage

``` r
ensure_columns(df, cols)
```

## Arguments

- df:

  A data frame to check and modify.

- cols:

  Character vector of expected column names.

## Value

A data frame containing all requested columns (`cols`), in the same
order, with any missing columns added and filled with `NA`.

## Details

This is useful when working with Wonde datasets or other API responses
where field availability can vary between schools. It ensures consistent
column structures across all resulting data frames.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(a = 1:3, b = 4:6)
ensure_columns(df, c("a", "b", "c"))
# Returns a data frame with columns a, b, c (column c filled with NA)
} # }
```
