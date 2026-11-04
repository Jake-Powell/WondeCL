# Set Column Names

Updates the column names used internally by the package. Any names not
supplied retain their current values.

## Usage

``` r
set_column_names(...)
```

## Arguments

- ...:

  Named arguments specifying column names to update.

## Value

Invisibly returns the updated column-name mapping.

## Examples

``` r
set_column_names(urn = "URN", dob = "DOB")
```
