# Combine data frames with non-identical columns

Safely combines a list of data frames that may have different column
names, missing columns, or contain NULL or empty elements. Any missing
columns are added to each data frame (filled with \`NA\`) before
applying \`rbind()\`. NULL and empty data frames are skipped
automatically.

## Usage

``` r
rbind_aggro(list_of_data.frames, warn_on_skip = TRUE)
```

## Arguments

- list_of_data.frames:

  A list of data frames to combine. NULL or zero-row data frames will be
  ignored.

- warn_on_skip:

  Logical, default \`TRUE\`. If \`TRUE\`, prints informative messages
  when NULL or empty data frames are skipped.

## Value

A single data frame containing all rows and all unique columns from the
input list. Missing columns are filled with \`NA\`. If no valid data
frames are present, returns an empty data frame.

## Details

This function identifies all unique column names across all data frames
in \`list_of_data.frames\`, ensures each has those columns (adding any
missing columns filled with \`NA\`), and then performs a row bind.

The function will:

- Remove NULL elements before combining.

- Remove data frames with zero rows.

- Optionally print a message for each skipped element when
  \`warn_on_skip = TRUE\`.

- Return an empty data frame if nothing valid remains.

## Examples

``` r
df1 <- data.frame(a = 1:3, b = 4:6)
df2 <- data.frame(a = 7:8, c = c("x", "y"))
df3 <- NULL

result <- rbind_aggro(list(df1, df2, df3))
#> rbind_aggro: Skipping 1 NULL data frame(s).
print(result)
#>   a  b    c
#> 1 1  4 <NA>
#> 2 2  5 <NA>
#> 3 3  6 <NA>
#> 4 7 NA    x
#> 5 8 NA    y
```
