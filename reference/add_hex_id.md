# Add Hexadecimal Representation of Numeric IDs

Adds a hexadecimal representation of an existing numeric or numeric-like
identifier column to a data frame.

Both the default source ID (\`ome_id\`) and hexadecimal output column
(\`hex_id\`) use the package's column configuration.

## Usage

``` r
add_hex_id(data, ID_column = "ome_id")
```

## Arguments

- data:

  A data frame containing numeric or numeric-like IDs.

- ID_column:

  Character string identifying the source ID. Defaults to \`"ome_id"\`.
  Accepts an internal column key, configured column name, or another
  column present in the data.

## Value

The input data frame with an additional hexadecimal column named
according to \`get_column_names()\[\["hex_id"\]\]\`. Existing column
names and order are preserved.

## Details

Non-numeric and missing values are converted to NA.

## Examples

``` r
if (FALSE) { # \dontrun{
add_hex_id(class_list)
add_hex_id(df, ID_column = "ID")
add_hex_id(df, ID_column = "print_id")
} # }
```
