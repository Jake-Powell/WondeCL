# Rename Columns to Internal Names

Renames dataframe columns from the configured external names back to the
internal identifiers used by the package. Columns not included in the
lookup are left unchanged.

## Usage

``` r
rename_to_internal_names(df)
```

## Arguments

- df:

  A dataframe.

## Value

A dataframe with internal column names.
