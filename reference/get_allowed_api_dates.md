# Get Allowed Wonde API Dates

Returns the currently active annual date windows for querying the Wonde
API.

Settings are resolved in priority order:

1\. Session overrides created with \`set_allowed_api_dates()\`. 2. The
\`WondeCL.allowed_api_dates\` R option. 3. The package's built-in
defaults.

The R option can be set in \`.Rprofile\` to persist across R sessions.

## Usage

``` r
get_allowed_api_dates(source = FALSE)
```

## Arguments

- source:

  Logical. If TRUE, returns the configuration source together with the
  active date windows.

## Value

A character vector of date windows, or a list containing \`source\` and
\`dates\` when \`source = TRUE\`.

## Examples

``` r
get_allowed_api_dates()
#> character(0)
get_allowed_api_dates(source = TRUE)
#> $source
#> [1] "package defaults"
#> 
#> $dates
#> character(0)
#> 
```
