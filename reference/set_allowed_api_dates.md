# Set Allowed Wonde API Dates

Sets a temporary session-level override for the permitted Wonde API
query dates.

The override takes precedence over the user's \`.Rprofile\`
configuration and package defaults.

## Usage

``` r
set_allowed_api_dates(dates)
```

## Arguments

- dates:

  Character vector of annual date windows, formatted as DD/MM-DD/MM.

## Value

Invisibly returns the configured windows.

## Examples
