# Reset Allowed Wonde API Dates

Removes the session-level override and restores the dates configured in
\`.Rprofile\`, or the package defaults if no R option exists.

This does not modify the user's \`.Rprofile\`.

## Usage

``` r
reset_allowed_api_dates()
```

## Value

Invisibly returns the currently active dates.
