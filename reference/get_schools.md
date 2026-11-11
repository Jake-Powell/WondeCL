# Retrieve school information from the Wonde API

Queries the Wonde API to retrieve information about all schools linked
to your Wonde account.

## Usage

``` r
get_schools(token = NULL)
```

## Arguments

- token:

  Character string. Your Wonde API token. If not supplied, the function
  will attempt to use the environment variable \`WONDE_KEY\`.

## Value

A data frame or list of school information returned by the Wonde API.
The structure will depend on the response from
[`WondeCL::get_query()`](https://Jake-Powell.github.io/WondeCL/reference/get_query.md).

## Details

This function is a wrapper around
[`WondeCL::get_query()`](https://Jake-Powell.github.io/WondeCL/reference/get_query.md),
specifically for retrieving school data from the Wonde API from the
endpoint "https://api.wonde.com/v1.0/schools/".

## See also

\[WondeCL::get_query()\] for the underlying query function.

Other wonde:
[`get_primary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md),
[`get_query()`](https://Jake-Powell.github.io/WondeCL/reference/get_query.md)

## Examples

``` r
if (FALSE) {
if (FALSE) { # \dontrun{
# Example usage
schools <- get_schools(token = "your_api_token_here")
} # }
}
```
