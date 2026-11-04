# Pull from Wonde API (with error handling)

Safely queries a Wonde API endpoint, handling pagination,
authentication, and potential access or parsing errors. If an endpoint
is inaccessible or returns no data (e.g., 0 rows × 0 columns), the
function returns \`NULL\` instead of stopping execution.

## Usage

``` r
get_query(url_path, filter = "", token = NULL)
```

## Arguments

- url_path:

  The API endpoint to query.

- filter:

  Additional parameters to include in the request (e.g.,
  \`&include=groups\`).

- token:

  The Wonde API access token.

## Value

A combined \`data.frame\` of the pulled information if data are
returned, otherwise \`NULL\` if the endpoint cannot be accessed or
contains no records.

## Details

The function automatically paginates through all available pages of
results and concatenates them into a single data frame using
\`rbind_aggro()\`.

If the endpoint is inaccessible (e.g., insufficient permissions, invalid
ID) or returns an empty result, \`NULL\` is returned so that downstream
code can gracefully skip processing.

## See also

\[httr2::req_perform()\], \[jsonlite::fromJSON()\]

Other wonde:
[`get_primary_school_student_data()`](https://Jake-Powell.github.io/WondeCL/reference/get_primary_school_student_data.md),
[`get_schools()`](https://Jake-Powell.github.io/WondeCL/reference/get_schools.md)

## Examples

``` r
if (FALSE) {
school_id <- "Enter_ID_here"
token <- "Enter_token_here"

# Employee information
url_path <- paste0("https://api.wonde.com/v1.0/schools/", school_id, "/employees/")
employees <- get_query(url_path, filter = '&include=groups', token = token)

if (!is.null(employees)) {
  employees_groups <- employees |>
    convert_list_element_to_df(column_to_unnest = 'groups.data') |>
    tidyr::unnest(groups.data, names_sep = '__', keep_empty = TRUE)
}
}
```
