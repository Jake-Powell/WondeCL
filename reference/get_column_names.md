# Get Column Names

Returns the currently configured column names.

## Usage

``` r
get_column_names(name = NULL)
```

## Arguments

- name:

  Optional character vector of internal column identifiers. If NULL,
  returns all configured column names.

## Value

A named character vector of column names.

## Examples

``` r
get_column_names()
#>                  urn                  dob               est_id 
#>                "URN"                "DOB"             "EST_ID" 
#>             est_name         est_wonde_id       est_wonde_name 
#>           "EST_NAME"       "EST_WONDE_ID"     "EST_WONDE_NAME" 
#>      est_wonde_estno        est_wonde_urn           s_wonde_id 
#>   "EST_WONDE_EST_NO"      "EST_WONDE_URN"   "STUDENT_WONDE_ID" 
#>                  sfn                  sln                  upn 
#>   "PUPIL_FIRST_NAME"    "PUPIL_LAST_NAME"                "UPN" 
#>           c_wonde_id         c_wonde_name           t_wonde_ID 
#>     "CLASS_WONDE_ID"   "CLASS_WONDE_NAME"   "TEACHER_WONDE_ID" 
#>                  tfn                  tln               t_main 
#> "TEACHER_FIRST_NAME"  "TEACHER_LAST_NAME"       "TEACHER_MAIN" 
#>               t_full             class_id           class_name 
#>  "TEACHER_FULL_NAME"           "CLASS_ID"         "CLASS_NAME" 
#>            withdrawn      withdrawal_date                spare 
#>          "WITHDRAWN"    "WITHDRAWAL_DATE"              "SPARE" 
#>               hex_id               ome_id 
#>           "PRINT_ID"             "OME_ID" 
get_column_names("urn")
#>   urn 
#> "URN" 
get_column_names(c("urn", "dob"))
#>   urn   dob 
#> "URN" "DOB" 
```
