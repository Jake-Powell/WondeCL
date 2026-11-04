# Helper to get and save Wonde API token

Provides instructions on how to obtain and store an API token for the
Wonde API. Users must complete this step before using any functionality
that requires authentication. Obtaining a token requires contacting the
OME team via email.

## Usage

``` r
wonde_use_token()
```

## Details

After requesting access, you will receive an API token by email. Once
received, the token should be stored in your `.Renviron` file under the
name `WONDE_TOKEN` and R must be restarted for the change to take
effect.

## Examples

``` r
if (FALSE) { # \dontrun{
# Display instructions to set up an Wonde API token
wonde_use_token()
} # }
```
