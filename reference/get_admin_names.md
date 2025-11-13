# Retrieve Administrative Names from GeoNames

This function grabs administrative region names (such as districts,
provinces, etc.) for a given country from the \`GeoNames\` website. It
accepts both country names and various country coding schemes.

## Usage

``` r
get_admin_names(country_name_or_code, silent_mode = TRUE)
```

## Arguments

- country_name_or_code:

  Character or numeric. The name or code of the country for which
  administrative names are to be retrieved. This can be in various
  formats such as country name, ISO codes, UN codes, etc., see
  \`countrycode::codelist()\` for the full list of codes and naming
  conventions used.

- silent_mode:

  A logical indicating whether to suppress messages. Default is TRUE.

## Value

A list containing administrative region names and details for different
administrative levels (e.g., ADM1, ADM2, etc.). Each element of the list
corresponds to a different administrative level and contains a data
frame with columns such as country_code, ascii name, alternate names,
latitude, longitude, and date last updated.

## See also

\`Geonames\` website for the source of admin names data

## Examples

``` r
# \donttest{
# example using different naming/code conventions
three_digit <- get_admin_names("TGO")   # using 3 digit iso codes
two_digit <- get_admin_names("TG")      # using 2 digit iso codes
un_code <- get_admin_names(768)         # using UN codes
full_name <-  get_admin_names("Togo")   # using full names

str(full_name$adm2)
#> 'data.frame':    30 obs. of  7 variables:
#>  $ country_code  : chr  "TG" "TG" "TG" "TG" ...
#>  $ asciiname     : chr  "Vo Prefecture" "Zio Prefecture" "Tchaoudjo" "Tchamba" ...
#>  $ alternatenames: chr  "Circonscription de Vogan, Prefecture de Vo, Préfecture de Vo, Vo" "Circonscription de Tsevie, Circonscription de Tsévié, Prefecture de Tsevie, Prefecture du Zio, Préfecture de Ts"| __truncated__ "Circonscription de Tchaoudjo, Prefecture de Tchaoudjo, Préfecture de Tchaoudjo, Sokode, Sokodé, Tchaoudjo, Tcha"| __truncated__ "Circonscription de Tchamba, Prefecture de Tchamba, Préfecture de Tchamba, Tchamba, Tchamba Prefecture" ...
#>  $ adm2          : chr  "Vo Prefecture" "Zio Prefecture" "Tchaoudjo" "Tchamba" ...
#>  $ latitude      : num  6.42 6.58 9 8.83 6.67 ...
#>  $ longitude     : num  1.5 1.17 1.17 1.42 1.5 ...
#>  $ last_updated  : IDate, format: "2025-06-18" "2025-06-18" ...
# }
```
