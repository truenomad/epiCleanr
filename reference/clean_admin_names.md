# Clean and Match Administrative Names

This function takes administrative names and cleans them using various
matching and string distance algorithms. It can also match the cleaned
names with a base list provided by the user or fetched from
\`GeoNames\`, which is a official repository of standard spellings of
all foreign geographic names.

## Usage

``` r
clean_admin_names(
  admin_names_to_clean,
  country_code,
  admin_level = "adm2",
  user_base_admin_names = NULL,
  user_base_only = FALSE,
  report_mode = FALSE
)
```

## Arguments

- admin_names_to_clean:

  A character vector of administrative names to clean.

- country_code:

  sed if \`use_get_admin_names\` is TRUE. A character string or
  numerical value of the country code (e.g., "KE"). This can be in
  various formats such as country name, ISO codes, UN codes, etc., see
  [`countrycode::codelist()`](https://vincentarelbundock.github.io/countrycode/reference/codelist.html)
  for the full list of codes and naming conventions used.

- admin_level:

  A character string indicating the administrative level (e.g., "adm2").

- user_base_admin_names:

  A character of of administrative names that the use would like to use
  as reference. This is no necessary, downloaded \`GeoNames\` will be
  used if missing.

- user_base_only:

  A logical indicating whether to use only the user-provided base
  administrative names (\`user_base_admin_names\`) for matching. If
  TRUE, \`country_code\` and \`admin_names_to_clean\` are not required.
  Default is FALSE.

- report_mode:

  A logical indicating whether to return a detailed report. Default is
  FALSE.

## Value

If \`report_mode\` is set to TRUE, a data frame containing the original
admin names and the matched and cleaned admin names with inormation of
the source of data used to clean including the algorithm used, else a
cleaned list of names is returned.

## See also

[`countrycode::codelist()`](https://vincentarelbundock.github.io/countrycode/reference/codelist.html)
for the full list of codes and naming conventions.

## Examples

``` r
 # \donttest{
# Example with country code
base_names <- c(
  "Paris", "Marseille", "Lyon",
  "Toulouse", "Nice", "Nantes", "Strasbourg",
  "Montpellier", "Bordeaux", "Lille"
)

unclean_names <- c(
  "Pariis", "Marseill", "Lyone",
  "Toulous", "Niice", "Nantees", "Strasbourgh",
  "Montpeelier", "Bordeuax", "Lilie"
)

france_new <- clean_admin_names(
  country_code = "Fr",
  user_base_admin_names = base_names,
  admin_names_to_clean = unclean_names
)
#> There are 10 out of 10 (100%) admins that have been perfectly matched! 
#>  Use `report_mode` to double check your matches.

print(france_new)
#>  [1] "Paris"                 "Mariculum"             "Lyon"                 
#>  [4] "Toulouse"              "Nice"                  "Nantes"               
#>  [7] "Strasbourg"            "Mayenne'i departemang" "Bordeaux"             
#> [10] "lu hua lei"           
# }
```
