# Create Test Function

This function creates a test function to perform various data validation
checks. The returned function can be applied to a dataset to perform the
specified tests.

## Usage

``` r
create_test(
  dimension_test = NULL,
  combinations_test = NULL,
  row_duplicates = FALSE,
  col_duplicates = FALSE,
  min_threshold_test = NULL,
  max_threshold_test = NULL
)
```

## Arguments

- dimension_test:

  A vector of two integers specifying the expected number of rows and
  columns.

- combinations_test:

  A list with the elements \`variables\` (character vector of variable
  names) and \`expectation\` (integer specifying the expected number of
  unique combinations for each column).

- row_duplicates:

  Logical. If TRUE, checks for duplicate rows.

- col_duplicates:

  Logical. If TRUE, checks for duplicate columns.

- min_threshold_test:

  Named list of minimum threshold values for specified columns.

- max_threshold_test:

  Named list of maximum threshold values for specified columns.

## Value

A function to be applied to the dataset.

## Examples

``` r
# get path
path <- system.file(
        "extdata",
        "fake_epi_df_togo.rds",
         package = "epiCleanr")

fake_epi_df_togo <- import(path)
#> Warning: Missing `trust` will be set to FALSE by default for RDS in 2.0.0.

# Set up unit-test function
my_tests <- create_test(
  # For checking the dimension of the data
  dimension_test = c(900, 9),
  # For expected number of combinations in data
 combinations_test = list(
   variables = c("month", "year", "district"),
   expectation = 12 * 5 * 15),
  # Check repeated cols, rows and max and min thresholds
  row_duplicates = TRUE, col_duplicates = TRUE,
  max_threshold_test = list(malaria_tests = 1000, cholera_tests = 1000),
 min_threshold_test = list(cholera_cases = 0, cholera_cases = 0)
)

result <- my_tests(fake_epi_df_togo)
#> Test passed! You have the correct number of dimensions!
#> Test passed! No duplicate rows found!
#> Test passed! No repeated columns found!
#> Test passed! You have the correct number of combinations for month, year, district!
#> Test passed! Values in column cholera_cases are above the threshold.
#> Test passed! Values in column cholera_cases are above the threshold.
#> Test passed! Values in column malaria_tests are below the threshold.
#> Test passed! Values in column cholera_tests are below the threshold.
#> Congratulations! All tests passed: 8/8 (100%) 🎊
```
