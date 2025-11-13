# Import Data from Various File Formats

This function provides a unified interface for importing data from
various file formats supported by the `rio` package. The format is
automatically detected from the file extension to simplify the importing
process.

## Usage

``` r
import(file_path, ...)
```

## Arguments

- file_path:

  Character string specifying the path to the input file or a URL
  pointing to the dataset.

- ...:

  Additional arguments to be passed to the underlying read functions.
  These arguments are specific to the file format being imported. Please
  refer to the documentation of each package used for more information.

## Value

A data frame or appropriate R object containing the imported data.

## See also

[`rio::import()`](http://gesistsa.github.io/rio/reference/import.md),
which this function is based on.

## Examples

``` r
# Locate test data directory
path <-  system.file("extdata",
                     package = "epiCleanr")

# Import a CSV file
data_csv <- import(file_path = file.path(path, "test_data.csv"))

# Import an Excel file
data_excel <- import(file_path = file.path(path, "test_data.xlsx"))

# Import a Stata DTA file
data_dta <- import(file_path = file.path(path, "test_data.dta"))

# Import an RDS file
data_rds <- import(file_path = file.path(path, "test_data.rds"))
#> Warning: Missing `trust` will be set to FALSE by default for RDS in 2.0.0.

# Import an RData file
data_rdata <- import(file_path = file.path(path, "test_data.RData"))
#> Warning: Missing `trust` will be set to FALSE by default for RData in 2.0.0.

# Import an SPSS file
data_spss <- import(file_path = file.path(path, "test_data.sav"))
```
