# Export Data to Various File Formats

This function provides a unified interface for exporting data to various
file formats supported by the
[`rio::export()`](http://gesistsa.github.io/rio/reference/export.md)
function. The format is automatically detected from the file extension
to simplify the exporting process.

## Usage

``` r
export(data, file_path, ...)
```

## Arguments

- data:

  The dataset to be exported.

- file_path:

  Character string specifying the path to the output file.

- ...:

  Additional arguments to be passed to the underlying write functions.
  These arguments are specific to the file format being exported. Please
  refer to the documentation of each package used for more information.

## Value

No return value, called for side effects.

## See also

[`rio::export()`](http://gesistsa.github.io/rio/reference/export.md),
which this function is based on.

## Examples

``` r
# Create temporary account
tmpdir <- tempfile()
dir.create(tmpdir)

# Export a CSV file
export(mtcars, file_path = file.path(tmpdir, "file.csv"))

# Export an Excel file
export(mtcars, file_path = file.path(tmpdir, "file.xlsx"))

# Export a Stata DTA file
export(mtcars, file_path = file.path(tmpdir, "file.dta"))

# Export an RDS file
export(mtcars, file_path = file.path(tmpdir, "file.rds"))

# Export an RData file
export(list(mtcars = mtcars, iris = iris),
       file_path = file.path(tmpdir, "file.RData"))

# Remove the temporary directory and its contents
unlink(tmpdir, recursive = TRUE)
```
