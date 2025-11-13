# Plot Missing data over time

This function visualizes the proportion of missing data or reporting
rate for specified variables in a dataset. It creates a tile plot using
`ggplot2`; where the x-axis can represent any categorical time such as
time (e.g., year, month), and the y-axis can represents either variables
or groupings (e.g., state). The output can further be manipulated to
one's needs.

## Usage

``` r
missing_plot(data, x_var, y_var = NULL, miss_vars = NULL, use_rep_rate = FALSE)
```

## Arguments

- data:

  A data frame containing the data to be visualized. Must include
  columns specified in 'x_var', 'y_var', and 'vars'.

- x_var:

  A character string specifying the time variable in 'data' (e.g.,
  "year", "month"). Must be provided.

- y_var:

  An optional character string specifying the grouping variable in
  'data' (e.g., "state"). If provided, only one variable can be
  specified in 'vars'.

- miss_vars:

  An optional character vector specifying the variables to be visualized
  in 'data'. If NULL, all variables except 'x_var' and 'y_var' will be
  used.

- use_rep_rate:

  A logical value. If TRUE, the reporting rate is visualized; otherwise,
  the proportion of missing data is visualized. Defaults to FALSE

## Value

A ggplot2 object representing the tile plot.

## Examples

``` r

# get path
path <- system.file(
        "extdata",
        "fake_epi_df_togo.rds",
         package = "epiCleanr")

fake_epi_df_togo <- import(path)
#> Warning: Missing `trust` will be set to FALSE by default for RDS in 2.0.0.

# Check misisng data by year
result <- missing_plot(fake_epi_df_togo,
             x_var = "year", use_rep_rate = FALSE)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic arguments:
#> • key.height = ggplot2::unit(1, "lines")
#> • key.width = ggplot2::unit(1, "lines")
#> ℹ Did you misspell an argument name?
```
