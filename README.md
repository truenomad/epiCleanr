
# epiCleanr <a <img src="images/testlogo.png" align="right" height="138" /></a>

<!-- badges: start -->
[![R build status](https://github.com/truenomad/epicleanr/workflows/R-CMD-check/badge.svg)](https://github.com/truenomad/epicleanr/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/truenomad/epicleanr/badge)](https://www.codefactor.io/repository/github/truenomad/epicleanr)
[![Codecov test coverage](https://codecov.io/gh/truenomad/epiCleanr/branch/main/graph/badge.svg)](https://codecov.io/gh/truenomad/epiCleanr?branch=main)

## Description 

**epiCleanr** offers a tidy solution for epidemiological data. It houses a range
of functions for epidemiologists and public health data wizards for managing and
cleaning epidemiological data:

- `import()` import files of any formats.
- `export()` export files of any formats.
- `missing_plot()` plot missing data or reporting rate for given variable(s).
- `create_test()` creates a unit test functions to perform various data validation.
- `consistency_check()` plot to see certain variables exceed others (i.e., tests vs cases).
- `get_admin_names()` download admin names using various country codes and naming conventions.
- `clean_admin_names()` cleans admin names using both user-provided and downloaded admin data.

## Installation ##

The package is available on 
[CRAN](http://cran.r-project.org/web/packages/epiCleanr/) and can be installed 
directly in R using:

```R
install.packages("epiCleanr")
```

You can install the latest development version from GitHub by using:

```R
# If you haven't installed the 'devtools' package, run:
# install.packages("devtools")
devtools::install_github("truenomad/epiCleanr")
```

An example of using the **epiCleanr** package:

```R
# Load the epiCleanr package
library(epiCleanr)

# Use the import function to read data from various file formats
data <- import("path/to/your/file.csv")
```
