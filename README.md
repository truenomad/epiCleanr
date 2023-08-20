
# epiCleanr: A Tidy Solution for Epidemiological Data 

[![R build status](https://github.com/truenomad/epicleanr/workflows/R-CMD-check/badge.svg)](https://github.com/truenomad/epicleanr/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/truenomad/epicleanr/badge)](https://www.codefactor.io/repository/github/truenomad/epicleanr)
[![Codecov test coverage](https://codecov.io/gh/truenomad/epiCleanr/branch/main/graph/badge.svg)](https://codecov.io/gh/truenomad/epiCleanr?branch=main)

## Description ##

**epiCleanr** is specifically designed to meet the requirements of 
epidemiologists and public health data experts. It offers a comprehensive suite 
of functionalities for importing, cleaning, unit testing, visualizing, and 
preprocessing data—tasks that are frequently encountered in epidemiological 
analysis. By focusing on addressing the unique challenges inherent in 
epidemiological datasets, epiCleanr enhances both the efficiency and 
reproducibility of your work. 

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
