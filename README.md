
# epiCleanr: The epidemiologist's Swiss army knife #

[![R build status](https://github.com/truenomad/epicleanr/workflows/R-CMD-check/badge.svg)](https://github.com/truenomad/epicleanr/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/truenomad/epicleanr/badge)](https://www.codefactor.io/repository/github/truenomad/epicleanr)
[![GitHub](https://img.shields.io/github/license/truenomad/epiCleanr)](https://github.com/truenomad/epiCleanr/blob/main/LICENSE)
[![R-CMD-check](https://github.com/truenomad/epiCleanr/workflows/R-CMD-check/badge.svg)](https://github.com/truenomad/epiCleanr/actions)
[![Codecov test coverage](https://codecov.io/gh/truenomad/epiCleanr/branch/main/graph/badge.svg)](https://codecov.io/gh/truenomad/epiCleanr?branch=main)

## Description ##

**epiCleanr** is an R package tailored for epidemiologists, 
offering functionalities for importing and cleaning data prevalent in 
epidemiological studies. The package includes utilities for data cleaning and 
preprocessing, specifically designed for common challenges in epidemiological 
datasets, thereby enhancing the efficiency and reproducibility of 
epidemiological research.

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
