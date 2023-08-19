
# epiCleanr: A Tidy Solution for Epidemiological Data 

[![CodeFactor](https://www.codefactor.io/repository/github/truenomad/epicleanr/badge)](https://www.codefactor.io/repository/github/truenomad/epicleanr)
[![GitHub](https://img.shields.io/github/license/truenomad/epiCleanr)](https://github.com/truenomad/epiCleanr/blob/main/LICENSE)
[![Codecov test coverage](https://codecov.io/gh/truenomad/epiCleanr/branch/main/graph/badge.svg)](https://codecov.io/gh/truenomad/epiCleanr?branch=main)

## Description ##

**epiCleanr** is an all-inclusive R package developed not just for researchers, 
but also for those working on the front lines of public health. Intuitively 
tailored to address the needs of epidemiologists, it boasts a range of 
functionalities for importing, cleaning, and preprocessing data, which are 
commonly found in epidemiological studies. With a special focus on resolving the 
unique challenges of epidemiological datasets, this package amplifies the 
efficiency and reproducibility of your work, whether it's in-depth research or 
actionable public health analytics.

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
