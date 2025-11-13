# Clean variable names or column names in various styles

This function transforms variable names or column names into one of the
standard cleaned formats specified by the \`style\` argument. It offers
more flexibility than
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
function by supporting individual strings and providing multiple naming
styles.

## Usage

``` r
clean_names_strings(input, style = "snake_case")
```

## Arguments

- input:

  A data frame, tibble, matrix, list, or character vector representing
  the names to be cleaned.

- style:

  A character string specifying the naming style to use. Available
  options are "snake_case" (default), "camel_case", and "simple_clean".

## Value

The object with cleaned names.

## See also

[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)

## Examples

``` r
library(data.table)
library(zoo)
#> 
#> Attaching package: ‘zoo’
#> The following objects are masked from ‘package:data.table’:
#> 
#>     yearmon, yearqtr
#> The following objects are masked from ‘package:base’:
#> 
#>     as.Date, as.Date.numeric
library(xts)
#> 
#> Attaching package: ‘xts’
#> The following objects are masked from ‘package:data.table’:
#> 
#>     first, last

# For data frame with snake_case (default)
data("iris")
cleaned_iris <- clean_names_strings(iris)
colnames(cleaned_iris)
#> [1] "sepal_length" "sepal_width"  "petal_length" "petal_width"  "species"     

# For data frame with camel_case
cleaned_iris_camel <- clean_names_strings(iris, style = "camel_case")
colnames(cleaned_iris_camel)
#> [1] "sepalLength" "sepalWidth"  "petalLength" "petalWidth"  "species"    

# For character vector
original_names <- c("Some Column", "Another-Column!", "Yet Another Column")
cleaned_names <- clean_names_strings(original_names, style = "simple_clean")
print(cleaned_names)
#> [1] "somecolumn"       "anothercolumn"    "yetanothercolumn"

# For matrix
mat <- matrix(1:4, ncol = 2)
colnames(mat) <- c("Some Column", "Another Column")
cleaned_mat <- clean_names_strings(mat)
colnames(cleaned_mat)
#> [1] "some_column"    "another_column"

# For list
lst <- list("Some Column" = 1, "Another Column" = 2)
cleaned_lst <- clean_names_strings(lst)
names(cleaned_lst)
#> [1] "some_column"    "another_column"

# For xts object
xts_obj <- xts(x = matrix(1:4, ncol = 2),
               order.by = as.Date('2021-01-01') + 0:1)
colnames(xts_obj) <- c("Some Column", "Another Column")
cleaned_xts <- clean_names_strings(xts_obj)
print(colnames(cleaned_xts))
#> [1] "some_column"    "another_column"

zoo_obj <- zoo(matrix(1:4, ncol = 2), order.by = 1:2)
colnames(zoo_obj) <- c("Some Column", "Another Column")
cleaned_zoo <- clean_names_strings(zoo_obj)
print(colnames(cleaned_zoo))
#> [1] "some_column"    "another_column"

# for Data table
dt <- data.table("Some Column" = 1:2, "Another Column" = 3:4)
cleaned_dt <- clean_names_strings(dt)
print(names(cleaned_dt))
#> [1] "some_column"    "another_column"
```
