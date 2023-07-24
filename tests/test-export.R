library(testthat)

# Load the export_data function
source("export_data.R")

test_that("export_data correctly exports to csv, rds, dta, xlsx, RData formats", {

  test_dir <- "tests/testdata"

  # Testing csv format
  export_data(mtcars, paste0(test_dir, "/mtcars.csv"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.csv")))

  # Testing rds format
  export_data(mtcars, paste0(test_dir, "/mtcars.rds"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.rds")))

  # Testing dta format
  export_data(mtcars, paste0(test_dir, "/mtcars.dta"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.dta")))

  # Testing xlsx format
  export_data(mtcars, paste0(test_dir, "/mtcars.xlsx"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.xlsx")))

  # Testing RData format
  export_data(list(mtcars = mtcars, iris = iris), paste0(test_dir, "/datasets.RData"))
  expect_true(file.exists(paste0(test_dir, "/datasets.RData")))
})
