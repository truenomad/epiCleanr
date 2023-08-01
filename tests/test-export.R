testthat::test_that("export correctly exports to csv, rds, dta, xlsx, RData formats", {

  test_dir <- "tests/testdata"

  # Testing csv format
  epiCleanr::export(mtcars, paste0(test_dir, "/mtcars.csv"))
  testthat::expect_true(file.exists(paste0(test_dir, "/mtcars.csv")))

  # Testing rds format
  epiCleanr::export(mtcars, paste0(test_dir, "/mtcars.rds"))
  testthat::expect_true(file.exists(paste0(test_dir, "/mtcars.rds")))

  # Testing dta format
  epiCleanr::export(mtcars, paste0(test_dir, "/mtcars.dta"))
  testthat::expect_true(file.exists(paste0(test_dir, "/mtcars.dta")))

  # Testing xlsx format
  epiCleanr::export(mtcars, paste0(test_dir, "/mtcars.xlsx"))
  testthat::expect_true(file.exists(paste0(test_dir, "/mtcars.xlsx")))

  # Testing RData format
  epiCleanr::export(list(mtcars = mtcars, iris = iris),
              paste0(test_dir, "/datasets.RData"))
  testthat::expect_true(file.exists(paste0(test_dir, "/datasets.RData")))
})
