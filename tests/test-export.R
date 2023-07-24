test_that("export correctly exports to csv, rds, dta, xlsx, RData formats", {

  test_dir <- "tests/testdata"

  # Testing csv format
  export(mtcars, paste0(test_dir, "/mtcars.csv"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.csv")))

  # Testing rds format
  export(mtcars, paste0(test_dir, "/mtcars.rds"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.rds")))

  # Testing dta format
  export(mtcars, paste0(test_dir, "/mtcars.dta"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.dta")))

  # Testing xlsx format
  export(mtcars, paste0(test_dir, "/mtcars.xlsx"))
  expect_true(file.exists(paste0(test_dir, "/mtcars.xlsx")))

  # Testing RData format
  export(list(mtcars = mtcars, iris = iris),
              paste0(test_dir, "/datasets.RData"))
  expect_true(file.exists(paste0(test_dir, "/datasets.RData")))
})
