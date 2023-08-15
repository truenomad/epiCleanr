
# 1. Test for different file formats

# Define the test cases for the 'import' function
testthat::test_that("Function imports supported file formats correctly", {
  # List of supported file formats to test
  supported_formats <- c(
    "csv", "tsv", "txt", "csvy", "sas7bdat", "sav",
    "dta", "xpt", "xlsx", "RData", "rds", "tsv"
  )

  # Loop through each supported format and test importing the file
  for (format in supported_formats) {
    # File path for the test data with the specific format
    file_path <- paste0("testdata/test_data.", format)

    # Check if file exists before trying to import it
    if (!file.exists(file_path)) {
      message(paste("File does not exist:", file_path))
      next
    }

    # Import the data using the 'import' function
    imported_data <- epiCleanr::import(file_path)

    # Check if the imported data has the expected structure and values
    testthat::expect_true("ID" %in% colnames(imported_data),
                          info = paste("ID column not found in", file_path))
    testthat::expect_true("Name" %in% colnames(imported_data),
                          info = paste("Name column not found in", file_path))
    testthat::expect_true("Age" %in% colnames(imported_data),
                          info = paste("Age column not found in", file_path))
    testthat::expect_true("Score" %in% colnames(imported_data),
                          info = paste("Score column not found in", file_path))

    testthat::expect_equal(as.integer(imported_data$ID), 1:5,
                           info = paste("ID values mismatch in", file_path))
    testthat::expect_equal(as.character(imported_data$Name),
                           c("Alice", "Bob", "Charlie", "David", "Eva"),
                           info = paste("Name values mismatch in", file_path))
    testthat::expect_equal(as.integer(imported_data$Age),
                           c(25, 30, 28, 22, 27),
                           info = paste("Age values mismatch in", file_path))
    testthat::expect_equal(as.integer(imported_data$Score),
                           c(85, 90, 78, 95, 88),
                           info = paste("Score values mismatch in", file_path))
  }
})

# 2. Test for Unsupported Formats
testthat::test_that("Function throws error for unsupported file formats", {
  file_path <- "testdata/test_data.xyz"
  expect_error(
    epiCleanr::import(file_path),
    paste0(
      "File format 'xyz' not supported by 'rio'. ",
      "Please refer to the package documentation for a full list",
      "of supported formats."
    )
  )
})

# 3. Test for File Without Extension
testthat::test_that("Function throws error for file without extension", {
  file_path <- "testthat/testdata/test_data"
  expect_error(
    epiCleanr::import(file_path),
    paste("The provided file has no extension.",
          "Please specify a file with a valid extension.")
  )
})

# 4. Testing URL imports
testthat::test_that("Function imports data from URLs correctly", {

  # The raw URL for the mtcars.csv file from GitHub
  github_url <- paste0("https://raw.githubusercontent.com/truenomad/",
                       "epiCleanr/main/tests/testthat/testdata/mtcars.csv")

  imprt_data <- epiCleanr::import(github_url)
  imprt_data_compare <- epiCleanr::import("testdata/mtcars.csv")

  expect_identical(imprt_data, imprt_data_compare)
})


# 5. Test to see if the relevant packages for the formats exist
testthat::test_that("All supported formats by rio are installed on package load", {

  # Unload the package if it's already loaded (to ensure the .onLoad function runs again)
  if ("epiCleanr" %in% loadedNamespaces()) {
    unloadNamespace("epiCleanr")
  }

  # Load the package
  library(epiCleanr)

  installed_format <- rio::install_formats()

  # Check if rio::install_formats() returns TRUE
  testthat::expect_true(installed_format)
})

