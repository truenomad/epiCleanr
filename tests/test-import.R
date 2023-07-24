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

    # Import the data using the 'import' function
    imported_data <- import(file_path)

    # Check if the imported data has the expected structure and values
    testthat::test_that(paste("Imports data from", toupper(format), "correctly"), {
      expect_true("ID" %in% names(imported_data))
      expect_true("Name" %in% names(imported_data))
      expect_true("Age" %in% names(imported_data))
      expect_true("Score" %in% names(imported_data))

      expect_equal(as.integer(imported_data$ID), 1:5)
      expect_equal(as.character(imported_data$Name), c("Alice", "Bob", "Charlie", "David", "Eva"))
      expect_equal(as.integer(imported_data$Age), c(25, 30, 28, 22, 27))
      expect_equal(as.integer(imported_data$Score), c(85, 90, 78, 95, 88))
    })
  }
})

testthat::test_that("Function correctly extracts file extension", {

  get_file_extension <- function(file_path) {
    # Use regex to extract the file extension from the file path
    extension <- sub(".+\\.([^.]+)$", "\\1", file_path)

    # Check if a file extension is found or not
    if (extension == file_path) {
      stop("File extension not found in the path.")
    }

    # Return the file extension
    extension
  }

  # Test with a valid file path containing an extension
  file_path_with_extension <- "tests/testdata/test_data.csv"
  expect_equal(get_file_extension(file_path_with_extension), "csv")

  # Test with a valid file path without an extension
  file_path_without_extension <- "tests/testdata/test_data"
  expect_error(get_file_extension(file_path_without_extension),
               "File extension not found in the path.")
})


testthat::test_that("Function handles invalid file name gracefully", {
  # Provide an invalid file path that does not exist
  invalid_file_path <- "tests/testdata/dataset.csv"

  # Call the 'import' function and expect an error
  expect_error(import(invalid_file_path), "No such file")

})

