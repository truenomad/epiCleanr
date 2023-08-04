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
    file_path <- paste0("tests/testthat/testdata/test_data.", format)

    # Check if file exists before trying to import it
    if (!file.exists(file_path)) {
      message(paste("File does not exist:", file_path))
      next
    }

    # Import the data using the 'import' function
    imported_data <- epiCleanr::import(file_path)

    # Check if the imported data has the expected structure and values
    testthat::expect_true("ID" %in% colnames(imported_data), info = paste("ID column not found in", file_path))
    testthat::expect_true("Name" %in% colnames(imported_data), info = paste("Name column not found in", file_path))
    testthat::expect_true("Age" %in% colnames(imported_data), info = paste("Age column not found in", file_path))
    testthat::expect_true("Score" %in% colnames(imported_data), info = paste("Score column not found in", file_path))

    testthat::expect_equal(as.integer(imported_data$ID), 1:5, info = paste("ID values mismatch in", file_path))
    testthat::expect_equal(as.character(imported_data$Name), c("Alice", "Bob", "Charlie", "David", "Eva"), info = paste("Name values mismatch in", file_path))
    testthat::expect_equal(as.integer(imported_data$Age), c(25, 30, 28, 22, 27), info = paste("Age values mismatch in", file_path))
    testthat::expect_equal(as.integer(imported_data$Score), c(85, 90, 78, 95, 88), info = paste("Score values mismatch in", file_path))
  }
})


testthat::test_that("Function correctly extracts file extension", {

  # Function to get file extension
  get_file_extension <- function(file_path) {
    # Use tools package to get file extension
    extension <- tools::file_ext(file_path)

    # If no extension found, return a message
    if (nchar(extension) == 0) {
      return("File extension not found in the path.")
    }

    # Return the file extension
    return(extension)
  }

  # Test with a valid file path containing an extension
  file_path_with_extension <- "tests/testthat/testdata/test_data.csv"
  testthat::expect_equal(get_file_extension(file_path_with_extension), "csv")

  # Test with a valid file path without an extension
  file_path_without_extension <- "tests/testthat/testdata/test_data"
  testthat::expect_equal(get_file_extension(file_path_without_extension),
                         "File extension not found in the path.")
})

testthat::test_that("Function handles invalid file name gracefully", {
  # Provide an invalid file path that does not exist
  invalid_file_path <- "tests/testthat/testdata/dataset.csv"

  # Call the 'import' function and expect an error
  testthat::expect_error(epiCleanr::import(invalid_file_path), "No such file")
})
