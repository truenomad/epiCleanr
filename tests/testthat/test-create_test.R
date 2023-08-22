# Create a fake data frame for testing purposes
fake_epi_data <- data.frame(
  month = rep(1:12, 1200),
  year = rep(2000:2011, each = 12 * 100),
  polio_tests = sample(0:122, 14400, replace = TRUE),
  stringsAsFactors = FALSE
)

suppressMessages({
  testthat::test_that("create_test handles dimensions correctly", {
    my_tests <- create_test(dimension_test = c(14400, 3))
    result <- my_tests(fake_epi_data)
    testthat::expect_identical(result[[1]], 100)
  })

  testthat::test_that("create_test handles row duplicates correctly", {
    my_tests <- create_test(row_duplicates = TRUE)
    result <- my_tests(fake_epi_data)
    testthat::expect_identical(result[[2]], 0)
  })

  testthat::test_that("create_test handles column duplicates correctly", {
    my_tests <- create_test(col_duplicates = TRUE)
    result <- my_tests(fake_epi_data)
    testthat::expect_identical(result[[1]], 100)
  })

  testthat::test_that("create_test handles combinations correctly", {
    my_tests <- create_test(combinations_test = list(
      variables = c("month", "year"), expectation = 12 * 24
    ))

    result <- my_tests(fake_epi_data)
    testthat::expect_identical(result[[1]], 0)
  })

  testthat::test_that("create_test handles min threshold correctly", {
    my_tests <- create_test(min_threshold_test = list(polio_tests = 0))
    result <- my_tests(fake_epi_data)
    testthat::expect_identical(result[[1]], 100)
  })

  testthat::test_that("create_test handles max threshold correctly", {
    my_tests <- create_test(max_threshold_test = list(polio_tests = 122))
    result <- my_tests(fake_epi_data)
    testthat::expect_identical(result[[1]], 100)
  })

  testthat::test_that("create_test uses big_mark correctly in error messages", {
    # Define a data frame that will fail the dimension test
    test_data <- data.frame(x = 1:10000, y = 1:10000)

    my_tests <- create_test(dimension_test = c(100001, 2))

    messages <- capture_messages(my_tests(test_data))[1]

    actual <- paste(
      "\033[31mWarning! Test failed. Expected 100,001 rows",
      "and 2 columns, but got 10,000 rows and 2 columns.\033[39m\n"
    )

    testthat::expect_equal(writeLines(actual), writeLines(messages))
  })


  testthat::test_that("create_test detects and warns about duplicate rows", {
    # Create a data frame with duplicate rows
    test_data <- data.frame(x = c(1, 1, 2), y = c(3, 3, 4))

    my_tests <- create_test(row_duplicates = TRUE)

    messages <- capture_messages(my_tests(test_data))[1]

    expected <-
      paste(
        "\033[31mWarning! Test failed. Duplicate rows found.",
        "See output$duplicate_rows.\033[39m\n"
      )

    testthat::expect_equal(writeLines(expected), writeLines(messages))
  })

  testthat::test_that(
    "create_test detects no repeated columns and returns success message",
    {
      test_data <- data.frame(x = 1:3, y = 4:6)

      my_tests <- create_test(col_duplicates = TRUE)

      messages <- capture_messages(my_tests(test_data))[1]

      expected <- "\033[32mTest passed! No repeated columns found!\033[39m\n"

      testthat::expect_equal(writeLines(expected), writeLines(messages))
    }
  )

  testthat::test_that(
    "create_test throws an error when specified variables do not exist",
    {
      test_data <- data.frame(a = 1:3, b = 4:6)

      combinations_test <- list(variables = c("x", "y"), expectation = 6)

      my_tests <- create_test(combinations_test = combinations_test)

      actual_error <- as.character(capture_error(my_tests(test_data))[1])
      expected_error <-
        "Error! The following variables do not exist in the dataset: x, y"
      testthat::expect_equal(actual_error, expected_error)
    }
  )


  testthat::test_that(
    "create_test generates correct warning for failed combinations test",
    {
      # Create a data frame that will fail the combinations test
      test_data <- data.frame(month = 1:12, year = 2000:2003)

      combinations_test <- list(
        variables = c("month", "year"),
        expectation = 12 * 25
      )

      my_tests <- create_test(combinations_test = combinations_test)

      messages <- capture_messages(my_tests(test_data))[1]

      actual <- paste(
        "\033[31mWarning! Test failed. Expected 300 combinations",
        "but found 48 combinations for month, year.\033[39m\n"
      )

      testthat::expect_equal(writeLines(actual), writeLines(messages))
    }
  )

  testthat::test_that("create_test raises error if col does not exist in df", {
    # Create a data frame that lacks the specified column
    test_data <- data.frame(month = 1:12, year = 2000:2003)

    # Threshold test for a nonexistent column
    max_threshold_test <- list(nonexistent_column = 30)

    # Create a test function with the threshold_test
    my_tests <- create_test(max_threshold_test = max_threshold_test)

    actual_error <- as.character(capture_error(my_tests(test_data))[1])
    expected_error <-
      "Error! Column nonexistent_column does not exist in the dataset."
    testthat::expect_equal(actual_error, expected_error)
  })
})
