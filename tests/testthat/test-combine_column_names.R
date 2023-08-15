
# 1. Test error when name_row_count is 0
testthat::test_that("Test error when name_row_count is 0", {
  df <- data.frame("Name" = c("Alice", "Bob", "Charlie"),
                   "Age" = c(25, 30, 22),
                   "Salary" = c(50000, 60000, 55000))

  testthat::expect_error(epiCleanr::combine_column_names(df, name_row_count = 0),
               "If name_row_count is 0, column names aren't split")
})

# 2. Test reshaping and transformation

# testthat::test_that("Test reshaping and transformation", {
#   df <- data.frame("Name" = c("Alice", "Bob", "Charlie"),
#                    NA, "Age" = c(25, 30, 22),
#                    NA, "Salary" = c(50000, 60000, 55000))
#
#   name_row_count <- 2
#   expected_reshaped <- data.frame(
#     original_columns = c("Name", "Age", "Salary"),
#     values = c("Alice", "25", "50000"),
#     column_position = c(1, 2, 3)
#   )
#
#   reshaped_names_df <- epiCleanr::combine_column_names(df, name_row_count)
#
#   testthat::expect_identical(reshaped_names_df, expected_reshaped)
# })

# # 3. Test sliding headers
# testthat::test_that("Test sliding headers", {
#   df <- data.frame("Name" = c("Alice", "Bob", "Charlie"),
#                    NA, NA,
#                    "Age" = c(25, 30, 22),
#                    NA, "Salary" = c(50000, 60000, 55000))
#
#   name_row_count <- 2
#   expected_slided <- data.frame(
#     original_columns = c("Name", "Age", "Salary"),
#     values = c("Alice", "25", "50000"),
#     column_position = c(1, 2, 3)
#   )
#
#   reshaped_names_df <- epiCleanr::combine_column_names(df,
#                                                        name_row_count = 2,
#                                                        slide_headers = TRUE)
#
#   testthat::expect_identical(reshaped_names_df, expected_slided)
#
# })


# 4. Test warning for potential NA in column names
# testthat::test_that("Test warning for potential NA in column names", {
#   # Load the test data from the Excel file
#   head_data <- epiCleanr::import("tests/testthat/testdata/header_test.xlsx")
#
#   # Capture the warning message
#   captured_warning <- NULL
#   withCallingHandlers(
#     {
#       epiCleanr::combine_column_names(head_data, name_row_count = 1)
#     },
#     warning = function(w) {
#       captured_warning <<- w$message
#     }
#   )
#
#   # Check if the captured warning message matches the expected warning
#   expected_warning <-
#     "Potential NA in column names; verify the name_row_count argument."
#   testthat::expect_equal(captured_warning, expected_warning)
# })
