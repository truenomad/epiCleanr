
# Sample data for testing
fake_data <- tidyr::expand_grid(state = state.name,
                                month = 1:12, year = 2000:2023) |>
  dplyr::mutate(
    measles = sample(0:1000, size = dplyr::n(), replace = TRUE),
    polio = sample(0:500, size = dplyr::n(), replace = TRUE),
    malaria = sample(0:300, size = dplyr::n(), replace = TRUE),
    cholera = sample(0:700, size = dplyr::n(), replace = TRUE)
  ) |>
  dplyr::mutate(across(c(measles, polio, malaria, cholera),
                      ~ replace(., sample(1:dplyr::n(),
                                          size = dplyr::n() * 0.4), NA)))

# Testing 'plot_missing_rates' function

# Test 1: Check for error if 'time_var' is NULL or does not exist in the data
test_that("Error is thrown if 'time_var' is NULL or not in data", {
  expect_error(plot_missing_rates(fake_data, NULL),
               "A valid 'time_var' must be provided")
  expect_error(plot_missing_rates(fake_data, "invalid_time_var"),
               "A valid 'time_var' must be provided")
})

# Test 2: Check error if 'group_var' provided with >1 variable in 'vars'
test_that("Error if 'group_var' provided with >1 variable in 'vars'", {
  expect_error(
    plot_missing_rates(
      fake_data, "year", "state",
      c("polio", "measles")),
    "When 'group_var' is provided only one variable can be specified in 'vars'")
})

# Test 3: Check that the function returns a ggplot object
test_that("Returns ggplot object", {
  plot <- plot_missing_rates(fake_data, "year", "state", "polio", TRUE)
  expect_s3_class(plot, "ggplot")
})

# Test 4: Check that the function works without 'group_var' and 'vars'
test_that("Works without 'group_var' and 'vars'", {
  plot <- plot_missing_rates(fake_data, "year")
  expect_s3_class(plot, "ggplot")
})

# Test 5: Check that the function works with all provided parameters
test_that("Works with all provided parameters", {
  plot <- plot_missing_rates(fake_data, "year", "state", "polio", TRUE)
  expect_s3_class(plot, "ggplot")
})


# Test 6: Check the title construction with 'use_rep_rate = TRUE'
test_that("Title construction with 'use_rep_rate = TRUE'", {
  plot <- plot_missing_rates(fake_data, "year", "state", "polio", TRUE)
  expect_match(plot$labels$title, "Reporting rate of polio by year and state")
})

# Test 7: Check the title construction with 'use_rep_rate = FALSE'
test_that("Title construction with 'use_rep_rate = FALSE'", {
  plot <- plot_missing_rates(fake_data, "year", "state", "polio", FALSE)
  expect_match(
    plot$labels$title,
    "The proportion of missing data for polio by year and state")
})

# Test 8: Check the title construction without 'group_var'
test_that("Title construction without 'group_var'", {
  plot <- plot_missing_rates(fake_data, "year", "state", "polio", TRUE)
  expect_match(plot$labels$title, "Reporting rate of polio by year")
})

# Test 9: Check the y-axis label with 'group_var'
test_that("Y-axis label with 'group_var'", {
  plot <- plot_missing_rates(
    fake_data, "year", "state", "polio", FALSE)
  expect_equal(plot$labels$y, "State")
})

# Test 10: Check title construction with and without 'vars'"
test_that("Title construction with and without 'vars'", {

  # Case when 'vars' is provided
  plot_with_vars <- plot_missing_rates(
    fake_data, "year", "state", "polio", FALSE)
  expect_true(
    grepl("The proportion of missing data for polio by year and state",
          plot_with_vars$labels$title))

  # Case when 'vars' is not provided
  plot_without_vars <- plot_missing_rates(
    fake_data, time_var = "year", vars = NULL, use_rep_rate = FALSE)
  expect_true(grepl("year", plot_without_vars$labels$title))
})

# Test 11: Check title construction when there more than one var
test_that("Title is constructed correctly with remaining variables", {
  # Call your function with appropriate arguments

  fake_data2 <- fake_data |> select(-polio)
  plot <- plot_missing_rates(fake_data2,
                             time_var = "year", vars = NULL,
                             use_rep_rate = FALSE)

  # Construct the expected title
  expected_title <- paste("The proportion of missing data for state,",
                          "month, measles, malaria and cholera by year ")

  # Assert that the actual title matches the expected title
  expect_identical(plot$labels$title, expected_title)
})

# Test 12: Check title construction when there to many vars
test_that("Title construction when there are too many variables", {
  plot <- plot_missing_rates(fake_data, "year", NULL, NULL, FALSE)
  expect_match(
    plot$labels$title,
    "The proportion of missing data for various variables by year"
  )
})
