testthat::test_that("epiCleanr::clean_names function works correctly", {
  # Test 1: Check if spaces are replaced with underscores
  testthat::expect_equal(epiCleanr::clean_names("Hello World"), "hello_world")

  # Test 2: Check if punctuation is replaced with underscores
  testthat::expect_equal(epiCleanr::clean_names("Hello.World"), "hello_world")

  # Test 3: Check if mix of spaces and punctuation are replaced with underscores
  testthat::expect_equal(epiCleanr::clean_names("Hello World!"), "hello_world")

  # Test 4: Check if camel case strings are correctly handled
  testthat::expect_equal(epiCleanr::clean_names("HelloWorld"), "helloworld")

  # Test 5: Check if underscores are removed from start or end of string
  testthat::expect_equal(epiCleanr::clean_names("_Hello_World_"), "hello_world")

  # Test 6: Check if function converts to lowercase
  testthat::expect_equal(epiCleanr::clean_names("HELLO"), "hello")
})
