library(testthat)
testthat::test_that("function restore_spaces produces correct result", {

  # Tests that the function produces exactly expected results.

  # Construct test data - artificial examples
  test_strings <- c(
    "abc",
    "loremIpsum",
    "LoremIpsum",
    ".dummy",
    "ParEnThesis(test)",
    "UkAndUs"
  )
  correct_res <- c(
    "Abc",
    "Lorem Ipsum",
    "Lorem Ipsum",
    ".dummy",
    "Par En Thesis(test)",
    "UK and US"
  )

  # Call the function.
  res <- restore_spaces(test_strings)

  # Compare test data vs function outputs.
  expect_equal(res, correct_res)
})
