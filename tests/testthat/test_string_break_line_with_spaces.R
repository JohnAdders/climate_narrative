library(climate.narrative)
testthat::test_that("function string_break_line_with_spaces produces correct result", {

  # Tests that the function produces exactly expected results.

  # Construct test data - artificial examples
  test_strings <- c(
    "ab c",
    "abcdef"
  )
  correct_res <- c(
    "ab   c",
    "ab   def"
  )

  # Call the function.
  res <- test_strings
  for (i in 1:length(res)) {
    res[i] <- string_break_line_with_spaces(test_strings[i], 5, 3)
  }

  # Compare test data vs function outputs.
  expect_equal(res, correct_res)
})
