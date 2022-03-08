library(climate.narrative)
testthat::test_that("function string_format_lines produces correct result", {

  # Tests that the function produces exactly expected results.

  # Construct test data - artificial examples
  test_strings <- c(
    "abc",
    "abc def",
    "ab<br>c d ef ghij"
  )
  correct_res <- c(
    "abc",
    "abc   def",
    "- ab   - c def    ghij"
  )

  # Call the function.
  res <- test_strings
  for (i in 1: length(res)) {
    res[i] <- string_format_lines(test_strings[i], 6)
  }

  # Compare test data vs function outputs.
  expect_equal(res, correct_res)

  # additionally, ensure warning appears as expected
  expect_warning(string_format_lines("abcdef", 5))
})
