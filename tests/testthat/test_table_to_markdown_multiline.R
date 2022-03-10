library(climate.narrative)
testthat::test_that("function table_to_markdown_multiline produces correct result", {

  # Tests that the function produces exactly expected results.

  # Construct test data - artificial example
  test_df <- data.frame(
    x1 = "abc",
    x2 = "def ghijk",
    x3.123 = "foo"
  )
  correct_res <- paste0(
    "+-------+-------+-------+\n",
    "| x1    | x2    | x3    |\n",
    "|       |       | 123   |\n",
    "+=======+=======+=======+\n",
    "| abc   | def   | foo   |\n",
    "|       | ghijk |       |\n",
    "+-------+-------+-------+\n"
  )

  # Call the function.
  res <- table_to_markdown_multiline(test_df, col_widths = c(7, 7, 7))

  # Compare test data vs function outputs.
  expect_equal(res, correct_res)
})
