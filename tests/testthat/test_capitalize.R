test_that("function capitalize produces correct result", {

  # Tests that the function produces exactly expected results.

  # Construct test data - artificial examples
  test_strings <- c(
    "abc",
    "lorem ipsum",
    ".dummy"
  )
  correct_res <- c(
    "Abc",
    "Lorem ipsum",
    ".dummy"
  )

  # Call the function.
  res <- capitalize(test_strings)

  # Compare test data vs function outputs.
  expect_equal(res, correct_res)
})
